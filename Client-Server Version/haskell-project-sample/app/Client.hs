{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Client where

import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P

import Miso.CSS (Color (..))
import qualified Miso.Canvas as Canvas

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified Language.Javascript.JSaddle as JSaddle
import qualified Miso.CSS as CSS
import Miso.Canvas (TextAlignType (TextAlignCenter))

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

import qualified Common
import Data.Foldable (forM_)
import Data.Functor (void)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)

-- Global MVar to hold the WebSocket connection, initialized empty
-- This allows the WebSocket connection to be accessed from different parts of the code
connMVar :: MVar WS.Connection
connMVar = unsafePerformIO newEmptyMVar

-- Canvas dimensions in pixels
screenWidth :: Double
screenWidth = 300

screenHeight :: Double
screenHeight = 300

-- Size of each player sprite in pixels
squareSize :: Double
squareSize = 40

-- Main entry point for the client, takes server IP and port
mainClient :: String -> Int -> IO ()
mainClient ip port = do
  -- Fork a background thread to handle WebSocket connection
  liftIO $ void $ forkIO $ do
    withSocketsDo $ WS.runClient ip port "/" $ \conn -> do
      putStrLn "Connected!"
      -- Store the connection in the MVar so other parts of the app can use it
      putMVar connMVar conn
      -- Keep the thread alive indefinitely to maintain the connection
      let waitForever = threadDelay maxBound
      waitForever

  putStrLn "Starting game..."
  -- Start the Miso application
  M.run $ M.startApp app
 where
  app =
    (M.component initModel update view)
      { M.initialAction = Just MsgGetTime -- Start by getting the current time
      , M.subs =
          [ M.keyboardSub handleKey -- Subscribe to keyboard events
          , testSub -- Subscribe to WebSocket messages
          ]
      }

-- Subscription that listens for WebSocket messages from the server
testSub :: M.Sub Msg
testSub dispatch = do
  -- Dispatch a no-op to initialize
  dispatch MsgNoOp

  -- Run forever in JavaScript monad
  JSaddle.liftJSM $ forever $ do
    -- Read the WebSocket connection from the MVar (blocks until available)
    conn <- M.liftIO $ readMVar connMVar
    -- Receive data from the server
    raw <- M.liftIO $ WS.receiveData conn
    M.liftIO $ putStrLn $ "Got raw: " <> T.unpack raw

    -- Try to parse the received data as ViewData
    case Common.textToViewData raw of
      Just viewData ->
        -- Successfully parsed, dispatch update message
        dispatch (MsgUpdateViewData viewData)
      Nothing -> do
        -- Failed to parse, log error
        M.liftIO $ putStrLn $ "ERROR: Cannot parse " <> T.unpack raw
        dispatch MsgNoOp

-- Holds the sprite images for animation frames
data CanvasState = CanvasState
  { poring1 :: M.Image
  , poring2 :: M.Image
  , poring3 :: M.Image
  , poring4 :: M.Image
  }

-- Allow CanvasState to be converted from JavaScript values
instance JSaddle.FromJSVal CanvasState where
  fromJSVal v = do
    (p1, p2, p3, p4) <- JSaddle.fromJSValUnchecked v
    pure $ Just $ CanvasState p1 p2 p3 p4

-- Allow CanvasState to be converted to JavaScript values
instance JSaddle.ToJSVal CanvasState where
  toJSVal st =
    JSaddle.toJSVal
      ( st.poring1
      , st.poring2
      , st.poring3
      , st.poring4
      )

-- Initialize the canvas by loading all sprite images
initCanvas :: M.DOMRef -> Canvas.Canvas CanvasState
initCanvas _ = JSaddle.liftJSM $ do
  -- Load each frame of the poring sprite animation
  poring1 <- M.newImage "resources/poring1.gif"
  poring2 <- M.newImage "resources/poring2.gif"
  poring3 <- M.newImage "resources/poring3.gif"
  poring4 <- M.newImage "resources/poring4.gif"
  -- Return the canvas state with all loaded images
  pure $
    CanvasState
      { poring1
      , poring2
      , poring3
      , poring4
      }

-- Convert keyboard key codes to KeyState
-- Arrow keys: 37=left, 38=up, 39=right, 40=down, 32=space
handleKey :: IntSet -> Msg
handleKey intSet =
  MsgUpdateKeyState $
    Common.KeyState
      { Common.left = IntSet.member 37 intSet   -- Left arrow
      , Common.up = IntSet.member 38 intSet     -- Up arrow
      , Common.right = IntSet.member 39 intSet  -- Right arrow
      , Common.down = IntSet.member 40 intSet   -- Down arrow
      , Common.placeBomb = IntSet.member 32 intSet   -- Spacebar
      }

-- Application state model
data Model = Model
  { viewData :: Maybe Common.ViewData  -- Game state from server (Nothing until first update)
  , keyState :: Common.KeyState        -- Current keyboard input state
  }
  deriving (Show, Eq)

-- Initial model with no view data and default key state
initModel :: Model
initModel =
  Model
    { viewData = Nothing
    , keyState = Common.defaultKeyState
    }

-- Messages that can update the model
data Msg
  = MsgGetTime                              -- Request current time
  | MsgSetTime Double                       -- Set current time (for animation timing)
  | MsgUpdateKeyState Common.KeyState       -- Update keyboard state
  | MsgUpdateViewData Common.ViewData       -- Update game state from server
  | MsgNoOp                                 -- No operation
  deriving (Show, Eq)

-- Send the current client input (key state) to the server via WebSocket
sendClientInput :: Model -> M.Transition Model Msg
sendClientInput model =
  M.io_ $ M.liftIO $ do
    -- Get the WebSocket connection
    conn <- readMVar connMVar

    -- Send the current key state to the server as JSON text
    WS.sendTextData
      conn
      ( Common.clientInputToText
          (Common.ClientInput{Common.keyState = model.keyState})
      )

-- Update function: handles all messages and updates the model
update :: Msg -> M.Transition Model Msg
---
-- When MsgGetTime is received, get the current time in milliseconds
update MsgGetTime = do
  M.io $ do
    date <- M.newDate
    milli <- M.getMilliseconds date
    pure $ MsgSetTime milli
---
-- When time is set, check if any player changed direction and play sound if so
update (MsgSetTime _) = do
  model <- M.get

  case model.viewData of
    Nothing ->
      -- No game data yet, do nothing
      M.io $ pure MsgNoOp
    Just viewData -> do
      -- Check if any player changed direction (to trigger jump sound)
      if any (\p -> p.changedDirection) viewData.players
        then M.io $ do
          -- Create and play a jump sound effect
          cons <- JSaddle.jsg ("Audio" :: String)
          audio <- JSaddle.new cons ["resources/jump.wav" :: String]
          _ <- audio JSaddle.# ("play" :: String) $ ()
          pure MsgNoOp
        else do
          M.io $ pure MsgNoOp

  -- Request time again for next frame (animation loop)
  M.issue MsgGetTime
---
-- When keyboard state changes, update the model and send to server
update (MsgUpdateKeyState keyState) = do
  M.modify $ \model -> model{keyState = keyState}

  -- Send the updated input to the server
  model <- M.get
  sendClientInput model
---
-- When view data is received from server, update the model
update (MsgUpdateViewData viewData) = do
  M.modify $ \model -> model{viewData = Just viewData}
---
-- No operation message, do nothing
update MsgNoOp = do
  pure ()

-- View function: renders the HTML/Canvas UI
view :: Model -> M.View Model Msg
view model =
  H.div_
    []
    [ -- Debug textarea showing the current model state
      H.textarea_ [P.rows_ "20", P.cols_ "40"] [M.text $ M.ms $ show model]
    , H.br_ []
    , -- Canvas element where the game is rendered
      Canvas.canvas
        [ P.width_ (M.ms screenWidth)
        , P.height_ (M.ms screenHeight)
        ]
        initCanvas      -- Initialize canvas with sprites
        (viewCanvas model)  -- Render function
    , -- Hidden images to preload sprites (so they're ready when canvas needs them)
      H.img_
        [ CSS.style_ [CSS.display "none"]
        , P.src_ "resources/poring1.gif"
        ]
    , H.img_
        [ CSS.style_ [CSS.display "none"]
        , P.src_ "resources/poring2.gif"
        ]
    , H.img_
        [ CSS.style_ [CSS.display "none"]
        , P.src_ "resources/poring3.gif"
        ]
    , H.img_
        [ CSS.style_ [CSS.display "none"]
        , P.src_ "resources/poring4.gif"
        ]
    ]

-- Render the canvas content (called every frame)
viewCanvas :: Model -> CanvasState -> Canvas.Canvas ()
viewCanvas model canvasState = do
  -- Draw black background to clear previous frame
  Canvas.fillStyle (Canvas.ColorArg (RGB 0 0 0))
  Canvas.fillRect (0, 0, screenWidth, screenHeight)

  case model.viewData of
    Nothing -> do
      -- No game data yet, show waiting message
      Canvas.fillStyle (Canvas.ColorArg (RGB 255 255 255))
      Canvas.font "24px arial"
      Canvas.textAlign TextAlignCenter
      Canvas.fillText ("Waiting for other player...", screenWidth / 2, screenHeight / 2)
    Just viewData -> do
      -- Draw each player sprite at their current position
      forM_ viewData.players $ \player -> do
        -- Draw the animated sprite at the player's x,y position
        Canvas.drawImage' (poringSprite viewData, player.x, player.y, squareSize, squareSize)
 where
  -- Select which animation frame to show based on server ticks
  -- Dividing by 8 slows down the animation, mod 4 cycles through 4 frames
  poringSprite :: Common.ViewData -> M.Image
  poringSprite viewData
    | (viewData.serverTicks `div` 8) `mod` 4 == 0 = canvasState.poring1
    | (viewData.serverTicks `div` 8) `mod` 4 == 1 = canvasState.poring2
    | (viewData.serverTicks `div` 8) `mod` 4 == 2 = canvasState.poring3
    | otherwise = canvasState.poring4