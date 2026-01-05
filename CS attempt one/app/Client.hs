{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Map as Map

-- Global MVar to hold WebSocket connection
connMVar :: MVar WS.Connection
connMVar = unsafePerformIO newEmptyMVar

-- Canvas dimensions
screenWidth :: Double
screenWidth = 750

screenHeight :: Double
screenHeight = 650

squareSize :: Double
squareSize = 50

-- Main client entry point
mainClient :: String -> Int -> IO ()
mainClient ip port = do
  liftIO $ void $ forkIO $ do
    withSocketsDo $ WS.runClient ip port "/" $ \conn -> do
      putStrLn "Connected!"
      putMVar connMVar conn
      let waitForever = threadDelay maxBound
      waitForever

  putStrLn "Starting game..."
  M.run $ M.startApp app
 where
  app =
    (M.component initModel update view)
      { M.initialAction = Just MsgGetTime
      , M.subs =
          [ M.keyboardSub handleKey
          , serverSub
          ]
      }

-- Subscription that listens for WebSocket messages
serverSub :: M.Sub Msg
serverSub dispatch = do
  dispatch MsgNoOp

  JSaddle.liftJSM $ forever $ do
    conn <- M.liftIO $ readMVar connMVar
    raw <- M.liftIO $ WS.receiveData conn
    M.liftIO $ putStrLn $ "Got raw: " <> T.unpack raw

    case Common.textToViewData raw of
      Just viewData ->
        dispatch (MsgUpdateViewData viewData)
      Nothing -> do
        M.liftIO $ putStrLn $ "ERROR: Cannot parse " <> T.unpack raw
        dispatch MsgNoOp

-- Convert keyboard key codes to KeyState
handleKey :: IntSet -> Msg
handleKey intSet =
  MsgUpdateKeyState $
    Common.KeyState
      { Common.left = IntSet.member 37 intSet
      , Common.up = IntSet.member 38 intSet
      , Common.right = IntSet.member 39 intSet
      , Common.down = IntSet.member 40 intSet
      , Common.placeBomb = IntSet.member 32 intSet
      }

-- Application state model
data Model = Model
  { viewData :: Maybe Common.ViewData
  , keyState :: Common.KeyState
  }
  deriving (Show, Eq)

-- Initial model
initModel :: Model
initModel =
  Model
    { viewData = Nothing
    , keyState = Common.defaultKeyState
    }

-- Messages
data Msg
  = MsgGetTime
  | MsgSetTime Double
  | MsgUpdateKeyState Common.KeyState
  | MsgUpdateViewData Common.ViewData
  | MsgNoOp
  deriving (Show, Eq)

-- Send client input to server
sendClientInput :: Model -> M.Transition Model Msg
sendClientInput model =
  M.io_ $ M.liftIO $ do
    conn <- readMVar connMVar
    WS.sendTextData
      conn
      ( Common.clientInputToText
          (Common.ClientInput{Common.keyState = model.keyState})
      )

-- Update function
update :: Msg -> M.Transition Model Msg

update MsgGetTime = do
  M.io $ do
    date <- M.newDate
    milli <- M.getMilliseconds date
    pure $ MsgSetTime milli

update (MsgSetTime _) = do
  M.issue MsgGetTime

update (MsgUpdateKeyState keyState) = do
  M.modify $ \model -> model{keyState = keyState}
  model <- M.get
  sendClientInput model

update (MsgUpdateViewData viewData) = do
  M.modify $ \model -> model{viewData = Just viewData}

update MsgNoOp = do
  pure ()

-- View function
view :: Model -> M.View Model Msg
view model =
  H.div_
    []
    [ H.textarea_ [P.rows_ "3", P.cols_ "40"] [M.text $ M.ms $ show (keyState model)]
    , H.br_ []
    , timerView model
    , Canvas.canvas
        [ P.width_ (M.ms screenWidth)
        , P.height_ (M.ms screenHeight)
        ]
        (\_ -> pure ())
        (viewCanvas model)
    ]

timerView :: Model -> M.View Model Msg
timerView model =
  case model.viewData of
    Nothing ->
      H.p_ [] [ M.text "Waiting for server..." ]

    Just vd ->
      H.p_ [] [ M.text (M.ms (displayTimer vd.gameTimer)) ]

-- Display timer in mm:ss format
displayTimer :: Float -> String
displayTimer gameTimer =
  let timer = round gameTimer
      (m, s) = divMod timer 60
      mPadded = if m < 10 then "0" ++ show m else show m
      sPadded = if s < 10 then "0" ++ show s else show s
  in "Time Left: " ++ mPadded ++ ":" ++ sPadded

-- Canvas rendering
viewCanvas :: Model -> () -> Canvas.Canvas ()
viewCanvas model () = do
  case model.viewData of
    Nothing -> do
      Canvas.fillStyle (Canvas.ColorArg (RGB 255 255 255))
      Canvas.font "24px arial"
      Canvas.textAlign Canvas.TextAlignCenter
      Canvas.fillText ("Waiting for other player...", screenWidth / 2, screenHeight / 2)
    Just vd -> do
      if vd.gameOver == Common.YesGO
        then do
          -- Game over screen
          Canvas.fillStyle (Canvas.ColorArg (RGB 0 0 0))
          Canvas.fillRect (0, 0, screenWidth, screenHeight)
          
          -- Draw all players in black
          forM_ (Map.elems vd.players) $ \p -> do
            Canvas.fillStyle (Canvas.ColorArg (RGB 0 0 0))
            let ps = p.playerSize
            let px = p.playerX - ps / 2
            let py = p.playerY - ps / 2
            Canvas.fillRect (px, py, ps, ps)
          
          Canvas.fillStyle (Canvas.ColorArg (RGB 255 0 0))
          Canvas.font "48px Arial"
          Canvas.textAlign Canvas.TextAlignCenter
          Canvas.fillText ("GAME OVER", screenWidth / 2, screenHeight / 2)
        else do
          -- Clear screen
          Canvas.fillStyle (Canvas.ColorArg (RGB 0 0 0))
          Canvas.fillRect (0, 0, screenWidth, screenHeight)
          
          -- Draw timer
          Canvas.fillStyle (Canvas.ColorArg (RGB 255 255 255))
          Canvas.font "16px Arial"
          Canvas.textAlign Canvas.TextAlignLeft
          Canvas.fillText (M.ms $ displayTimer vd.gameTimer, 10, 20)
          
          -- Draw obstacles
          forM_ vd.obstacles $ \obs -> do
            if obs.isHardBlock
              then Canvas.fillStyle (Canvas.ColorArg (RGB 64 64 64))
              else Canvas.fillStyle (Canvas.ColorArg (RGB 192 192 192))
            Canvas.fillRect (obs.obstacleX - 25, obs.obstacleY - 25, squareSize, squareSize)
          
          -- Draw bombs
          forM_ vd.bombs $ \b -> do
            if b.isDetonated == Common.Ticking
              then Canvas.fillStyle (Canvas.ColorArg (RGB 122 22 22))
              else Canvas.fillStyle (Canvas.ColorArg (RGB 193 35 35))
            Canvas.fillRect (b.bombX - 25, b.bombY - 25, b.bombSize, b.bombSize)
          
          -- Draw players
          forM_ (Map.elems vd.players) $ \p -> do
            Canvas.fillStyle (Canvas.ColorArg (RGB 51 124 179))
            let ps = p.playerSize
            let px = p.playerX - ps / 2
            let py = p.playerY - ps / 2
            Canvas.fillRect (px, py, ps, ps)