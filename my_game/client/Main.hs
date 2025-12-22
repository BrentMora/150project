{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- JSON handling
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- GHCJS FFI for JavaScript interop
import GHCJS.Foreign.Callback
import JavaScript.Web.WebSocket

-- Miso framework for reactive UI
import Miso
import Miso.String (MisoString, ms, fromMisoString)

-- Our shared game types
import Game.Types

-- | Client application model
-- Contains all state needed by the client UI
data Model = Model
  { modelWs :: Maybe WebSocket      -- WebSocket connection (Nothing until connected)
  , modelGameState :: Maybe GameState  -- Current game state from server
  , modelPlayerId :: Maybe Text     -- This client's assigned player ID
  , modelPlayerName :: Text         -- Name entered by user
  , modelConnected :: Bool          -- Whether we're connected to server
  } deriving (Eq)

-- | Actions that can occur in the application
data Action
  = NoOp                          -- No operation
  | UpdateName Text               -- User typed in name field
  | Connect                       -- User clicked connect button
  | HandleWsOpen                  -- WebSocket connection opened
  | HandleWsMessage MisoString    -- Received message from server
  | HandleWsClose                 -- WebSocket connection closed
  | MovePlayer Direction          -- Send movement command to server
  | KeyDown Int                   -- Keyboard key pressed

-- | Application entry point
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model = Model Nothing Nothing Nothing "" False  -- Start disconnected
    update = updateModel    -- Update function for state changes
    view = viewModel        -- View function for rendering
    events = defaultEvents  -- Default Miso event handlers
    subs = [ keyboardSub KeyDown ]  -- Subscribe to keyboard events
    mountPoint = Nothing    -- Mount to document body
    logLevel = Off          -- Disable logging

-- | Update the model based on actions
-- This is the Elm Architecture update function
updateModel :: Action -> Model -> Effect Action Model
updateModel action m = case action of
  NoOp -> noEff m  -- Do nothing
  
  -- User is typing their name
  UpdateName name -> noEff $ m { modelPlayerName = name }
  
  -- User clicked connect button - establish WebSocket connection
  Connect -> m <# do
    ws <- newWebSocket "ws://localhost:9160"  -- Create WebSocket to server
    onOpen ws $ HandleWsOpen                  -- Register open handler
    onMessage ws $ HandleWsMessage . ms       -- Register message handler
    onClose ws $ HandleWsClose                -- Register close handler
    return $ NoOp
  
  -- WebSocket connection opened - send Join message
  HandleWsOpen -> effectSub m $ \sink -> do
    case modelWs m of
      Just ws -> do
        let joinMsg = encode $ Join (modelPlayerName m)
        send joinMsg ws  -- Send Join message with player name
      Nothing -> return ()
    sink NoOp
  
  -- Received message from server - decode and update state
  HandleWsMessage msg -> noEff $ case decode (fromMisoString msg) of
    Just (Welcome pid gs) ->
      -- Server welcomed us with our player ID and initial game state
      m { modelPlayerId = Just pid
        , modelGameState = Just gs
        , modelConnected = True
        }
    Just (StateUpdate gs) ->
      -- Server sent updated game state (someone moved)
      m { modelGameState = Just gs }
    Just (PlayerJoined player) ->
      -- New player joined - add them to our game state
      case modelGameState m of
        Just gs -> m { modelGameState = Just gs { players = Map.insert (playerId player) player (players gs) } }
        Nothing -> m
    Just (PlayerLeft pid) ->
      -- Player left - remove them from our game state
      case modelGameState m of
        Just gs -> m { modelGameState = Just gs { players = Map.delete pid (players gs) } }
        Nothing -> m
    _ -> m  -- Unknown message, ignore
  
  -- WebSocket closed - reset connection state
  HandleWsClose -> noEff $ m { modelConnected = False, modelWs = Nothing }
  
  -- Send movement command to server
  MovePlayer dir -> effectSub m $ \sink -> do
    case modelWs m of
      Just ws -> send (encode $ Move dir) ws  -- Send Move message
      Nothing -> return ()
    sink NoOp
  
  -- Handle keyboard input - arrow keys move player
  KeyDown key -> case key of
    37 -> updateModel (MovePlayer West) m   -- Left arrow
    38 -> updateModel (MovePlayer North) m  -- Up arrow
    39 -> updateModel (MovePlayer East) m   -- Right arrow
    40 -> updateModel (MovePlayer South) m  -- Down arrow
    _ -> noEff m  -- Ignore other keys

-- | Main view function - renders entire UI
viewModel :: Model -> View Action
viewModel Model{..} =
  div_ [] $
    if not modelConnected
      then [ viewConnect modelPlayerName ]  -- Show connection screen
      else case modelGameState of
        Just gs -> [ viewGame gs modelPlayerId ]  -- Show game
        Nothing -> [ text "Loading..." ]           -- Loading state

-- | Connection screen view - name input and connect button
viewConnect :: Text -> View Action
viewConnect name =
  div_ [ style_ $ Map.fromList [("padding", "20px")] ]
    [ h1_ [] [ text "Game Client" ]
    , input_
        [ type_ "text"
        , placeholder_ "Enter your name"
        , value_ (ms name)
        , onInput $ UpdateName . fromMisoString  -- Update model on input
        ]
    , button_ [ onClick Connect ] [ text "Connect" ]
    ]

-- | Game view - canvas and controls
viewGame :: GameState -> Maybe Text -> View Action
viewGame gs myId =
  div_ []
    [ canvas_
        [ id_ "gameCanvas"
        , width_ "640"       -- Canvas width
        , height_ "480"      -- Canvas height
        , style_ $ Map.fromList [("border", "2px solid black")]
        ]
        []
    , script_ [] [ text $ renderScript gs myId ]  -- JavaScript to render game
    , div_ [ style_ $ Map.fromList [("margin-top", "10px")] ]
        [ text "Use arrow keys to move" ]
    ]

-- | Generate JavaScript code to render the game on canvas
renderScript :: GameState -> Maybe Text -> MisoString
renderScript gs myId = ms $ T.unpack $ T.unlines
  [ "const canvas = document.getElementById('gameCanvas');"
  , "const ctx = canvas.getContext('2d');"
  , "ctx.clearRect(0, 0, 640, 480);"  -- Clear canvas
  , T.concat $ map (renderPlayer myId) $ Map.elems (players gs)  -- Render each player
  ]

-- | Generate JavaScript to render a single player as a rectangle
renderPlayer :: Maybe Text -> Player -> Text
renderPlayer myId p =
  let isMe = Just (playerId p) == myId  -- Check if this is our player
      size = if isMe then "15" else "10"  -- Our player is larger
      x = T.pack $ show $ posX $ playerPos p
      y = T.pack $ show $ posY $ playerPos p
  in T.unlines
    [ "ctx.fillStyle = '" <> playerColor p <> "';"  -- Set fill color
    , "ctx.fillRect(" <> x <> ", " <> y <> ", " <> size <> ", " <> size <> ");"  -- Draw rectangle
    ]

-- | Subscription to keyboard events
-- Listens for keydown events and triggers KeyDown action
keyboardSub :: (Int -> action) -> Sub action model
keyboardSub f = \sink -> do
  cb <- asyncCallback1 $ \e -> do
    key <- getKeyCode e  -- Get key code from event
    sink (f key)         -- Trigger action with key code
  addEventListener "keydown" cb  -- Register listener
  return $ return ()

-- | FFI: Get key code from JavaScript keyboard event
foreign import javascript unsafe "$1.keyCode"
  getKeyCode :: JSVal -> IO Int

-- | FFI: Add event listener to window
foreign import javascript unsafe "window.addEventListener($1, $2)"
  addEventListener :: MisoString -> Callback (JSVal -> IO ()) -> IO ()