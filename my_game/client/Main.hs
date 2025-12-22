{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.Foreign.Callback
import JavaScript.Web.WebSocket
import Miso
import Miso.String (MisoString, ms, fromMisoString)

import Game.Types

data Model = Model
  { modelWs :: Maybe WebSocket
  , modelGameState :: Maybe GameState
  , modelPlayerId :: Maybe Text
  , modelPlayerName :: Text
  , modelConnected :: Bool
  } deriving (Eq)

data Action
  = NoOp
  | UpdateName Text
  | Connect
  | HandleWsOpen
  | HandleWsMessage MisoString
  | HandleWsClose
  | MovePlayer Direction
  | KeyDown Int

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model = Model Nothing Nothing Nothing "" False
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs = [ keyboardSub KeyDown ]
    mountPoint = Nothing
    logLevel = Off

updateModel :: Action -> Model -> Effect Action Model
updateModel action m = case action of
  NoOp -> noEff m
  
  UpdateName name -> noEff $ m { modelPlayerName = name }
  
  Connect -> m <# do
    ws <- newWebSocket "ws://localhost:9160"
    onOpen ws $ HandleWsOpen
    onMessage ws $ HandleWsMessage . ms
    onClose ws $ HandleWsClose
    return $ NoOp
  
  HandleWsOpen -> effectSub m $ \sink -> do
    case modelWs m of
      Just ws -> do
        let joinMsg = encode $ Join (modelPlayerName m)
        send joinMsg ws
      Nothing -> return ()
    sink NoOp
  
  HandleWsMessage msg -> noEff $ case decode (fromMisoString msg) of
    Just (Welcome pid gs) ->
      m { modelPlayerId = Just pid
        , modelGameState = Just gs
        , modelConnected = True
        }
    Just (StateUpdate gs) ->
      m { modelGameState = Just gs }
    Just (PlayerJoined player) ->
      case modelGameState m of
        Just gs -> m { modelGameState = Just gs { players = Map.insert (playerId player) player (players gs) } }
        Nothing -> m
    Just (PlayerLeft pid) ->
      case modelGameState m of
        Just gs -> m { modelGameState = Just gs { players = Map.delete pid (players gs) } }
        Nothing -> m
    _ -> m
  
  HandleWsClose -> noEff $ m { modelConnected = False, modelWs = Nothing }
  
  MovePlayer dir -> effectSub m $ \sink -> do
    case modelWs m of
      Just ws -> send (encode $ Move dir) ws
      Nothing -> return ()
    sink NoOp
  
  KeyDown key -> case key of
    37 -> updateModel (MovePlayer West) m  -- Left
    38 -> updateModel (MovePlayer North) m -- Up
    39 -> updateModel (MovePlayer East) m  -- Right
    40 -> updateModel (MovePlayer South) m -- Down
    _ -> noEff m

viewModel :: Model -> View Action
viewModel Model{..} =
  div_ [] $
    if not modelConnected
      then [ viewConnect modelPlayerName ]
      else case modelGameState of
        Just gs -> [ viewGame gs modelPlayerId ]
        Nothing -> [ text "Loading..." ]

viewConnect :: Text -> View Action
viewConnect name =
  div_ [ style_ $ Map.fromList [("padding", "20px")] ]
    [ h1_ [] [ text "Game Client" ]
    , input_
        [ type_ "text"
        , placeholder_ "Enter your name"
        , value_ (ms name)
        , onInput $ UpdateName . fromMisoString
        ]
    , button_ [ onClick Connect ] [ text "Connect" ]
    ]

viewGame :: GameState -> Maybe Text -> View Action
viewGame gs myId =
  div_ []
    [ canvas_
        [ id_ "gameCanvas"
        , width_ "640"
        , height_ "480"
        , style_ $ Map.fromList [("border", "2px solid black")]
        ]
        []
    , script_ [] [ text $ renderScript gs myId ]
    , div_ [ style_ $ Map.fromList [("margin-top", "10px")] ]
        [ text "Use arrow keys to move" ]
    ]

renderScript :: GameState -> Maybe Text -> MisoString
renderScript gs myId = ms $ T.unpack $ T.unlines
  [ "const canvas = document.getElementById('gameCanvas');"
  , "const ctx = canvas.getContext('2d');"
  , "ctx.clearRect(0, 0, 640, 480);"
  , T.concat $ map (renderPlayer myId) $ Map.elems (players gs)
  ]

renderPlayer :: Maybe Text -> Player -> Text
renderPlayer myId p =
  let isMe = Just (playerId p) == myId
      size = if isMe then "15" else "10"
      x = T.pack $ show $ posX $ playerPos p
      y = T.pack $ show $ posY $ playerPos p
  in T.unlines
    [ "ctx.fillStyle = '" <> playerColor p <> "';"
    , "ctx.fillRect(" <> x <> ", " <> y <> ", " <> size <> ", " <> size <> ");"
    ]

keyboardSub :: (Int -> action) -> Sub action model
keyboardSub f = \sink -> do
  cb <- asyncCallback1 $ \e -> do
    key <- getKeyCode e
    sink (f key)
  addEventListener "keydown" cb
  return $ return ()

foreign import javascript unsafe "$1.keyCode"
  getKeyCode :: JSVal -> IO Int

foreign import javascript unsafe "window.addEventListener($1, $2)"
  addEventListener :: MisoString -> Callback (JSVal -> IO ()) -> IO ()