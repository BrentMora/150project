{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, forM_)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Network.HTTP.Types.Status (status400)
import System.Random

import Game.Types

type ClientId = Text
type Clients = Map.Map ClientId Connection

data ServerState = ServerState
  { stateClients :: TVar Clients
  , stateGame :: TVar GameState
  }

main :: IO ()
main = do
  putStrLn "Starting game server on port 9160..."
  state <- newServerState
  run 9160 $ websocketsOr defaultConnectionOptions (wsApp state) httpApp

newServerState :: IO ServerState
newServerState = do
  clients <- newTVarIO Map.empty
  game <- newTVarIO $ GameState Map.empty
  return $ ServerState clients game

httpApp :: Application
httpApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

wsApp :: ServerState -> ServerApp
wsApp state pending = do
  conn <- acceptRequest pending
  withPingThread conn 30 (return ()) $ handleClient state conn

handleClient :: ServerState -> Connection -> IO ()
handleClient state conn = flip finally (clientDisconnect state Nothing) $ do
  msg <- receiveData conn
  case decode msg of
    Just (Join name) -> do
      cid <- generateClientId
      atomically $ do
        modifyTVar' (stateClients state) $ Map.insert cid conn
        let player = Player cid (Position 50 50) (randomColor cid)
        modifyTVar' (stateGame state) $ \gs ->
          gs { players = Map.insert cid player (players gs) }
      
      gameState <- readTVarIO (stateGame state)
      sendJSON conn $ Welcome cid gameState
      broadcast state (Just cid) $ PlayerJoined (players gameState Map.! cid)
      
      clientLoop state cid conn
    _ -> sendJSON conn $ Game.Types.Error "First message must be Join"

clientLoop :: ServerState -> ClientId -> Connection -> IO ()
clientLoop state cid conn = forever $ do
  msg <- receiveData conn
  case decode msg of
    Just (Move dir) -> do
      atomically $ modifyTVar' (stateGame state) $ updatePlayerPosition cid dir
      gameState <- readTVarIO (stateGame state)
      broadcast state Nothing $ StateUpdate gameState
    Just Disconnect -> clientDisconnect state (Just cid)
    _ -> sendJSON conn $ Game.Types.Error "Invalid message"

clientDisconnect :: ServerState -> Maybe ClientId -> IO ()
clientDisconnect state mcid = forM_ mcid $ \cid -> do
  atomically $ do
    modifyTVar' (stateClients state) $ Map.delete cid
    modifyTVar' (stateGame state) $ \gs ->
      gs { players = Map.delete cid (players gs) }
  broadcast state (Just cid) $ PlayerLeft cid

updatePlayerPosition :: ClientId -> Direction -> GameState -> GameState
updatePlayerPosition cid dir gs =
  let updatePos p = p { playerPos = movePosition dir (playerPos p) }
  in gs { players = Map.adjust updatePos cid (players gs) }

movePosition :: Direction -> Position -> Position
movePosition dir (Position x y) = case dir of
  North -> Position x (max 0 (y - 5))
  South -> Position x (min 480 (y + 5))
  East -> Position (min 630 (x + 5)) y
  West -> Position (max 0 (x - 5)) y

broadcast :: ServerState -> Maybe ClientId -> ServerMessage -> IO ()
broadcast state excludeId msg = do
  clients <- readTVarIO (stateClients state)
  let filtered = case excludeId of
        Just cid -> Map.delete cid clients
        Nothing -> clients
  forM_ (Map.elems filtered) $ \conn ->
    sendJSON conn msg

sendJSON :: ToJSON a => Connection -> a -> IO ()
sendJSON conn = sendTextData conn . encode

generateClientId :: IO Text
generateClientId = T.pack . take 8 . randomRs ('a', 'z') <$> newStdGen

randomColor :: Text -> Text
randomColor seed = colors !! (sum (map fromEnum $ T.unpack seed) `mod` length colors)
  where colors = ["#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8"]
