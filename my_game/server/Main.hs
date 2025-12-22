{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types (status400)

-- Concurrency and STM for thread-safe state management
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, forM_)

-- JSON and data handling
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- Web server and WebSocket libraries
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets

-- Random generation for player IDs and colors
import System.Random

-- Our shared game types
import Game.Types

-- Type aliases for clarity
type ClientId = Text
type Clients = Map.Map ClientId Connection  -- Map of connected clients

-- | Server state containing all connected clients and current game state
-- Uses TVar for thread-safe concurrent access
data ServerState = ServerState
  { stateClients :: TVar Clients    -- Thread-safe map of connected WebSocket clients
  , stateGame :: TVar GameState     -- Thread-safe game state
  }

-- | Main entry point - starts the WebSocket server
main :: IO ()
main = do
  putStrLn "Starting game server on port 9160..."
  state <- newServerState
  -- Run Warp server with WebSocket support, fallback to HTTP app for non-WS requests
  run 9160 $ websocketsOr defaultConnectionOptions (wsApp state) httpApp

-- | Initialize a new server state with empty clients and game state
newServerState :: IO ServerState
newServerState = do
  clients <- newTVarIO Map.empty           -- Create empty clients map
  game <- newTVarIO $ GameState Map.empty  -- Create empty game state
  return $ ServerState clients game

-- | HTTP application for non-WebSocket requests (returns error)
httpApp :: Application
httpApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | WebSocket application handler
wsApp :: ServerState -> ServerApp
wsApp state pending = do
  conn <- acceptRequest pending           -- Accept the WebSocket connection
  -- Keep connection alive with ping every 30 seconds, handle client communication
  withPingThread conn 30 (return ()) $ handleClient state conn

-- | Handle a new client connection
-- Expects the first message to be a Join message, then enters main loop
handleClient :: ServerState -> Connection -> IO ()
handleClient state conn = flip finally (clientDisconnect state Nothing) $ do
  msg <- receiveData conn  -- Wait for first message
  case decode msg of
    Just (Join name) -> do
      -- Generate unique client ID
      cid <- generateClientId
      -- Atomically update server state (thread-safe)
      atomically $ do
        -- Add client connection to clients map
        modifyTVar' (stateClients state) $ Map.insert cid conn
        -- Create new player at starting position with random color
        let player = Player cid (Position 50 50) (randomColor cid)
        -- Add player to game state
        modifyTVar' (stateGame state) $ \gs ->
          gs { players = Map.insert cid player (players gs) }
      
      -- Send welcome message with player ID and current game state
      gameState <- readTVarIO (stateGame state)
      sendJSON conn $ Welcome cid gameState
      -- Notify other clients about the new player
      broadcast state (Just cid) $ PlayerJoined (players gameState Map.! cid)
      
      -- Enter main message loop for this client
      clientLoop state cid conn
    -- If first message is not Join, send error
    _ -> sendJSON conn $ Game.Types.Error "First message must be Join"

-- | Main message loop for a connected client
-- Continuously processes messages until disconnect or error
clientLoop :: ServerState -> ClientId -> Connection -> IO ()
clientLoop state cid conn = forever $ do
  msg <- receiveData conn  -- Wait for next message
  case decode msg of
    Just (Move dir) -> do
      -- Update player position atomically (server is authoritative)
      atomically $ modifyTVar' (stateGame state) $ updatePlayerPosition cid dir
      -- Broadcast updated game state to all clients
      gameState <- readTVarIO (stateGame state)
      broadcast state Nothing $ StateUpdate gameState
    Just Disconnect -> clientDisconnect state (Just cid)  -- Handle explicit disconnect
    _ -> sendJSON conn $ Game.Types.Error "Invalid message"  -- Unknown message type

-- | Remove a client from the game
-- Cleans up client connection and removes player from game state
clientDisconnect :: ServerState -> Maybe ClientId -> IO ()
clientDisconnect state mcid = forM_ mcid $ \cid -> do
  -- Atomically remove client and player
  atomically $ do
    modifyTVar' (stateClients state) $ Map.delete cid        -- Remove from clients map
    modifyTVar' (stateGame state) $ \gs ->
      gs { players = Map.delete cid (players gs) }           -- Remove from game state
  -- Notify remaining clients that player left
  broadcast state (Just cid) $ PlayerLeft cid

-- | Update a player's position based on direction (server-side validation)
updatePlayerPosition :: ClientId -> Direction -> GameState -> GameState
updatePlayerPosition cid dir gs =
  let updatePos p = p { playerPos = movePosition dir (playerPos p) }
  in gs { players = Map.adjust updatePos cid (players gs) }

-- | Calculate new position after moving in a direction
-- Includes boundary checking to keep player within canvas (0-640 x 0-480)
movePosition :: Direction -> Position -> Position
movePosition dir (Position x y) = case dir of
  North -> Position x (max 0 (y - 5))        -- Move up, minimum y = 0
  South -> Position x (min 480 (y + 5))      -- Move down, maximum y = 480
  East -> Position (min 630 (x + 5)) y       -- Move right, maximum x = 630
  West -> Position (max 0 (x - 5)) y         -- Move left, minimum x = 0

-- | Broadcast a message to all connected clients (optionally excluding one)
broadcast :: ServerState -> Maybe ClientId -> ServerMessage -> IO ()
broadcast state excludeId msg = do
  clients <- readTVarIO (stateClients state)  -- Get current clients
  let filtered = case excludeId of
        Just cid -> Map.delete cid clients    -- Exclude specified client (e.g., sender)
        Nothing -> clients                    -- Send to all clients
  -- Send message to each client
  forM_ (Map.elems filtered) $ \conn ->
    sendJSON conn msg

-- | Helper to send JSON data over WebSocket
sendJSON :: ToJSON a => Connection -> a -> IO ()
sendJSON conn = sendTextData conn . encode  -- Encode to JSON and send as text

-- | Generate a random 8-character client ID
generateClientId :: IO Text
generateClientId = T.pack . take 8 . randomRs ('a', 'z') <$> newStdGen

-- | Assign a color to a player based on their ID (deterministic but pseudo-random)
randomColor :: Text -> Text
randomColor seed = colors !! (sum (map fromEnum $ T.unpack seed) `mod` length colors)
  where colors = ["#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8"]