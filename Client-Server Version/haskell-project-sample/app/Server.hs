{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Server where

import qualified Common
import Control.Concurrent (MVar, forkIO, modifyMVar, modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Monad (forM_, forever, when)
import Data.Function ((&))
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.WebSockets as WS

-- Number of players required before the game starts
requiredPlayerCount :: Int
requiredPlayerCount = 2

-- Server state containing all connected clients and the current game state
data ServerState = ServerState
  { clients :: Map.Map Common.PlayerId Client  -- Map of all connected clients
  , gameState :: GameState                     -- Current game state
  }

-- Represents a connected client with their ID and WebSocket connection
data Client = Client
  { id :: Common.PlayerId      -- Unique player ID
  , conn :: WS.Connection      -- WebSocket connection to this client
  }

-- Game state containing all game logic data
data GameState = GameState
  { ticks :: Int                                    -- Frame counter (increments each update)
  , players :: Map Common.PlayerId Common.Player    -- Map of all players in the game
  , eventQueue :: [GameEvent]                       -- Queue of events to process this frame
  }
  deriving (Show, Eq)

-- Events that can occur in the game
data GameEvent
  = NewPlayerEvent Common.PlayerId                      -- A new player has joined
  | PlayerInputEvent Common.PlayerId Common.KeyState    -- A player sent input
  deriving (Show, Eq)

-- Main server entry point
mainServer :: IO ()
mainServer = do
  putStrLn "Server is starting..."
  -- Create an MVar to hold the server state (thread-safe shared state)
  stateMVar <- newMVar initServerState

  putStrLn "Main game loop is starting..."
  -- Fork a background thread that runs the game loop forever
  void $ forkIO $ forever $ mainGameLoop stateMVar

  putStrLn "Listening process is starting..."
  -- Start WebSocket server on localhost:15000
  WS.runServer "127.0.0.1" 15000 $ app stateMVar

-- Initial server state with no clients and empty game state
initServerState :: ServerState
initServerState =
  ServerState
    { clients = Map.empty
    , gameState =
        GameState
          { ticks = 0
          , players = Map.empty
          , eventQueue = []
          }
    }

-- Handle a new WebSocket connection
app :: MVar ServerState -> WS.PendingConnection -> IO ()
app stateMVar pendingConn = do
  -- Accept the WebSocket connection
  conn <- WS.acceptRequest pendingConn
  -- Set up ping/pong to keep connection alive (ping every 30 seconds)
  WS.withPingThread conn 30 (pure ()) $ do
    -- Add the new client to the server state
    maybeClient <- modifyMVar stateMVar $ \serverState -> do
      -- Calculate the new player number based on current client count
      let newPlayerNum = Map.size serverState.clients + 1

      -- Check if server is full
      if newPlayerNum > requiredPlayerCount
        then
          -- Server is full, don't add the client
          pure (serverState, Nothing)
        else do
          putStrLn $ "Player " <> show newPlayerNum <> " has connected"

          -- Create a new client record
          let newClient = Client newPlayerNum conn
          let newServerState =
                serverState
                  { clients = Map.insert newPlayerNum newClient serverState.clients
                  , gameState =
                      serverState.gameState
                        { -- Add a NewPlayerEvent to the event queue
                          eventQueue = serverState.gameState.eventQueue <> [NewPlayerEvent newPlayerNum]
                        }
                  }

          pure (newServerState, Just newClient)

    -- If client was added successfully, start listening for their messages
    case maybeClient of
      Just client -> getDataFromClient client stateMVar
      Nothing -> putStrLn "Rejected new client; server is full"

-- Listen for messages from a client in an infinite loop
getDataFromClient :: Client -> MVar ServerState -> IO ()
getDataFromClient client stateMVar = forever $ do
  -- Receive text data from the client
  raw <- WS.receiveData client.conn :: IO T.Text
  putStrLn $ "Got data: " <> T.unpack raw

  -- Try to parse the data as ClientInput
  case Common.textToClientInput raw of
    Nothing -> do
      -- Failed to parse, log error
      putStrLn $ "Failed to parse client input: " <> T.unpack raw
    Just clientInput -> do
      -- Successfully parsed, add to event queue if game is active
      modifyMVar_ stateMVar $ \serverState -> do
        if isGameActive serverState.gameState
          then
            -- Game is active, add the input event to the queue
            pure $
              serverState
                { gameState =
                    serverState.gameState
                      { eventQueue =
                          serverState.gameState.eventQueue
                            <> [PlayerInputEvent client.id clientInput.keyState]
                      }
                }
          else pure serverState  -- Game not active yet, ignore input

-- Check if the game has enough players to be active
isGameActive :: GameState -> Bool
isGameActive state = Map.size state.players == requiredPlayerCount

-- Main game loop that runs approximately 60 times per second
mainGameLoop :: MVar ServerState -> IO ()
mainGameLoop stateMVar = do
  -- Update the game state
  modifyMVar_ stateMVar $ \serverState -> do
    let newGameState = updateGameState serverState.gameState
    pure $ serverState{gameState = newGameState}

  -- Send updated game state to all clients
  broadcastToClients stateMVar

  -- Sleep for ~16.666ms to achieve ~60 FPS (1000000 microseconds / 60 = 16666)
  threadDelay 16666

-- Update the game state by processing all events and updating positions
updateGameState :: GameState -> GameState
updateGameState gameState =
  gameState
    & clearDirectionChange  -- Reset the changedDirection flag on all players
    & processEvents         -- Process all events in the queue
    & updateMovement        -- Update player positions based on input
    & updateTicks           -- Increment the tick counter

-- Clear the changedDirection flag on all players (reset each frame)
clearDirectionChange :: GameState -> GameState
clearDirectionChange state =
  state
    { players =
        Map.map
          ( \p -> do
              p
                { Common.changedDirection = False
                }
          )
          state.players
    }

-- Increment the tick counter if the game is active
updateTicks :: GameState -> GameState
updateTicks state =
  if Map.size state.players == requiredPlayerCount
    then
      -- Game is active, increment ticks
      state{ticks = state.ticks + 1}
    else state  -- Not enough players, don't increment

-- Update all player positions based on their current key state
updateMovement :: GameState -> GameState
updateMovement state =
  state
    { players =
        Map.map
          ( \p -> do
              -- Calculate horizontal velocity based on left/right keys
              let vx
                    | p.playerKeyState.left = -2   -- Move left at speed 2
                    | p.playerKeyState.right = 2   -- Move right at speed 2
                    | otherwise = 0                -- No horizontal movement
              -- Calculate vertical velocity based on up/down keys
              let vy
                    | p.playerKeyState.up = -2     -- Move up at speed 2
                    | p.playerKeyState.down = 2    -- Move down at speed 2
                    | otherwise = 0                -- No vertical movement
              -- Update player position
              p
                { Common.x = p.x + vx
                , Common.y = p.y + vy
                }
          )
          state.players
    }

-- Process all events in the event queue recursively
processEvents :: GameState -> GameState
processEvents gameState =
  case gameState.eventQueue of
    [] -> gameState  -- No more events, done
    _ ->
      gameState
        & processNextEvent  -- Process the first event
        & processEvents     -- Recursively process remaining events

-- Process a single event from the front of the queue
processNextEvent :: GameState -> GameState
processNextEvent state =
  case state.eventQueue of
    [] -> state  -- No events to process
    NewPlayerEvent newPlayerId : evs -> do
      -- Create a new player with default values
      let newPlayer =
            Common.Player
              { Common.id = newPlayerId
              , Common.x = 0
              , Common.y = 20 * fromIntegral newPlayerId  -- Offset each player vertically
              , Common.playerKeyState = Common.KeyState False False False False False
              , Common.changedDirection = False
              }
      -- Add the new player to the game state
      state
        { players = Map.insert newPlayerId newPlayer state.players
        , eventQueue = evs  -- Remove this event from the queue
        }
    PlayerInputEvent playerId keyState : evs -> do
      -- Update the player's key state
      state
        { players =
            Map.adjust
              ( \p ->
                  p
                    { Common.playerKeyState = keyState
                    , -- Set changedDirection if the key state is different
                      Common.changedDirection = p.playerKeyState /= keyState
                    }
              )
              playerId
              state.players
        , eventQueue = evs  -- Remove this event from the queue
        }

-- Send the current game state to all connected clients
broadcastToClients :: MVar ServerState -> IO ()
broadcastToClients stateMVar = do
  -- Read the current server state
  serverState <- readMVar stateMVar
  -- Print game state for debugging
  print serverState.gameState

  -- Only broadcast if the game is active (enough players)
  when (isGameActive serverState.gameState) $ do
    let clients = serverState.clients
    -- Convert game state to view data (the data clients need to render)
    let viewData = gameStateToViewData serverState.gameState
    -- Serialize to JSON text
    let payload = Common.viewDataToText viewData
    putStrLn $ T.unpack payload
    putStrLn ""

    -- Send the payload to each client in a separate thread (non-blocking)
    forM_ clients $ \client -> do
      void $ forkIO $ WS.sendTextData client.conn payload

-- Convert internal game state to the view data format sent to clients
gameStateToViewData :: GameState -> Common.ViewData
gameStateToViewData gameState =
  Common.ViewData
    { Common.players = gameState.players
    , Common.serverTicks = gameState.ticks
    }