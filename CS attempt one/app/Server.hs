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
import Data.List (minimumBy)

-- Number of players required
requiredPlayerCount :: Int
requiredPlayerCount = 1

-- Constants
squareSize :: Double
squareSize = 50

screenWidth :: Double
screenWidth = 750

screenHeight :: Double
screenHeight = 650

-- Server state
data ServerState = ServerState
  { clients :: Map.Map Common.PlayerId Client
  , gameState :: GameState
  }

-- Client record
data Client = Client
  { id :: Common.PlayerId
  , conn :: WS.Connection
  }

-- Game state
data GameState = GameState
  { ticks :: Int
  , players :: Map Common.PlayerId Common.Player
  , bombs :: [Common.Bomb]
  , obstacles :: [Common.Obstacle]
  , eventQueue :: [GameEvent]
  , gameOver :: Common.GameOverFlag
  , gameTimer :: Float
  }
  deriving (Show, Eq)

-- Game events
data GameEvent
  = NewPlayerEvent Common.PlayerId
  | PlayerInputEvent Common.PlayerId Common.KeyState
  deriving (Show, Eq)

-- Main server entry point
mainServer :: IO ()
mainServer = do
  putStrLn "Server is starting..."
  stateMVar <- newMVar initServerState

  putStrLn "Main game loop is starting..."
  void $ forkIO $ forever $ mainGameLoop stateMVar

  putStrLn "Listening process is starting..."
  WS.runServer "127.0.0.1" 15000 $ app stateMVar

-- Initial server state
initServerState :: ServerState
initServerState =
  ServerState
    { clients = Map.empty
    , gameState = initGameState
    }

-- Initial game state
initGameState :: GameState
initGameState =
  GameState
    { ticks = 0
    , players = Map.empty
    , bombs = []
    , obstacles = initialObstacles
    , eventQueue = []
    , gameOver = Common.NotGO
    , gameTimer = 60
    }

-- Initial obstacles (border + grid + soft blocks)
initialObstacles :: [Common.Obstacle]
initialObstacles =
  [ -- Top Border
    Common.Obstacle 25 25 squareSize squareSize True
  , Common.Obstacle 75 25 squareSize squareSize True
  , Common.Obstacle 125 25 squareSize squareSize True
  , Common.Obstacle 175 25 squareSize squareSize True
  , Common.Obstacle 225 25 squareSize squareSize True
  , Common.Obstacle 275 25 squareSize squareSize True
  , Common.Obstacle 325 25 squareSize squareSize True
  , Common.Obstacle 375 25 squareSize squareSize True
  , Common.Obstacle 425 25 squareSize squareSize True
  , Common.Obstacle 475 25 squareSize squareSize True
  , Common.Obstacle 525 25 squareSize squareSize True
  , Common.Obstacle 575 25 squareSize squareSize True
  , Common.Obstacle 625 25 squareSize squareSize True
  , Common.Obstacle 675 25 squareSize squareSize True
  , Common.Obstacle 725 25 squareSize squareSize True
  -- Left Border
  , Common.Obstacle 25 75 squareSize squareSize True
  , Common.Obstacle 25 125 squareSize squareSize True
  , Common.Obstacle 25 175 squareSize squareSize True
  , Common.Obstacle 25 225 squareSize squareSize True
  , Common.Obstacle 25 275 squareSize squareSize True
  , Common.Obstacle 25 325 squareSize squareSize True
  , Common.Obstacle 25 375 squareSize squareSize True
  , Common.Obstacle 25 425 squareSize squareSize True
  , Common.Obstacle 25 475 squareSize squareSize True
  , Common.Obstacle 25 525 squareSize squareSize True
  , Common.Obstacle 25 575 squareSize squareSize True
  , Common.Obstacle 25 625 squareSize squareSize True
  -- Right Border
  , Common.Obstacle 725 75 squareSize squareSize True
  , Common.Obstacle 725 125 squareSize squareSize True
  , Common.Obstacle 725 175 squareSize squareSize True
  , Common.Obstacle 725 225 squareSize squareSize True
  , Common.Obstacle 725 275 squareSize squareSize True
  , Common.Obstacle 725 325 squareSize squareSize True
  , Common.Obstacle 725 375 squareSize squareSize True
  , Common.Obstacle 725 425 squareSize squareSize True
  , Common.Obstacle 725 475 squareSize squareSize True
  , Common.Obstacle 725 525 squareSize squareSize True
  , Common.Obstacle 725 575 squareSize squareSize True
  , Common.Obstacle 725 625 squareSize squareSize True
  -- Bottom Border
  , Common.Obstacle 75 625 squareSize squareSize True
  , Common.Obstacle 125 625 squareSize squareSize True
  , Common.Obstacle 175 625 squareSize squareSize True
  , Common.Obstacle 225 625 squareSize squareSize True
  , Common.Obstacle 275 625 squareSize squareSize True
  , Common.Obstacle 325 625 squareSize squareSize True
  , Common.Obstacle 375 625 squareSize squareSize True
  , Common.Obstacle 425 625 squareSize squareSize True
  , Common.Obstacle 475 625 squareSize squareSize True
  , Common.Obstacle 525 625 squareSize squareSize True
  , Common.Obstacle 575 625 squareSize squareSize True
  , Common.Obstacle 625 625 squareSize squareSize True
  , Common.Obstacle 675 625 squareSize squareSize True
  -- Grid blocks (hard)
  , Common.Obstacle 125 125 squareSize squareSize True
  , Common.Obstacle 225 125 squareSize squareSize True
  , Common.Obstacle 325 125 squareSize squareSize True
  , Common.Obstacle 425 125 squareSize squareSize True
  , Common.Obstacle 525 125 squareSize squareSize True
  , Common.Obstacle 625 125 squareSize squareSize True
  , Common.Obstacle 125 225 squareSize squareSize True
  , Common.Obstacle 225 225 squareSize squareSize True
  , Common.Obstacle 325 225 squareSize squareSize True
  , Common.Obstacle 425 225 squareSize squareSize True
  , Common.Obstacle 525 225 squareSize squareSize True
  , Common.Obstacle 625 225 squareSize squareSize True
  , Common.Obstacle 125 325 squareSize squareSize True
  , Common.Obstacle 225 325 squareSize squareSize True
  , Common.Obstacle 325 325 squareSize squareSize True
  , Common.Obstacle 425 325 squareSize squareSize True
  , Common.Obstacle 525 325 squareSize squareSize True
  , Common.Obstacle 625 325 squareSize squareSize True
  , Common.Obstacle 125 425 squareSize squareSize True
  , Common.Obstacle 225 425 squareSize squareSize True
  , Common.Obstacle 325 425 squareSize squareSize True
  , Common.Obstacle 425 425 squareSize squareSize True
  , Common.Obstacle 525 425 squareSize squareSize True
  , Common.Obstacle 625 425 squareSize squareSize True
  , Common.Obstacle 125 525 squareSize squareSize True
  , Common.Obstacle 225 525 squareSize squareSize True
  , Common.Obstacle 325 525 squareSize squareSize True
  , Common.Obstacle 425 525 squareSize squareSize True
  , Common.Obstacle 525 525 squareSize squareSize True
  , Common.Obstacle 625 525 squareSize squareSize True
  -- Soft blocks
  , Common.Obstacle 75 575 squareSize squareSize False
  , Common.Obstacle 175 175 squareSize squareSize False
  , Common.Obstacle 375 275 squareSize squareSize False
  , Common.Obstacle 575 375 squareSize squareSize False
  , Common.Obstacle 525 275 squareSize squareSize False
  , Common.Obstacle 375 525 squareSize squareSize False
  , Common.Obstacle 625 475 squareSize squareSize False
  , Common.Obstacle 675 575 squareSize squareSize False
  , Common.Obstacle 75 175 squareSize squareSize False
  , Common.Obstacle 625 575 squareSize squareSize False
  ]

-- Handle new connection
app :: MVar ServerState -> WS.PendingConnection -> IO ()
app stateMVar pendingConn = do
  conn <- WS.acceptRequest pendingConn
  WS.withPingThread conn 30 (pure ()) $ do
    maybeClient <- modifyMVar stateMVar $ \serverState -> do
      let newPlayerNum = Map.size serverState.clients + 1

      if newPlayerNum > requiredPlayerCount
        then pure (serverState, Nothing)
        else do
          putStrLn $ "Player " <> show newPlayerNum <> " has connected"

          let newClient = Client newPlayerNum conn
          let newServerState =
                serverState
                  { clients = Map.insert newPlayerNum newClient serverState.clients
                  , gameState =
                      serverState.gameState
                        { eventQueue = serverState.gameState.eventQueue <> [NewPlayerEvent newPlayerNum]
                        }
                  }

          pure (newServerState, Just newClient)

    case maybeClient of
      Just client -> getDataFromClient client stateMVar
      Nothing -> putStrLn "Rejected new client; server is full"

-- Listen for client messages
getDataFromClient :: Client -> MVar ServerState -> IO ()
getDataFromClient client stateMVar = forever $ do
  raw <- WS.receiveData client.conn :: IO T.Text
  putStrLn $ "Got data: " <> T.unpack raw

  case Common.textToClientInput raw of
    Nothing -> do
      putStrLn $ "Failed to parse client input: " <> T.unpack raw
    Just clientInput -> do
      modifyMVar_ stateMVar $ \serverState -> do
        if isGameActive serverState.gameState
          then
            pure $
              serverState
                { gameState =
                    serverState.gameState
                      { eventQueue =
                          serverState.gameState.eventQueue
                            <> [PlayerInputEvent client.id clientInput.keyState]
                      }
                }
          else pure serverState

-- Check if game is active
isGameActive :: GameState -> Bool
isGameActive state = Map.size state.players == requiredPlayerCount

-- Main game loop (~60 FPS)
mainGameLoop :: MVar ServerState -> IO ()
mainGameLoop stateMVar = do
  modifyMVar_ stateMVar $ \serverState -> do
    let newGameState = updateGameState serverState.gameState
    pure $ serverState{gameState = newGameState}

  broadcastToClients stateMVar
  threadDelay 16666

-- Update game state
updateGameState :: GameState -> GameState
updateGameState gameState =
  gameState
    & processEvents
    & updatePlayers
    & updateBombsHeld
    & updateBombOverlapping
    & placeBombs
    & updateBombTimers
    & detonateBombs
    & bombBombDetonation
    & removeBombs
    & checkCollisions
    & updateTimer
    & updateTicks

-- Process all events
processEvents :: GameState -> GameState
processEvents gameState =
  case gameState.eventQueue of
    [] -> gameState
    _ ->
      gameState
        & processNextEvent
        & processEvents

-- Process single event
processNextEvent :: GameState -> GameState
processNextEvent state =
  case state.eventQueue of
    [] -> state
    NewPlayerEvent newPlayerId : evs -> do
      let spawnPositions = [(75, 75), (675, 575)]
      let (spawnX, spawnY) = spawnPositions !! (newPlayerId - 1)
      let newPlayer =
            Common.Player
              { Common.playerId = newPlayerId
              , Common.playerX = spawnX
              , Common.playerY = spawnY
              , Common.playerVelX = 0
              , Common.playerVelY = 0
              , Common.playerSize = 30
              , Common.bombsHeld = 1
              , Common.targetX = spawnX
              , Common.targetY = spawnY
              , Common.spaceRequest = Common.Released
              , Common.xCoords = [25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 625, 675, 725]
              , Common.yCoords = [25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 625]
              , Common.currentDirection = Just Common.DirNone
              }
      state
        { players = Map.insert newPlayerId newPlayer state.players
        , eventQueue = evs
        }
    PlayerInputEvent playerId keyState : evs -> do
      let newDirection =
            if keyState.up then Just Common.DirUp
            else if keyState.down then Just Common.DirDown
            else if keyState.left then Just Common.DirLeft
            else if keyState.right then Just Common.DirRight
            else Just Common.DirNone
      
      state
        { players =
            Map.adjust
              ( \p ->
                  let isPrevReleased = p.spaceRequest == Common.Released
                      isPrevBlocked = p.spaceRequest == Common.Blocked
                      newSpaceRequest =
                        if not keyState.placeBomb then Common.Released
                        else if keyState.placeBomb && isPrevReleased then Common.Valid
                        else p.spaceRequest
                  in p
                      { Common.currentDirection = newDirection
                      , Common.spaceRequest = newSpaceRequest
                      }
              )
              playerId
              state.players
        , eventQueue = evs
        }

-- Update player positions
updatePlayers :: GameState -> GameState
updatePlayers state =
  state
    { players = Map.map (updatePlayerPosition state.obstacles state.bombs) state.players
    }

-- Update single player position
updatePlayerPosition :: [Common.Obstacle] -> [Common.Bomb] -> Common.Player -> Common.Player
updatePlayerPosition obstacles bombs p =
  let dir = p.currentDirection
      speed = 5
      vx = case dir of
        Just Common.DirLeft -> -speed
        Just Common.DirRight -> speed
        _ -> 0
      vy = case dir of
        Just Common.DirUp -> -speed
        Just Common.DirDown -> speed
        _ -> 0
      
      newX = clamp 0 screenWidth (p.playerX + vx)
      newY = clamp 0 screenHeight (p.playerY + vy)
      newTargetX = computeTarget newX p.xCoords
      newTargetY = computeTarget newY p.yCoords
      
      testPlayer = p { Common.playerX = newX, Common.playerY = newY, Common.targetX = newTargetX, Common.targetY = newTargetY }
      
      wouldCollide = any (checkPlayerObstacleCollision testPlayer) obstacles
      wouldCollideB = any (checkPlayerBombCollision testPlayer) bombs
      
  in if wouldCollide || wouldCollideB
     then p
     else testPlayer

-- Clamp value
clamp :: Ord a => a -> a -> a -> a
clamp minV maxV = max minV . min maxV

-- Compute target coordinate
computeTarget :: Double -> [Double] -> Double
computeTarget pXY =
  minimumBy (\a b -> compare (abs (pXY - a)) (abs (pXY - b)))

-- Check player-obstacle collision
checkPlayerObstacleCollision :: Common.Player -> Common.Obstacle -> Bool
checkPlayerObstacleCollision p obs =
  let px = p.playerX
      py = p.playerY
      ps = p.playerSize
      ox = obs.obstacleX
      oy = obs.obstacleY
      ow = obs.obstacleWidth
      oh = obs.obstacleHeight
      
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      
      oLeft = ox - ow / 2
      oRight = ox + ow / 2
      oTop = oy - oh / 2
      oBottom = oy + oh / 2
      
  in pRight > oLeft && pLeft < oRight &&
     pBottom > oTop && pTop < oBottom

-- Check player-bomb collision (ticking bombs only)
checkPlayerBombCollision :: Common.Player -> Common.Bomb -> Bool
checkPlayerBombCollision p b =
  let px = p.playerX
      py = p.playerY
      ps = p.playerSize
      bx = b.bombX
      by = b.bombY
      bs = b.bombSize
      
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
      
  in pRight > bLeft && pLeft < bRight &&
     pBottom > bTop && pTop < bBottom &&
     b.isDetonated == Common.Ticking &&
     not b.isOverlapping

-- Update bombs held
updateBombsHeld :: GameState -> GameState
updateBombsHeld state =
  state
    { players = Map.map (updatePlayerBombIncrement state.bombs) state.players
    }

-- Increment player bombs if all bombs detonated
updatePlayerBombIncrement :: [Common.Bomb] -> Common.Player -> Common.Player
updatePlayerBombIncrement bombs p =
  let lenB = length bombs
      oldBH = p.bombsHeld
  in if lenB == 0 && oldBH < 1
     then p { Common.bombsHeld = oldBH + 1 }
     else p

-- Update bomb overlapping status
updateBombOverlapping :: GameState -> GameState
updateBombOverlapping state =
  state
    { bombs = map (updateBombOverlappingWithPlayers state.players) state.bombs
    }

-- Check if bomb overlaps with any player
updateBombOverlappingWithPlayers :: Map Common.PlayerId Common.Player -> Common.Bomb -> Common.Bomb
updateBombOverlappingWithPlayers players b =
  let isNotColliding = not $ any (checkBombPlayerCollision b) (Map.elems players)
  in if isNotColliding
     then b { Common.isOverlapping = False }
     else b

-- Check bomb-player collision
checkBombPlayerCollision :: Common.Bomb -> Common.Player -> Bool
checkBombPlayerCollision b p =
  let px = p.playerX
      py = p.playerY
      ps = p.playerSize
      bx = b.bombX
      by = b.bombY
      bs = b.bombSize
      
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
      
  in pRight > bLeft && pLeft < bRight &&
     pBottom > bTop && pTop < bBottom &&
     b.isDetonated == Common.Ticking

-- Place bombs
placeBombs :: GameState -> GameState
placeBombs state =
  let (newBombs, updatedPlayers) = foldl placeBombForPlayer (state.bombs, state.players) (Map.elems state.players)
  in state { bombs = newBombs, players = updatedPlayers }

-- Place bomb for a single player
placeBombForPlayer :: ([Common.Bomb], Map Common.PlayerId Common.Player) -> Common.Player -> ([Common.Bomb], Map Common.PlayerId Common.Player)
placeBombForPlayer (bombs, players) p =
  if p.bombsHeld > 0 && p.spaceRequest == Common.Valid
     && not (checkIfBombExists (Common.Bomb p.targetX p.targetY 0 0 squareSize Common.Ticking 3 True) bombs)
  then
    let newBomb = Common.Bomb p.targetX p.targetY 0 0 squareSize Common.Ticking 3 True
        updatedPlayer = p { Common.bombsHeld = p.bombsHeld - 1, Common.spaceRequest = Common.Blocked }
    in (bombs ++ [newBomb], Map.insert p.playerId updatedPlayer players)
  else (bombs, players)

-- Check if bomb exists at position
checkIfBombExists :: Common.Bomb -> [Common.Bomb] -> Bool
checkIfBombExists b bombs =
  any (\x -> x.bombX == b.bombX && x.bombY == b.bombY) bombs

-- Update bomb timers
updateBombTimers :: GameState -> GameState
updateBombTimers state =
  state { bombs = map updateBombTimer state.bombs }

-- Update single bomb timer
updateBombTimer :: Common.Bomb -> Common.Bomb
updateBombTimer b =
  let oldTime = b.timer
      timeTick = 0.0167
      bDS = b.isDetonated
  in if oldTime <= 0 && bDS == Common.Ticking
     then b { Common.isDetonated = Common.Detonating, Common.timer = 1 }
     else if oldTime <= 0 && bDS == Common.Detonating
     then b { Common.isDetonated = Common.Done }
     else b { Common.timer = oldTime - timeTick }

-- Detonate bombs
detonateBombs :: GameState -> GameState
detonateBombs state =
  let (newBombs, newObstacles) = foldl processOneBomb ([], state.obstacles) state.bombs
  in state { bombs = newBombs, obstacles = newObstacles }
  where
    processOneBomb (accBombs, accObs) bomb =
      if bomb.isDetonated == Common.Ticking && bomb.timer <= 0
        then let (newBombs, newObs) = detonateBomb accObs bomb
             in (accBombs ++ newBombs, newObs)
        else (accBombs ++ [bomb], accObs)

-- Detonate single bomb
detonateBomb :: [Common.Obstacle] -> Common.Bomb -> ([Common.Bomb], [Common.Obstacle])
detonateBomb obs b =
  let bX = b.bombX
      bY = b.bombY
      
      newBombUp = Common.Bomb bX (bY - 50) 0 0 squareSize Common.Detonating 1 False
      newBombDown = Common.Bomb bX (bY + 50) 0 0 squareSize Common.Detonating 1 False
      newBombLeft = Common.Bomb (bX - 50) bY 0 0 squareSize Common.Detonating 1 False
      newBombRight = Common.Bomb (bX + 50) bY 0 0 squareSize Common.Detonating 1 False
      
      testBombs = [newBombUp, newBombDown, newBombLeft, newBombRight, b]
      
      isNotColliding bomb = not $ any (checkBombObstacleCollision bomb) obs
      
      obstaclesNotColliding o =
        o.isHardBlock ||
        (not (checkBombObstacleCollision newBombUp o) &&
         not (checkBombObstacleCollision newBombDown o) &&
         not (checkBombObstacleCollision newBombLeft o) &&
         not (checkBombObstacleCollision newBombRight o))
      
  in (filter isNotColliding testBombs, filter obstaclesNotColliding obs)

-- Check bomb-obstacle collision
checkBombObstacleCollision :: Common.Bomb -> Common.Obstacle -> Bool
checkBombObstacleCollision b obs =
  let bx = b.bombX
      by = b.bombY
      bs = b.bombSize
      ox = obs.obstacleX
      oy = obs.obstacleY
      ow = obs.obstacleWidth
      oh = obs.obstacleHeight
      
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
      
      oLeft = ox - ow / 2
      oRight = ox + ow / 2
      oTop = oy - oh / 2
      oBottom = oy + oh / 2
      
  in bRight > oLeft && bLeft < oRight &&
     bBottom > oTop && bTop < oBottom

-- Bomb-bomb detonation chain
bombBombDetonation :: GameState -> GameState
bombBombDetonation state =
  let (newBombs, newObstacles) = foldl processOneBomb ([], state.obstacles) state.bombs
  in state { bombs = newBombs, obstacles = newObstacles }
  where
    processOneBomb (accBombs, accObs) bomb =
      if any (checkBombBombCollision bomb) accBombs
        then let (newBombs, newObs) = detonateBomb accObs bomb
             in (accBombs ++ newBombs, newObs)
        else (accBombs ++ [bomb], accObs)

-- Check bomb-bomb collision
checkBombBombCollision :: Common.Bomb -> Common.Bomb -> Bool
checkBombBombCollision b ob =
  let bds = b.isDetonated
      obds = ob.isDetonated
      
      bx = b.bombX
      by = b.bombY
      bs = b.bombSize
      ox = ob.bombX
      oy = ob.bombY
      ow = ob.bombSize
      oh = ob.bombSize
      
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
      
      oLeft = ox - ow / 2
      oRight = ox + ow / 2
      oTop = oy - oh / 2
      oBottom = oy + oh / 2
      
  in bRight > oLeft && bLeft < oRight &&
     bBottom > oTop && bTop < oBottom &&
     ((bds == Common.Detonating && obds == Common.Ticking) ||
      (obds == Common.Detonating && bds == Common.Ticking))

-- Remove finished bombs
removeBombs :: GameState -> GameState
removeBombs state =
  state { bombs = filter keep state.bombs }
  where
    keep :: Common.Bomb -> Bool
    keep b = not (b.isDetonated == Common.Detonating && b.timer <= 0)

-- Check collisions with players
checkCollisions :: GameState -> GameState
checkCollisions state =
  let collision = any (\p -> any (checkPlayerDetonatingBombCollision p) state.bombs) (Map.elems state.players)
      isGameOver = state.gameOver == Common.YesGO || state.gameOver == Common.Render || collision || state.gameTimer <= 0
      newGameOver =
        if isGameOver && state.gameOver == Common.Render then Common.YesGO
        else if isGameOver && state.gameOver == Common.NotGO then Common.Render
        else if state.gameOver == Common.YesGO then Common.YesGO
        else state.gameOver
  in state { gameOver = newGameOver }

-- Check player collision with detonating bomb
checkPlayerDetonatingBombCollision :: Common.Player -> Common.Bomb -> Bool
checkPlayerDetonatingBombCollision p b =
  let px = p.playerX
      py = p.playerY
      ps = p.playerSize
      bx = b.bombX
      by = b.bombY
      bs = b.bombSize
      
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
      
  in pRight > bLeft && pLeft < bRight &&
     pBottom > bTop && pTop < bBottom &&
     b.isDetonated == Common.Detonating

-- Update game timer
updateTimer :: GameState -> GameState
updateTimer state =
  if state.gameOver /= Common.NotGO
  then state
  else state { gameTimer = state.gameTimer - 0.0167 }

-- Update ticks
updateTicks :: GameState -> GameState
updateTicks state =
  if Map.size state.players == requiredPlayerCount
  then state { ticks = state.ticks + 1 }
  else state

-- Broadcast to all clients
broadcastToClients :: MVar ServerState -> IO ()
broadcastToClients stateMVar = do
  serverState <- readMVar stateMVar
  print serverState.gameState

  when (isGameActive serverState.gameState) $ do
    let clients = serverState.clients
    let viewData = gameStateToViewData serverState.gameState
    let payload = Common.viewDataToText viewData
    putStrLn $ T.unpack payload
    putStrLn ""

    forM_ clients $ \client -> do
      void $ forkIO $ WS.sendTextData client.conn payload

-- Convert game state to view data
gameStateToViewData :: GameState -> Common.ViewData
gameStateToViewData gameState =
  Common.ViewData
    { Common.players = gameState.players
    , Common.bombs = gameState.bombs
    , Common.obstacles = gameState.obstacles
    , Common.serverTicks = gameState.ticks
    , Common.gameOver = gameState.gameOver
    , Common.gameTimer = gameState.gameTimer
    }