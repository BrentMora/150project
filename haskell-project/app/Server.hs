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
import Data.List (minimumBy, find, delete, (\\))
import Data.Maybe (catMaybes)

requiredPlayerCount :: Int
requiredPlayerCount = 1

squareSize :: Double
squareSize = 50

data ServerState = ServerState
  { clients :: Map Common.PlayerId Client
  , gameState :: GameState
  }

data Client = Client
  { id :: Common.PlayerId
  , conn :: WS.Connection
  }

data GameState = GameState
  { ticks :: Int
  , players :: Map Common.PlayerId Common.Player
  , bombs :: [Common.Bomb]
  , obstacles :: [Common.Obstacle]
  , powerups :: [Common.PowerUp]
  , gameTimer :: Float
  , gameOverFlags :: Map Common.PlayerId Bool
  , eventQueue :: [GameEvent]
  }
  deriving (Show, Eq)

data GameEvent
  = NewPlayerEvent Common.PlayerId
  | PlayerInputEvent Common.PlayerId Common.KeyState
  deriving (Show, Eq)

mainServer :: IO ()
mainServer = do
  putStrLn "Server is starting..."
  stateMVar <- newMVar initServerState

  putStrLn "Main game loop is starting..."
  void $ forkIO $ forever $ mainGameLoop stateMVar

  putStrLn "Listening process is starting..."
  WS.runServer "127.0.0.1" 15000 $ app stateMVar

initServerState :: ServerState
initServerState =
  ServerState
    { clients = Map.empty
    , gameState = initGameState
    }

initGameState :: GameState
initGameState =
  GameState
    { ticks = 0
    , players = Map.empty
    , bombs = []
    , obstacles = initialObstacles
    , powerups = []
    , gameTimer = 60.0
    , gameOverFlags = Map.empty
    , eventQueue = []
    }

initialObstacles :: [Common.Obstacle]
initialObstacles =
  -- Top border
  [ Common.Obstacle 25 25 squareSize squareSize True
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
  -- Left border
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
  -- Right border
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
  -- Bottom border
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
  -- Interior hard blocks
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

getDataFromClient :: Client -> MVar ServerState -> IO ()
getDataFromClient client stateMVar = forever $ do
  raw <- WS.receiveData client.conn :: IO T.Text
  putStrLn $ "Got data from player " <> show client.id

  case Common.textToClientInput raw of
    Nothing -> putStrLn $ "Failed to parse client input: " <> T.unpack raw
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

isGameActive :: GameState -> Bool
isGameActive state = Map.size state.players == requiredPlayerCount

mainGameLoop :: MVar ServerState -> IO ()
mainGameLoop stateMVar = do
  modifyMVar_ stateMVar $ \serverState -> do
    let newGameState = updateGameState serverState.gameState
    pure $ serverState{gameState = newGameState}

  broadcastToClients stateMVar
  threadDelay 16666

updateGameState :: GameState -> GameState
updateGameState gameState =
  gameState
    & processEvents
    & updatePlayerMovement
    & updateBombPlacement
    & updateBombTimers
    & updateBombDetonations
    & updateBombBombDetonations
    & updateBombConnections
    & updateBombRemoval
    & updatePlayerBombOverlapping
    & updatePlayerBombIncrement
    & updatePlayerPowerUps
    & updateCollisions
    & updateGameTimer
    & updateTicks

updateTicks :: GameState -> GameState
updateTicks state =
  if Map.size state.players == requiredPlayerCount
    then state{ticks = state.ticks + 1}
    else state

updateGameTimer :: GameState -> GameState
updateGameTimer state =
  if Map.size state.players == requiredPlayerCount && state.gameTimer > 0
    then state{gameTimer = state.gameTimer - 0.0167}
    else state

processEvents :: GameState -> GameState
processEvents gameState =
  case gameState.eventQueue of
    [] -> gameState
    _ -> gameState & processNextEvent & processEvents

processNextEvent :: GameState -> GameState
processNextEvent state =
  case state.eventQueue of
    [] -> state
    NewPlayerEvent newPlayerId : evs ->
      let startPos = if newPlayerId == 1 then (75, 575) else (675, 75)
          newPlayer =
            Common.Player
              { Common.id = newPlayerId
              , Common.x = fst startPos
              , Common.y = snd startPos
              , Common.velX = 0
              , Common.velY = 0
              , Common.size = 30
              , Common.bombsHeld = 1
              , Common.targetX = fst startPos
              , Common.targetY = snd startPos
              , Common.spaceRequest = Common.Released
              , Common.currentDirection = Just Common.DirNone
              , Common.maxbombs = 1
              , Common.speedups = 0
              , Common.fireups = 1
              }
      in state
           { players = Map.insert newPlayerId newPlayer state.players
           , gameOverFlags = Map.insert newPlayerId False state.gameOverFlags
           , eventQueue = evs
           }
    PlayerInputEvent playerId keyState : evs ->
      state
        { players =
            Map.adjust
              ( \p ->
                  let newDir =
                        if keyState.up
                          then Just Common.DirUp
                          else if keyState.down
                            then Just Common.DirDown
                          else if keyState.left
                            then Just Common.DirLeft
                          else if keyState.right
                            then Just Common.DirRight
                          else Just Common.DirNone
                      isSpacePressed = keyState.space
                      isPrevTickRel = p.spaceRequest == Common.Released
                      newSpaceRequest =
                        if not isSpacePressed
                          then Common.Released
                          else if isSpacePressed && isPrevTickRel
                            then Common.Valid
                            else p.spaceRequest
                  in p
                       { Common.currentDirection = newDir
                       , Common.spaceRequest = newSpaceRequest
                       }
              )
              playerId
              state.players
        , eventQueue = evs
        }

computeTarget :: Double -> [Double] -> Double
computeTarget pXY = minimumBy (\a b -> compare (abs (pXY - a)) (abs (pXY - b)))

clamp :: Ord a => a -> a -> a -> a
clamp minV maxV = max minV . min maxV

checkPlayerObstacleCollision :: Common.Player -> Common.Obstacle -> Bool
checkPlayerObstacleCollision p obs =
  let px = p.x
      py = p.y
      ps = p.size
      ox = obs.x
      oy = obs.y
      ow = obs.width
      oh = obs.height
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      oLeft = ox - ow / 2
      oRight = ox + ow / 2
      oTop = oy - oh / 2
      oBottom = oy + oh / 2
  in pRight > oLeft && pLeft < oRight && pBottom > oTop && pTop < oBottom

checkPlayerBombCollision :: Common.Player -> Common.Bomb -> Bool
checkPlayerBombCollision p b =
  let px = p.x
      py = p.y
      ps = p.size
      bx = b.x
      by = b.y
      bs = b.size
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
  in pRight > bLeft && pLeft < bRight && pBottom > bTop && pTop < bBottom
       && b.isDetonated == Common.Ticking
       && not b.isOverlapping

updatePlayerMovement :: GameState -> GameState
updatePlayerMovement state =
  state
    { players =
        Map.map
          ( \p ->
              let speed = 5 + (2 * p.speedups)
                  vx = case p.currentDirection of
                    Just Common.DirLeft -> -speed
                    Just Common.DirRight -> speed
                    _ -> 0
                  vy = case p.currentDirection of
                    Just Common.DirUp -> -speed
                    Just Common.DirDown -> speed
                    _ -> 0
                  newX = p.x + vx
                  newY = p.y + vy
                  newTargetX = computeTarget newX Common.xCoords
                  newTargetY = computeTarget newY Common.yCoords
                  testPlayer =
                    p
                      { Common.x = clamp 0 750 newX
                      , Common.y = clamp 0 650 newY
                      , Common.targetX = newTargetX
                      , Common.targetY = newTargetY
                      }
                  wouldCollide = any (checkPlayerObstacleCollision testPlayer) state.obstacles
                  wouldCollideB = any (checkPlayerBombCollision testPlayer) state.bombs
              in if wouldCollide || wouldCollideB then p else testPlayer
          )
          state.players
    }

checkIfBombExists :: Common.Bomb -> [Common.Bomb] -> Bool
checkIfBombExists b bombs = any (\x -> x.x == b.x && x.y == b.y) bombs

updateBombPlacement :: GameState -> GameState
updateBombPlacement state =
  let (newBombs, updatedPlayers) = Map.foldr placeBombForPlayer (state.bombs, state.players) state.players
  in state{bombs = newBombs, players = updatedPlayers}
  where
    placeBombForPlayer p (bombs, players) =
      let newBomb =
            Common.Bomb
              { Common.x = p.targetX
              , Common.y = p.targetY
              , Common.velX = 0
              , Common.velY = 0
              , Common.size = squareSize
              , Common.isDetonated = Common.Ticking
              , Common.timer = 3
              , Common.isOverlapping = True
              , Common.bombDirection = Common.Core
              , Common.growth = 0
              }
          canPlace =
            p.bombsHeld > 0
              && p.spaceRequest == Common.Valid
              && not (checkIfBombExists newBomb bombs)
          newBombs' = if canPlace then bombs ++ [newBomb] else bombs
          newPlayers' =
            if canPlace
              then
                Map.adjust
                  ( \pl ->
                      pl
                        { Common.bombsHeld = pl.bombsHeld - 1
                        , Common.spaceRequest = Common.Blocked
                        }
                  )
                  p.id
                  players
              else players
      in (newBombs', newPlayers')

updateBombTimers :: GameState -> GameState
updateBombTimers state =
  state{bombs = map updateTimer state.bombs}
  where
    updateTimer b
      | b.timer <= 0 && b.isDetonated == Common.Ticking =
          b{Common.isDetonated = Common.Detonating, Common.timer = 1}
      | b.timer <= 0 && b.isDetonated == Common.Detonating =
          b{Common.isDetonated = Common.Done}
      | otherwise = b{Common.timer = b.timer - 0.0167}

checkBombObstacleCollision :: Common.Bomb -> Common.Obstacle -> Bool
checkBombObstacleCollision b obs =
  let bx = b.x
      by = b.y
      bs = b.size
      ox = obs.x
      oy = obs.y
      ow = obs.width
      oh = obs.height
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
      oLeft = ox - ow / 2
      oRight = ox + ow / 2
      oTop = oy - oh / 2
      oBottom = oy + oh / 2
  in bRight > oLeft && bLeft < oRight && bBottom > oTop && bTop < oBottom

checkBombBombCollision :: Common.Bomb -> Common.Bomb -> Bool
checkBombBombCollision b ob =
  let bx = b.x
      by = b.y
      bs = b.size
      ox = ob.x
      oy = ob.y
      ow = ob.size
      oh = ob.size
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
      oLeft = ox - ow / 2
      oRight = ox + ow / 2
      oTop = oy - oh / 2
      oBottom = oy + oh / 2
  in bRight > oLeft && bLeft < oRight
       && bBottom > oTop && bTop < oBottom
       && ((b.isDetonated == Common.Detonating && ob.isDetonated == Common.Ticking)
            || (ob.isDetonated == Common.Detonating && b.isDetonated == Common.Ticking))

updateBombDetonations :: GameState -> GameState
updateBombDetonations state =
  let (newBombs, newObs, newPups) = foldl processOneBomb ([], state.obstacles, state.powerups) state.bombs
  in state{bombs = newBombs, obstacles = newObs, powerups = newPups}
  where
    processOneBomb (accBombs, accObs, accPups) bomb =
      if bomb.isDetonated == Common.Ticking && bomb.timer <= 0
        then
          let (detonatedBombs, newObs, newPups) = detonateBomb accObs accPups bomb state.players
          in (accBombs ++ detonatedBombs, newObs, newPups)
        else (accBombs ++ [bomb], accObs, accPups)

updateBombBombDetonations :: GameState -> GameState
updateBombBombDetonations state =
  let (newBombs, newObs, newPups) = foldl processOneBomb ([], state.obstacles, state.powerups) state.bombs
  in state{bombs = newBombs, obstacles = newObs, powerups = newPups}
  where
    processOneBomb (accBombs, accObs, accPups) bomb =
      if any (checkBombBombCollision bomb) accBombs
        then
          let (detonatedBombs, newObs, newPups) = detonateBomb accObs accPups bomb state.players
          in (accBombs ++ detonatedBombs, newObs, newPups)
        else (accBombs ++ [bomb], accObs, accPups)

detonateBomb :: [Common.Obstacle] -> [Common.PowerUp] -> Common.Bomb -> Map Common.PlayerId Common.Player -> ([Common.Bomb], [Common.Obstacle], [Common.PowerUp])
detonateBomb obs pups b players =
  let bX = b.x
      bY = b.y
      range = maximum $ map (.fireups) $ Map.elems players
      newBombUp = b{Common.x = bX, Common.y = bY - 50, Common.isDetonated = Common.Detonating, Common.timer = 1, Common.isOverlapping = False, Common.bombDirection = Common.North, Common.growth = range - 1}
      newBombDown = b{Common.x = bX, Common.y = bY + 50, Common.isDetonated = Common.Detonating, Common.timer = 1, Common.isOverlapping = False, Common.bombDirection = Common.South, Common.growth = range - 1}
      newBombLeft = b{Common.x = bX - 50, Common.y = bY, Common.isDetonated = Common.Detonating, Common.timer = 1, Common.isOverlapping = False, Common.bombDirection = Common.West, Common.growth = range - 1}
      newBombRight = b{Common.x = bX + 50, Common.y = bY, Common.isDetonated = Common.Detonating, Common.timer = 1, Common.isOverlapping = False, Common.bombDirection = Common.East, Common.growth = range - 1}
      testBombs = [newBombUp, newBombDown, newBombLeft, newBombRight, b]
      testBombs' = growBombs obs testBombs []
      powerUpsRemain pw = not $ any (`checkBombPowerUpCollision` pw) testBombs'
      isNotColliding bomb = not $ any (checkBombObstacleCollision bomb) obs
      obstaclesNotColliding o = o.isHardBlock || not (any (`checkBombObstacleCollision` o) testBombs')
  in (filter isNotColliding testBombs', filter obstaclesNotColliding obs, filter powerUpsRemain pups)

checkBombPowerUpCollision :: Common.Bomb -> Common.PowerUp -> Bool
checkBombPowerUpCollision b pu =
  let bx = b.x
      by = b.y
      bs = b.size
      px = pu.x
      py = pu.y
      ps = pu.size
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
  in bRight > pLeft && bLeft < pRight && bBottom > pTop && bTop < pBottom

growBombs :: [Common.Obstacle] -> [Common.Bomb] -> [Common.Bomb] -> [Common.Bomb]
growBombs _ [] acc = acc
growBombs obs (b : bt) acc
  | b.bombDirection == Common.Core = growBombs obs bt (acc ++ [b])
  | b.growth == 0 = growBombs obs bt (acc ++ [b])
  | any (checkBombObstacleCollision b) obs = growBombs obs bt (acc ++ [b{Common.growth = 0}])
  | b.growth == 1 =
      let newBomb = createBomb b
          decB = b{Common.growth = b.growth - 1}
      in growBombs obs (bt ++ [newBomb]) (acc ++ [decB])
  | otherwise =
      let newBomb = createBomb b
          decB = b{Common.growth = b.growth - 1}
      in growBombs obs (bt ++ [newBomb] ++ [decB]) acc

createBomb :: Common.Bomb -> Common.Bomb
createBomb b
  | b.bombDirection == Common.Core = b
  | b.growth == 0 = b
  | b.bombDirection == Common.North = b{Common.x = b.x, Common.y = b.y - 50, Common.growth = b.growth - 1}
  | b.bombDirection == Common.South = b{Common.x = b.x, Common.y = b.y + 50, Common.growth = b.growth - 1}
  | b.bombDirection == Common.West = b{Common.x = b.x - 50, Common.y = b.y, Common.growth = b.growth - 1}
  | otherwise = b{Common.x = b.x + 50, Common.y = b.y, Common.growth = b.growth - 1}

updateBombConnections :: GameState -> GameState
updateBombConnections state =
  state{bombs = filter (isBombConnected state.obstacles state.bombs) state.bombs}

isBombConnected :: [Common.Obstacle] -> [Common.Bomb] -> Common.Bomb -> Bool
isBombConnected obs _ b =
  case b.bombDirection of
    Common.Core -> True
    Common.North -> not $ any (\o -> o.x == b.x && o.y == b.y + 50) obs
    Common.South -> not $ any (\o -> o.x == b.x && o.y == b.y - 50) obs
    Common.East -> not $ any (\o -> o.x == b.x + 50 && o.y == b.y) obs
    Common.West -> not $ any (\o -> o.x == b.x - 50 && o.y == b.y) obs

updateBombRemoval :: GameState -> GameState
updateBombRemoval state =
  state{bombs = filter (\b -> not (b.isDetonated == Common.Detonating && b.timer <= 0)) state.bombs}

checkBombPlayerCollision :: Common.Bomb -> Common.Player -> Bool
checkBombPlayerCollision b p =
  let px = p.x
      py = p.y
      ps = p.size
      bx = b.x
      by = b.y
      bs = b.size
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
  in pRight > bLeft && pLeft < bRight && pBottom > bTop && pTop < bBottom && b.isDetonated == Common.Ticking

updateBombOverlapping :: Common.Player -> Common.Bomb -> Common.Bomb
updateBombOverlapping p b =
  if not (checkBombPlayerCollision b p)
    then b{Common.isOverlapping = False}
    else b

updatePlayerBombOverlapping :: GameState -> GameState
updatePlayerBombOverlapping state =
  state
    { bombs =
        map
          ( \b ->
              foldl (flip updateBombOverlapping) b (Map.elems state.players)
          )
          state.bombs
    }

updatePlayerBombIncrement :: GameState -> GameState
updatePlayerBombIncrement state =
  state
    { players =
        Map.map
          ( \p ->
              let lenB = length state.bombs
                  mb = p.maxbombs
              in if lenB < mb && p.bombsHeld < mb
                   then p{Common.bombsHeld = mb - lenB}
                   else p
          )
          state.players
    }

checkPlayerPowerUpCollision :: Common.Player -> Common.PowerUp -> Bool
checkPlayerPowerUpCollision p pu =
  let px = p.x
      py = p.y
      ps = p.size
      pux = pu.x
      puy = pu.y
      puw = pu.size
      puh = pu.size
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      puLeft = pux - puw / 2
      puRight = pux + puw / 2
      puTop = puy - puh / 2
      puBottom = puy + puh / 2
  in pRight > puLeft && pLeft < puRight && pBottom > puTop && pTop < puBottom

addPowerUpToPlayer :: Common.Player -> Common.PowerUp -> Common.Player
addPowerUpToPlayer p pu =
  case pu.powerupType of
    Common.FireUp -> p{Common.fireups = p.fireups + 1}
    Common.BombUp -> p{Common.maxbombs = p.maxbombs + 1}
    Common.SpeedUp -> p{Common.speedups = p.speedups + 1}

updatePlayerPowerUps :: GameState -> GameState
updatePlayerPowerUps state =
  let (newPlayers, newPowerups) = Map.foldr updateOnePlayer (state.players, state.powerups) state.players
  in state{players = newPlayers, powerups = newPowerups}
  where
    updateOnePlayer p (players, powerups) =
      case find (checkPlayerPowerUpCollision p) powerups of
        Just pu ->
          ( Map.adjust (flip addPowerUpToPlayer pu) p.id players
          , delete pu powerups
          )
        Nothing -> (players, powerups)

pseudoRandom :: Int -> Int -> Int
pseudoRandom seed offset = ((seed + offset) * 1103515245 + 12345) `mod` 10

isPowerUp :: Int -> Bool
isPowerUp n = (n `mod` 10) == 1

selectPowerUp :: Int -> Common.PowerUpType
selectPowerUp n =
  case n `mod` 3 of
    0 -> Common.FireUp
    1 -> Common.SpeedUp
    _ -> Common.BombUp

getNewPowerUp :: Int -> Common.Obstacle -> Maybe Common.PowerUp
getNewPowerUp n o =
  if isPowerUp n
    then
      Just $
        Common.PowerUp
          { Common.x = o.x
          , Common.y = o.y
          , Common.size = 50
          , Common.powerupType = selectPowerUp n
          }
    else Nothing

checkCollision :: Common.Player -> Common.Bomb -> Bool
checkCollision p b =
  let px = p.x
      py = p.y
      ps = p.size
      bx = b.x
      by = b.y
      bs = b.size
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
  in pRight > bLeft && pLeft < bRight && pBottom > bTop && pTop < bBottom && b.isDetonated == Common.Detonating

updateCollisions :: GameState -> GameState
updateCollisions state =
  state
    { gameOverFlags =
        Map.mapWithKey
          ( \playerId flag ->
              case Map.lookup playerId state.players of
                Nothing -> flag
                Just p -> flag || any (checkCollision p) state.bombs || state.gameTimer <= 0
          )
          state.gameOverFlags
    }

broadcastToClients :: MVar ServerState -> IO ()
broadcastToClients stateMVar = do
  serverState <- readMVar stateMVar

  when (isGameActive serverState.gameState) $ do
    let clients = serverState.clients
    let viewData = gameStateToViewData serverState.gameState
    let payload = Common.viewDataToText viewData

    forM_ clients $ \client -> do
      void $ forkIO $ WS.sendTextData client.conn payload

gameStateToViewData :: GameState -> Common.ViewData
gameStateToViewData gameState =
  Common.ViewData
    { Common.players = gameState.players
    , Common.bombs = gameState.bombs
    , Common.obstacles = gameState.obstacles
    , Common.powerups = gameState.powerups
    , Common.gameTimer = gameState.gameTimer
    , Common.serverTicks = gameState.ticks
    , Common.gameOverFlags = gameState.gameOverFlags
    }