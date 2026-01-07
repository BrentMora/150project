{-# LANGUAGE OverloadedStrings #-}

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
  { clientId :: Common.PlayerId
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
      let newPlayerNum = Map.size (clients serverState) + 1

      if newPlayerNum > requiredPlayerCount
        then pure (serverState, Nothing)
        else do
          putStrLn $ "Player " <> show newPlayerNum <> " has connected"

          let newClient = Client newPlayerNum conn
          let newServerState =
                serverState
                  { clients = Map.insert newPlayerNum newClient (clients serverState)
                  , gameState =
                      (gameState serverState)
                        { eventQueue = eventQueue (gameState serverState) <> [NewPlayerEvent newPlayerNum]
                        }
                  }

          pure (newServerState, Just newClient)

    case maybeClient of
      Just client -> getDataFromClient client stateMVar
      Nothing -> putStrLn "Rejected new client; server is full"

getDataFromClient :: Client -> MVar ServerState -> IO ()
getDataFromClient client stateMVar = forever $ do
  raw <- WS.receiveData (conn client) :: IO T.Text
  putStrLn $ "Got data from player " <> show (clientId client)

  case Common.textToClientInput raw of
    Nothing -> putStrLn $ "Failed to parse client input: " <> T.unpack raw
    Just clientInput -> do
      modifyMVar_ stateMVar $ \serverState -> do
        if isGameActive (gameState serverState)
          then
            pure $
              serverState
                { gameState =
                    (gameState serverState)
                      { eventQueue =
                          eventQueue (gameState serverState)
                            <> [PlayerInputEvent (clientId client) (Common.keyState clientInput)]
                      }
                }
          else pure serverState

isGameActive :: GameState -> Bool
isGameActive state = Map.size (players state) == requiredPlayerCount

mainGameLoop :: MVar ServerState -> IO ()
mainGameLoop stateMVar = do
  modifyMVar_ stateMVar $ \serverState -> do
    let newGameState = updateGameState (gameState serverState)
    pure $ serverState{gameState = newGameState}

  broadcastToClients stateMVar
  threadDelay 16666

updateGameState :: GameState -> GameState
updateGameState gs =
  gs
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
  if Map.size (players state) == requiredPlayerCount
    then state{ticks = ticks state + 1}
    else state

updateGameTimer :: GameState -> GameState
updateGameTimer state =
  if Map.size (players state) == requiredPlayerCount && gameTimer state > 0
    then state{gameTimer = gameTimer state - 0.0167}
    else state

processEvents :: GameState -> GameState
processEvents gs =
  case eventQueue gs of
    [] -> gs
    _ -> gs & processNextEvent & processEvents

processNextEvent :: GameState -> GameState
processNextEvent state =
  case eventQueue state of
    [] -> state
    NewPlayerEvent newPlayerId : evs ->
      let startPos = if newPlayerId == 1 then (75, 525) else (675, 75)
          newPlayer =
            Common.Player
              { Common.playerId = newPlayerId
              , Common.playerX = fst startPos
              , Common.playerY = snd startPos
              , Common.playerVelX = 0
              , Common.playerVelY = 0
              , Common.playerSize = 30
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
           { players = Map.insert newPlayerId newPlayer (players state)
           , gameOverFlags = Map.insert newPlayerId False (gameOverFlags state)
           , eventQueue = evs
           }
    PlayerInputEvent playerId keyState : evs ->
      state
        { players =
            Map.adjust
              ( \p ->
                  let newDir =
                        if Common.up keyState
                          then Just Common.DirUp
                          else if Common.down keyState
                            then Just Common.DirDown
                          else if Common.left keyState
                            then Just Common.DirLeft
                          else if Common.right keyState
                            then Just Common.DirRight
                          else Just Common.DirNone
                      isSpacePressed = Common.space keyState
                      isPrevTickRel = Common.spaceRequest p == Common.Released
                      newSpaceRequest =
                        if not isSpacePressed
                          then Common.Released
                          else if isSpacePressed && isPrevTickRel
                            then Common.Valid
                            else Common.spaceRequest p
                  in p
                       { Common.currentDirection = newDir
                       , Common.spaceRequest = newSpaceRequest
                       }
              )
              playerId
              (players state)
        , eventQueue = evs
        }

computeTarget :: Double -> [Double] -> Double
computeTarget pXY = minimumBy (\a b -> compare (abs (pXY - a)) (abs (pXY - b)))

clamp :: Ord a => a -> a -> a -> a
clamp minV maxV = max minV . min maxV

checkPlayerObstacleCollision :: Common.Player -> Common.Obstacle -> Bool
checkPlayerObstacleCollision p obs =
  let px = Common.playerX p
      py = Common.playerY p
      ps = Common.playerSize p
      ox = Common.obstacleX obs
      oy = Common.obstacleY obs
      ow = Common.obstacleWidth obs
      oh = Common.obstacleHeight obs
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
  let px = Common.playerX p
      py = Common.playerY p
      ps = Common.playerSize p
      bx = Common.bombX b
      by = Common.bombY b
      bs = Common.bombSize b
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
  in pRight > bLeft && pLeft < bRight && pBottom > bTop && pTop < bBottom
       && Common.isDetonated b == Common.Ticking
       && not (Common.isOverlapping b)

updatePlayerMovement :: GameState -> GameState
updatePlayerMovement state =
  state
    { players =
        Map.map
          ( \p ->
              let speed = 5 + (2 * Common.speedups p)
                  vx = case Common.currentDirection p of
                    Just Common.DirLeft -> -speed
                    Just Common.DirRight -> speed
                    _ -> 0
                  vy = case Common.currentDirection p of
                    Just Common.DirUp -> -speed
                    Just Common.DirDown -> speed
                    _ -> 0
                  newX = Common.playerX p + vx
                  newY = Common.playerY p + vy
                  newTargetX = computeTarget newX Common.xCoords
                  newTargetY = computeTarget newY Common.yCoords
                  testPlayer =
                    p
                      { Common.playerX = clamp 0 750 newX
                      , Common.playerY = clamp 0 650 newY
                      , Common.targetX = newTargetX
                      , Common.targetY = newTargetY
                      }
                  wouldCollide = any (checkPlayerObstacleCollision testPlayer) (obstacles state)
                  wouldCollideB = any (checkPlayerBombCollision testPlayer) (bombs state)
              in if wouldCollide || wouldCollideB then p else testPlayer
          )
          (players state)
    }

checkIfBombExists :: Common.Bomb -> [Common.Bomb] -> Bool
checkIfBombExists b bs = any (\x -> Common.bombX x == Common.bombX b && Common.bombY x == Common.bombY b) bs

updateBombPlacement :: GameState -> GameState
updateBombPlacement state =
  let (newBombs, updatedPlayers) = Map.foldr placeBombForPlayer (bombs state, players state) (players state)
  in state{bombs = newBombs, players = updatedPlayers}
  where
    placeBombForPlayer p (bs, ps) =
      let newBomb =
            Common.Bomb
              { Common.bombX = Common.targetX p
              , Common.bombY = Common.targetY p
              , Common.bombVelX = 0
              , Common.bombVelY = 0
              , Common.bombSize = squareSize
              , Common.isDetonated = Common.Ticking
              , Common.timer = 3
              , Common.isOverlapping = True
              , Common.bombDirection = Common.Core
              , Common.growth = 0
              }
          canPlace =
            Common.bombsHeld p > 0
              && Common.spaceRequest p == Common.Valid
              && not (checkIfBombExists newBomb bs)
          newBombs' = if canPlace then bs ++ [newBomb] else bs
          newPlayers' =
            if canPlace
              then
                Map.adjust
                  ( \pl ->
                      pl
                        { Common.bombsHeld = Common.bombsHeld pl - 1
                        , Common.spaceRequest = Common.Blocked
                        }
                  )
                  (Common.playerId p)
                  ps
              else ps
      in (newBombs', newPlayers')

updateBombTimers :: GameState -> GameState
updateBombTimers state =
  state{bombs = map updateTimer (bombs state)}
  where
    updateTimer b
      | Common.timer b <= 0 && Common.isDetonated b == Common.Ticking =
          b{Common.isDetonated = Common.Detonating, Common.timer = 1}
      | Common.timer b <= 0 && Common.isDetonated b == Common.Detonating =
          b{Common.isDetonated = Common.Done}
      | otherwise = b{Common.timer = Common.timer b - 0.0167}

checkBombObstacleCollision :: Common.Bomb -> Common.Obstacle -> Bool
checkBombObstacleCollision b obs =
  let bx = Common.bombX b
      by = Common.bombY b
      bs = Common.bombSize b
      ox = Common.obstacleX obs
      oy = Common.obstacleY obs
      ow = Common.obstacleWidth obs
      oh = Common.obstacleHeight obs
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
  let bx = Common.bombX b
      by = Common.bombY b
      bs = Common.bombSize b
      ox = Common.bombX ob
      oy = Common.bombY ob
      ow = Common.bombSize ob
      oh = Common.bombSize ob
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
       && ((Common.isDetonated b == Common.Detonating && Common.isDetonated ob == Common.Ticking)
            || (Common.isDetonated ob == Common.Detonating && Common.isDetonated b == Common.Ticking))

updateBombDetonations :: GameState -> GameState
updateBombDetonations state =
  let (newBombs, newObs, newPups) = foldl processOneBomb ([], obstacles state, powerups state) (bombs state)
  in state{bombs = newBombs, obstacles = newObs, powerups = newPups}
  where
    processOneBomb (accBombs, accObs, accPups) bomb =
      if Common.isDetonated bomb == Common.Ticking && Common.timer bomb <= 0
        then
          let (detonatedBombs, newObs, newPups) = detonateBomb accObs accPups bomb (players state)
          in (accBombs ++ detonatedBombs, newObs, newPups)
        else (accBombs ++ [bomb], accObs, accPups)

updateBombBombDetonations :: GameState -> GameState
updateBombBombDetonations state =
  let (newBombs, newObs, newPups) = foldl processOneBomb ([], obstacles state, powerups state) (bombs state)
  in state{bombs = newBombs, obstacles = newObs, powerups = newPups}
  where
    processOneBomb (accBombs, accObs, accPups) bomb =
      if any (checkBombBombCollision bomb) accBombs
        then
          let (detonatedBombs, newObs, newPups) = detonateBomb accObs accPups bomb (players state)
          in (accBombs ++ detonatedBombs, newObs, newPups)
        else (accBombs ++ [bomb], accObs, accPups)

detonateBomb :: [Common.Obstacle] -> [Common.PowerUp] -> Common.Bomb -> Map Common.PlayerId Common.Player -> ([Common.Bomb], [Common.Obstacle], [Common.PowerUp])
detonateBomb obs pups b ps =
  let bX = Common.bombX b
      bY = Common.bombY b
      range = if null (Map.elems ps) then 1 else maximum $ map Common.fireups $ Map.elems ps
      newBombUp = b{Common.bombX = bX, Common.bombY = bY - 50, Common.isDetonated = Common.Detonating, Common.timer = 1, Common.isOverlapping = False, Common.bombDirection = Common.North, Common.growth = range - 1}
      newBombDown = b{Common.bombX = bX, Common.bombY = bY + 50, Common.isDetonated = Common.Detonating, Common.timer = 1, Common.isOverlapping = False, Common.bombDirection = Common.South, Common.growth = range - 1}
      newBombLeft = b{Common.bombX = bX - 50, Common.bombY = bY, Common.isDetonated = Common.Detonating, Common.timer = 1, Common.isOverlapping = False, Common.bombDirection = Common.West, Common.growth = range - 1}
      newBombRight = b{Common.bombX = bX + 50, Common.bombY = bY, Common.isDetonated = Common.Detonating, Common.timer = 1, Common.isOverlapping = False, Common.bombDirection = Common.East, Common.growth = range - 1}
      testBombs = [newBombUp, newBombDown, newBombLeft, newBombRight, b]
      testBombs' = growBombs obs testBombs []
      powerUpsRemain pw = not $ any (`checkBombPowerUpCollision` pw) testBombs'
      isNotColliding bomb = not $ any (checkBombObstacleCollision bomb) obs
      obstaclesNotColliding o = Common.isHardBlock o || not (any (`checkBombObstacleCollision` o) testBombs')
  in (filter isNotColliding testBombs', filter obstaclesNotColliding obs, filter powerUpsRemain pups)

checkBombPowerUpCollision :: Common.Bomb -> Common.PowerUp -> Bool
checkBombPowerUpCollision b pu =
  let bx = Common.bombX b
      by = Common.bombY b
      bs = Common.bombSize b
      px = Common.powerupX pu
      py = Common.powerupY pu
      ps = Common.powerupSize pu
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
  | Common.bombDirection b == Common.Core = growBombs obs bt (acc ++ [b])
  | Common.growth b == 0 = growBombs obs bt (acc ++ [b])
  | any (checkBombObstacleCollision b) obs = growBombs obs bt (acc ++ [b{Common.growth = 0}])
  | Common.growth b == 1 =
      let newBomb = createBomb b
          decB = b{Common.growth = Common.growth b - 1}
      in growBombs obs (bt ++ [newBomb]) (acc ++ [decB])
  | otherwise =
      let newBomb = createBomb b
          decB = b{Common.growth = Common.growth b - 1}
      in growBombs obs (bt ++ [newBomb] ++ [decB]) acc

createBomb :: Common.Bomb -> Common.Bomb
createBomb b
  | Common.bombDirection b == Common.Core = b
  | Common.growth b == 0 = b
  | Common.bombDirection b == Common.North = b{Common.bombX = Common.bombX b, Common.bombY = Common.bombY b - 50, Common.growth = Common.growth b - 1}
  | Common.bombDirection b == Common.South = b{Common.bombX = Common.bombX b, Common.bombY = Common.bombY b + 50, Common.growth = Common.growth b - 1}
  | Common.bombDirection b == Common.West = b{Common.bombX = Common.bombX b - 50, Common.bombY = Common.bombY b, Common.growth = Common.growth b - 1}
  | otherwise = b{Common.bombX = Common.bombX b + 50, Common.bombY = Common.bombY b, Common.growth = Common.growth b - 1}

updateBombConnections :: GameState -> GameState
updateBombConnections state =
  state{bombs = filter (isBombConnected (obstacles state) (bombs state)) (bombs state)}

isBombConnected :: [Common.Obstacle] -> [Common.Bomb] -> Common.Bomb -> Bool
isBombConnected obs _ b =
  case Common.bombDirection b of
    Common.Core -> True
    Common.North -> not $ any (\o -> Common.obstacleX o == Common.bombX b && Common.obstacleY o == Common.bombY b + 50) obs
    Common.South -> not $ any (\o -> Common.obstacleX o == Common.bombX b && Common.obstacleY o == Common.bombY b - 50) obs
    Common.East -> not $ any (\o -> Common.obstacleX o == Common.bombX b + 50 && Common.obstacleY o == Common.bombY b) obs
    Common.West -> not $ any (\o -> Common.obstacleX o == Common.bombX b - 50 && Common.obstacleY o == Common.bombY b) obs

updateBombRemoval :: GameState -> GameState
updateBombRemoval state =
  state{bombs = filter (\b -> not (Common.isDetonated b == Common.Detonating && Common.timer b <= 0)) (bombs state)}

checkBombPlayerCollision :: Common.Bomb -> Common.Player -> Bool
checkBombPlayerCollision b p =
  let px = Common.playerX p
      py = Common.playerY p
      ps = Common.playerSize p
      bx = Common.bombX b
      by = Common.bombY b
      bs = Common.bombSize b
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
  in pRight > bLeft && pLeft < bRight && pBottom > bTop && pTop < bBottom && Common.isDetonated b == Common.Ticking

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
              foldl (flip updateBombOverlapping) b (Map.elems (players state))
          )
          (bombs state)
    }

updatePlayerBombIncrement :: GameState -> GameState
updatePlayerBombIncrement state =
  state
    { players =
        Map.map
          ( \p ->
              let lenB = length (bombs state)
                  mb = Common.maxbombs p
              in if lenB < mb && Common.bombsHeld p < mb
                   then p{Common.bombsHeld = mb - lenB}
                   else p
          )
          (players state)
    }

checkPlayerPowerUpCollision :: Common.Player -> Common.PowerUp -> Bool
checkPlayerPowerUpCollision p pu =
  let px = Common.playerX p
      py = Common.playerY p
      ps = Common.playerSize p
      pux = Common.powerupX pu
      puy = Common.powerupY pu
      puw = Common.powerupSize pu
      puh = Common.powerupSize pu
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
  case Common.powerupType pu of
    Common.FireUp -> p{Common.fireups = Common.fireups p + 1}
    Common.BombUp -> p{Common.maxbombs = Common.maxbombs p + 1}
    Common.SpeedUp -> p{Common.speedups = Common.speedups p + 1}

updatePlayerPowerUps :: GameState -> GameState
updatePlayerPowerUps state =
  let (newPlayers, newPowerups) = Map.foldr updateOnePlayer (players state, powerups state) (players state)
  in state{players = newPlayers, powerups = newPowerups}
  where
    updateOnePlayer p (ps, pups) =
      case find (checkPlayerPowerUpCollision p) pups of
        Just pu ->
          ( Map.adjust (flip addPowerUpToPlayer pu) (Common.playerId p) ps
          , delete pu pups
          )
        Nothing -> (ps, pups)

checkCollision :: Common.Player -> Common.Bomb -> Bool
checkCollision p b =
  let px = Common.playerX p
      py = Common.playerY p
      ps = Common.playerSize p
      bx = Common.bombX b
      by = Common.bombY b
      bs = Common.bombSize b
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
  in pRight > bLeft && pLeft < bRight && pBottom > bTop && pTop < bBottom && Common.isDetonated b == Common.Detonating

updateCollisions :: GameState -> GameState
updateCollisions state =
  state
    { gameOverFlags =
        Map.mapWithKey
          ( \playerId flag ->
              case Map.lookup playerId (players state) of
                Nothing -> flag
                Just p -> flag || any (checkCollision p) (bombs state) || gameTimer state <= 0
          )
          (gameOverFlags state)
    }

broadcastToClients :: MVar ServerState -> IO ()
broadcastToClients stateMVar = do
  serverState <- readMVar stateMVar

  when (isGameActive (gameState serverState)) $ do
    let cs = clients serverState
    let viewData = gameStateToViewData (gameState serverState)
    let payload = Common.viewDataToText viewData

    forM_ cs $ \client -> do
      void $ forkIO $ WS.sendTextData (conn client) payload

gameStateToViewData :: GameState -> Common.ViewData
gameStateToViewData gs =
  Common.ViewData
    { Common.players = players gs
    , Common.bombs = bombs gs
    , Common.obstacles = obstacles gs
    , Common.powerups = powerups gs
    , Common.gameTimer = gameTimer gs
    , Common.serverTicks = ticks gs
    , Common.gameOverFlags = gameOverFlags gs
    }