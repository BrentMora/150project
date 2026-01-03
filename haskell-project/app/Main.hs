-- From previous version
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE OverloadedRecordDot #-}  -- Allows dot notation for record fields (model.x instead of x model)
{-# LANGUAGE OverloadedStrings #-}    -- Allows string literals to be polymorphic (useful for Text, etc.)
{-# LANGUAGE NoFieldSelectors #-}     -- Prevents automatic generation of field selector functions to avoid name conflicts

module Main where

-- Miso Imports
import qualified Miso as M                    -- Main Miso framework module
import qualified Miso.Html as H               -- HTML element constructors
import qualified Miso.Html.Property as P      -- HTML properties and attributes
import Miso.CSS (Color (..))                  -- Color type for CSS
import qualified Miso.Canvas as Canvas        -- Canvas API for drawing graphics

-- IntSet Imports
import Data.IntSet (IntSet)                   -- Efficient set of integers
import qualified Data.IntSet as IntSet        -- IntSet operations
import Debug.Trace (trace)                    -- Debug tracing utility

-- =========================
-- Constants
-- =========================

screenWidth :: Double
screenWidth = 750                             -- Canvas width in pixels

screenHeight :: Double
screenHeight = 650                            -- Canvas height in pixels

squareSize :: Double
squareSize = 50                               -- Size of the moving square (50x50 pixels)

-- =========================
-- Main Entry Point
-- =========================

main :: IO ()
main = M.run $ M.startApp app                 -- Entry point: starts the Miso application
 where
  app =
    (M.component initModel update view)       -- Create app with initial model, update function, and view function
      { M.initialAction = Just MsgGetTime     -- First action to trigger when app starts (begins animation loop)
      , M.subs = [M.keyboardSub handleKey]    -- Subscribe to keyboard events, routing them through handleKey
      }

-- =========================
-- Keys Inputs
-- =========================

handleKey :: IntSet -> Msg                    -- Converts keyboard key codes to messages
handleKey intSet
  | IntSet.member 37 intSet = MsgMoveLeft     -- Key code 37 = left arrow
  | IntSet.member 38 intSet = MsgMoveUp       -- Key code 38 = up arrow
  | IntSet.member 39 intSet = MsgMoveRight    -- Key code 39 = right arrow
  | IntSet.member 40 intSet = MsgMoveDown     -- Key code 40 = down arrow
  | IntSet.member 32 intSet = MsgPlaceBomb    -- Key code 32 = space bar
  | otherwise = MsgNoOp                       -- Any other key does nothing

-- =========================
-- Data
-- =========================

data DetonationStatus = Ticking | Detonating | Done
  deriving (Show, Eq)

data Direction                                -- Enumeration of possible movement directions
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  | DirNone
  deriving (Show, Eq) 

-- Player represents the blue square controlled by the user
data Player = Player
  { playerX :: Double      -- X position on canvas
  , playerY :: Double      -- Y position on canvas
  , playerVelX :: Double   -- X velocity (speed of horizontal movement)
  , playerVelY :: Double   -- Y velocity (speed of vertical movement)
  , playerSize :: Double   -- Size of the player square
  , bombsHeld :: Int      -- Number of bombs held
  , targetX :: Double      -- target is the target bomb placement with
  , targetY :: Double      -- X and Y coordinates
  , spacePressed :: Bool  -- saves state, whether or not spacekey was pressed
  , xCoords :: [Double]    -- list of X coordinates for player to target
  , yCoords :: [Double]    -- list of Y coordinates for player to target
  } deriving (Show)

-- Bomb represents a red square that can detonate into player-hurting red squares
data Bomb = Bomb
  { bombX :: Double       -- X position
  , bombY :: Double       -- Y position
  , bombVelX :: Double    -- X velocity (horizontal speed)
  , bombVelY :: Double    -- Y velocity (vertical speed)
  , bombSize :: Double    -- Size of the bomb square
  , isDetonated :: DetonationStatus  -- boolean to track detonating status
  , timer :: Double       -- float timer that ticks down to detonation
  } deriving (Show)

-- Obstacle represents solid blocks that block movement
data Obstacle = Obstacle
  { obstacleX :: Double      -- X position (center)
  , obstacleY :: Double      -- Y position (center)
  , obstacleWidth :: Double  -- Width of the obstacle
  , obstacleHeight :: Double -- Height of the obstacle
  , isHardBlock :: Bool      -- Type of the obstacle (Boolean bc there's only two)
  } deriving (Show)

-- Gamestate
data Model = Model                            -- Application state
  { time :: Double                            -- Accumulated time in milliseconds
  , lastMilli :: Double                       -- Timestamp of last frame
  , tick :: Int                               -- Frame counter for debugging
  , player :: Player                          -- The player character
  , bombs :: [Bomb]                           -- List of bombs
  , obstacles :: [Obstacle]                   -- List of solid obstacle blocks
  , score :: Int                              -- Current score
  , gameOver :: Bool                          -- Whether the game has ended
  , currentDirection :: Maybe Direction       -- Currently pressed direction key (if any)
  }
  deriving (Show, Eq)                         

-- =========================
-- Initializers
-- =========================

-- Create the initial player in the center of the screen
initialPlayer :: Player
initialPlayer = Player
  { playerX = 375           -- Center X
  , playerY = 575           -- Center Y
  , playerVelX = 0          -- Not moving initially
  , playerVelY = 0          -- Not moving initially
  , playerSize = 30         -- 30 pixels square
  , bombsHeld = 100         -- 1 bomb held by default
  , targetX = 375           -- same as player position
  , targetY = 575           -- same as player position
  , spacePressed = False    -- spacekey is not pressed by default
  , xCoords = 
    [25, 75, 125, 175, 225, 275, 
    325, 375, 425, 475, 525, 575, 
    625, 675, 725]
  , yCoords = 
    [25, 75, 125, 175, 225, 275, 
    325, 375, 425, 475, 525, 575, 625]
  }


-- Initial Game State
initModel :: Model                            -- Initial application state
initModel =
  Model
    { time = 0                                -- Start time at 0
    , lastMilli = 0                           -- No previous frame yet
    , tick = 0                                -- No frames rendered yet
    , player = initialPlayer
    , bombs = 
      [ Bomb 75 75 0 0 50 Ticking 3           -- First bomb: top-left, moving right-down, size 25
      , Bomb 575 575 0 0 50 Ticking 3         -- Second bomb: bottom-right, moving left-up, size 25
      ]
    , obstacles =
      [ Obstacle 25 25 cellSize cellSize  True -- Top Border hard blocks...
      , Obstacle 75 25 cellSize cellSize  True  
      , Obstacle 125 25 cellSize cellSize  True 
      , Obstacle 175 25 cellSize cellSize  True 
      , Obstacle 225 25 cellSize cellSize  True 
      , Obstacle 275 25 cellSize cellSize  True 
      , Obstacle 325 25 cellSize cellSize  True 
      , Obstacle 375 25 cellSize cellSize  True 
      , Obstacle 425 25 cellSize cellSize  True 
      , Obstacle 475 25 cellSize cellSize  True 
      , Obstacle 525 25 cellSize cellSize  True 
      , Obstacle 575 25 cellSize cellSize  True 
      , Obstacle 625 25 cellSize cellSize  True 
      , Obstacle 675 25 cellSize cellSize  True 
      , Obstacle 725 25 cellSize cellSize  True 
      , Obstacle 25 75 cellSize cellSize  True -- Left Border Hard blocks ...
      , Obstacle 25 125 cellSize cellSize  True 
      , Obstacle 25 175 cellSize cellSize  True 
      , Obstacle 25 225 cellSize cellSize  True 
      , Obstacle 25 275 cellSize cellSize  True 
      , Obstacle 25 325 cellSize cellSize  True 
      , Obstacle 25 375 cellSize cellSize  True 
      , Obstacle 25 425 cellSize cellSize  True 
      , Obstacle 25 475 cellSize cellSize  True 
      , Obstacle 25 525 cellSize cellSize  True 
      , Obstacle 25 575 cellSize cellSize  True 
      , Obstacle 25 625 cellSize cellSize  True 
      , Obstacle 725 75 cellSize cellSize   True -- Right Border Hard Blocks 
      , Obstacle 725 125 cellSize cellSize  True 
      , Obstacle 725 175 cellSize cellSize  True 
      , Obstacle 725 225 cellSize cellSize  True 
      , Obstacle 725 275 cellSize cellSize  True 
      , Obstacle 725 325 cellSize cellSize  True 
      , Obstacle 725 375 cellSize cellSize  True 
      , Obstacle 725 425 cellSize cellSize  True 
      , Obstacle 725 475 cellSize cellSize  True 
      , Obstacle 725 525 cellSize cellSize  True 
      , Obstacle 725 575 cellSize cellSize  True 
      , Obstacle 725 625 cellSize cellSize  True 
      , Obstacle 75 625 cellSize cellSize   True  -- Bottom Border Blocks
      , Obstacle 125 625 cellSize cellSize  True 
      , Obstacle 175 625 cellSize cellSize  True 
      , Obstacle 225 625 cellSize cellSize  True 
      , Obstacle 275 625 cellSize cellSize  True 
      , Obstacle 325 625 cellSize cellSize  True 
      , Obstacle 375 625 cellSize cellSize  True 
      , Obstacle 425 625 cellSize cellSize  True 
      , Obstacle 475 625 cellSize cellSize  True 
      , Obstacle 525 625 cellSize cellSize  True 
      , Obstacle 575 625 cellSize cellSize  True 
      , Obstacle 625 625 cellSize cellSize  True 
      , Obstacle 675 625 cellSize cellSize  True 
      , Obstacle 125 125 cellSize cellSize True -- First row blocks => 125 Y height (index 52) (made it soft blocks first so easily distinguishable)
      , Obstacle 225 125 cellSize cellSize True 
      , Obstacle 325 125 cellSize cellSize True 
      , Obstacle 425 125 cellSize cellSize True 
      , Obstacle 525 125 cellSize cellSize True 
      , Obstacle 625 125 cellSize cellSize True 
      , Obstacle 125 225 cellSize cellSize True -- Second row blocks => 225 Y height
      , Obstacle 225 225 cellSize cellSize True 
      , Obstacle 325 225 cellSize cellSize True 
      , Obstacle 425 225 cellSize cellSize True 
      , Obstacle 525 225 cellSize cellSize True 
      , Obstacle 625 225 cellSize cellSize True 
      , Obstacle 125 325 cellSize cellSize True -- Third row blocks => 325 Y height
      , Obstacle 225 325 cellSize cellSize True 
      , Obstacle 325 325 cellSize cellSize True 
      , Obstacle 425 325 cellSize cellSize True 
      , Obstacle 525 325 cellSize cellSize True 
      , Obstacle 625 325 cellSize cellSize True 
      , Obstacle 125 425 cellSize cellSize True -- Fourth row blocks => 425 Y height
      , Obstacle 225 425 cellSize cellSize True 
      , Obstacle 325 425 cellSize cellSize True 
      , Obstacle 425 425 cellSize cellSize True 
      , Obstacle 525 425 cellSize cellSize True 
      , Obstacle 625 425 cellSize cellSize True 
      , Obstacle 125 525 cellSize cellSize True  -- Fifth row blocks => 525 Y height
      , Obstacle 225 525 cellSize cellSize True 
      , Obstacle 325 525 cellSize cellSize True 
      , Obstacle 425 525 cellSize cellSize True 
      , Obstacle 525 525 cellSize cellSize True 
      , Obstacle 625 525 cellSize cellSize True
      , Obstacle 75 575 cellSize cellSize False -- 10 Arbitrary Soft Blocks
      , Obstacle 175 175 cellSize cellSize False
      , Obstacle 375 275 cellSize cellSize False
      , Obstacle 575 375 cellSize cellSize False
      , Obstacle 525 275 cellSize cellSize False
      , Obstacle 375 525 cellSize cellSize False
      , Obstacle 625 475 cellSize cellSize False
      , Obstacle 675 575 cellSize cellSize False
      , Obstacle 75 175 cellSize cellSize False
      , Obstacle 625 575 cellSize cellSize False
      ]
    , score = 0       -- Start with 0 points
    , gameOver = False -- Game starts running
    }

-- =========================
-- Messages
-- =========================

data Msg                                      
  = MsgGetTime                                -- Request current timestamp
  | MsgSetTime Double                         -- Receive timestamp and update animation
  | MsgMoveUp                                 -- Up arrow pressed
  | MsgMoveDown                               -- Down arrow pressed
  | MsgMoveLeft                               -- Left arrow pressed
  | MsgMoveRight                              -- Right arrow pressed
  | MsgPlaceBomb                              -- Backspace pressed
  | MsgNoOp                                   -- No operation (ignored key)
  deriving (Show, Eq)                         -- Auto-generate Show and Eq instances

-- =========================
-- Helper Functions
-- =========================

-- Check if player collides with an obstacle (for blocking movement)
checkPlayerObstacleCollision :: Player -> Obstacle -> Bool
checkPlayerObstacleCollision p obs =
  let px = playerX p
      py = playerY p
      ps = playerSize p
      ox = obstacleX obs
      oy = obstacleY obs
      ow = obstacleWidth obs
      oh = obstacleHeight obs
      
      -- Calculate edges of each rectangle
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      
      oLeft = ox - ow / 2
      oRight = ox + ow / 2
      oTop = oy - oh / 2
      oBottom = oy + oh / 2
      
  in -- Rectangles collide if they overlap on both axes
     pRight > oLeft && pLeft < oRight &&
     pBottom > oTop && pTop < oBottom

-- add bombsHeld updater as a separate function because it's not movement-related
-- decrements bombsHeld by player -> checks if bomb placing is valid
updatePlayerPlaceBomb :: Map.Map Word () -> Bool -> Player -> Player
updatePlayerPlaceBomb keys isBA p =
  let -- get current bombsHeld
      oldBH = bombsHeld p
      newBH = bombsHeld p - 1        -- new bombsHeld
      spaceState = spacePressed p    -- state of spacePressed

      -- Create a test player with decremented bomb count and spacePressed as True
      testPlayer = p { bombsHeld = newBH, spacePressed = True }

  in if oldBH > 0           -- if a bomb can be placed
    && spaceState == False  -- if spacekey was released previously
    && keyPress == 1        -- if spacekey is now pressed
    && isBA                 -- and bomb was successfully added
      then testPlayer       -- bomb can be placed
    else if keyPress == 0   -- reset if spacekey was released
      then p { spacePressed = False }
    else
      p                     -- bomb cannot be placed
  
  where
    keyPress = 
      if Map.member 32 keys -- if backspace is pressed, decrement value is 1
      then 1
    else 0

-- Update player position based on which keys are pressed
-- Takes a Map of currently pressed keys and the current player state
-- Returns updated player state
-- also updates player targeting
updatePlayer :: Map.Map Word () -> [Obstacle] -> Player -> Player
updatePlayer keys obstacles p = 
  let -- Calculate desired new position
      newX = playerX p + vx
      newY = playerY p + vy

      -- Calculate desired new target
      newTargetX = computeTarget newX (xCoords p)
      newTargetY = computeTarget newY (yCoords p)
      
      -- Create a test player at the new position with new target
      testPlayer = p { playerX = clamp 0 canvasWidth newX, playerY = clamp 0 canvasLength newY,
                        targetX = newTargetX, targetY = newTargetY }

      -- Check if new position and new target would collide with any obstacle
      wouldCollide = any (checkPlayerObstacleCollision testPlayer) obstacles
      
  in if wouldCollide
     then p  -- Don't move if it would hit an obstacle
     else testPlayer  -- Move to new position if clear
  where
    speed = 9

    -- Horizontal intent
    vxRaw
      | Map.member 37 keys || Map.member 65 keys = -speed  -- Left / A
      | Map.member 39 keys || Map.member 68 keys =  speed  -- Right / D
      | otherwise = 0

    -- Vertical intent
    vyRaw
      | Map.member 38 keys || Map.member 87 keys = -speed  -- Up / W
      | Map.member 40 keys || Map.member 83 keys =  speed  -- Down / S
      | otherwise = 0

    -- Enforce orthogonal movement
    (vx, vy)
      | vxRaw /= 0 = (vxRaw, 0)   -- prioritize horizontal movement
      | vyRaw /= 0 = (0, vyRaw)
      | otherwise  = (0, 0)

-- computes the nearest point for each coordinate
-- takes in the coordinate of player and list of possible target coordinates
computeTarget :: Double -> [Double] -> Double
computeTarget pXY =
  minimumBy (\a b -> compare (abs (pXY - a)) (abs (pXY - b)))

-- Check if bomb collides with an obstacle
checkBombObstacleCollision :: Bomb -> Obstacle -> Bool
checkBombObstacleCollision b obs =
  let bx = bombX b
      by = bombY b
      bs = bombSize b
      ox = obstacleX obs
      oy = obstacleY obs
      ow = obstacleWidth obs
      oh = obstacleHeight obs
      
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

-- Check if bomb collides with a bombs
checkBombBombCollision :: Bomb -> Bomb -> Bool
checkBombBombCollision b ob =
  let 
      bds = isDetonated b
      obds = isDetonated ob
    
      bx = bombX b
      by = bombY b
      bs = bombSize b
      ox = bombX ob
      oy = bombY ob
      ow = bombSize ob
      oh = bombSize ob
      
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
     ((bds == Detonating && obds == Ticking) ||
     (obds == Detonating && bds == Ticking))

-- spawns a bomb and checks for collisions
-- returns a (new bomb, success signal) to be added to the list
updateBomb :: Map.Map Word () -> Player -> [Bomb] -> ([Bomb], Bool)
updateBomb keys p bombs =
  let -- set new bomb attributes
    newBX =  targetX p 
    newBY = targetY p
    bombsH = bombsHeld p
    spaceState = spacePressed p

    newBomb = Bomb newBX newBY 0 0 cellSize Ticking 3
    -- newBomb is a new bomb that has player coordinates and unmoving
    -- unmoving == not detonating

  in if backPressed == 1    -- boolean variable to check if backspace is pressed
    && bombsH > 0           -- bombs can be placed
    && spaceState == False  -- spacekey was released
    && checkIfBombExists newBomb bombs == False -- bomb does not already exist
    then (bombs ++ [newBomb], True) -- append newBomb to list of bombs, and signify success True
    else (bombs, False) -- returns Nothing if backspace not pressed
  
  where
    backPressed =
      if Map.member 32 keys
        then 1 
      else 0

checkIfBombExists :: Bomb -> [Bomb] -> Bool
checkIfBombExists b bombs =
  any (\x -> 
    bombX x == bombX b
    && bombY x == bombY b) bombs

-- update bomb timer, ticks down every server tick which is 0.16 seconds, explodes if 0 seconds
updateBombTimer :: Bomb -> Bomb
updateBombTimer b =
  let -- decrement bomb timer by 0.16 seconds every function call
    oldTime = timer b
    timeTick = 0.08
    bDS = isDetonated b
  
  in if oldTime <= 0 -- if time is up, detonate
    && bDS == Ticking
    then
      b { isDetonated = Detonating , timer = 1}
    else if
      oldTime <= 0
      && bDS == Detonating
      then 
        b { isDetonated = Done }
    else 
      b { timer = oldTime - timeTick }

-- updates Bombs to detonate them, then returns obstacles with updated softblocks
updateBombDetonate :: [Obstacle] -> [Bomb] -> ([Bomb], [Obstacle])
updateBombDetonate obs bombs = foldl processOneBomb ([], obs) bombs
  where
    processOneBomb (accBombs, accObs) bomb =
      if isDetonated bomb == Ticking && timer bomb <= 0
        then let (newBombs, newObs) = detonateBomb accObs bomb
             in (accBombs ++ newBombs, newObs)
        else (accBombs ++ [bomb], accObs)

-- updates Bombs detonation if hit by Detonating bomb
updateBombBombDetonation :: [Obstacle] -> [Bomb] -> ([Bomb], [Obstacle])
updateBombBombDetonation obs bombs = foldl processOneBomb ([], obs) bombs
  where
    processOneBomb (accBombs, accObs) bomb =
      if any (checkBombBombCollision bomb) accBombs
        then let (newBombs, newObs) = detonateBomb accObs bomb
             in (accBombs ++ newBombs, newObs)
        else (accBombs ++ [bomb], accObs)

-- returns new bomb state and new obstacle state (those that should be destroyed)
detonateBomb :: [Obstacle] -> Bomb -> ([Bomb], [Obstacle])
detonateBomb obs b =
  let
    bX = bombX b
    bY = bombY b 

    newBombUp = Bomb { 
      bombX = bX
      , bombY = bY - 50
      , bombVelX = 0
      , bombVelY = 0
      , bombSize = cellSize
      , isDetonated = Detonating
      , timer = 1
     }
    
    newBombDown = Bomb {
      bombX = bX
      , bombY = bY + 50
      , bombVelX = 0
      , bombVelY = 0
      , bombSize = cellSize
      , isDetonated = Detonating
      , timer = 1
    }

    newBombLeft = Bomb {
      bombX = bX - 50
      , bombY = bY
      , bombVelX = 0
      , bombVelY = 0
      , bombSize = cellSize
      , isDetonated = Detonating
      , timer = 1
    }

    newBombRight = Bomb {
      bombX = bX + 50
      , bombY = bY
      , bombVelX = 0
      , bombVelY = 0
      , bombSize = cellSize
      , isDetonated = Detonating
      , timer = 1
    }

    testBombs = [newBombUp, newBombDown, newBombLeft, newBombRight, b]

    isNotColliding bomb = not $ any (checkBombObstacleCollision bomb) obs
    
    obstaclesNotColliding o =
      isHardBlock o
      || -- does not overlap with any of the newBombs
      not (checkBombObstacleCollision newBombUp o)
      && not (checkBombObstacleCollision newBombDown o)
      && not (checkBombObstacleCollision newBombLeft o)
      && not (checkBombObstacleCollision newBombRight o)

  in
    (filter isNotColliding testBombs, filter obstaclesNotColliding obs)

updateBombRemoval :: [Bomb] -> [Bomb]
updateBombRemoval bombs =
  filter keep bombs
  where
    keep b =
      not (isDetonated b == Detonating && timer b <= 0)

-- Update a single enemy's position and handle wall and obstacle bouncing
updateEnemy :: [Obstacle] -> Bomb -> Bomb -- REFACTOR TO SPAWNING BOMBS
updateEnemy obstacles b = 
  let -- Calculate new position
      newX = bombX b + bombVelX b
      newY = bombY b + bombVelY b
      
      -- Check wall collisions
      hitLeftWall = newX <= 0
      hitRightWall = newX >= canvasWidth
      hitTopWall = newY <= 0
      hitBottomWall = newY >= canvasLength
      
      -- Create test enemy at new position
      testBomb = b { bombX = newX, bombY = newY }
      
      -- Check if new position would collide with obstacles
      hitObstacle = any (checkBombObstacleCollision testBomb) obstacles
      
      -- Reverse velocity if hitting wall or obstacle
      finalVelX = if hitLeftWall || hitRightWall || hitObstacle 
                  then -(bombVelX b) 
                  else bombVelX b
      finalVelY = if hitTopWall || hitBottomWall || hitObstacle 
                  then -(bombVelY b) 
                  else bombVelY b
      
      -- Clamp position to stay in bounds
      finalX = clamp 0 canvasWidth newX
      finalY = clamp 0 canvasLength newY
      
  in b { bombX = finalX
       , bombY = finalY
       , bombVelX = finalVelX
       , bombVelY = finalVelY
       }

-- Check if two rectangles (player and bomb) are colliding
-- Uses AABB (Axis-Aligned Bounding Box) collision detection
checkCollision :: Player -> Bomb -> Bool
checkCollision p b = 
  -- Check if rectangles overlap on both X and Y axes
  let px = playerX p
      py = playerY p
      ps = playerSize p
      bx = bombX b
      by = bombY b
      bs = bombSize b
      
      -- Calculate edges of each rectangle
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      
      bLeft = bx - bs / 2
      bRight = bx + bs / 2
      bTop = by - bs / 2
      bBottom = by + bs / 2
      
  in -- Rectangles collide if they overlap on both axes
     pRight > bLeft && pLeft < bRight &&  -- X axis overlap
     pBottom > bTop && pTop < bBottom     -- Y axis overlap
     && (isDetonated b == Detonating)

-- Clamp a value between a minimum and maximum
-- Example: clamp 0 100 150 = 100, clamp 0 100 50 = 50
clamp :: Ord a => a -> a -> a -> a
clamp minV maxV = max minV . min maxV

-- =========================
-- Update Functions
-- =========================

update :: Msg -> M.Transition Model Msg

---

update MsgGetTime = do                        -- Handle request for current time
  M.io $ do                                   -- Perform IO operation
    date <- M.newDate                         -- Get current date/time object
    milli <- M.getMilliseconds date           -- Extract milliseconds component
    pure $ MsgSetTime milli                   -- Return message with timestamp

---

update (MsgSetTime milli) = do                -- Handle received timestamp (main animation loop)
  model <- M.get                              -- Get current model state

  let lastMilli = model.lastMilli             -- Get previous frame's timestamp
  let diff = if milli >= lastMilli then milli - lastMilli else 1000 - lastMilli + milli
  -- Calculate time delta, handling millisecond wraparound at 1000
  
  let 
    updatedPlayer = updatePlayer keys (obstacles gs) (player gs)  -- Update player position, checking obstacles
    (updatedBombs, isBombAdded) = updateBomb keys updatedPlayer (bombs gs)  -- Update bombs (placing bombs)

    updatedPlayer' = updatePlayerPlaceBomb keys isBombAdded updatedPlayer   -- Update player again for bombsHeld updating
        
    updatedBombs' = map updateBombTimer updatedBombs  -- decrement bombTimers
    (updatedBombs'', updatedObstacles) = updateBombDetonate (obstacles gs) updatedBombs'
    -- detonate depleted bomb timers and destroy appropriate softblocks
        
    (updatedBombs''', updatedObstacles') = updateBombBombDetonation (updatedObstacles) updatedBombs''
    -- detonate bombs that touched by other detonated bombs

    updatedBombs'''' = updateBombRemoval updatedBombs''' -- remove detonated bombs

    -- Check if player collides with any detonating bomb
    collision = any (checkCollision updatedPlayer') updatedBombs''''
        
    -- Increment score each frame if still alive
    newScore = if collision then score gs else score gs + 1
  
  M.put $                                     -- Update the model with new state
    model
      { time = model.time + diff              -- Accumulate elapsed time
      , lastMilli = milli                     -- Store current timestamp for next frame
      , tick = model.tick + 1                 -- Increment frame counter
      , player = updatedPlayer'
      , bombs = updatedBombs''''
      , obstacles = updatedObstacles'
      , score = newScore
      , gameOver = collision
      , currentDirection = Just DirNone 
      }
  M.issue MsgGetTime                          -- Issue next frame request (continues animation loop)

---

update MsgMoveUp = do                         -- Handle up arrow key press
  model <- M.get                              -- Get current model
  M.put $ model{currentDirection = Just DirUp}  -- Set current direction to up

---

update MsgMoveDown = do                       -- Handle down arrow key press
  model <- M.get                              -- Get current model
  M.put $ model{currentDirection = Just DirDown}  -- Set current direction to down

---

update MsgMoveLeft = do                       -- Handle left arrow key press
  model <- M.get                              -- Get current model
  M.put $ model{currentDirection = Just DirLeft}  -- Set current direction to left (not currently used in physics)

---

update MsgMoveRight = do                      -- Handle right arrow key press
  model <- M.get                              -- Get current model
  M.put $ model{currentDirection = Just DirRight}  -- Set current direction to right (not currently used in physics)

---

update MsgPlaceBomb = do
  model <- M.get
  M.put $ model

---

update MsgNoOp = pure ()                      -- Handle no-op message: do nothing

-- =========================
-- View
-- =========================

view :: Model -> M.View Model Msg             -- View function: renders UI from model
view model =
  trace (show model.tick <> " " <> show model.time) $  -- Debug trace: logs tick count and time to console
    H.div_                                    -- Container div
      []                                      -- No attributes
      [ H.textarea_ [P.rows_ "20", P.cols_ "40"] [M.text $ M.ms $ show model]  -- Textarea showing model state (debugging)
      , H.br_ []                              -- Line break
      , Canvas.canvas                         -- Canvas element for drawing
          [ P.width_ (M.ms screenWidth)       -- Set canvas width attribute
          , P.height_ (M.ms screenHeight)     -- Set canvas height attribute
          ]
          (\_ -> pure ())                     -- Event handler (unused here)
          (viewCanvas model)                  -- Canvas drawing function
      ]

viewCanvas :: Model -> () -> Canvas.Canvas ()  -- Canvas rendering function
viewCanvas model () = do
  -- Black background; clears graphics of previous frame
  Canvas.fillStyle (Canvas.ColorArg (RGB 0 0 0))  -- Set fill color to black
  Canvas.fillRect (0, 0, screenWidth, screenHeight)  -- Fill entire canvas with black (clears previous frame)
  
  -- Red box, 50x50
  Canvas.fillStyle (Canvas.ColorArg (RGB 255 0 0))  -- Set fill color to red
  Canvas.fillRect (model.x, model.y, squareSize, squareSize)  -- Draw square at current position