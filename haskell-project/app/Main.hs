-- From previous version
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE OverloadedRecordDot #-}  -- Allows dot notation for record fields (model.x instead of x model)
{-# LANGUAGE OverloadedStrings #-}    -- Allows string literals to be polymorphic (useful for Text, etc.)

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

-- Other Imports
import Data.List (minimumBy)

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
  , currentDirection :: Maybe Direction    -- Currently pressed direction key (if any)
  } deriving (Show, Eq)

-- Bomb represents a red square that can detonate into player-hurting red squares
data Bomb = Bomb
  { bombX :: Double       -- X position
  , bombY :: Double       -- Y position
  , bombVelX :: Double    -- X velocity (horizontal speed)
  , bombVelY :: Double    -- Y velocity (vertical speed)
  , bombSize :: Double    -- Size of the bomb square
  , isDetonated :: DetonationStatus  -- boolean to track detonating status
  , timer :: Double       -- float timer that ticks down to detonation
  } deriving (Show, Eq)

-- Obstacle represents solid blocks that block movement
data Obstacle = Obstacle
  { obstacleX :: Double      -- X position (center)
  , obstacleY :: Double      -- Y position (center)
  , obstacleWidth :: Double  -- Width of the obstacle
  , obstacleHeight :: Double -- Height of the obstacle
  , isHardBlock :: Bool      -- Type of the obstacle (Boolean bc there's only two)
  } deriving (Show, Eq)

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
  , currentDirection = Just DirNone  -- not moving
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
      [ Obstacle 25 25 squareSize squareSize  True -- Top Border hard blocks...
      , Obstacle 75 25 squareSize squareSize  True  
      , Obstacle 125 25 squareSize squareSize  True 
      , Obstacle 175 25 squareSize squareSize  True 
      , Obstacle 225 25 squareSize squareSize  True 
      , Obstacle 275 25 squareSize squareSize  True 
      , Obstacle 325 25 squareSize squareSize  True 
      , Obstacle 375 25 squareSize squareSize  True 
      , Obstacle 425 25 squareSize squareSize  True 
      , Obstacle 475 25 squareSize squareSize  True 
      , Obstacle 525 25 squareSize squareSize  True 
      , Obstacle 575 25 squareSize squareSize  True 
      , Obstacle 625 25 squareSize squareSize  True 
      , Obstacle 675 25 squareSize squareSize  True 
      , Obstacle 725 25 squareSize squareSize  True 
      , Obstacle 25 75 squareSize squareSize  True -- Left Border Hard blocks ...
      , Obstacle 25 125 squareSize squareSize  True 
      , Obstacle 25 175 squareSize squareSize  True 
      , Obstacle 25 225 squareSize squareSize  True 
      , Obstacle 25 275 squareSize squareSize  True 
      , Obstacle 25 325 squareSize squareSize  True 
      , Obstacle 25 375 squareSize squareSize  True 
      , Obstacle 25 425 squareSize squareSize  True 
      , Obstacle 25 475 squareSize squareSize  True 
      , Obstacle 25 525 squareSize squareSize  True 
      , Obstacle 25 575 squareSize squareSize  True 
      , Obstacle 25 625 squareSize squareSize  True 
      , Obstacle 725 75 squareSize squareSize   True -- Right Border Hard Blocks 
      , Obstacle 725 125 squareSize squareSize  True 
      , Obstacle 725 175 squareSize squareSize  True 
      , Obstacle 725 225 squareSize squareSize  True 
      , Obstacle 725 275 squareSize squareSize  True 
      , Obstacle 725 325 squareSize squareSize  True 
      , Obstacle 725 375 squareSize squareSize  True 
      , Obstacle 725 425 squareSize squareSize  True 
      , Obstacle 725 475 squareSize squareSize  True 
      , Obstacle 725 525 squareSize squareSize  True 
      , Obstacle 725 575 squareSize squareSize  True 
      , Obstacle 725 625 squareSize squareSize  True 
      , Obstacle 75 625 squareSize squareSize   True  -- Bottom Border Blocks
      , Obstacle 125 625 squareSize squareSize  True 
      , Obstacle 175 625 squareSize squareSize  True 
      , Obstacle 225 625 squareSize squareSize  True 
      , Obstacle 275 625 squareSize squareSize  True 
      , Obstacle 325 625 squareSize squareSize  True 
      , Obstacle 375 625 squareSize squareSize  True 
      , Obstacle 425 625 squareSize squareSize  True 
      , Obstacle 475 625 squareSize squareSize  True 
      , Obstacle 525 625 squareSize squareSize  True 
      , Obstacle 575 625 squareSize squareSize  True 
      , Obstacle 625 625 squareSize squareSize  True 
      , Obstacle 675 625 squareSize squareSize  True 
      , Obstacle 125 125 squareSize squareSize True -- First row blocks => 125 Y height (index 52) (made it soft blocks first so easily distinguishable)
      , Obstacle 225 125 squareSize squareSize True 
      , Obstacle 325 125 squareSize squareSize True 
      , Obstacle 425 125 squareSize squareSize True 
      , Obstacle 525 125 squareSize squareSize True 
      , Obstacle 625 125 squareSize squareSize True 
      , Obstacle 125 225 squareSize squareSize True -- Second row blocks => 225 Y height
      , Obstacle 225 225 squareSize squareSize True 
      , Obstacle 325 225 squareSize squareSize True 
      , Obstacle 425 225 squareSize squareSize True 
      , Obstacle 525 225 squareSize squareSize True 
      , Obstacle 625 225 squareSize squareSize True 
      , Obstacle 125 325 squareSize squareSize True -- Third row blocks => 325 Y height
      , Obstacle 225 325 squareSize squareSize True 
      , Obstacle 325 325 squareSize squareSize True 
      , Obstacle 425 325 squareSize squareSize True 
      , Obstacle 525 325 squareSize squareSize True 
      , Obstacle 625 325 squareSize squareSize True 
      , Obstacle 125 425 squareSize squareSize True -- Fourth row blocks => 425 Y height
      , Obstacle 225 425 squareSize squareSize True 
      , Obstacle 325 425 squareSize squareSize True 
      , Obstacle 425 425 squareSize squareSize True 
      , Obstacle 525 425 squareSize squareSize True 
      , Obstacle 625 425 squareSize squareSize True 
      , Obstacle 125 525 squareSize squareSize True  -- Fifth row blocks => 525 Y height
      , Obstacle 225 525 squareSize squareSize True 
      , Obstacle 325 525 squareSize squareSize True 
      , Obstacle 425 525 squareSize squareSize True 
      , Obstacle 525 525 squareSize squareSize True 
      , Obstacle 625 525 squareSize squareSize True
      , Obstacle 75 575 squareSize squareSize False -- 10 Arbitrary Soft Blocks
      , Obstacle 175 175 squareSize squareSize False
      , Obstacle 375 275 squareSize squareSize False
      , Obstacle 575 375 squareSize squareSize False
      , Obstacle 525 275 squareSize squareSize False
      , Obstacle 375 525 squareSize squareSize False
      , Obstacle 625 475 squareSize squareSize False
      , Obstacle 675 575 squareSize squareSize False
      , Obstacle 75 175 squareSize squareSize False
      , Obstacle 625 575 squareSize squareSize False
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
updatePlayerPlaceBomb :: Bool -> Player -> Player
updatePlayerPlaceBomb isBA p =
  let -- get current bombsHeld
      oldBH = bombsHeld p
      newBH = bombsHeld p - 1        -- new bombsHeld
      spaceState = spacePressed p    -- state of spacePressed

      -- Create a test player with decremented bomb count
      testPlayer = p { bombsHeld = newBH }

  in if oldBH > 0           -- if a bomb can be placed
    && spaceState == True   -- if spacekey is being requested
    && isBA                 -- and bomb was successfully added
      then testPlayer       -- bomb can be placed
    else if spaceState == False -- spacekeyis not requested so you can reset
      then p { spacePressed = False }
    else
      p                     -- bomb cannot be placed, nothing should happen

-- Update player position based on which keys are pressed
-- Takes a Map of currently pressed keys and the current player state
-- Returns updated player state
-- also updates player targeting
updatePlayer :: [Obstacle] -> Player -> Player
updatePlayer obstacles p = 
  let -- Calculate desired new position
      dir = currentDirection p -- direction request
      newX = playerX p + vx
      newY = playerY p + vy

      -- Calculate desired new target
      newTargetX = computeTarget newX (xCoords p)
      newTargetY = computeTarget newY (yCoords p)
      
      -- Create a test player at the new position with new target
      testPlayer = p { playerX = clamp 0 screenWidth newX, playerY = clamp 0 screenHeight newY,
                        targetX = newTargetX, targetY = newTargetY }

      -- Check if new position and new target would collide with any obstacle
      wouldCollide = any (checkPlayerObstacleCollision testPlayer) obstacles

      speed = 5

      -- Horizontal cases
      vx =
        if dir == Just DirLeft
          then -speed
        else if dir == Just DirRight
          then speed
        else 0

      -- Vertical cases
      vy = 
        if dir == Just DirUp
          then -speed
        else if dir == Just DirDown
          then speed
        else 0
      
  in if wouldCollide
     then p  -- Don't move if it would hit an obstacle
     else testPlayer  -- Move to new position if clear


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
updateBomb :: Player -> [Bomb] -> ([Bomb], Bool)
updateBomb p bombs =
  let -- set new bomb attributes
    newBX =  targetX p 
    newBY = targetY p
    bombsH = bombsHeld p
    spaceState = spacePressed p -- if space was requested

    newBomb = Bomb newBX newBY 0 0 squareSize Ticking 3
    -- newBomb is a new bomb that has player coordinates and unmoving
    -- unmoving == not detonating

  in if
    bombsH > 0          -- if bombs can be placed
    && spaceState == True  -- if spacekey was requested
    && checkIfBombExists newBomb bombs == False -- if bomb does not already exist
    then (bombs ++ [newBomb], True) -- append newBomb to list of bombs, and signify success True
    else (bombs, False) -- returns Nothing if backspace not pressed
    -- { spacePressed = False } not handled here because it is handled in updatePlayerPlaceBomb function
  
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
    timeTick = 0.16
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

-- updates Bombs to detonate them, then returns obstacles with updated softblocks (if softblocks were destroyed in the process)
updateBombDetonate :: [Obstacle] -> [Bomb] -> ([Bomb], [Obstacle])
updateBombDetonate obs bombs = foldl processOneBomb ([], obs) bombs
-- iterate to the left over all bombs, detonate one by one, check if softblocks were hit
  where
    processOneBomb (accBombs, accObs) bomb =                -- accBombs -> accumulates bombs, accObs -> accumulates new Obstacles
      if isDetonated bomb == Ticking && timer bomb <= 0     -- if bomb is Ticking and timer is up, then detonate
        then let (newBombs, newObs) = detonateBomb accObs bomb
             in (accBombs ++ newBombs, newObs)
        else (accBombs ++ [bomb], accObs)                   -- else, (bomb is not meant to be deontated) just add the original bomb

-- detonates Bombs if hit by Detonating bomb
updateBombBombDetonation :: [Obstacle] -> [Bomb] -> ([Bomb], [Obstacle])
updateBombBombDetonation obs bombs = foldl processOneBomb ([], obs) bombs
-- iterates to the left over all bombs, detonate if they are touched by detonating bombs
  where
    processOneBomb (accBombs, accObs) bomb =
      if any (checkBombBombCollision bomb) accBombs             -- if the bomb is touched by any detonating bomb
        then let (newBombs, newObs) = detonateBomb accObs bomb  -- detonate the bomb
             in (accBombs ++ newBombs, newObs)
        else (accBombs ++ [bomb], accObs)                       -- else, do nothing

-- returns new bomb state and new obstacle state (those that should be destroyed)
detonateBomb :: [Obstacle] -> Bomb -> ([Bomb], [Obstacle])
detonateBomb obs b =
  let
    bX = bombX b
    bY = bombY b 

    -- create four new bombs relative to source bomb's orthogonal dircetions

    newBombUp = Bomb { 
      bombX = bX
      , bombY = bY - 50
      , bombVelX = 0
      , bombVelY = 0
      , bombSize = squareSize
      , isDetonated = Detonating
      , timer = 1
     }
    
    newBombDown = Bomb {
      bombX = bX
      , bombY = bY + 50
      , bombVelX = 0
      , bombVelY = 0
      , bombSize = squareSize
      , isDetonated = Detonating
      , timer = 1
    }

    newBombLeft = Bomb {
      bombX = bX - 50
      , bombY = bY
      , bombVelX = 0
      , bombVelY = 0
      , bombSize = squareSize
      , isDetonated = Detonating
      , timer = 1
    }

    newBombRight = Bomb {
      bombX = bX + 50
      , bombY = bY
      , bombVelX = 0
      , bombVelY = 0
      , bombSize = squareSize
      , isDetonated = Detonating
      , timer = 1
    }

    testBombs = [newBombUp, newBombDown, newBombLeft, newBombRight, b]
    -- testBombs should include original bomb

    isNotColliding bomb = not $ any (checkBombObstacleCollision bomb) obs
    -- isNotColliding checks if a new bomb will overlap with an obstacle
    
    obstaclesNotColliding o =
      isHardBlock o
      || -- does not overlap with any of the newBombs
      not (checkBombObstacleCollision newBombUp o)
      && not (checkBombObstacleCollision newBombDown o)
      && not (checkBombObstacleCollision newBombLeft o)
      && not (checkBombObstacleCollision newBombRight o)
    -- obstacleNotColliding checks for obstacles that are hard blocks or not colliding with newBombs
    -- only softblocks that collided with the detonating bombs should be removed

  in
    (filter isNotColliding testBombs, filter obstaclesNotColliding obs)
    -- filter base don the above conditions

-- remove bombs that blew up
updateBombRemoval :: [Bomb] -> [Bomb]
updateBombRemoval bombs =
  filter keep bombs -- filter out bombs that were detonating and reached the end of the detonating timer
  where
    keep b =
      not (isDetonated b == Detonating && timer b <= 0)

-- Update a single enemy's position and handle wall and obstacle bouncing
-- updateEnemy :: [Obstacle] -> Bomb -> Bomb -- REFACTOR TO SPAWNING BOMBS
-- updateEnemy obstacles b = 
  -- let -- Calculate new position
      -- newX = bombX b + bombVelX b
      -- newY = bombY b + bombVelY b
      
      -- Check wall collisions
      -- hitLeftWall = newX <= 0
      -- hitRightWall = newX >= canvasWidth
      -- hitTopWall = newY <= 0
      -- hitBottomWall = newY >= canvasLength
      
      -- Create test enemy at new position
      -- testBomb = b { bombX = newX, bombY = newY }
      
      -- Check if new position would collide with obstacles
      -- hitObstacle = any (checkBombObstacleCollision testBomb) obstacles
      
      -- Reverse velocity if hitting wall or obstacle
      -- finalVelX = if hitLeftWall || hitRightWall || hitObstacle 
                  -- then -(bombVelX b) 
                  -- else bombVelX b
      -- finalVelY = if hitTopWall || hitBottomWall || hitObstacle 
                  -- then -(bombVelY b) 
                  -- else bombVelY b
      
      -- Clamp position to stay in bounds
      -- finalX = clamp 0 canvasWidth newX
      -- finalY = clamp 0 canvasLength newY
      
  -- in b { bombX = finalX
       -- , bombY = finalY
       -- , bombVelX = finalVelX
       -- , bombVelY = finalVelY
       -- }

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
     && (isDetonated b == Detonating)     -- only if the bomb is detonating

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
  
  let updatedPlayer = updatePlayer (obstacles model) (player model)  -- Update player position, checking obstacles
  let (updatedBombs, isBombAdded) = updateBomb updatedPlayer (bombs model)  -- Update bombs (placing bombs)

  let updatedPlayer' = updatePlayerPlaceBomb isBombAdded updatedPlayer   -- Update player again for bombsHeld updating
        
  let updatedBombs' = map updateBombTimer updatedBombs  -- decrement bombTimers
  let (updatedBombs'', updatedObstacles) = updateBombDetonate (obstacles model) updatedBombs'
    -- detonate depleted bomb timers and destroy appropriate softblocks
        
  let (updatedBombs''', updatedObstacles') = updateBombBombDetonation (updatedObstacles) updatedBombs''
    -- detonate bombs that touched by other detonated bombs

  let updatedBombs'''' = updateBombRemoval updatedBombs''' -- remove detonated bombs

    -- Check if player collides with any detonating bomb
  let collision = any (checkCollision updatedPlayer') updatedBombs''''
        
    -- Increment score each frame if still alive
  let newScore = if collision then score model else score model + 1
  
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
      }
  M.issue MsgGetTime                          -- Issue next frame request (continues animation loop)

---

update MsgMoveUp = do                         -- Handle up arrow key press
  model <- M.get                              -- Get current model
  let oldPlayer = model.player
  let newPlayer = oldPlayer { currentDirection = Just DirUp }
  M.put $ model { player = newPlayer }  -- Set current direction to up

---

update MsgMoveDown = do                       -- Handle down arrow key press
  model <- M.get                              -- Get current model
  let oldPlayer = model.player
  let newPlayer = oldPlayer { currentDirection = Just DirDown }
  M.put $ model { player = newPlayer }  -- Set current direction to down

---

update MsgMoveLeft = do                       -- Handle left arrow key press
  model <- M.get                              -- Get current model
  let oldPlayer = model.player
  let newPlayer = oldPlayer { currentDirection = Just DirLeft }
  M.put $ model { player = newPlayer }  -- Set current direction to left (not currently used in physics)

---

update MsgMoveRight = do                      -- Handle right arrow key press
  model <- M.get                              -- Get current model
  let oldPlayer = model.player
  let newPlayer = oldPlayer { currentDirection = Just DirRight }
  M.put $ model { player = newPlayer }  -- Set current direction to right (not currently used in physics)

---

update MsgPlaceBomb = do                      -- Handle backspace
  model <- M.get
  let oldPlayer = model.player
  let sp = oldPlayer.spacePressed
  let model' = if sp == False                 -- if spacePressed was released, then valid re-click
      then 
        let newPlayer = oldPlayer { spacePressed = True }
        in model { player = newPlayer }
                else  
                    model
  M.put $ model'

---

update MsgNoOp = do                           -- Handle no-op message: do nothing
  model <- M.get
  let oldPlayer = model.player
  let newPlayer = oldPlayer { currentDirection = Just DirNone, spacePressed = False }
  M.put $ model { player = newPlayer }      -- reset spacePressed and direction

-- =========================
-- View
-- =========================

view :: Model -> M.View Model Msg             -- View function: renders UI from model
view model =
  trace (show model.tick <> " " <> show model.time) $  -- Debug trace: logs tick count and time to console
    H.div_                                    -- Container div
      []                                      -- No attributes
      [ H.textarea_ [P.rows_ "20", P.cols_ "100"] [M.text $ M.ms $ show model]  -- Textarea showing model state (debugging)
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
  if model.gameOver
    then do 
      Canvas.fillStyle (Canvas.ColorArg (RGB 0 0 0))  -- Set fill color to black
      Canvas.fillRect (0, 0, screenWidth, screenHeight)  -- Fill entire canvas with black
      
      -- Game Over message
      Canvas.fillStyle (Canvas.ColorArg (RGB 255 0 0))  -- Red text
      Canvas.font "48px Arial"                          -- Set font size and family
      Canvas.textAlign Canvas.TextAlignCenter           -- Center the text
      Canvas.fillText ("GAME OVER", (screenWidth / 2), (screenHeight / 2))  -- Draw text centered
      
  else do 
    -- Black background; clears graphics of previous frame
    Canvas.fillStyle (Canvas.ColorArg (RGB 0 0 0))  -- Set fill color to black
    Canvas.fillRect (0, 0, screenWidth, screenHeight)  -- Fill entire canvas with black (clears previous frame)
    
    -- Draw obstacles first (so they appear behind player)
    mapM_ drawObstacles model.obstacles
    
    -- Draw bombs
    mapM_ drawBombs model.bombs
    
    -- Blue box, 50x50 (player - drawn last so it appears on top)
    Canvas.fillStyle (Canvas.ColorArg (RGB 51 124 179))  -- Set fill color to blue
    let px = model.player.playerX
    let py = model.player.playerY
    let ps = model.player.playerSize
    Canvas.fillRect (px, py, ps, ps)  -- Draw square at current position
    
  where
    drawObstacles :: Obstacle -> Canvas.Canvas () -- helper function to draw obstacles
    drawObstacles obs = do
      if obs.isHardBlock
        then Canvas.fillStyle (Canvas.ColorArg (RGB 64 64 64))      -- dark gray
        else Canvas.fillStyle (Canvas.ColorArg (RGB 192 192 192))   -- light gray
        
      Canvas.fillRect (obs.obstacleX, obs.obstacleY, squareSize, squareSize)
    
    drawBombs :: Bomb -> Canvas.Canvas () -- helper function to draw bombs
    drawBombs b = do
      if b.isDetonated == Ticking
        then Canvas.fillStyle (Canvas.ColorArg (RGB 122 22 22))        -- dark red
        else Canvas.fillStyle (Canvas.ColorArg (RGB 193 35 35))        -- brighter red for detonating
      
      Canvas.fillRect (b.bombX, b.bombY, b.bombSize, b.bombSize)