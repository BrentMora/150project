{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

-- Import Reflex DOM for reactive web programming
import Reflex.Dom
import qualified Data.Map as Map
import Data.Text (pack)
import qualified Data.Text as T
import Control.Monad (void, forM_)
import Control.Monad.IO.Class (liftIO)

-- Import GHCJS DOM types for working with browser APIs
import GHCJS.DOM.Types (JSM, liftJSM, castTo, CanvasRenderingContext2D, HTMLCanvasElement(..), toJSString)
import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.CanvasRenderingContext2D

-- Import other GHCJS modules
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Document (getDocumentElement)
import GHCJS.DOM.EventM (on, event)
import GHCJS.DOM.GlobalEventHandlers (keyDown, keyUp)
import GHCJS.DOM.KeyboardEvent (getKeyCode)

-- Import time for game loop timing
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)
import Data.IORef
import Control.Concurrent (threadDelay)

-- Other Imports
import Data.List (minimumBy)

-- ============================================================================
-- CONSTANTS
-- ============================================================================

canvasWidth :: Float
canvasWidth = 750

canvasLength :: Float
canvasLength = 650

cellSize :: Float
cellSize = 50

-- ============================================================================
-- DATA TYPES - Define the structure of our game state
-- ============================================================================

-- Player represents the blue square controlled by the user
data Player = Player
  { playerX :: Float      -- X position on canvas
  , playerY :: Float      -- Y position on canvas
  , playerVelX :: Float   -- X velocity (speed of horizontal movement)
  , playerVelY :: Float   -- Y velocity (speed of vertical movement)
  , playerSize :: Float   -- Size of the player square
  , bombsHeld :: Int      -- Number of bombs held
  , targetX :: Float      -- target is the target bomb placement with
  , targetY :: Float      -- X and Y coordinates
  , spacePressed :: Bool  -- saves state, whether or not spacekey was pressed
  , xCoords :: [Float]    -- list of X coordinates for player to target
  , yCoords :: [Float]    -- list of Y coordinates for player to target
  } deriving (Show)

-- GameState holds all the data about the current game
data GameState = GameState
  { player :: Player      -- The player character
  , bombs :: [Bomb]       -- List of bombs
  , obstacles :: [Obstacle] -- List of solid obstacle blocks
  , score :: Int          -- Current score
  , gameOver :: Bool      -- Whether the game has ended
  } deriving (Show)

-- Bomb represents a red square that can detonate into player-hurting red squares
data Bomb = Bomb
  { bombX :: Float       -- X position
  , bombY :: Float       -- Y position
  , bombVelX :: Float    -- X velocity (horizontal speed)
  , bombVelY :: Float    -- Y velocity (vertical speed)
  , bombSize :: Float    -- Size of the enemy square
  , isDetonated :: Bool  -- boolean to track detonating status
  , timer :: Float       -- float timer that ticks down to detonation
  } deriving (Show)

-- Obstacle represents solid blocks that block movement
data Obstacle = Obstacle
  { obstacleX :: Float      -- X position (center)
  , obstacleY :: Float      -- Y position (center)
  , obstacleWidth :: Float  -- Width of the obstacle
  , obstacleHeight :: Float -- Height of the obstacle
  , isHardBlock :: Bool -- Type of the obstacle (Boolean bc there's only two)
  } deriving (Show)

--bomb data type question mark ifl it'll help for the next phases

-- ============================================================================
-- INITIAL STATE - Starting values when the game begins
-- ============================================================================

-- Create the initial player in the center of the screen
initialPlayer :: Player
initialPlayer = Player
  { playerX = 375      -- Center X
  , playerY = 575      -- Center Y
  , playerVelX = 0     -- Not moving initially
  , playerVelY = 0     -- Not moving initially
  , playerSize = 30    -- 30 pixels square
  , bombsHeld = 100      -- 1 bomb held by default
  , targetX = 375      -- same as player position
  , targetY = 575      -- same as player position
  , spacePressed = False -- spacekey is not pressed by default
  , xCoords = 
    [25, 75, 125, 175, 225, 275, 
    325, 375, 425, 475, 525, 575, 
    625, 675, 725]
  , yCoords = 
    [25, 75, 125, 175, 225, 275, 
    325, 375, 425, 475, 525, 575, 625]
  }

-- Create the initial game state with 2 enemies and obstacles
initialGameState :: GameState
initialGameState = GameState
  { player = initialPlayer
  , bombs = 
      [ Bomb 75 75 0 0 50 False 0       -- First enemy: top-left, moving right-down, size 25
      , Bomb 575 575 0 0 50 False 0    -- Second enemy: bottom-right, moving left-up, size 25
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
      , Obstacle 75 575 cellSize cellSize False -- Arbitrary Soft Blocks
      ]
  , score = 0       -- Start with 0 points
  , gameOver = False -- Game starts running
  }

-- ============================================================================
-- GAME LOGIC - Functions that update the game state
-- ============================================================================

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
updatePlayerPlaceBomb :: Map.Map Word () -> Player -> Player
updatePlayerPlaceBomb keys p =
  let -- get current bombsHeld
      oldBH = bombsHeld p
      newBH = bombsHeld p - keyPress -- new bombsHeld
      spaceState = spacePressed p    -- state of spacePressed

      -- Create a test player with decremented bomb count and spacePressed as True
      testPlayer = p { bombsHeld = newBH, spacePressed = True }

  in if oldBH > 0 && spaceState == False -- if player has a bomb to place and has released the spacekey
      then testPlayer -- bomb can be placed
      else p { spacePressed = False } -- bomb cannot be placed and reset space state
  
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
    speed = 5

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
computeTarget :: Float -> [Float] -> Float
computeTarget pXY =
  minimumBy (\a b -> compare (abs (pXY - a)) (abs (pXY - b)))

-- Check if enemy collides with an obstacle (for bouncing)
checkEnemyObstacleCollision :: Bomb -> Obstacle -> Bool
checkEnemyObstacleCollision b obs =
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

-- new updateEnemy -> spawns a bomb and checks for collisions
-- returns a new bomb to be added to the list
updateBomb :: Map.Map Word () -> Player -> [Bomb] -> [Bomb]
updateBomb keys p bombs =
  let -- set new bomb attributes
    newBX =  targetX p 
    newBY = targetY p
    bombsH = bombsHeld p
    spaceState = spacePressed p

    newBomb = Bomb newBX newBY 0 0 cellSize False 0
    -- newBomb is a new bomb that has player coordinates and unmoving
    -- unmoving == not detonating

  in if backPressed == 1 && bombsH > 0 && spaceState == False
    -- boolean variable to check if backspace is pressed, bombs can be placed, and spacekey is not pressed
    then bombs ++ [newBomb] -- append newBomb to list of bombs
    else bombs -- returns Nothing if backspace not pressed
  
  where
    backPressed =
      if Map.member 32 keys
        then 1 
      else 0

-- update bomb timer, ticks down every server tick which is 0.16 seconds, explodes if 0 seconds
updateBombTimer :: Bomb -> Bomb
updateBombTimer b =
  let -- decrement bomb timer by 0.16 seconds every function call
    oldTime = timer b
    timeTick = 0.16
  
  in if oldTime <= 0 -- if time is up, detonate
    then
      b { isDetonated = True }
    else
      b { timer = oldTime - timeTick }

-- OBSOLETE
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
      hitObstacle = any (checkEnemyObstacleCollision testBomb) obstacles
      
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

-- Check if two rectangles (player and enemy) are colliding
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
     && isDetonated b

-- Update the entire game state each frame
updateGameState :: Map.Map Word () -> GameState -> GameState
updateGameState keys gs = 
  -- Don't update if game is over
  if gameOver gs 
  then gs
  else
    let updatedPlayer = updatePlayer keys (obstacles gs) (player gs)  -- Update player, checking obstacles
        updatedPlayer' = updatePlayerPlaceBomb keys updatedPlayer   -- Update player again for bomb updating
        updatedBombs = updateBomb keys updatedPlayer (bombs gs)  -- Update all enemies, bouncing off obstacles
        
        -- Check if player collides with any enemy
        collision = any (checkCollision updatedPlayer') updatedBombs
        
        -- Increment score each frame if still alive
        newScore = if collision then score gs else score gs + 1
        
    in gs
      { player = updatedPlayer'
      , bombs = updatedBombs
      , score = newScore
      , gameOver = collision  -- Game ends on collision
      }

-- Clamp a value between a minimum and maximum
-- Example: clamp 0 100 150 = 100, clamp 0 100 50 = 50
clamp :: Ord a => a -> a -> a -> a
clamp minV maxV = max minV . min maxV

-- ============================================================================
-- RENDERING - Functions that draw the game to the canvas
-- ============================================================================

-- Draw the entire game state to the canvas
drawGame :: CanvasRenderingContext2D -> GameState -> JSM ()
drawGame ctx gs = do
  -- Clear the canvas with a dark background
  setFillStyle ctx (toJSString ("rgb(20, 20, 30)" :: String))  -- Dark blue-gray
  fillRect ctx 0 0 canvasWidth canvasLength
  
  -- Draw all obstacles with different colors based on hardness
  mapM_ (\obs -> do
          if isHardBlock obs
            then setFillStyle ctx (toJSString ("rgb(80, 80, 90)" :: String))   -- Dark gray
            else setFillStyle ctx (toJSString ("rgb(170, 170, 180)" :: String)) -- Light gray

          fillRect ctx
            (obstacleX obs - obstacleWidth obs / 2)   -- Top-left X
            (obstacleY obs - obstacleHeight obs / 2)  -- Top-left Y
            (obstacleWidth obs)                       -- Width
            (obstacleHeight obs)                      -- Height
        )
        (obstacles gs)
    
-- if bomb == explode then smth smth block up/down/left/right/origin setFillStyle red chuchu
-- idk how to do the 1s part

  -- Draw the player as a blue square (or gray if game over)
  if gameOver gs
    then setFillStyle ctx (toJSString ("rgb(100, 100, 100)" :: String))  -- Gray if dead
    else setFillStyle ctx (toJSString ("rgb(100, 200, 255)" :: String))  -- Light blue if alive
  let p = player gs
  -- Draw square centered on player position
  fillRect ctx (playerX p - playerSize p / 2)   -- Top-left X
               (playerY p - playerSize p / 2)   -- Top-left Y
               (playerSize p)                    -- Width
               (playerSize p)                    -- Height
  
  -- Draw all enemies as red squares
  setFillStyle ctx (toJSString ("rgb(255, 100, 100)" :: String))  -- Light red
  -- Map over each enemy and draw it
  mapM_ (\b -> fillRect ctx 
                 (bombX b - bombSize b / 2)    -- Top-left X
                 (bombY b - bombSize b / 2)    -- Top-left Y
                 (bombSize b)                    -- Width
                 (bombSize b))                   -- Height
        (bombs gs)
  
  -- Draw the score text in the top-left corner
  setFillStyle ctx (toJSString ("rgb(255, 255, 255)" :: String))  -- White
  setFont ctx (toJSString ("20px Arial" :: String))  -- 20pt Arial font
  fillText ctx (toJSString ("Score: " ++ show (score gs) :: String)) 
               10           -- X position
               30           -- Y position
               (Nothing :: Maybe Float)  -- No maximum width
  
  -- Draw GAME OVER message if player hit an enemy
  if gameOver gs
    then do
      setFillStyle ctx (toJSString ("rgb(255, 50, 50)" :: String))  -- Bright red
      setFont ctx (toJSString ("48px Arial" :: String))  -- Large font
      fillText ctx (toJSString ("GAME OVER!" :: String))
                   250        -- Centered X
                   300        -- Centered Y
                   (Nothing :: Maybe Float)
      setFont ctx (toJSString ("24px Arial" :: String))  -- Smaller font
      fillText ctx (toJSString ("Refresh to restart" :: String))
                   280
                   350
                   (Nothing :: Maybe Float)
    else return ()

-- ============================================================================
-- MAIN APPLICATION - Set up the UI and game loop
-- ============================================================================

main :: IO ()
main = mainWidget $ do
  el "div" $ do
    -- Display title and instructions
    el "h1" $ text "Haskell Canvas 2D Game Demo"
    el "p" $ text "Use WASD or Arrow Keys to move the blue square. Avoid the red enemies!"
    
    -- Create the canvas element with attributes
    (canvasEl, _) <- elAttr' "canvas" 
      (Map.fromList 
        [ ("width", "750")                              -- Canvas width
        , ("height", "650")                             -- Canvas height
        , ("style", "border: 2px solid #333; background: #000")  -- Styling
        , ("tabindex", "0")                             -- Make it focusable for keyboard
        ]) 
      blank
    
    -- Get the raw DOM element from Reflex's element wrapper
    let canvas = _element_raw canvasEl
    
    -- ========================================================================
    -- KEYBOARD INPUT HANDLING
    -- ========================================================================
    
    -- Listen for keyboard events on the canvas
    let keyDownEvt = domEvent Keydown canvasEl  -- Fires when key is pressed
    let keyUpEvt = domEvent Keyup canvasEl      -- Fires when key is released
    
    -- Convert keyboard events to key codes (Word numbers like 87 for 'W')
    let keyDownCode = fmap (fromIntegral . fromEnum) keyDownEvt
    let keyUpCode = fmap (fromIntegral . fromEnum) keyUpEvt
    
    -- Build a map tracking which keys are currently pressed
    -- rec allows recursive definition (keyState depends on events that use keyState)
    rec keyState <- foldDyn ($) Map.empty $ mergeWith (.)
          [ fmap (\k -> Map.insert k ()) keyDownCode   -- Add key when pressed
          , fmap Map.delete keyUpCode                   -- Remove key when released
          ]
    
    -- ========================================================================
    -- GAME LOOP TIMING
    -- ========================================================================
    
-- timer na di ko alam pano gagawin but ig set a variable to be 1min tapos parang yung tick sa baba na magdedecrement

    -- Get current time to start the ticker
    now <- liftIO getCurrentTime
    
    -- Create a tick event that fires ~60 times per second (every 0.016 seconds)
    tick <- tickLossy (0.016 :: NominalDiffTime) now
    
    -- ========================================================================
    -- GAME STATE UPDATES
    -- ========================================================================
    
    -- Update game state on every tick
    -- foldDyn accumulates state: takes update function, initial state, and event
    gameState <- foldDyn (\keys gs -> updateGameState keys gs) 
                         initialGameState           -- Start with initial state
                         (tagPromptlyDyn keyState tick)  -- Sample keyState on each tick
    
    -- ========================================================================
    -- RENDERING
    -- ========================================================================
    
    -- Render the game whenever the game state changes
    performEvent_ $ ffor (updated gameState) $ \gs -> liftJSM $ do
      -- Cast the raw DOM element to HTMLCanvasElement
      mCanvasEl <- DOM.castTo DOM.HTMLCanvasElement canvas
      case mCanvasEl of
        Nothing -> return ()  -- If cast fails, do nothing
        Just canvasEl -> do
          -- Get the 2D rendering context from the canvas
          mRenderingContext <- getContext canvasEl (toJSString ("2d" :: String)) ([] :: [T.Text])
          case mRenderingContext of
            Nothing -> return ()  -- If context not available, do nothing
            Just renderingContext -> do
              -- Cast the generic rendering context to 2D context
              mCtx <- DOM.castTo DOM.CanvasRenderingContext2D renderingContext
              case mCtx of
                Nothing -> return ()  -- If cast fails, do nothing
                Just ctx -> drawGame ctx gs  -- Finally, draw the game!
    
    -- Display game state details below the canvas
    el "div" $ do
      el "h3" $ text "Game State Debug Info:"
      
      -- Display current game state values
      el "div" $ do
        dynText $ ffor gameState $ \gs -> 
          "Game Over: " <> (if gameOver gs then "TRUE" else "FALSE") <> " | Score: " <> pack (show $ score gs)
      
      -- Display player state
      el "div" $ do
        el "h4" $ text "Player:"
        dynText $ ffor gameState $ \gs ->
          let p = player gs
          in "X: " <> pack (show $ playerX p) <> 
             " | Y: " <> pack (show $ playerY p) <> 
             " | VelX: " <> pack (show $ playerVelX p) <> 
             " | VelY: " <> pack (show $ playerVelY p) <> 
             " | Size: " <> pack (show $ playerSize p) <>
             " | bombsHeld: " <> pack (show $ bombsHeld p) <>
             " | targetX: " <> pack (show $ targetX p) <>
             " | targetY: " <> pack (show $ targetY p) <>
             " | xCoords: " <> pack (show $ xCoords p) <>
             " | yCoords: " <> pack (show $ yCoords p)
      
      -- Display enemies state
      el "div" $ do
        el "h4" $ text "Bombs:"
        dyn_ $ ffor gameState $ \gs ->
          el "div" $ do
            forM_ (zip [1..] (bombs gs)) $ \(i, b) ->
              el "div" $ text $
                "Bombs " <> pack (show (i :: Int)) <> ": " <>
                "X: " <> pack (show $ bombX b) <> 
                " | Y: " <> pack (show $ bombY b) <> 
                " | VelX: " <> pack (show $ bombVelX b) <> 
                " | VelY: " <> pack (show $ bombY b) <> 
                " | Size: " <> pack (show $ bombSize b) <>
                " | isDetonated: " <> pack (show $ isDetonated b) <>
                " | timer: " <> pack (show $ timer b)
      
      -- Display obstacles state
      el "div" $ do
        el "h4" $ text "Obstacles:"
        dyn_ $ ffor gameState $ \gs ->
          el "div" $ do
            forM_ (zip [1..] (obstacles gs)) $ \(i, obs) ->
              el "div" $ text $
                "Obstacle " <> pack (show (i :: Int)) <> ": " <>
                "X: " <> pack (show $ obstacleX obs) <> 
                " | Y: " <> pack (show $ obstacleY obs) <> 
                " | Width: " <> pack (show $ obstacleWidth obs) <> 
                " | Height: " <> pack (show $ obstacleHeight obs) <>
                " | isHard: " <> pack (show $ isHardBlock obs)
    
    return ()

  -- Display game features below the canvas
  el "div" $ do
    el "h3" $ text "Game Features:"
    el "ul" $ do
      el "li" $ text "Player movement with keyboard input (WASD/Arrows)"
      el "li" $ text "Enemy entities with bouncing physics"
      el "li" $ text "Canvas 2D rendering at 60 FPS"
      el "li" $ text "Game state management with Reflex FRP"