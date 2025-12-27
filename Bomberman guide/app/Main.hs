{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

-- Import Reflex DOM for reactive web programming
import Reflex.Dom
import qualified Data.Map as Map
import Data.Text (pack)
import qualified Data.Text as T
import Control.Monad (void)
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

-- ============================================================================
-- DATA TYPES - Define the structure of our game state
-- ============================================================================

-- Player represents the blue square controlled by the user
data Player = Player
  { playerX :: Float      -- X position on canvas (0-800)
  , playerY :: Float      -- Y position on canvas (0-600)
  , playerVelX :: Float   -- X velocity (speed of horizontal movement)
  , playerVelY :: Float   -- Y velocity (speed of vertical movement)
  , playerSize :: Float   -- Size of the player square
  } deriving (Show)

-- GameState holds all the data about the current game
data GameState = GameState
  { player :: Player      -- The player character
  , enemies :: [Enemy]    -- List of enemy squares
  , score :: Int          -- Current score
  , gameOver :: Bool      -- Whether the game has ended
  } deriving (Show)

-- Enemy represents the red bouncing squares
data Enemy = Enemy
  { enemyX :: Float       -- X position
  , enemyY :: Float       -- Y position
  , enemyVelX :: Float    -- X velocity (horizontal speed)
  , enemyVelY :: Float    -- Y velocity (vertical speed)
  , enemySize :: Float    -- Size of the enemy square
  } deriving (Show)

-- ============================================================================
-- INITIAL STATE - Starting values when the game begins
-- ============================================================================

-- Create the initial player in the center of the screen
initialPlayer :: Player
initialPlayer = Player
  { playerX = 400      -- Center X (canvas is 800 wide)
  , playerY = 300      -- Center Y (canvas is 600 tall)
  , playerVelX = 0     -- Not moving initially
  , playerVelY = 0     -- Not moving initially
  , playerSize = 30    -- 30 pixels square
  }

-- Create the initial game state with 2 enemies
initialGameState :: GameState
initialGameState = GameState
  { player = initialPlayer
  , enemies = 
      [ Enemy 100 100 2 1.5 25        -- First enemy: top-left, moving right-down, size 25
      , Enemy 600 400 (-1.5) (-2) 25  -- Second enemy: bottom-right, moving left-up, size 25
      ]
  , score = 0       -- Start with 0 points
  , gameOver = False -- Game starts running
  }

-- ============================================================================
-- GAME LOGIC - Functions that update the game state
-- ============================================================================

-- Update player position based on which keys are pressed
-- Takes a Map of currently pressed keys and the current player state
-- Returns updated player state
updatePlayer :: Map.Map Word () -> Player -> Player
updatePlayer keys p = p
  { playerX = clamp 0 800 (playerX p + playerVelX p)  -- Keep X within canvas bounds
  , playerY = clamp 0 600 (playerY p + playerVelY p)  -- Keep Y within canvas bounds
  , playerVelX = vx  -- Set new X velocity based on keys
  , playerVelY = vy  -- Set new Y velocity based on keys
  }
  where
    speed = 5  -- Movement speed in pixels per frame
    
    -- Calculate X velocity: left if Left/A pressed, right if Right/D pressed
    vx = if Map.member 37 keys || Map.member 65 keys -- 37=Left Arrow, 65=A
         then -speed
         else if Map.member 39 keys || Map.member 68 keys -- 39=Right Arrow, 68=D
              then speed
              else 0
              
    -- Calculate Y velocity: up if Up/W pressed, down if Down/S pressed
    vy = if Map.member 38 keys || Map.member 87 keys -- 38=Up Arrow, 87=W
         then -speed
         else if Map.member 40 keys || Map.member 83 keys -- 40=Down Arrow, 83=S
              then speed
              else 0

-- Update a single enemy's position and handle wall bouncing
updateEnemy :: Enemy -> Enemy
updateEnemy e = e
  { enemyX = newX      -- Update X position
  , enemyY = newY      -- Update Y position
  , enemyVelX = newVelX  -- Update X velocity (might flip on wall hit)
  , enemyVelY = newVelY  -- Update Y velocity (might flip on wall hit)
  }
  where
    -- Calculate new positions by adding velocity
    newX = enemyX e + enemyVelX e
    newY = enemyY e + enemyVelY e
    
    -- Reverse X velocity if enemy hits left or right wall
    newVelX = if newX <= 0 || newX >= 800 then -(enemyVelX e) else enemyVelX e
    
    -- Reverse Y velocity if enemy hits top or bottom wall
    newVelY = if newY <= 0 || newY >= 600 then -(enemyVelY e) else enemyVelY e

-- Check if two rectangles (player and enemy) are colliding
-- Uses AABB (Axis-Aligned Bounding Box) collision detection
checkCollision :: Player -> Enemy -> Bool
checkCollision p e = 
  -- Check if rectangles overlap on both X and Y axes
  let px = playerX p
      py = playerY p
      ps = playerSize p
      ex = enemyX e
      ey = enemyY e
      es = enemySize e
      
      -- Calculate edges of each rectangle
      pLeft = px - ps / 2
      pRight = px + ps / 2
      pTop = py - ps / 2
      pBottom = py + ps / 2
      
      eLeft = ex - es / 2
      eRight = ex + es / 2
      eTop = ey - es / 2
      eBottom = ey + es / 2
      
  in -- Rectangles collide if they overlap on both axes
     pRight > eLeft && pLeft < eRight &&  -- X axis overlap
     pBottom > eTop && pTop < eBottom     -- Y axis overlap

-- Update the entire game state each frame
updateGameState :: Map.Map Word () -> GameState -> GameState
updateGameState keys gs = 
  -- Don't update if game is over
  if gameOver gs 
  then gs
  else
    let updatedPlayer = updatePlayer keys (player gs)  -- Update player based on input
        updatedEnemies = map updateEnemy (enemies gs)  -- Update all enemies
        
        -- Check if player collides with any enemy
        collision = any (checkCollision updatedPlayer) updatedEnemies
        
        -- Increment score each frame if still alive
        newScore = if collision then score gs else score gs + 1
        
    in gs
      { player = updatedPlayer
      , enemies = updatedEnemies
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
  fillRect ctx 0 0 800 600  -- Fill entire 800x600 canvas
  
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
  mapM_ (\e -> fillRect ctx 
                 (enemyX e - enemySize e / 2)    -- Top-left X
                 (enemyY e - enemySize e / 2)    -- Top-left Y
                 (enemySize e)                    -- Width
                 (enemySize e))                   -- Height
        (enemies gs)
  
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
        [ ("width", "800")                              -- Canvas width
        , ("height", "600")                             -- Canvas height
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
    
    return ()

  -- Display game features below the canvas
  el "div" $ do
    el "h3" $ text "Game Features:"
    el "ul" $ do
      el "li" $ text "Player movement with keyboard input (WASD/Arrows)"
      el "li" $ text "Enemy entities with bouncing physics"
      el "li" $ text "Canvas 2D rendering at 60 FPS"
      el "li" $ text "Game state management with Reflex FRP"