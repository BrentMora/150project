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

-- Constants
screenWidth :: Double
screenWidth = 300                             -- Canvas width in pixels

screenHeight :: Double
screenHeight = 300                            -- Canvas height in pixels

squareSize :: Double
squareSize = 50                               -- Size of the moving square (50x50 pixels)

main :: IO ()
main = M.run $ M.startApp app                 -- Entry point: starts the Miso application
 where
  app =
    (M.component initModel update view)       -- Create app with initial model, update function, and view function
      { M.initialAction = Just MsgGetTime     -- First action to trigger when app starts (begins animation loop)
      , M.subs = [M.keyboardSub handleKey]    -- Subscribe to keyboard events, routing them through handleKey
      }

handleKey :: IntSet -> Msg                    -- Converts keyboard key codes to messages
handleKey intSet
  | IntSet.member 37 intSet = MsgMoveLeft     -- Key code 37 = left arrow
  | IntSet.member 38 intSet = MsgMoveUp       -- Key code 38 = up arrow
  | IntSet.member 39 intSet = MsgMoveRight    -- Key code 39 = right arrow
  | IntSet.member 40 intSet = MsgMoveDown     -- Key code 40 = down arrow
  | otherwise = MsgNoOp                       -- Any other key does nothing

data Model = Model                            -- Application state
  { time :: Double                            -- Accumulated time in milliseconds
  , lastMilli :: Double                       -- Timestamp of last frame (for calculating delta)
  , x :: Double                               -- X position of the square
  , y :: Double                               -- Y position of the square
  , vx :: Double                              -- X velocity (horizontal speed)
  , vy :: Double                              -- Y velocity (vertical speed)
  , tick :: Int                               -- Frame counter for debugging
  , currentDirection :: Maybe Direction       -- Currently pressed direction key (if any)
  }
  deriving (Show, Eq)                         -- Auto-generate Show and Eq instances

data Direction                                -- Enumeration of possible movement directions
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Show, Eq)                         -- Auto-generate Show and Eq instances

initModel :: Model                            -- Initial application state
initModel =
  Model
    { time = 0                                -- Start time at 0
    , lastMilli = 0                           -- No previous frame yet
    , x = 0                                   -- Square starts at left edge
    , y = 0                                   -- Square starts at top edge
    , vx = 2                                  -- Moving right at 2 pixels per frame
    , vy = 0                                  -- No vertical movement initially
    , tick = 0                                -- No frames rendered yet
    , currentDirection = Nothing              -- No key pressed initially
    }

data Msg                                      -- Messages that can trigger state updates
  = MsgGetTime                                -- Request current timestamp
  | MsgSetTime Double                         -- Receive timestamp and update animation
  | MsgMoveUp                                 -- Up arrow pressed
  | MsgMoveDown                               -- Down arrow pressed
  | MsgMoveLeft                               -- Left arrow pressed
  | MsgMoveRight                              -- Right arrow pressed
  | MsgNoOp                                   -- No operation (ignored key)
  deriving (Show, Eq)                         -- Auto-generate Show and Eq instances

update :: Msg -> M.Transition Model Msg       -- Update function: processes messages and updates model
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
  let diff = if milli >= lastMilli then milli - lastMilli else 1000 - lastMilli + milli  -- Calculate time delta, handling millisecond wraparound at 1000
  let x' = model.x + model.vx                 -- Calculate new X position
  let newX                                    -- Clamp X position to canvas bounds
        | x' <= 0 = 0                         -- Hit left edge: stop at 0
        | x' + squareSize >= screenWidth = screenWidth - squareSize  -- Hit right edge: stop at right boundary
        | otherwise = x'                      -- Otherwise: use calculated position
  let newVx                                   -- Update X velocity (bounce on edges)
        | x' <= 0 || x' + squareSize >= screenWidth = model.vx * (-1.0)  -- Reverse direction if hit edge
        | otherwise = model.vx                -- Otherwise: maintain velocity
  let y' = model.y + model.vy                 -- Calculate new Y position
  let newY                                    -- Clamp Y position to canvas bounds
        | y' <= 0 = 0                         -- Hit top edge: stop at 0
        | y' + squareSize >= screenHeight = screenHeight - squareSize  -- Hit bottom edge: stop at bottom boundary
        | otherwise = y'                      -- Otherwise: use calculated position
  let newVy = case model.currentDirection of  -- Update Y velocity based on pressed key
        Just DirUp -> -2                      -- Up arrow: move up at 2 pixels per frame
        Just DirDown -> 2                     -- Down arrow: move down at 2 pixels per frame
        _ -> 0                                -- No vertical key or left/right: no vertical movement
  M.put $                                     -- Update the model with new state
    model
      { time = model.time + diff              -- Accumulate elapsed time
      , lastMilli = milli                     -- Store current timestamp for next frame
      , x = newX                              -- Update X position
      , vx = newVx                            -- Update X velocity
      , tick = model.tick + 1                 -- Increment frame counter
      , y = newY                              -- Update Y position
      , vy = newVy                            -- Update Y velocity
      --, currentDirection = Nothing          -- (Commented out) Would reset direction, but we want it to persist
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
update MsgNoOp = pure ()                      -- Handle no-op message: do nothing

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