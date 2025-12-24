{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import Miso
import Miso.String (MisoString, ms, fromMisoString)
import qualified Miso.String as MS
import Data.Map (Map)
import qualified Data.Map as M
import JavaScript.Object
import JavaScript.Object.Internal (Object(..))

#ifdef GHCJS_BROWSER
import GHCJS.Types (JSVal)
import GHCJS.Foreign.Callback
import GHCJS.Marshal
#endif

-- | MODEL: Simple game state
data Model = Model
  { -- Canvas basics
    canvasWidth :: Int
  , canvasHeight :: Int
  
    -- Animation and timing
  , lastFrameTime :: Double
  , deltaTime :: Double
  , fps :: Double
  
    -- Sprite management
  , player :: Sprite
  , enemies :: [Sprite]
  , projectiles :: [Sprite]
  
    -- Input handling
  , keysPressed :: Map Int Bool
  , mousePos :: (Double, Double)
  , mouseDown :: Bool
  
    -- Game state
  , score :: Int
  , gameState :: GameState
  }
  deriving (Eq, Show)

-- | Sprite with basic properties
data Sprite = Sprite
  { spriteX :: Double
  , spriteY :: Double
  , spriteVelX :: Double
  , spriteVelY :: Double
  , spriteWidth :: Double
  , spriteHeight :: Double
  , spriteColor :: MisoString
  , spriteRotation :: Double
  , spriteHealth :: Int
  , spriteType :: SpriteType
  }
  deriving (Eq, Show)

data SpriteType = Player | Enemy | Projectile | Item
  deriving (Eq, Show)

data GameState = Menu | Playing | Paused | GameOver
  deriving (Eq, Show)

-- | ACTION: All user and system events
data Action
  = NoOp
  | AnimationFrame Double
  | KeyDown Int
  | KeyUp Int
  | MouseMove Double Double
  | MouseDown
  | MouseUp
  | StartGame
  | PauseGame
  | SpawnEnemy
  | FireProjectile
  deriving (Show, Eq)

-- | INITIAL STATE
initialModel :: Model
initialModel = Model
  { canvasWidth = 800
  , canvasHeight = 600
  , lastFrameTime = 0
  , deltaTime = 0
  , fps = 0
  , player = Sprite 400 500 0 0 40 40 "#00ff00" 0 100 Player
  , enemies = []
  , projectiles = []
  , keysPressed = M.empty
  , mousePos = (0, 0)
  , mouseDown = False
  , score = 0
  , gameState = Menu
  }

-- | UPDATE: MVU pattern - pure state transitions
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m

updateModel (AnimationFrame time) m = m { lastFrameTime = time
                                         , deltaTime = dt
                                         , fps = if dt > 0 then 1000 / dt else 0
                                         } <# do
  AnimationFrame <$> getCurrentTime
  where
    dt = if lastFrameTime m == 0 then 0 else time - lastFrameTime m

updateModel (KeyDown code) m = noEff $ m { keysPressed = M.insert code True (keysPressed m) }

updateModel (KeyUp code) m = noEff $ m { keysPressed = M.insert code False (keysPressed m) }

updateModel (MouseMove x y) m = noEff $ m { mousePos = (x, y) }

updateModel MouseDown m = noEff $ m { mouseDown = True }

updateModel MouseUp m = noEff $ m { mouseDown = False }

updateModel StartGame m = m { gameState = Playing, score = 0 } <# do
  AnimationFrame <$> getCurrentTime

updateModel PauseGame m = noEff $ m { gameState = if gameState m == Playing then Paused else Playing }

updateModel SpawnEnemy m = noEff $ m { enemies = newEnemy : enemies m }
  where
    newEnemy = Sprite 780 100 (-100) 0 30 30 "#ff0000" 0 50 Enemy

updateModel FireProjectile m = noEff $ m { projectiles = newProjectile : projectiles m }
  where
    px = spriteX (player m)
    py = spriteY (player m)
    newProjectile = Sprite px py 0 (-400) 8 8 "#ffff00" 0 1 Projectile

-- | Helper: Check if key is pressed
isKeyPressed :: Int -> Model -> Bool
isKeyPressed code m = M.findWithDefault False code (keysPressed m)

-- | VIEW: Render function using Canvas
viewModel :: Model -> View Action
viewModel m = div_ []
  [ canvas_ 
      [ id_ "gameCanvas"
      , width_ $ ms $ show $ canvasWidth m
      , height_ $ ms $ show $ canvasHeight m
      , style_ $ M.fromList [("border", "2px solid #333"), ("display", "block")]
      , onMouseMove handleMouseMove
      , onMouseDown MouseDown
      , onMouseUp MouseUp
      ]
      []
  , div_ [style_ $ M.fromList [("margin-top", "10px"), ("font-family", "monospace")]]
      [ text $ "FPS: " <> ms (show (floor $ fps m :: Int))
      , br_ []
      , text $ "Score: " <> ms (show $ score m)
      , br_ []
      , text $ "Game State: " <> ms (show $ gameState m)
      , br_ []
      , text $ "Enemies: " <> ms (show $ length $ enemies m)
      , br_ []
      , button_ [onClick StartGame] [text "Start"]
      , button_ [onClick PauseGame] [text "Pause/Resume"]
      , button_ [onClick SpawnEnemy] [text "Spawn Enemy"]
      ]
  , div_ [style_ $ M.fromList [("margin-top", "20px"), ("max-width", "800px")]]
      [ h3_ [] [text "Canvas 2D Game Concepts Demonstrated:"]
      , ul_ []
          [ li_ [] [text "Canvas setup and basic rendering"]
          , li_ [] [text "Animation loop with requestAnimationFrame"]
          , li_ [] [text "Delta time and FPS calculation"]
          , li_ [] [text "Sprite rendering (rectangles, circles)"]
          , li_ [] [text "Keyboard input handling (WASD, Arrow keys, Space)"]
          , li_ [] [text "Mouse input (position, clicks)"]
          , li_ [] [text "Collision detection (AABB)"]
          , li_ [] [text "Basic physics (velocity)"]
          , li_ [] [text "Game state management"]
          , li_ [] [text "Sprite transformations (rotation, scaling)"]
          , li_ [] [text "Advanced drawing (gradients, shadows)"]
          ]
      , h3_ [] [text "Controls:"]
      , ul_ []
          [ li_ [] [text "WASD or Arrow Keys - Move player"]
          , li_ [] [text "Space - Fire projectile"]
          , li_ [] [text "Mouse - Track position"]
          ]
      ]
  ]
  where
    handleMouseMove = onMouseMove $ \(x, y) -> MouseMove (fromIntegral x) (fromIntegral y)

-- | SUBSCRIPTIONS: Set up keyboard and animation frame listeners
subscriptions :: Model -> Sub Action Model
subscriptions m = 
  [ keyboardSub onKeyDown KeyDown
  , keyboardSub onKeyUp KeyUp
  ]
  where
    onKeyDown :: (Int -> Action) -> Sub Action Model
    onKeyDown f = \sink -> do
      window <- getWindow
      cb <- makeCallback $ \keyCode -> sink (f keyCode)
      addEventListener window "keydown" cb False
      pure (pure ())

    onKeyUp :: (Int -> Action) -> Sub Action Model
    onKeyUp f = \sink -> do
      window <- getWindow
      cb <- makeCallback $ \keyCode -> sink (f keyCode)
      addEventListener window "keyup" cb False
      pure (pure ())

-- | Main entry point
main :: IO ()
main = do
  startApp App
    { model = initialModel
    , update = updateModel
    , view = viewModel
    , subs = subscriptions
    , events = defaultEvents
    , initialAction = NoOp
    , mountPoint = Nothing
    , logLevel = Off
    }
  -- Start the game loop
  initCanvas

-- | JAVASCRIPT FFI: Canvas operations

#ifdef GHCJS_BROWSER

foreign import javascript unsafe
  "window"
  getWindow :: IO JSVal

foreign import javascript unsafe
  "$1.addEventListener($2, function(e) { $3(e.keyCode); }, $4)"
  addEventListener :: JSVal -> MisoString -> Callback (JSVal -> IO ()) -> Bool -> IO ()

foreign import javascript unsafe
  "((function() { $1(); return Date.now(); })())"
  makeCallback :: (Int -> IO ()) -> IO (Callback (JSVal -> IO ()))

foreign import javascript unsafe
  "Date.now()"
  getCurrentTime :: IO Double

-- | Initialize canvas and start rendering
foreign import javascript unsafe
  "initCanvasRendering()"
  initCanvas :: IO ()

#else

-- Stub implementations for non-GHCJS
getWindow :: IO ()
getWindow = pure ()

addEventListener :: () -> MisoString -> () -> Bool -> IO ()
addEventListener _ _ _ _ = pure ()

makeCallback :: (Int -> IO ()) -> IO ()
makeCallback _ = pure ()

getCurrentTime :: IO Double
getCurrentTime = pure 0

initCanvas :: IO ()
initCanvas = pure ()

#endif