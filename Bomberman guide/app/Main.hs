{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Miso
import Miso.String (MisoString, ms)

-- | Model
data Model = Model
  { playerX :: Double
  , playerY :: Double
  , keys :: KeyState
  } deriving (Eq, Show)

data KeyState = KeyState
  { upPressed :: Bool
  , downPressed :: Bool
  , leftPressed :: Bool
  , rightPressed :: Bool
  } deriving (Eq, Show)

-- | Grid constants
gridWidth :: Int
gridWidth = 13

gridHeight :: Int
gridHeight = 15

cellSize :: Double
cellSize = 40.0

playerSpeed :: Double
playerSpeed = 0.1

-- | Action
data Action
  = NoOp
  | KeyDown Int
  | KeyUp Int
  | AnimationFrame
  deriving (Show, Eq)

-- | Initial state
initialModel :: Model
initialModel = Model
  { playerX = 6.5
  , playerY = 7.5
  , keys = KeyState False False False False
  }

-- | Update
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (KeyDown code) m = noEff $ m { keys = updateKey code True (keys m) }
updateModel (KeyUp code) m = noEff $ m { keys = updateKey code False (keys m) }
updateModel AnimationFrame m = noEff $ movePlayer m

updateKey :: Int -> Bool -> KeyState -> KeyState
updateKey code pressed ks = case code of
  37 -> ks { leftPressed = pressed }
  38 -> ks { upPressed = pressed }
  39 -> ks { rightPressed = pressed }
  40 -> ks { downPressed = pressed }
  65 -> ks { leftPressed = pressed }
  87 -> ks { upPressed = pressed }
  68 -> ks { rightPressed = pressed }
  83 -> ks { downPressed = pressed }
  _ -> ks

movePlayer :: Model -> Model
movePlayer m@Model{..} =
  let dx = (if rightPressed keys then playerSpeed else 0) - (if leftPressed keys then playerSpeed else 0)
      dy = (if downPressed keys then playerSpeed else 0) - (if upPressed keys then playerSpeed else 0)
      newX = clamp 0.5 (fromIntegral gridWidth - 0.5) (playerX + dx)
      newY = clamp 0.5 (fromIntegral gridHeight - 0.5) (playerY + dy)
  in m { playerX = newX, playerY = newY }

clamp :: Double -> Double -> Double -> Double
clamp minVal maxVal val = max minVal (min maxVal val)

-- | View
viewModel :: Model -> View Action
viewModel m@Model{..} =
  div_ []
    [ div_ [ style_ $ Map.fromList [("text-align", "center"), ("padding", "20px")] ]
        [ h1_ [] [ text "Grid Game" ]
        , p_ [] [ text "Use arrow keys or WASD to move" ]
        , canvas_
            [ id_ "gameCanvas"
            , width_ $ ms $ show (floor (cellSize * fromIntegral gridWidth) :: Int)
            , height_ $ ms $ show (floor (cellSize * fromIntegral gridHeight) :: Int)
            , style_ $ Map.fromList 
                [ ("border", "2px solid #333")
                , ("background", "#f0f0f0")
                , ("display", "block")
                , ("margin", "0 auto")
                ]
            ]
            []
        , script_ [] [ text $ renderScript m ]
        ]
    ]

-- | Generate JavaScript to render the game
renderScript :: Model -> MisoString
renderScript Model{..} = ms $ T.unpack $ T.unlines
  [ "const canvas = document.getElementById('gameCanvas');"
  , "const ctx = canvas.getContext('2d');"
  , "ctx.clearRect(0, 0, canvas.width, canvas.height);"
  , ""
  , "// Draw grid"
  , "ctx.strokeStyle = '#ccc';"
  , "ctx.lineWidth = 1;"
  , T.concat
      [ "for (let x = 0; x < " <> T.pack (show gridWidth) <> "; x++) {"
      , "  for (let y = 0; y < " <> T.pack (show gridHeight) <> "; y++) {"
      , "    ctx.strokeRect(x * " <> T.pack (show cellSize) <> ", y * " <> T.pack (show cellSize) <> ", "
      , T.pack (show cellSize) <> ", " <> T.pack (show cellSize) <> ");"
      , "  }"
      , "}"
      ]
  , ""
  , "// Draw player"
  , "ctx.beginPath();"
  , "ctx.arc("
  , "  " <> T.pack (show (playerX * cellSize)) <> ","
  , "  " <> T.pack (show (playerY * cellSize)) <> ","
  , "  " <> T.pack (show (cellSize * 0.4)) <> ","
  , "  0, 2 * Math.PI"
  , ");"
  , "ctx.fillStyle = '#4CAF50';"
  , "ctx.fill();"
  , "ctx.strokeStyle = '#2E7D32';"
  , "ctx.lineWidth = 2;"
  , "ctx.stroke();"
  ]

-- | Main
main :: IO ()
main = startApp App
  { initialAction = NoOp
  , model = initialModel
  , update = updateModel
  , view = viewModel
  , events = Map.fromList 
      [ ("keydown", \e -> KeyDown (keyCode e))
      , ("keyup", \e -> KeyUp (keyCode e))
      ]
  , subs = [ animationFrameSub ]
  , mountPoint = Nothing
  , logLevel = Off
  }

-- | Animation frame subscription
animationFrameSub :: Sub Action
animationFrameSub sink = do
  let loop = do
        _ <- requestAnimationFrame (\_ -> sink AnimationFrame >> loop)
        pure ()
  loop
  pure (pure ())