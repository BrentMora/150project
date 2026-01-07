{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Client where

import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Miso.WebSocket as WS

import Miso.CSS (Color (..))
import qualified Miso.Canvas as Canvas
import Miso.Canvas (TextAlignType (TextAlignCenter))

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified Language.Javascript.JSaddle as JSaddle
import qualified Miso.CSS as CSS

import qualified Common
import Data.Foldable (forM_)
import qualified Data.Map as Map
import Control.Monad (when)


screenWidth :: Double
screenWidth = 750

screenHeight :: Double
screenHeight = 650

squareSize :: Double
squareSize = 50

mainClient :: String -> Int -> IO ()
mainClient ip port = do
  putStrLn "Starting Bomberman game..."
  M.run $ M.startApp app
 where
  url = "ws://" <> M.ms ip <> ":" <> M.ms port

  app =
    (M.component initModel update view)
      { M.initialAction = Just (MsgStartConnection url)
      , M.subs = [M.keyboardSub handleKey]
      }

data CanvasState = CanvasState
  { csPlayerImg :: M.Image
  , csHardBlockImg :: M.Image
  , csSoftBlockImg :: M.Image
  , csBombTickingImg :: M.Image
  , csBombDetonatingImg :: M.Image
  , csFireUpImg :: M.Image
  , csSpeedUpImg :: M.Image
  , csBombUpImg :: M.Image
  , csBackgroundImg :: M.Image
  }

instance JSaddle.FromJSVal CanvasState where
  fromJSVal v = do
    ((p, hb, sb, bt), (bd, fu, su, bu, bg)) <- JSaddle.fromJSValUnchecked v
    pure $ Just $ CanvasState p hb sb bt bd fu su bu bg

instance JSaddle.ToJSVal CanvasState where
  toJSVal st =
    JSaddle.toJSVal
      ( ( csPlayerImg st
        , csHardBlockImg st
        , csSoftBlockImg st
        , csBombTickingImg st
        )
      , ( csBombDetonatingImg st
        , csFireUpImg st
        , csSpeedUpImg st
        , csBombUpImg st
        , csBackgroundImg st
        )
      )

initCanvas :: M.DOMRef -> Canvas.Canvas CanvasState
initCanvas _ = JSaddle.liftJSM $ do
  playerImg <- M.newImage "resources/player.png"
  hardBlockImg <- M.newImage "resources/hard_block.png"
  softBlockImg <- M.newImage "resources/soft_block.png"
  bombTickingImg <- M.newImage "resources/bomb.png"
  bombDetonatingImg <- M.newImage "resources/explosion.png"
  fireUpImg <- M.newImage "resources/fireup.png"
  speedUpImg <- M.newImage "resources/speedup.png"
  bombUpImg <- M.newImage "resources/bombup.png"
  backgroundImg <- M.newImage "resources/background.png"
  pure $
    CanvasState
      { csPlayerImg = playerImg
      , csHardBlockImg = hardBlockImg
      , csSoftBlockImg = softBlockImg
      , csBombTickingImg = bombTickingImg
      , csBombDetonatingImg = bombDetonatingImg
      , csFireUpImg = fireUpImg
      , csSpeedUpImg = speedUpImg
      , csBombUpImg = bombUpImg
      , csBackgroundImg = backgroundImg
      }

handleKey :: IntSet -> Msg
handleKey intSet =
  MsgUpdateKeyState $
    Common.KeyState
      { Common.up = IntSet.member 38 intSet
      , Common.down = IntSet.member 40 intSet
      , Common.left = IntSet.member 37 intSet
      , Common.right = IntSet.member 39 intSet
      , Common.space = IntSet.member 32 intSet
      }

data Model = Model
  { viewData :: Maybe Common.ViewData
  , keyState :: Common.KeyState
  , websocket :: WS.WebSocket
  , myPlayerId :: Maybe Common.PlayerId
  , lastBombCount :: Int
  , lastPowerUpCount :: Int
  , lastDetonationCount :: Int
  }
  deriving (Eq)

initModel :: Model
initModel =
  Model
    { viewData = Nothing
    , keyState = Common.defaultKeyState
    , websocket = WS.emptyWebSocket
    , myPlayerId = Nothing
    , lastBombCount = 0
    , lastPowerUpCount = 0
    , lastDetonationCount = 0
    }

data Msg
  = MsgStartConnection M.MisoString
  | MsgOnOpen WS.WebSocket
  | MsgOnClosed WS.Closed
  | MsgOnMessage Common.ViewData
  | MsgOnError M.MisoString
  | MsgUpdateKeyState Common.KeyState
  | MsgNoOp
  deriving (Eq)

sendClientInput :: Model -> M.Transition Model Msg
sendClientInput model =
  WS.sendJSON
    (websocket model)
    Common.ClientInput{Common.keyState = keyState model}

update :: Msg -> M.Transition Model Msg
---
update (MsgStartConnection url) = do
  WS.connectJSON url MsgOnOpen MsgOnClosed MsgOnMessage MsgOnError
---
update (MsgOnOpen websocket) = do
  M.modify $ \model -> model{websocket}
---
update (MsgOnClosed _) = pure ()
---
update (MsgOnMessage newViewData) = do
  model <- M.get
  
  -- Determine player ID (first player to receive data)
  let newPlayerId = case myPlayerId model of
        Just pid -> Just pid
        Nothing -> case Map.keys (Common.players newViewData) of
          [] -> Nothing
          (pid:_) -> Just pid
  
  -- Count detonating bombs
  let detonatingCount = length $ filter (\b -> Common.isDetonated b == Common.Detonating) (Common.bombs newViewData)
  let powerUpCount = length (Common.powerups newViewData)
  
  -- Play sound effects
  M.io_ $ do
    -- Bomb detonation sound
    when (detonatingCount > lastDetonationCount model) $ do
      cons <- JSaddle.jsg ("Audio" :: String)
      audio <- JSaddle.new cons ["resources/bomb_detonate.wav" :: String]
      _ <- audio JSaddle.# ("play" :: String) $ ()
      pure ()
    
    -- Powerup pickup sound
    when (powerUpCount < lastPowerUpCount model && myPlayerId model == newPlayerId) $ do
      cons <- JSaddle.jsg ("Audio" :: String)
      audio <- JSaddle.new cons ["resources/powerup_pickup.wav" :: String]
      _ <- audio JSaddle.# ("play" :: String) $ ()
      pure ()
    
    -- Player death sound
    case myPlayerId model of
      Just pid -> do
        let wasAlive = case viewData model of
              Just vd -> Map.lookup pid (Common.gameOverFlags vd) == Just False
              Nothing -> True
        let isNowDead = Map.lookup pid (Common.gameOverFlags newViewData) == Just True
        when (wasAlive && isNowDead) $ do
          cons <- JSaddle.jsg ("Audio" :: String)
          audio <- JSaddle.new cons ["resources/player_dies.wav" :: String]
          _ <- audio JSaddle.# ("play" :: String) $ ()
          pure ()
      Nothing -> pure ()
  
  M.modify $ \m -> m
    { viewData = Just newViewData
    , myPlayerId = newPlayerId
    , lastDetonationCount = detonatingCount
    , lastPowerUpCount = powerUpCount
    }

---

update (MsgOnError _) = pure ()
---
update (MsgUpdateKeyState keyState) = do
  M.modify $ \model -> model{keyState = keyState}
  model <- M.get
  sendClientInput model
---
update MsgNoOp = pure ()

view :: Model -> M.View Model Msg
view model =
  H.div_
    []
    [ Canvas.canvas
        [ P.width_ (M.ms screenWidth)
        , P.height_ (M.ms screenHeight)
        ]
        initCanvas
        (viewCanvas model)
    -- Preload images (hidden)
    , H.img_ [CSS.style_ [CSS.display "none"], P.src_ "resources/player.png"]
    , H.img_ [CSS.style_ [CSS.display "none"], P.src_ "resources/hard_block.png"]
    , H.img_ [CSS.style_ [CSS.display "none"], P.src_ "resources/soft_block.png"]
    , H.img_ [CSS.style_ [CSS.display "none"], P.src_ "resources/bomb.png"]
    , H.img_ [CSS.style_ [CSS.display "none"], P.src_ "resources/explosion.png"]
    , H.img_ [CSS.style_ [CSS.display "none"], P.src_ "resources/fireup.png"]
    , H.img_ [CSS.style_ [CSS.display "none"], P.src_ "resources/speedup.png"]
    , H.img_ [CSS.style_ [CSS.display "none"], P.src_ "resources/bombup.png"]
    , H.img_ [CSS.style_ [CSS.display "none"], P.src_ "resources/background.png"]
    ]

viewCanvas :: Model -> CanvasState -> Canvas.Canvas ()
viewCanvas model canvasState = do
  case viewData model of
    Nothing -> do
      -- Black background
      Canvas.fillStyle (Canvas.ColorArg (RGB 0 0 0))
      Canvas.fillRect (0, 0, screenWidth, screenHeight)
      
      -- Waiting message
      Canvas.fillStyle (Canvas.ColorArg (RGB 255 255 255))
      Canvas.font "24px Arial"
      Canvas.textAlign TextAlignCenter
      Canvas.fillText ("Waiting for other player...", screenWidth / 2, screenHeight / 2)
    
    Just vd -> do
      -- Draw tiled background
      let numTilesX = ceiling (screenWidth / squareSize) :: Int
      let numTilesY = ceiling (screenHeight / squareSize) :: Int
      forM_ [0 .. numTilesX - 1] $ \x -> do
        forM_ [0 .. numTilesY - 1] $ \y -> do
          Canvas.drawImage' (csBackgroundImg canvasState, fromIntegral x * squareSize, fromIntegral y * squareSize, squareSize, squareSize)
      
      -- Check if my player is dead
      let myDead = case myPlayerId model of
            Just pid -> Map.lookup pid (Common.gameOverFlags vd) == Just True
            Nothing -> False
      
      -- Draw obstacles
      forM_ (Common.obstacles vd) $ \obs -> do
        let img = if Common.isHardBlock obs then csHardBlockImg canvasState else csSoftBlockImg canvasState
        Canvas.drawImage' (img, Common.obstacleX obs - 25, Common.obstacleY obs - 25, squareSize, squareSize)
      
      -- Draw powerups
      forM_ (Common.powerups vd) $ \pu -> do
        let img = case Common.powerupType pu of
              Common.FireUp -> csFireUpImg canvasState
              Common.SpeedUp -> csSpeedUpImg canvasState
              Common.BombUp -> csBombUpImg canvasState
        Canvas.drawImage' (img, Common.powerupX pu - 25, Common.powerupY pu - 25, Common.powerupSize pu, Common.powerupSize pu)
      
      -- Draw bombs
      forM_ (Common.bombs vd) $ \bomb -> do
        let img = if Common.isDetonated bomb == Common.Ticking 
                    then csBombTickingImg canvasState 
                    else csBombDetonatingImg canvasState
        Canvas.drawImage' (img, Common.bombX bomb - 25, Common.bombY bomb - 25, Common.bombSize bomb, Common.bombSize bomb)
      
      -- Draw players
      forM_ (Map.elems (Common.players vd)) $ \player -> do
        Canvas.drawImage' (csPlayerImg canvasState, Common.playerX player - 15, Common.playerY player - 15, Common.playerSize player, Common.playerSize player)
      
      -- Draw timer
      Canvas.fillStyle (Canvas.ColorArg (RGB 255 255 255))
      Canvas.font "20px Arial"
      Canvas.textAlign Canvas.TextAlignLeft
      let timer = round (Common.gameTimer vd) :: Int
      let (m, s) = divMod timer 60
      let mPadded = if m < 10 then "0" ++ show m else show m
      let sPadded = if s < 10 then "0" ++ show s else show s
      Canvas.fillText (M.ms ("Time: " ++ mPadded ++ ":" ++ sPadded), 10, 30)
      
      -- Draw player stats (if we know our player ID)
      case myPlayerId model >>= flip Map.lookup (Common.players vd) of
        Just myPlayer -> do
          Canvas.fillText (M.ms ("Bombs: " ++ show (Common.bombsHeld myPlayer) ++ "/" ++ show (Common.maxbombs myPlayer)), 10, 60)
          Canvas.fillText (M.ms ("Fire Range: " ++ show (Common.fireups myPlayer)), 10, 90)
          Canvas.fillText (M.ms ("Speed: " ++ show (round (Common.speedups myPlayer) :: Int)), 10, 120)   
        Nothing -> pure ()
      
      -- Game over overlay
      when myDead $ do
        Canvas.fillStyle (Canvas.ColorArg (RGBA 0 0 0 0.7))
        Canvas.fillRect (0, 0, screenWidth, screenHeight)
        Canvas.fillStyle (Canvas.ColorArg (RGB 255 0 0))
        Canvas.font "48px Arial"
        Canvas.textAlign TextAlignCenter
        Canvas.fillText (M.ms ("GAME OVER" :: String), screenWidth / 2, screenHeight / 2)