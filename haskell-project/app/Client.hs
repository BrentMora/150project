{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

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
  { playerImg :: M.Image
  , hardBlockImg :: M.Image
  , softBlockImg :: M.Image
  , bombTickingImg :: M.Image
  , bombDetonatingImg :: M.Image
  , fireUpImg :: M.Image
  , speedUpImg :: M.Image
  , bombUpImg :: M.Image
  , backgroundImg :: M.Image
  }

instance JSaddle.FromJSVal CanvasState where
  fromJSVal v = do
    (p, hb, sb, bt, bd, fu, su, bu, bg) <- JSaddle.fromJSValUnchecked v
    pure $ Just $ CanvasState p hb sb bt bd fu su bu bg

instance JSaddle.ToJSVal CanvasState where
  toJSVal st =
    JSaddle.toJSVal
      ( st.playerImg
      , st.hardBlockImg
      , st.softBlockImg
      , st.bombTickingImg
      , st.bombDetonatingImg
      , st.fireUpImg
      , st.speedUpImg
      , st.bombUpImg
      , st.backgroundImg
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
      { playerImg
      , hardBlockImg
      , softBlockImg
      , bombTickingImg
      , bombDetonatingImg
      , fireUpImg
      , speedUpImg
      , bombUpImg
      , backgroundImg
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
    model.websocket
    Common.ClientInput{Common.keyState = model.keyState}

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
update (MsgOnMessage viewData) = do
  model <- M.get
  
  -- Determine player ID (first player to receive data)
  let newPlayerId = case model.myPlayerId of
        Just pid -> Just pid
        Nothing -> case Map.keys viewData.players of
          [] -> Nothing
          (pid:_) -> Just pid
  
  -- Count detonating bombs
  let detonatingCount = length $ filter (\b -> b.isDetonated == Common.Detonating) viewData.bombs
  let powerUpCount = length viewData.powerups
  
  -- Play sound effects
  M.io_ $ do
    -- Bomb detonation sound
    when (detonatingCount > model.lastDetonationCount) $ do
      cons <- JSaddle.jsg ("Audio" :: String)
      audio <- JSaddle.new cons ["resources/bomb_detonate.wav" :: String]
      _ <- audio JSaddle.# ("play" :: String) $ ()
      pure ()
    
    -- Powerup pickup sound
    when (powerUpCount < model.lastPowerUpCount && model.myPlayerId == newPlayerId) $ do
      cons <- JSaddle.jsg ("Audio" :: String)
      audio <- JSaddle.new cons ["resources/powerup_pickup.wav" :: String]
      _ <- audio JSaddle.# ("play" :: String) $ ()
      pure ()
    
    -- Player death sound
    case model.myPlayerId of
      Just pid -> do
        let wasAlive = case model.viewData of
              Just vd -> Map.lookup pid vd.gameOverFlags == Just False
              Nothing -> True
        let isNowDead = Map.lookup pid viewData.gameOverFlags == Just True
        when (wasAlive && isNowDead) $ do
          cons <- JSaddle.jsg ("Audio" :: String)
          audio <- JSaddle.new cons ["resources/player_dies.wav" :: String]
          _ <- audio JSaddle.# ("play" :: String) $ ()
          pure ()
      Nothing -> pure ()
  
  M.modify $ \m -> m
    { viewData = Just viewData
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
  case model.viewData of
    Nothing -> do
      -- Black background
      Canvas.fillStyle (Canvas.ColorArg (RGB 0 0 0))
      Canvas.fillRect (0, 0, screenWidth, screenHeight)
      
      -- Waiting message
      Canvas.fillStyle (Canvas.ColorArg (RGB 255 255 255))
      Canvas.font "24px Arial"
      Canvas.textAlign TextAlignCenter
      Canvas.fillText ("Waiting for other player...", screenWidth / 2, screenHeight / 2)
    
    Just viewData -> do
      -- Draw background
      Canvas.drawImage' (canvasState.backgroundImg, 0, 0, screenWidth, screenHeight)
      
      -- Check if my player is dead
      let myDead = case model.myPlayerId of
            Just pid -> Map.lookup pid viewData.gameOverFlags == Just True
            Nothing -> False
      
      -- Draw obstacles
      forM_ viewData.obstacles $ \obs -> do
        let img = if obs.isHardBlock then canvasState.hardBlockImg else canvasState.softBlockImg
        Canvas.drawImage' (img, obs.x - 25, obs.y - 25, squareSize, squareSize)
      
      -- Draw powerups
      forM_ viewData.powerups $ \pu -> do
        let img = case pu.powerupType of
              Common.FireUp -> canvasState.fireUpImg
              Common.SpeedUp -> canvasState.speedUpImg
              Common.BombUp -> canvasState.bombUpImg
        Canvas.drawImage' (img, pu.x - 25, pu.y - 25, pu.size, pu.size)
      
      -- Draw bombs
      forM_ viewData.bombs $ \bomb -> do
        let img = if bomb.isDetonated == Common.Ticking 
                    then canvasState.bombTickingImg 
                    else canvasState.bombDetonatingImg
        Canvas.drawImage' (img, bomb.x - 25, bomb.y - 25, bomb.size, bomb.size)
      
      -- Draw players
      forM_ (Map.elems viewData.players) $ \player -> do
        Canvas.drawImage' (canvasState.playerImg, player.x - 15, player.y - 15, player.size, player.size)
      
      -- Draw timer
      Canvas.fillStyle (Canvas.ColorArg (RGB 255 255 255))
      Canvas.font "20px Arial"
      Canvas.textAlign Canvas.TextAlignLeft
      let timer = round viewData.gameTimer :: Int
      let (m, s) = divMod timer 60
      let mPadded = if m < 10 then "0" ++ show m else show m
      let sPadded = if s < 10 then "0" ++ show s else show s
      Canvas.fillText ("Time: " ++ mPadded ++ ":" ++ sPadded, 10, 30)
      
      -- Draw player stats (if we know our player ID)
      case model.myPlayerId >>= flip Map.lookup viewData.players of
        Just myPlayer -> do
          Canvas.fillText ("Bombs: " ++ show myPlayer.bombsHeld ++ "/" ++ show myPlayer.maxbombs, 10, 60)
          Canvas.fillText ("Fire Range: " ++ show myPlayer.fireups, 10, 90)
          Canvas.fillText ("Speed: " ++ show (round myPlayer.speedups :: Int), 10, 120)
        Nothing -> pure ()
      
      -- Game over overlay
      when myDead $ do
        Canvas.fillStyle (Canvas.ColorArg (RGBA 0 0 0 0.7))
        Canvas.fillRect (0, 0, screenWidth, screenHeight)
        Canvas.fillStyle (Canvas.ColorArg (RGB 255 0 0))
        Canvas.font "48px Arial"
        Canvas.textAlign TextAlignCenter
        Canvas.fillText ("GAME OVER", screenWidth / 2, screenHeight / 2)