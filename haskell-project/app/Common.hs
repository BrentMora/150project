{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Common where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Map (Map)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

type PlayerId = Int

data PowerUpType = FireUp | SpeedUp | BombUp
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data DetonationStatus = Ticking | Detonating | Done
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data BombDirection = North | South | East | West | Core
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data Direction = DirUp | DirDown | DirLeft | DirRight | DirNone
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data Request = Valid | Blocked | Released
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data Player = Player
  { playerId :: PlayerId
  , playerX :: Double
  , playerY :: Double
  , playerVelX :: Double
  , playerVelY :: Double
  , playerSize :: Double
  , bombsHeld :: Int
  , targetX :: Double
  , targetY :: Double
  , spaceRequest :: Request
  , currentDirection :: Maybe Direction
  , maxbombs :: Int
  , speedups :: Double
  , fireups :: Int
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data PowerUp = PowerUp
  { powerupX :: Double
  , powerupY :: Double
  , powerupSize :: Double
  , powerupType :: PowerUpType
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data Bomb = Bomb
  { bombX :: Double
  , bombY :: Double
  , bombVelX :: Double
  , bombVelY :: Double
  , bombSize :: Double
  , isDetonated :: DetonationStatus
  , timer :: Double
  , isOverlapping :: Bool
  , bombDirection :: BombDirection
  , growth :: Int
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data Obstacle = Obstacle
  { obstacleX :: Double
  , obstacleY :: Double
  , obstacleWidth :: Double
  , obstacleHeight :: Double
  , isHardBlock :: Bool
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data ViewData = ViewData
  { players :: Map PlayerId Player
  , bombs :: [Bomb]
  , obstacles :: [Obstacle]
  , powerups :: [PowerUp]
  , gameTimer :: Float
  , serverTicks :: Int
  , gameOverFlags :: Map PlayerId Bool
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

viewDataToText :: ViewData -> Text
viewDataToText viewData = TE.decodeUtf8 . BL.toStrict . encode $ viewData

textToViewData :: Text -> Maybe ViewData
textToViewData raw = decode . BL.fromStrict . TE.encodeUtf8 $ raw

data KeyState = KeyState
  { up :: Bool
  , down :: Bool
  , left :: Bool
  , right :: Bool
  , space :: Bool
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

data ClientInput = ClientInput
  { keyState :: KeyState
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

clientInputToText :: ClientInput -> Text
clientInputToText clientInput = TE.decodeUtf8 . BL.toStrict . encode $ clientInput

textToClientInput :: Text -> Maybe ClientInput
textToClientInput raw = decode . BL.fromStrict . TE.encodeUtf8 $ raw

defaultKeyState :: KeyState
defaultKeyState = KeyState False False False False False

-- Grid coordinates for bomb placement
xCoords :: [Double]
xCoords = [25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 625, 675, 725]

yCoords :: [Double]
yCoords = [25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 625]