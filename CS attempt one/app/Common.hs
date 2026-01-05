{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Common where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Map (Map)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- Type alias for player identification
type PlayerId = Int

-- PowerUps
data PowerUpTypes = FireUp | BombUp | SpeedUp
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Direction enumeration
data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  | DirNone
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Request state for bomb placement
data Request = Valid | Blocked | Released
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Detonation status for bombs
data DetonationStatus = Ticking | Detonating | Done
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Game over flag
data GameOverFlag = NotGO | YesGO | Render
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- PowerUp Structure
data PowerUp = PowerUp
  { puX :: Double
  , puY :: Double
  , puWidth :: Double
  , puHeight :: Double
  , puType :: PowerUpTypes
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Player data structure
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
  , xCoords :: [Double]
  , yCoords :: [Double]
  , currentDirection :: Maybe Direction
  , maxBombs :: Int         -- refactor bomb increment to make use of this
  , speedUps :: Double         -- refactor so speed makes use of this
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Bomb data structure
data Bomb = Bomb
  { bombX :: Double
  , bombY :: Double
  , bombVelX :: Double
  , bombVelY :: Double
  , bombSize :: Double
  , isDetonated :: DetonationStatus
  , timer :: Double
  , isOverlapping :: Bool
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Obstacle data structure
data Obstacle = Obstacle
  { obstacleX :: Double
  , obstacleY :: Double
  , obstacleWidth :: Double
  , obstacleHeight :: Double
  , isHardBlock :: Bool
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- View data sent from server to client
data ViewData = ViewData
  { players :: Map PlayerId Player
  , bombs :: [Bomb]
  , obstacles :: [Obstacle]
  , serverTicks :: Int
  , gameOver :: GameOverFlag
  , gameTimer :: Float
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Convert ViewData to Text
viewDataToText :: ViewData -> Text
viewDataToText viewData = TE.decodeUtf8 . BL.toStrict . encode $ viewData

-- Parse Text to ViewData
textToViewData :: Text -> Maybe ViewData
textToViewData raw = decode . BL.fromStrict . TE.encodeUtf8 $ raw

-- Keyboard state
data KeyState = KeyState
  { up :: Bool
  , down :: Bool
  , left :: Bool
  , right :: Bool
  , placeBomb :: Bool
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Client input sent from client to server
data ClientInput = ClientInput
  { keyState :: KeyState
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Convert ClientInput to Text
clientInputToText :: ClientInput -> Text
clientInputToText clientInput = TE.decodeUtf8 . BL.toStrict . encode $ clientInput

-- Parse Text to ClientInput
textToClientInput :: Text -> Maybe ClientInput
textToClientInput raw = decode . BL.fromStrict . TE.encodeUtf8 $ raw

-- Default keyboard state
defaultKeyState :: KeyState
defaultKeyState = KeyState False False False False False