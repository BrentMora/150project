-- new common

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Common where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Map (Map)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- Type alias for player identification
-- Each player gets a unique integer ID (1, 2, etc.)
type PlayerId = Int

-- Direction enumeration - represents which way a player is moving
data Direction
  = DirUp      -- Moving upward
  | DirDown    -- Moving downward
  | DirLeft    -- Moving left
  | DirRight   -- Moving right
  | DirNone    -- Not moving
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Request state for bomb placement - tracks spacebar press state
data Request = Valid      -- Spacebar just pressed, can place bomb
             | Blocked    -- Just placed bomb, block until release
             | Released   -- Spacebar not pressed
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Detonation status for bombs - tracks bomb lifecycle
data DetonationStatus = Ticking     -- Bomb is counting down (red, solid)
                      | Detonating  -- Bomb is exploding (bright red, spreads)
                      | Done        -- Explosion finished, remove bomb
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Game over flag - tracks game state
data GameOverFlag = NotGO   -- Game is running normally
                  | YesGO   -- Game over, show end screen
                  | Render  -- Render one more frame before showing game over
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- PowerUp types - three types of collectible powerups
data PowerUpTypes = FireUp    -- Increases bomb blast radius (orange)
                  | BombUp    -- Increases max bomb capacity (green)
                  | SpeedUp   -- Increases movement speed (blue)
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- PowerUp Structure - represents a collectible powerup on the map
data PowerUp = PowerUp
  { puX :: Double          -- X position (center)
  , puY :: Double          -- Y position (center)
  , puWidth :: Double      -- Width (50 pixels)
  , puHeight :: Double     -- Height (50 pixels)
  , puType :: PowerUpTypes -- Which type of powerup this is
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Player data structure - represents one player in the game
data Player = Player
  { playerId :: PlayerId              -- Unique player ID (1 or 2)
  , playerX :: Double                 -- X position on canvas
  , playerY :: Double                 -- Y position on canvas
  , playerVelX :: Double              -- X velocity (currently unused)
  , playerVelY :: Double              -- Y velocity (currently unused)
  , playerSize :: Double              -- Size of player square (30 pixels)
  , bombsHeld :: Int                  -- Current bombs available to place
  , targetX :: Double                 -- Target grid X for bomb placement
  , targetY :: Double                 -- Target grid Y for bomb placement
  , spaceRequest :: Request           -- Spacebar press state
  , xCoords :: [Double]               -- Valid X grid coordinates for snapping
  , yCoords :: [Double]               -- Valid Y grid coordinates for snapping
  , currentDirection :: Maybe Direction -- Current movement direction
  , maxBombs :: Int                   -- Maximum active bombs allowed
  , speedUps :: Double                -- Number of speed powerups collected
  , fireUps :: Int                    -- Number of fire powerups collected
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Bomb data structure - represents a bomb on the map
data Bomb = Bomb
  { bombX :: Double              -- X position (center)
  , bombY :: Double              -- Y position (center)
  , bombVelX :: Double           -- X velocity (unused, always 0)
  , bombVelY :: Double           -- Y velocity (unused, always 0)
  , bombSize :: Double           -- Size (50x50 pixels)
  , isDetonated :: DetonationStatus -- Current detonation state
  , timer :: Double              -- Countdown timer in seconds
  , isOverlapping :: Bool        -- True if player is standing on bomb
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Obstacle data structure - represents walls and blocks
data Obstacle = Obstacle
  { obstacleX :: Double      -- X position (center)
  , obstacleY :: Double      -- Y position (center)
  , obstacleWidth :: Double  -- Width (50 pixels)
  , obstacleHeight :: Double -- Height (50 pixels)
  , isHardBlock :: Bool      -- True = indestructible (gray), False = destructible (light gray)
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- View data sent from server to client - complete game state for rendering
data ViewData = ViewData
  { players :: Map PlayerId Player -- All players currently in game
  , bombs :: [Bomb]                -- All bombs on the map
  , obstacles :: [Obstacle]        -- All obstacles (walls and blocks)
  , powerUps :: [PowerUp]          -- All powerups on the map
  , serverTicks :: Int             -- Server frame counter (for animation)
  , gameOver :: GameOverFlag       -- Current game over state
  , gameTimer :: Float             -- Time remaining in seconds
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Convert ViewData to Text for sending over WebSocket
-- Encodes to JSON, then converts ByteString to Text
viewDataToText :: ViewData -> Text
viewDataToText viewData = TE.decodeUtf8 . BL.toStrict . encode $ viewData

-- Parse Text received from WebSocket into ViewData
-- Converts Text to ByteString, then decodes JSON
-- Returns Nothing if parsing fails
textToViewData :: Text -> Maybe ViewData
textToViewData raw = decode . BL.fromStrict . TE.encodeUtf8 $ raw

-- Keyboard state - represents which keys are currently pressed
data KeyState = KeyState
  { up :: Bool     -- Up arrow key
  , down :: Bool   -- Down arrow key
  , left :: Bool   -- Left arrow key
  , right :: Bool  -- Right arrow key
  , placeBomb :: Bool   -- Spacebar (bomb placement)
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Client input sent from client to server - keyboard state only
data ClientInput = ClientInput
  { keyState :: KeyState  -- Current keyboard state
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Convert ClientInput to Text for sending over WebSocket
-- Encodes to JSON, then converts ByteString to Text
clientInputToText :: ClientInput -> Text
clientInputToText clientInput = TE.decodeUtf8 . BL.toStrict . encode $ clientInput

-- Parse Text received from WebSocket into ClientInput
-- Converts Text to ByteString, then decodes JSON
-- Returns Nothing if parsing fails
textToClientInput :: Text -> Maybe ClientInput
textToClientInput raw = decode . BL.fromStrict . TE.encodeUtf8 $ raw

-- Default keyboard state with all keys unpressed
defaultKeyState :: KeyState
defaultKeyState = KeyState False False False False False