{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Common where
import Data.Aeson (FromJSON, decode, encode)
import Data.Map (Map)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- Type alias for player identification
type PlayerId = Int

-- Player data structure representing a player in the game
data Player = Player
  { id :: PlayerId              -- Unique identifier for the player
  , x :: Double                 -- X coordinate position on screen
  , y :: Double                 -- Y coordinate position on screen
  , playerKeyState :: KeyState  -- Current keyboard input state for this player
  , changedDirection :: Bool    -- Flag indicating if player changed direction this frame (for sound effects)
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)  -- Auto-derive JSON serialization and common type classes

-- View data sent from server to client containing the full game state
data ViewData = ViewData
  { players :: Map PlayerId Player  -- Map of all players currently in the game
  , serverTicks :: Int              -- Server frame counter (used for animation timing)
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)  -- Auto-derive JSON serialization and common type classes

-- Convert ViewData to Text for sending over WebSocket
-- Encodes to JSON, then converts ByteString to Text
viewDataToText :: ViewData -> Text
viewDataToText viewData = TE.decodeUtf8 . BL.toStrict . encode $ viewData

-- Parse Text received from WebSocket into ViewData
-- Converts Text to ByteString, then decodes JSON (returns Nothing if parsing fails)
textToViewData :: Text -> Maybe ViewData
textToViewData raw = decode . BL.fromStrict . TE.encodeUtf8 $ raw

-- Keyboard state representing which keys are currently pressed
data KeyState = KeyState
  { up :: Bool     -- Up arrow key
  , down :: Bool   -- Down arrow key
  , left :: Bool   -- Left arrow key
  , right :: Bool  -- Right arrow key
  , fire :: Bool   -- Spacebar (fire/action key)
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)  -- Auto-derive JSON serialization and common type classes

-- Client input sent from client to server containing keyboard state
data ClientInput = ClientInput
  { keyState :: KeyState  -- Current keyboard state to send to server
  }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)  -- Auto-derive JSON serialization and common type classes

-- Convert ClientInput to Text for sending over WebSocket
-- Encodes to JSON, then converts ByteString to Text
clientInputToText :: ClientInput -> Text
clientInputToText clientInput = TE.decodeUtf8 . BL.toStrict . encode $ clientInput

-- Parse Text received from WebSocket into ClientInput
-- Converts Text to ByteString, then decodes JSON (returns Nothing if parsing fails)
textToClientInput :: Text -> Maybe ClientInput
textToClientInput raw = decode . BL.fromStrict . TE.encodeUtf8 $ raw

-- Initial keyboard state with all keys unpressed
defaultKeyState :: KeyState
defaultKeyState = KeyState False False False False False