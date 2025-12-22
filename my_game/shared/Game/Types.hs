{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map.Strict as Map

-- | Represents a 2D position in the game world
data Position = Position
  { posX :: Int  -- X coordinate (horizontal)
  , posY :: Int  -- Y coordinate (vertical)
  } deriving (Show, Eq, Generic)

-- Automatically generate JSON encoding/decoding for Position
instance ToJSON Position
instance FromJSON Position

-- | Four cardinal directions for player movement
data Direction = North | South | East | West
  deriving (Show, Eq, Generic)

-- Automatically generate JSON encoding/decoding for Direction
instance ToJSON Direction
instance FromJSON Direction

-- | Represents a player in the game
data Player = Player
  { playerId :: Text      -- Unique identifier for the player
  , playerPos :: Position -- Current position in the game world
  , playerColor :: Text   -- Display color (hex code like "#FF6B6B")
  } deriving (Show, Eq, Generic)

-- Automatically generate JSON encoding/decoding for Player
instance ToJSON Player
instance FromJSON Player

-- | The complete game state containing all players
data GameState = GameState
  { players :: Map.Map Text Player  -- Map from player ID to Player data
  } deriving (Show, Eq, Generic)

-- Automatically generate JSON encoding/decoding for GameState
instance ToJSON GameState
instance FromJSON GameState

-- | Messages sent from client to server
data ClientMessage
  = Join Text       -- Join the game with a player name
  | Move Direction  -- Move in a specific direction
  | Disconnect      -- Leave the game
  deriving (Show, Eq, Generic)

-- Automatically generate JSON encoding/decoding for ClientMessage
instance ToJSON ClientMessage
instance FromJSON ClientMessage

-- | Messages sent from server to client
data ServerMessage
  = Welcome Text GameState  -- Welcome message with assigned player ID and current game state
  | StateUpdate GameState   -- Updated game state (sent after any change)
  | PlayerJoined Player     -- Notification that a new player joined
  | PlayerLeft Text         -- Notification that a player left (includes their ID)
  | Error Text              -- Error message (e.g., invalid action)
  deriving (Show, Eq, Generic)

-- Automatically generate JSON encoding/decoding for ServerMessage
instance ToJSON ServerMessage
instance FromJSON ServerMessage