{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map.Strict as Map

-- Player position
data Position = Position
  { posX :: Int
  , posY :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Position
instance FromJSON Position

-- Direction for movement
data Direction = North | South | East | West
  deriving (Show, Eq, Generic)

instance ToJSON Direction
instance FromJSON Direction

-- Player data
data Player = Player
  { playerId :: Text
  , playerPos :: Position
  , playerColor :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Player
instance FromJSON Player

-- Game state
data GameState = GameState
  { players :: Map.Map Text Player
  } deriving (Show, Eq, Generic)

instance ToJSON GameState
instance FromJSON GameState

-- Client -> Server messages
data ClientMessage
  = Join Text  -- Join with player name
  | Move Direction
  | Disconnect
  deriving (Show, Eq, Generic)

instance ToJSON ClientMessage
instance FromJSON ClientMessage

-- Server -> Client messages
data ServerMessage
  = Welcome Text GameState  -- Welcome with your player ID and initial state
  | StateUpdate GameState
  | PlayerJoined Player
  | PlayerLeft Text
  | Error Text
  deriving (Show, Eq, Generic)

instance ToJSON ServerMessage
instance FromJSON ServerMessage