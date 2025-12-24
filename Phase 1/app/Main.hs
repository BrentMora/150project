{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
 
module Main where

-- Generic not needed because it's only for JSON conversion

import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Data.Map.Strict as Map -- Map == Dictionary
 
-- === Main ===

main :: IO ()
main = M.run $ M.startApp app
  where
    app = M.component initModel update view

-- === Models ===

-- | Represents a 2D position in the game world
data Position = Position
  { 
    posX :: Int  -- X coordinate (horizontal)
  , posY :: Int  -- Y coordinate (vertical)
  }
  deriving (Show, Eq)

-- | Four cardinal directions for player movement
data Direction = North | South | East | West
  deriving (Show, Eq)

-- | Represents a player in the game
data Player = Player
  { 
    playerId :: String      -- Unique identifier for the player
  , playerPos :: Position   -- Current position in the game world
  , playerColor :: String   -- Display color (hex code like "#FF6B6B")
  } 
  deriving (Show, Eq)

-- | The complete game state containing all players
data GameState = GameState
  { 
    players :: Map.Map String Player  -- Map from player ID to Player data
  }
  deriving (Show, Eq, Generic)



data Model = ...
  deriving (Show, Eq)

-- === Msgs ===

data Msg = ...
  deriving (Show, Eq)
 
initModel :: Model
initModel = undefined
 
update :: Msg -> M.Transition Model Msg
update msg = undefined
 
view :: Model -> M.View Model Msg
view model = undefined