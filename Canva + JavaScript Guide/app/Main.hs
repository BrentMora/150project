{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Miso
import Miso.String (MisoString, ms)

-- | Model
data Model = Model
  { counter :: Int
  } deriving (Eq, Show)

-- | Actions
data Action
  = Increment
  | NoOp

-- | Entry point
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model  = Model 0
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing

-- | Update function
updateModel :: Action -> Model -> Effect Action Model
updateModel Increment m = noEff m { counter = counter m + 1 }
updateModel NoOp m      = noEff m

-- | View function
viewModel :: Model -> View Action
viewModel m =
  div_ []
    [ h1_ [] [ text "Simple Canvas Example" ]
    , canvas_ [ width_ 300, height_ 200, style_ "border:1px solid black;" ] []
    , button_ [ onClick Increment ] [ text $ ms ("Clicked: " ++ show (counter m)) ]
    ]
