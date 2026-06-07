module Blink.Input
  ( ButtonState (..)
  , InputState (..)
  ) where

import Blink.Geometry (Point)

data ButtonState
  = ButtonUp
  | ButtonDown
  | ButtonReleased
  deriving (Eq, Show)

data InputState = InputState
  { mousePosition :: Point
  , leftButton :: ButtonState
  } deriving (Eq, Show)
