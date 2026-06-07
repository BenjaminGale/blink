module Blink.Input
  ( InputState (..)
  ) where

import Blink.Geometry (Point)

data InputState = InputState
  { mousePosition :: Point
  } deriving (Eq, Show)
