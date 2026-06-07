module Blink.DrawCall
  ( Colour (..)
  , DrawCall (..)
  ) where

import Blink.Geometry (Rectangle)

data Colour = RGBA Double Double Double Double
  deriving (Eq, Show)

data DrawCall
  = FillRect Rectangle Colour
  deriving (Eq, Show)
