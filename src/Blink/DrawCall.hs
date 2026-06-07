module Blink.DrawCall
  ( Colour (..)
  , DrawCall (..)
  ) where

import Data.Text (Text)
import Blink.Geometry (Rectangle)

data Colour = RGBA Double Double Double Double
  deriving (Eq, Show)

data DrawCall
  = FillRect Rectangle Colour
  | DrawText Rectangle Text Colour
  deriving (Eq, Show)
