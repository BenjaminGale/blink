module Blink.DrawCall
  ( Colour (..)
  , TextAlign (..)
  , DrawCall (..)
  ) where

import Data.Text (Text)
import Blink.Geometry (Rectangle)

data Colour = RGBA Double Double Double Double
  deriving (Eq, Show)

data TextAlign = AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Show)

data DrawCall
  = FillRect Rectangle Colour
  | DrawText Rectangle Text Colour TextAlign
  | PushClip Rectangle
  | PopClip
  deriving (Eq, Show)
