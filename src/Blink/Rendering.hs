module Blink.Rendering
  ( Colour (..)
  , TextAlign (..)
  , DrawCommand (..)
  ) where

import Data.Text (Text)
import Blink.Geometry (Rectangle)

data Colour = RGBA Double Double Double Double
  deriving (Eq, Show)

data TextAlign = AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Show)

data DrawCommand
  = FillRect Rectangle Colour
  | DrawText Rectangle Text Colour TextAlign
  | PushClip Rectangle
  | PopClip
  deriving (Eq, Show)
