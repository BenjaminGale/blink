module Blink.Rendering
  ( Colour (..)
  , isOpaque
  , TextAlign (..)
  , DrawCommand (..)
  ) where

import Data.Text (Text)
import Blink.Geometry (Rectangle)

data Colour = RGBA Double Double Double Double
  deriving (Eq, Show)

isOpaque :: Colour -> Bool
isOpaque (RGBA _ _ _ a) = a /= 0

data TextAlign = AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Show)

data DrawCommand
  = FillRect   Rectangle Colour
  | StrokeRect Rectangle Colour Double
  | DrawText   Rectangle Text Colour TextAlign
  | PushClip   Rectangle
  | PopClip
  deriving (Eq, Show)
