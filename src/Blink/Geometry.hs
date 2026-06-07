module Blink.Geometry
  ( Point (..)
  , Size (..)
  , Rectangle (..)
  , rectCentredAt
  ) where

data Point = Point
  { pointX :: Double
  , pointY :: Double
  } deriving (Eq, Show)

data Size = Size
  { sizeWidth :: Double
  , sizeHeight :: Double
  } deriving (Eq, Show)

data Rectangle = Rectangle
  { rectX :: Double
  , rectY :: Double
  , rectWidth :: Double
  , rectHeight :: Double
  } deriving (Eq, Show)

rectCentredAt :: Point -> Size -> Rectangle
rectCentredAt p s = Rectangle
  { rectX = pointX p - sizeWidth s / 2
  , rectY = pointY p - sizeHeight s / 2
  , rectWidth = sizeWidth s
  , rectHeight = sizeHeight s
  }
