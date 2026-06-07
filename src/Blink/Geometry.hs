module Blink.Geometry
  ( Point (..)
  , Size (..)
  , Rectangle (..)
  , Alignment (..)
  , Insets (..)
  , uniform
  , insetRect
  , rectCentredAt
  , containsPoint
  , alignRect
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

data Insets = Insets
  { topInset :: Double
  , rightInset :: Double
  , bottomInset :: Double
  , leftInset :: Double
  } deriving (Eq, Show)

uniform :: Double -> Insets
uniform n = Insets { topInset = n, rightInset = n, bottomInset = n, leftInset = n }

insetRect :: Insets -> Rectangle -> Rectangle
insetRect ins r = Rectangle
  { rectX = rectX r + leftInset ins
  , rectY = rectY r + topInset ins
  , rectWidth = max 0 (rectWidth r - leftInset ins - rightInset ins)
  , rectHeight = max 0 (rectHeight r - topInset ins - bottomInset ins)
  }

data Alignment
  = TopLeft    | TopCenter    | TopRight
  | MiddleLeft | Center       | MiddleRight
  | BottomLeft | BottomCenter | BottomRight
  deriving (Eq, Ord, Show, Bounded, Enum)

alignRect :: Alignment -> Rectangle -> Size -> Rectangle
alignRect alignment container size = Rectangle
  { rectX = x
  , rectY = y
  , rectWidth = sizeWidth size
  , rectHeight = sizeHeight size
  }
  where
    x = case alignment of
      TopLeft    -> rectX container
      TopCenter  -> rectX container + (rectWidth container - sizeWidth size) / 2
      TopRight   -> rectX container + rectWidth container - sizeWidth size
      MiddleLeft -> rectX container
      Center     -> rectX container + (rectWidth container - sizeWidth size) / 2
      MiddleRight -> rectX container + rectWidth container - sizeWidth size
      BottomLeft  -> rectX container
      BottomCenter -> rectX container + (rectWidth container - sizeWidth size) / 2
      BottomRight  -> rectX container + rectWidth container - sizeWidth size
    y = case alignment of
      TopLeft    -> rectY container
      TopCenter  -> rectY container
      TopRight   -> rectY container
      MiddleLeft -> rectY container + (rectHeight container - sizeHeight size) / 2
      Center     -> rectY container + (rectHeight container - sizeHeight size) / 2
      MiddleRight -> rectY container + (rectHeight container - sizeHeight size) / 2
      BottomLeft  -> rectY container + rectHeight container - sizeHeight size
      BottomCenter -> rectY container + rectHeight container - sizeHeight size
      BottomRight  -> rectY container + rectHeight container - sizeHeight size

containsPoint :: Rectangle -> Point -> Bool
containsPoint r p =
  pointX p >= rectX r && pointX p <= rectX r + rectWidth r &&
  pointY p >= rectY r && pointY p <= rectY r + rectHeight r

rectCentredAt :: Point -> Size -> Rectangle
rectCentredAt p s = Rectangle
  { rectX = pointX p - sizeWidth s / 2
  , rectY = pointY p - sizeHeight s / 2
  , rectWidth = sizeWidth s
  , rectHeight = sizeHeight s
  }
