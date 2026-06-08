module Blink.Geometry
  ( Point (..)
  , Size (..)
  , Rectangle (..)
  , Alignment (..)
  , Insets (..)
  , uniform
  , insetRect
  , rectOrigin
  , resizeRect
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

data Align1D = AlignStart | AlignCenter | AlignEnd

alignRect :: Alignment -> Rectangle -> Rectangle -> Rectangle
alignRect alignment container rect =
  moveRect (Point x y) rect
  where
    (hPos, vPos) = split alignment
    x = align1D hPos (rectX container) (rectWidth container) (rectWidth rect)
    y = align1D vPos (rectY container) (rectHeight container) (rectHeight rect)

split :: Alignment -> (Align1D, Align1D)
split TopLeft      = (AlignStart, AlignStart)
split TopCenter    = (AlignCenter, AlignStart)
split TopRight     = (AlignEnd, AlignStart)
split MiddleLeft   = (AlignStart, AlignCenter)
split Center       = (AlignCenter, AlignCenter)
split MiddleRight  = (AlignEnd, AlignCenter)
split BottomLeft   = (AlignStart, AlignEnd)
split BottomCenter = (AlignCenter, AlignEnd)
split BottomRight  = (AlignEnd, AlignEnd)

align1D :: Align1D -> Double -> Double -> Double -> Double
align1D AlignStart  origin _            _       = origin
align1D AlignCenter origin containerLen itemLen = origin + (containerLen - itemLen) / 2
align1D AlignEnd    origin containerLen itemLen = origin + containerLen - itemLen

containsPoint :: Rectangle -> Point -> Bool
containsPoint r p =
  pointX p >= rectX r && pointX p <= rectX r + rectWidth r &&
  pointY p >= rectY r && pointY p <= rectY r + rectHeight r

rectOrigin :: Rectangle
rectOrigin = Rectangle 0 0 0 0

resizeRect :: Size -> Rectangle -> Rectangle
resizeRect s r = r
  { rectWidth = sizeWidth s
  , rectHeight = sizeHeight s
  }

moveRect :: Point -> Rectangle -> Rectangle
moveRect p r = r
  { rectX = pointX p
  , rectY = pointY p
  }

rectCentredAt :: Point -> Rectangle -> Rectangle
rectCentredAt p r = r
  { rectX = pointX p - rectWidth r / 2
  , rectY = pointY p - rectHeight r / 2
  }
