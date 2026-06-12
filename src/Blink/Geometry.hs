{- |
Primitive geometry types and operations used throughout Blink. All
coordinates are in pixels, measured from a top-left origin with Y
increasing downward.

The four core types are 'Point', 'Size', 'Rectangle', and 'Insets'.
'Rectangle' is the central type: most of the library passes bounding
rectangles around to describe where components are drawn. 'Insets'
describes four-sided offsets and is used to derive margin and padding
rectangles from a base 'Rectangle' via 'insetRect'. 'Alignment'
describes a 2D anchor position within a containing rectangle and is
used with 'alignRect' to place a child rectangle inside a parent.
-}
module Blink.Geometry
  ( -- * Types
    Point (..)
  , Size (..)
  , Rectangle (..)
  , Orientation (..)
    -- * Insets
  , Insets (..)
  , uniform
  , insetRect
    -- * Rectangle operations
  , rectFromSize
  , resizeRect
  , rectCentredAt
  , containsPoint
  , intersectRect
  , alignRect
    -- * Alignment
  , Alignment (..)
  ) where

-- | A point in 2D screen space (pixels, top-left origin, Y increases downward).
data Point = Point
  { pointX :: Double
  , pointY :: Double
  } deriving (Eq, Show)

-- | The dimensions of a 2D region in pixels.
data Size = Size
  { sizeWidth :: Double
  , sizeHeight :: Double
  } deriving (Eq, Show)

-- | An axis-aligned rectangle in screen coordinates.
data Rectangle = Rectangle
  { rectX :: Double      -- ^ Left edge.
  , rectY :: Double      -- ^ Top edge.
  , rectWidth :: Double
  , rectHeight :: Double
  } deriving (Eq, Show)

-- | Four-sided inset distances in pixels. Apply with 'insetRect';
-- construct uniform insets with 'uniform'.
data Insets = Insets
  { topInset :: Double    -- ^ Inset from the top edge.
  , rightInset :: Double  -- ^ Inset from the right edge.
  , bottomInset :: Double -- ^ Inset from the bottom edge.
  , leftInset :: Double   -- ^ Inset from the left edge.
  } deriving (Eq, Show)

-- | Creates 'Insets' with the same value on all four sides.
uniform :: Double -> Insets
uniform n = Insets { topInset = n, rightInset = n, bottomInset = n, leftInset = n }

-- | Shrinks @r@ by @ins@ on each edge. Width and height are clamped to
-- zero if the insets exceed the rectangle's dimensions.
insetRect :: Insets -> Rectangle -> Rectangle
insetRect ins r = Rectangle
  { rectX = rectX r + leftInset ins
  , rectY = rectY r + topInset ins
  , rectWidth = max 0 (rectWidth r - leftInset ins - rightInset ins)
  , rectHeight = max 0 (rectHeight r - topInset ins - bottomInset ins)
  }

-- | A 2D anchor position within a containing rectangle. Passed to
-- 'alignRect' to control where a child rectangle sits inside its parent.
data Alignment
  = TopLeft    | TopCenter    | TopRight
  | MiddleLeft | Center       | MiddleRight
  | BottomLeft | BottomCenter | BottomRight
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | The axis along which a component is laid out or oriented.
data Orientation = Horizontal | Vertical
  deriving (Eq, Ord, Show)

data Align1D = AlignStart | AlignCenter | AlignEnd

-- | Positions @rect@ within @container@ according to @alignment@.
-- Returns @rect@ moved so that the named anchor point aligns with
-- the corresponding position in @container@; dimensions are unchanged.
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

-- | The axis-aligned intersection of two rectangles. Returns a zero-area
-- rectangle when the inputs do not overlap.
intersectRect :: Rectangle -> Rectangle -> Rectangle
intersectRect a b =
  let x1 = max (rectX a) (rectX b)
      y1 = max (rectY a) (rectY b)
      x2 = min (rectX a + rectWidth a)  (rectX b + rectWidth b)
      y2 = min (rectY a + rectHeight a) (rectY b + rectHeight b)
  in Rectangle x1 y1 (max 0 (x2 - x1)) (max 0 (y2 - y1))

-- | 'True' when @p@ falls within (or on the boundary of) @r@.
containsPoint :: Point -> Rectangle -> Bool
containsPoint p r =
  pointX p >= rectX r && pointX p <= rectX r + rectWidth r &&
  pointY p >= rectY r && pointY p <= rectY r + rectHeight r

-- | Creates a rectangle at the origin @(0, 0)@ with the given dimensions.
rectFromSize :: Size -> Rectangle
rectFromSize s = Rectangle 0 0 (sizeWidth s) (sizeHeight s)

-- | Replaces the width and height of @r@ with those of @s@,
-- preserving the rectangle's origin.
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

-- | Moves @r@ so that its centre coincides with @p@, preserving its dimensions.
rectCentredAt :: Point -> Rectangle -> Rectangle
rectCentredAt p r =
  moveRect
    (Point (pointX p - rectWidth r / 2)
           (pointY p - rectHeight r / 2)
    ) r
