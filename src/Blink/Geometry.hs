{- |
Module: Blink.Geometry

Primitive geometry types and operations used throughout Blink. All
coordinates are in pixels, measured from a top-left origin with Y
increasing downward.

The four core types are 'Point', 'Size', 'Rectangle', and 'Insets'.
'Rectangle' is the central type: most of the library passes bounding
rectangles around to describe where components are drawn. 'Insets'
describes four-sided offsets and is used to derive margin and padding
rectangles from a base 'Rectangle' via 'insetRect'. 'Border' describes a
control's border as a stack of 'BorderLayer's; convert to 'Insets' with
'borderInsets' to apply with 'insetRect'. 'Alignment' describes a 2D
anchor position within a containing rectangle and is used with
'alignRect' to place a child rectangle inside a parent.
-}
module Blink.Geometry
  ( -- * Types
    Point (..)
  , Size (..)
  , Rectangle (..)
  , Orientation (..)
    -- * Colour
  , Colour (..)
  , isVisible
    -- * Insets
  , Insets (..)
  , uniform
  , insetRect
  , inflate
    -- * Border layers
  , CornerRadii (..)
  , EdgeVisibility (..)
  , BorderLayer (..)
  , Border
  , noBorder
  , uniformRadii
  , allEdgesVisible
  , borderInsets
    -- * Rectangle operations
  , rectFromSize
  , resizeRect
  , rectCentredAt
  , containsPoint
  , intersectRect
  , alignRect
    -- * Alignment
  , Alignment (..)
    -- * Popup placement
  , Side (..)
  , Edge (..)
  , placePopup
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

-- | Adds two 'Insets' side by side; @'mempty'@ is zero on every side. Lets a
-- caller combine several inset sources (e.g. margin, border, padding) into
-- one before applying them, rather than inset by each in turn.
instance Semigroup Insets where
  Insets t1 r1 b1 l1 <> Insets t2 r2 b2 l2 = Insets (t1 + t2) (r1 + r2) (b1 + b2) (l1 + l2)

instance Monoid Insets where
  mempty = Insets 0 0 0 0

-- | Grows a 'Size' by 'Insets' on each side -- the inverse of 'insetRect'
-- shrinking a 'Rectangle'. Used to inflate a measured content size back out
-- by the chrome that was subtracted from the space offered to it.
inflate :: Insets -> Size -> Size
inflate ins s = Size
  { sizeWidth  = sizeWidth s + leftInset ins + rightInset ins
  , sizeHeight = sizeHeight s + topInset ins + bottomInset ins
  }

-- | Per-corner radii for a rounded 'BorderLayer'. All four corners are
-- independent so a layer can be rounded on some corners and square on
-- others (e.g. squared off next to a hidden edge).
data CornerRadii = CornerRadii
  { radiusTopLeft :: Double
  , radiusTopRight :: Double
  , radiusBottomRight :: Double
  , radiusBottomLeft :: Double
  } deriving (Eq, Show)

-- | Equal radius on all four corners.
uniformRadii :: Double -> CornerRadii
uniformRadii r = CornerRadii r r r r

-- | Which of the four edges a 'BorderLayer' draws. Lets a layer omit an
-- edge entirely (e.g. a tab control's bottom edge) without needing
-- per-edge width.
data EdgeVisibility = EdgeVisibility
  { edgeTopVisible :: Bool
  , edgeRightVisible :: Bool
  , edgeBottomVisible :: Bool
  , edgeLeftVisible :: Bool
  } deriving (Eq, Show)

-- | Every edge drawn -- the usual case for a plain, unbroken border.
allEdgesVisible :: EdgeVisibility
allEdgesVisible = EdgeVisibility True True True True

-- | One layer of a control's border: a uniform-thickness stroke, its own
-- corner radii, which edges it draws, and how far outward from the
-- control's bounds it sits. See 'Border' for how layers stack.
data BorderLayer = BorderLayer
  { layerColour :: Colour
  , layerWidth :: Double
    -- ^ Uniform thickness in pixels; not per-edge (see "Blink.Geometry"
    -- module header for why).
  , layerOffset :: Double
    -- ^ Distance outward from the control's own bounds where this layer
    -- starts, e.g. a focus ring drawn just outside the base border.
  , layerRadii :: CornerRadii
  , layerVisible :: EdgeVisibility
  } deriving (Eq, Show)

-- | A control's border as a stack of layers, drawn back-to-front in list
-- order (the base border first, decorations like a focus ring after/on
-- top of it).
type Border = [BorderLayer]

-- | No border layers at all.
noBorder :: Border
noBorder = []

-- | The insets a 'Border' stack occupies, for use with 'insetRect'. Based
-- on whichever layer extends furthest outward
-- (@'layerOffset' + 'layerWidth'@), not the sum of all layers -- an outer
-- decorative layer (e.g. a focus ring) does not push the content box in
-- any further than the base border alone already does.
borderInsets :: Border -> Insets
borderInsets [] = mempty
borderInsets layers = uniform (maximum [layerOffset l + layerWidth l | l <- layers])

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

-- | An RGBA colour with components in @[0, 1]@.
data Colour = RGBA Double Double Double Double
  deriving (Eq, Show)

-- | 'True' when the colour has a non-zero alpha component and will
-- contribute visible output when rendered. Used to skip draw calls for
-- fully transparent fills.
isVisible :: Colour -> Bool
isVisible (RGBA _ _ _ a) = a /= 0

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

-- | Which edge of the anchor a popup opens from. Prefixed (@SideTop@, not
-- plain @Top@) to avoid colliding with 'Data.Either.Left'\/'Data.Either.Right'.
data Side = SideTop | SideBottom | SideLeft | SideRight
  deriving (Eq, Show)

-- | How a popup is aligned along the anchor's edge, on the axis
-- perpendicular to 'Side' -- e.g. for 'SideBottom', whether the popup's
-- left edge, centre, or right edge lines up with the anchor's.
data Edge = Start | Middle | End
  deriving (Eq, Show)

-- | Positions a popup of @size@ against @anchor@, per @(side, edge)@ and
-- @offset@ (the gap between the anchor's edge and the popup), then flips to
-- the opposite 'Side' if that placement would overflow @window@ -- e.g.
-- 'SideBottom' becomes 'SideTop' when there isn't room below the anchor.
-- Used by "Blink.Popup" to place a popup's content once its size is known.
placePopup :: Rectangle -> Rectangle -> Size -> (Side, Edge) -> Double -> Rectangle
placePopup anchor window size (side, edge) offset
  | overflowsOnSide window preferred side = placeAt anchor size (flipSide side) edge offset
  | otherwise                              = preferred
  where
    preferred = placeAt anchor size side edge offset

flipSide :: Side -> Side
flipSide SideTop    = SideBottom
flipSide SideBottom = SideTop
flipSide SideLeft   = SideRight
flipSide SideRight  = SideLeft

placeAt :: Rectangle -> Size -> Side -> Edge -> Double -> Rectangle
placeAt anchor (Size w h) side edge offset = case side of
  SideBottom -> Rectangle crossPos              (rectY anchor + rectHeight anchor + offset) w h
  SideTop    -> Rectangle crossPos              (rectY anchor - h - offset)                 w h
  SideRight  -> Rectangle (rectX anchor + rectWidth anchor + offset) crossPos               w h
  SideLeft   -> Rectangle (rectX anchor - w - offset)                crossPos               w h
  where
    crossPos = case side of
      SideBottom -> edgePos (rectX anchor) (rectWidth anchor) w edge
      SideTop    -> edgePos (rectX anchor) (rectWidth anchor) w edge
      SideRight  -> edgePos (rectY anchor) (rectHeight anchor) h edge
      SideLeft   -> edgePos (rectY anchor) (rectHeight anchor) h edge

edgePos :: Double -> Double -> Double -> Edge -> Double
edgePos anchorOrigin anchorLen popupLen edge = case edge of
  Start  -> anchorOrigin
  Middle -> anchorOrigin + (anchorLen - popupLen) / 2
  End    -> anchorOrigin + anchorLen - popupLen

-- | 'True' when @rect@ overflows @window@ on the edge @side@ opens toward
-- -- the only overflow 'placePopup' reacts to; it does not clamp
-- misalignment on the cross axis.
overflowsOnSide :: Rectangle -> Rectangle -> Side -> Bool
overflowsOnSide window rect side = case side of
  SideBottom -> rectY rect + rectHeight rect > rectY window + rectHeight window
  SideTop    -> rectY rect < rectY window
  SideRight  -> rectX rect + rectWidth rect > rectX window + rectWidth window
  SideLeft   -> rectX rect < rectX window
