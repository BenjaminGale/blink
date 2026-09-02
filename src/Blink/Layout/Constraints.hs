-- | The foundation "Blink.Layout.Box" and "Blink.Layout.Border" are both
-- built on: 'Length'\/'Layout' describe a single child's size and
-- position, and 'layoutWithConstraints' applies one to a single action.
module Blink.Layout.Constraints
  ( Length
  , exactly
  , fill
  , atLeast
  , atMost
  , between
  , fitContent
  , Layout (..)
  , layoutWithConstraints
  , preferredSize
  , resolveLength
  , minLength
  , naturalLength
  , canExpand
  , capLength
  , Available (..)
  , MeasureCtx (..)
  , shrink
    -- * Layout attributes
  , HasLayoutConfig (..)
  , width
  , height
  , align
  ) where

import Blink.Attribute (Attribute (..))
import Blink.Geometry
  (Alignment (..), Orientation (..), Rectangle (..), Size (..), alignRect)
import Blink.UI (UI, getBounds, withBounds)

-- | Describes how a child should be sized along a single axis. Built via
--   'exactly', 'fill', 'atLeast', 'atMost', 'between', or 'fitContent' --
--   see 'preferredSize' for worked examples of how each resolves against
--   different amounts of available space.
data Length
  = Exactly Double
  | Fill
  | AtLeast Double
  | AtMost Double
  | Between Double Double
  | FitContent
  deriving (Eq, Show)

-- | A fixed size. The available space is ignored.
exactly :: Double -> Length
exactly = Exactly

-- | Expands to fill all available space.
fill :: Length
fill = Fill

-- | Expands to fill available space, but never smaller than the given minimum.
atLeast :: Double -> Length
atLeast = AtLeast

-- | Expands to fill available space, but never larger than the given
-- maximum. Has no minimum -- can shrink to zero.
atMost :: Double -> Length
atMost = AtMost

-- | Expands to fill available space clamped between the given minimum and
-- maximum. Orders its two arguments itself, so the smaller is always the
-- floor and the larger always the ceiling regardless of the order passed in.
between :: Double -> Double -> Length
between lo hi = Between (min lo hi) (max lo hi)

-- | Exactly the widget's own preferred size, determined by measuring it.
-- Unlike the other constructors, this cannot be resolved from available
-- space alone -- see 'resolveLength'. A 'fitContent' reaching
-- 'preferredSize' unresolved is a bug in the caller: every path that can
-- encounter it (element measurement, box slot sizing) must resolve it to an
-- 'exactly' first.
fitContent :: Length
fitContent = FitContent

-- | Per-child sizing and alignment within a layout panel slot.
data Layout = Layout
  { layoutWidth :: Length
    -- ^ Constraint applied to the child's width.
  , layoutHeight :: Length
    -- ^ Constraint applied to the child's height.
  , layoutAlignment :: Alignment
    -- ^ How the child is positioned within its slot when it does not fill the
    --   slot on one or both axes.
  } deriving (Show)

-- | Sizes and positions a component within its parent bounds according to a
--   'Layout'. Since components are greedy by default, this is the escape
--   hatch for sizing and aligning one that shouldn't fill its parent.
--   'layoutWidth' and 'layoutHeight' control how much of the parent space the
--   component takes up on each axis. 'layoutAlignment' controls where it
--   sits within that space. If the resulting size is larger than the parent
--   bounds, this function does not clip it: the component draws in full,
--   spilling past the parent's edges and potentially over any siblings,
--   unless something further up the tree (such as 'Blink.Layout.Box.hBox' or
--   'Blink.Layout.Box.vBox') clips it.
--
-- @
-- layoutWithConstraints (Layout (exactly 120) (exactly 32) Center) $
--   button MyBtn [text "Click me"]
-- @
--
-- This renders the button at 120×32 pixels, centred in whatever space the
-- parent provides, regardless of how large that space is.
--
-- >  +----------------------------------------+
-- >  |                                        |
-- >  |            +------------+              |
-- >  |            |   MyBtn    |  120 x 32    |
-- >  |            +------------+              |
-- >  |                                        |
-- >  +----------------------------------------+
-- >                   parent bounds
--
-- A few more variations help build intuition.
--
-- @
-- layoutWithConstraints (Layout fill (exactly 3) TopLeft) toolbar
-- @
--
-- 'fill' on one axis and a fixed size on the other pins a full-width bar to
-- the top, regardless of the parent's height.
--
-- >  +------------------------------------------+
-- >  |            Toolbar (Fill x 3)            |
-- >  +------------------------------------------+
-- >  |                                          |
-- >  |                                          |
-- >  |                                          |
-- >  +------------------------------------------+
--
-- @
-- layoutWithConstraints (Layout (exactly 14) (exactly 3) BottomRight) badge
-- @
--
-- A fixed size with 'BottomRight' alignment pins a component to a corner.
--
-- >  +------------------------------------------+
-- >  |                                          |
-- >  |                                          |
-- >  |                                          |
-- >  |                            +-------------+
-- >  |                            |    Badge    |
-- >  +----------------------------+-------------+
layoutWithConstraints :: Layout -> UI e msg a -> UI e msg a
layoutWithConstraints rc ui = do
  r <- getBounds
  let w = preferredSize (layoutWidth rc) (rectWidth r)
      h = preferredSize (layoutHeight rc) (rectHeight r)
  withBounds (alignRect (layoutAlignment rc) r (Rectangle 0 0 w h)) ui

-- | Returns the preferred size for a 'Length' given the amount of available space.
--
-- >>> preferredSize (exactly 80) 200
-- 80.0
-- >>> preferredSize fill 200
-- 200.0
-- >>> preferredSize (atLeast 50) 200
-- 200.0
-- >>> preferredSize (atLeast 50) 20
-- 50.0
-- >>> preferredSize (atMost 150) 200
-- 150.0
-- >>> preferredSize (atMost 150) 100
-- 100.0
-- >>> preferredSize (between 50 150) 200
-- 150.0
-- >>> preferredSize (between 50 150) 100
-- 100.0
-- >>> preferredSize (between 50 150) 20
-- 50.0
preferredSize :: Length -> Double -> Double
preferredSize (Exactly w)     _         = w
preferredSize Fill            available  = available
preferredSize (AtLeast w)     available  = max w available
preferredSize (AtMost w)      available  = min w available
preferredSize (Between lo hi) available  = max lo (min hi available)
preferredSize FitContent      _          = 0
  -- Unreachable in practice: every caller resolves 'fitContent' to an
  -- 'exactly' via 'resolveLength' before a 'Layout' reaches here.

-- | Turns a possibly content-dependent 'Length' into a pure one by measuring
-- the element when it names 'fitContent', 'atLeast', or 'between'. @measure@
-- is the element's own preferred-size function; @orientation@ says which of
-- its two axes @len@ constrains, so @measure@ can answer accordingly.
resolveLength
  :: Orientation
  -> Length
  -> Available
  -> Available
  -> (MeasureCtx -> UI e msg Size)
  -> UI e msg Length
resolveLength orientation len mainAvail crossAvail measure =
  case len of
    FitContent  -> Exactly              <$> preferred
    AtLeast l   -> Exactly . max l      <$> preferred
    Between l h -> Exactly . clampTo l h <$> preferred
    l           -> pure l
  where
    preferred = sizeAlong orientation <$> measure MeasureCtx
      { measureAxis  = orientation
      , measureMain  = mainAvail
      , measureCross = crossAvail
      }
    clampTo lo hi = max lo . min hi

sizeAlong :: Orientation -> Size -> Double
sizeAlong Horizontal = sizeWidth
sizeAlong Vertical   = sizeHeight

-- | The smallest size a 'Length' can be resolved to -- the floor a box
-- guarantees each child before distributing any surplus space. See
-- 'Blink.Layout.Box.preferredSizes'.
minLength :: Length -> Double
minLength (Exactly w)    = w
minLength Fill           = 0
minLength (AtLeast w)    = w
minLength (AtMost _)     = 0
minLength (Between l _)  = l
minLength FitContent     = 0

-- | The size a resolved 'Length' takes when there is no space to
-- distribute -- used for a box that is itself sizing to content. Every
-- constructor that depends on measurement ('fitContent', 'atLeast',
-- 'between') has already been turned into an 'exactly' by 'resolveLength'
-- before reaching here.
naturalLength :: Length -> Double
naturalLength (Exactly w)   = w
naturalLength (AtLeast w)   = w
naturalLength (AtMost w)    = w
naturalLength (Between l _) = l
naturalLength Fill          = 0
  -- Degenerate: see the spec's note on Fill inside a content-sized box.
naturalLength FitContent    = 0
  -- Unreachable: resolveLength never leaves a FitContent unresolved.

-- | Whether a 'Length' can grow to absorb surplus space beyond its minimum.
canExpand :: Length -> Bool
canExpand (Exactly _) = False
canExpand _           = True

-- | The most extra space (above its minimum) a 'Length' can absorb, or
-- infinite when it has no ceiling. See 'Blink.Layout.Box.distributeSurplusSpace'.
capLength :: Length -> Double
capLength (AtMost w)    = w
capLength (Between l h) = h - l
capLength _             = 1 / 0

-- | What a parent can offer a child along one axis, for the purpose of
-- measuring the child's preferred size. @Bounded@ when the parent knows its
-- own extent there; 'Unbounded' when the parent is itself sizing to content
-- on that axis and has nothing to offer yet. Distinguishing the two lets a
-- measuring child tell "no room at all" apart from "no ceiling", which a
-- bare number cannot.
data Available = Bounded Double | Unbounded
  deriving (Eq, Show)

-- | What a parent offers a child when asking for its preferred size.
-- 'measureAxis' is which of the widget's two axes ('Blink.Geometry.Horizontal'
-- \/ 'Blink.Geometry.Vertical') the question is about -- needed because a
-- widget can answer differently depending on which extent is being asked
-- for, such as a wrapping text block whose height depends on the width it
-- is given.
data MeasureCtx = MeasureCtx
  { measureAxis  :: Orientation
    -- ^ Which axis the parent will read the result on.
  , measureMain  :: Available
    -- ^ Room along that axis, if the parent knows it.
  , measureCross :: Available
    -- ^ Room along the perpendicular axis, if the parent knows it.
  } deriving (Eq, Show)

-- | Reduces the room 'Available' along an axis by a fixed amount, such as
-- chrome a control draws around a child it measures. Total: shrinking
-- 'Unbounded' is a no-op, which is why chrome arithmetic never needs to
-- special-case infinity.
--
-- >>> shrink 10 (Bounded 100)
-- Bounded 90.0
-- >>> shrink 200 (Bounded 100)
-- Bounded 0.0
-- >>> shrink 10 Unbounded
-- Unbounded
shrink :: Double -> Available -> Available
shrink n (Bounded d) = Bounded (max 0 (d - n))
shrink _ Unbounded   = Unbounded

-- | Implemented by any config type that nests a 'Layout', letting 'width'\/
-- 'height'\/'align' be applied to it directly -- the same delegation
-- pattern as 'Blink.Controls.Control.HasControlConfig'\/
-- 'Blink.Controls.Element.HasElementConfig', minus the @e@\/@msg@
-- functional dependency those need and this doesn't: a 'Layout' is pure
-- geometry, with no element-identity or message type of its own to fix.
class HasLayoutConfig cfg where
  overLayout :: Attribute Layout -> Attribute cfg

instance HasLayoutConfig Layout where
  overLayout = id

-- | Sets the size request's width. See 'Length' for the available
-- constraints, and each control's own haddock for its default.
width :: HasLayoutConfig cfg => Length -> Attribute cfg
width l = overLayout (Attribute (\lay -> lay { layoutWidth = l }))

-- | Sets the size request's height. See 'Length' for the available
-- constraints, and each control's own haddock for its default.
height :: HasLayoutConfig cfg => Length -> Attribute cfg
height l = overLayout (Attribute (\lay -> lay { layoutHeight = l }))

-- | Sets how the control is positioned within its slot when it does not
-- fill the slot on one or both axes. Defaults to 'TopLeft'.
align :: HasLayoutConfig cfg => Alignment -> Attribute cfg
align a = overLayout (Attribute (\lay -> lay { layoutAlignment = a }))
