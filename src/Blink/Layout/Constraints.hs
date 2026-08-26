-- | The foundation "Blink.Layout.Box" and "Blink.Layout.Border" are both
-- built on: 'Length'\/'Layout' describe a single child's size and
-- position, and 'layoutWithConstraints' applies one to a single action.
module Blink.Layout.Constraints
  ( Length (..)
  , Layout (..)
  , layoutWithConstraints
  , preferredSize
  , Available (..)
  , MeasureCtx (..)
  , shrink
  ) where

import Blink.Geometry (Alignment (..), Orientation (..), Rectangle (..), alignRect)
import Blink.UI (UI, getBounds, withBounds)

-- | Describes how a child should be sized along a single axis. See
--   'preferredSize' for worked examples of how each constructor resolves
--   against different amounts of available space.
data Length
  = Exactly Double
    -- ^ A fixed size. The available space is ignored.
  | Fill
    -- ^ Expands to fill all available space.
  | AtLeast Double
    -- ^ Expands to fill available space, but never smaller than the given minimum.
  | AtMost Double
    -- ^ Expands to fill available space, but never larger than the given maximum. Has no minimum — can shrink to zero.
  | Between Double Double
    -- ^ Expands to fill available space clamped between the given minimum and maximum.
  | FitContent
    -- ^ Exactly the widget's own preferred size, determined by measuring it.
    -- Unlike the other constructors, this cannot be resolved from available
    -- space alone -- see 'Blink.UI.Element.resolveLength'. A 'FitContent'
    -- reaching 'preferredSize' unresolved is a bug in the caller: every path
    -- that can encounter it (element measurement, box slot sizing) must
    -- resolve it to an 'Exactly' first.
  deriving (Eq, Show)

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
-- layoutWithConstraints (Layout (Exactly 120) (Exactly 32) Center) $
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
-- layoutWithConstraints (Layout Fill (Exactly 3) TopLeft) toolbar
-- @
--
-- 'Fill' on one axis and a fixed size on the other pins a full-width bar to
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
-- layoutWithConstraints (Layout (Exactly 14) (Exactly 3) BottomRight) badge
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
-- >>> preferredSize (Exactly 80) 200
-- 80.0
-- >>> preferredSize Fill 200
-- 200.0
-- >>> preferredSize (AtLeast 50) 200
-- 200.0
-- >>> preferredSize (AtLeast 50) 20
-- 50.0
-- >>> preferredSize (AtMost 150) 200
-- 150.0
-- >>> preferredSize (AtMost 150) 100
-- 100.0
-- >>> preferredSize (Between 50 150) 200
-- 150.0
-- >>> preferredSize (Between 50 150) 100
-- 100.0
-- >>> preferredSize (Between 50 150) 20
-- 50.0
preferredSize :: Length -> Double -> Double
preferredSize (Exactly w)     _         = w
preferredSize Fill            available  = available
preferredSize (AtLeast w)     available  = max w available
preferredSize (AtMost w)      available  = min w available
preferredSize (Between lo hi) available  = max lo (min hi available)
preferredSize FitContent      _          = 0
  -- Unreachable in practice: every caller resolves 'FitContent' to an
  -- 'Exactly' via 'Blink.UI.Element.resolveLength' before a 'Layout'
  -- reaches here.

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
