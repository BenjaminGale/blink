-- | The foundation "Blink.Layout.Box" and "Blink.Layout.Border" are both
-- built on: 'Length'\/'Layout' describe a single child's size and
-- position, 'layoutWithConstraints' applies one to a single action, and
-- the 'AddLength'\/'MaxLength' monoids let a caller compute a 'Length'
-- from several others rather than only ever writing one down by hand.
module Blink.Layout.Core
  ( Length (..)
  , Layout (..)
  , layoutWithConstraints
  , preferredSize
  , AddLength (..)
  , MaxLength (..)
  , addLength
  , maxLength
  ) where

import Blink.Geometry (Alignment (..), Rectangle (..), alignRect)
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

-- | Wraps 'Length' for the additive monoid: @'mempty' = 'Exactly' 0@,
-- @('<>') = 'addLength'@. Use 'mconcat' to sum a list of lengths.
newtype AddLength = AddLength { getAddLength :: Length }

instance Semigroup AddLength where
  AddLength a <> AddLength b = AddLength (add a b)
    where
      add Fill              _                   = Fill
      add _                 Fill                = Fill
      add (Exactly a')      (Exactly b')         = Exactly     (a' + b')
      add (AtLeast a')      (Exactly b')         = AtLeast     (a' + b')
      add (Exactly a')      (AtLeast b')         = AtLeast     (a' + b')
      add (AtMost a')       (Exactly b')         = AtMost      (a' + b')
      add (Exactly a')      (AtMost b')          = AtMost      (a' + b')
      add (Between lo hi)   (Exactly b')         = Between     (lo + b') (hi + b')
      add (Exactly a')      (Between lo hi)      = Between     (a' + lo) (a' + hi)
      add (AtLeast a')      (AtLeast b')         = AtLeast     (a' + b')
      add (AtMost a')       (AtMost b')          = AtMost      (a' + b')
      add (Between lo1 hi1) (Between lo2 hi2)    = Between     (lo1 + lo2) (hi1 + hi2)
      add (AtLeast a')      (AtMost _)           = AtLeast     a'
      add (AtMost _)        (AtLeast b')         = AtLeast     b'
      add (AtLeast a')      (Between lo _)       = AtLeast     (a' + lo)
      add (Between lo _)    (AtLeast b')         = AtLeast     (lo + b')
      add (AtMost a')       (Between lo hi)      = Between     lo (a' + hi)
      add (Between lo hi)   (AtMost b')          = Between     lo (hi + b')

instance Monoid AddLength where
  mempty = AddLength (Exactly 0)

-- | Wraps 'Length' for the max monoid: @'mempty' = 'Exactly' 0@,
-- @('<>') = 'maxLength' of two@. Use 'mconcat' to find the largest in a list.
newtype MaxLength = MaxLength { getMaxLength :: Length }

instance Semigroup MaxLength where
  MaxLength a <> MaxLength b = MaxLength (maxL a b)
    where
      maxL Fill              _                   = Fill
      maxL _                 Fill                = Fill
      maxL (Exactly a')      (Exactly b')         = Exactly     (max a' b')
      maxL (AtLeast a')      (AtLeast b')         = AtLeast     (max a' b')
      maxL (AtMost a')       (AtMost b')          = AtMost      (max a' b')
      maxL (Between lo1 hi1) (Between lo2 hi2)    = Between     (max lo1 lo2) (max hi1 hi2)
      maxL (Exactly a')      (AtLeast b')         = AtLeast     (max a' b')
      maxL (AtLeast a')      (Exactly b')         = AtLeast     (max a' b')
      maxL (Exactly a')      (AtMost b')          = AtMost      (max a' b')
      maxL (AtMost a')       (Exactly b')         = AtMost      (max a' b')
      maxL (Exactly a')      (Between lo hi)      = Between     (max a' lo) (max a' hi)
      maxL (Between lo hi)   (Exactly b')         = Between     (max lo b') (max hi b')
      maxL (AtLeast a')      (AtMost _)           = AtLeast     a'
      maxL (AtMost _)        (AtLeast b')         = AtLeast     b'
      maxL (AtLeast a')      (Between lo _)       = AtLeast     (max a' lo)
      maxL (Between lo _)    (AtLeast b')         = AtLeast     (max lo b')
      maxL (AtMost a')       (Between _ hi)       = AtMost      (max a' hi)
      maxL (Between _ hi)    (AtMost b')          = AtMost      (max hi b')

instance Monoid MaxLength where
  mempty = MaxLength (Exactly 0)

-- | Add two 'Length' constraints. Convenience wrapper around 'AddLength'.
--
-- >>> addLength (Exactly 10) (Exactly 20)
-- Exactly 30.0
-- >>> addLength (Exactly 10) Fill
-- Fill
-- >>> addLength (AtLeast 10) (Exactly 5)
-- AtLeast 15.0
-- >>> addLength (AtLeast 10) (AtMost 20)
-- AtLeast 10.0
-- >>> addLength (Between 10 20) (Exactly 5)
-- Between 15.0 25.0
addLength :: Length -> Length -> Length
addLength a b = getAddLength (AddLength a <> AddLength b)

-- | Return the maximum 'Length' across a list. Convenience wrapper around 'MaxLength'.
-- Returns @'Exactly' 0@ for an empty list.
--
-- >>> maxLength [Exactly 10, Exactly 30, Exactly 20]
-- Exactly 30.0
-- >>> maxLength [Fill, Exactly 100]
-- Fill
-- >>> maxLength [AtLeast 10, AtMost 20]
-- AtLeast 10.0
-- >>> maxLength []
-- Exactly 0.0
maxLength :: [Length] -> Length
maxLength = getMaxLength . mconcat . map MaxLength
