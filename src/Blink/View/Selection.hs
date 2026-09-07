{- |
Module: Blink.View.Selection

A contiguous selection or cursor within a linear sequence, plus pure
helpers built on top of it: reading the low\/high bound of a range,
collapsing it to a cursor, and moving its active end.

No dependency on the 'Blink.View' monad -- 'Blink.View' holds a
'Selection' in its context and exposes monadic accessors
('Blink.View.getSelection', 'Blink.View.contextSelection') built on top of
what's defined here, the same relationship "Blink.View.Focus" has with the
focus state 'Blink.View' threads through its own context.
-}
module Blink.View.Selection
  ( Selection (..)
  , selectionLow
  , selectionHigh
  , selectionHasExtent
  , cursor
  , collapseToLow
  , collapseToHigh
  , collapseToActive
  , extendActive
  ) where

-- | A contiguous selection or cursor within a linear sequence. The selected
-- range is @(min anchor active, max anchor active)@. When @anchor == active@
-- the selection is a cursor with no extent.
data Selection = Selection
  { selectionAnchor :: Int  -- ^ The fixed end.
  , selectionActive :: Int  -- ^ The moving end (cursor position).
  }
  deriving (Eq, Show)

-- | The lower bound of the selected range: @min selectionAnchor selectionActive@.
selectionLow :: Selection -> Int
selectionLow s = min (selectionAnchor s) (selectionActive s)

-- | The upper bound of the selected range: @max selectionAnchor selectionActive@.
selectionHigh :: Selection -> Int
selectionHigh s = max (selectionAnchor s) (selectionActive s)

-- | 'True' when the selection has non-zero extent (anchor ≠ active).
selectionHasExtent :: Selection -> Bool
selectionHasExtent s = selectionAnchor s /= selectionActive s

-- | A cursor with no selection extent. Equivalent to @'Selection' n n@.
cursor :: Int -> Selection
cursor n = Selection n n

-- | Collapse the selection to a cursor at the lower bound.
collapseToLow :: Selection -> Selection
collapseToLow = cursor . selectionLow

-- | Collapse the selection to a cursor at the upper bound.
collapseToHigh :: Selection -> Selection
collapseToHigh = cursor . selectionHigh

-- | Collapse the selection to a cursor at the active (moving) end.
collapseToActive :: Selection -> Selection
collapseToActive = cursor . selectionActive

-- | Apply a function to the active end, keeping the anchor fixed.
extendActive :: (Int -> Int) -> Selection -> Selection
extendActive f s = s { selectionActive = f (selectionActive s) }
