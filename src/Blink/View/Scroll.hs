{- |
Module: Blink.View.Scroll

Per-element scroll position, normalised to @[0, 1]@.

No dependency on the 'Blink.View' monad -- 'Blink.View' holds a
'ScrollState' per element in its context and exposes monadic accessors
('Blink.View.getScrollState', 'Blink.View.contextScrollState') built on
top of what's defined here, the same relationship "Blink.View.Focus" has
with the focus state 'Blink.View' threads through its own context.
-}
module Blink.View.Scroll
  ( ScrollState (..)
  , clampScrollPos
  ) where

-- | Per-instance scroll position in @[0, 1]@.
newtype ScrollState = ScrollState { scrollPosition :: Double }
  deriving (Eq, Ord, Show)

-- | Clamp a scroll position to @[0, 1]@.
clampScrollPos :: Double -> Double
clampScrollPos = max 0 . min 1
