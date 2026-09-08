{- |
Module: Blink.View.Scroll

Per-element scroll position, normalised to @[0, 1]@: the pure 'ScrollState'
type and clamp, plus the monadic accessors ('getScrollState',
'requestScrollTo', 'requestScrollBy') built on top of it. See "Blink.View"
for the module overview; import that instead of this module directly.
-}
module Blink.View.Scroll
  ( ScrollState
  , clampScrollPos
  , getScrollState
  , contextScrollState
  , requestScrollTo
  , requestScrollBy
  , postScrollBy
  ) where

import qualified Data.Map.Strict as Map
import Blink.View.Context

-- | The current scroll position for the given element, in @[0, 1]@. Returns
-- @0@ when no position has been recorded yet.
getScrollState :: Ord e => e -> View e msg Double
getScrollState eid = gets (contextScrollState eid)

-- | The current scroll position for the given element, in @[0, 1]@, read
-- directly from a 'ViewContext' outside the 'View' monad — e.g. to assert on the
-- result of a completed frame. Returns @0@ when no position has been
-- recorded yet.
contextScrollState :: Ord e => e -> ViewContext e msg -> Double
contextScrollState eid ctx =
  scrollPosition (Map.findWithDefault (ScrollState 0) eid (elmScrollStates (ctxElements ctx)))

-- | Sets the given element's scroll position, clamped to @[0, 1]@, from the
-- next frame onward.
requestScrollTo :: e -> Double -> View e msg ()
requestScrollTo eid v = emitUi (ScrollTo eid v)

-- | Adjusts the given element's scroll position by @dv@, clamped to
-- @[0, 1]@, from the next frame onward. Multiple calls in the same frame
-- for the same element accumulate.
requestScrollBy :: e -> Double -> View e msg ()
requestScrollBy eid dv = emitUi (ScrollBy eid dv)

-- | 'requestScrollBy' as a handler reaction, ignoring the triggering
-- event's own data.
postScrollBy :: e -> Double -> a -> [Out e msg]
postScrollBy eid dv = const [OutUi (ScrollBy eid dv)]
