{-# LANGUAGE MultiParamTypeClasses #-}
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
  , setScrollStateNow
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
-- next frame onward. Callable from 'View' (queued immediately) or
-- 'Blink.Update.Update' (queued to apply once the frame's messages are
-- folded) -- see 'HasUiEffect'.
requestScrollTo :: HasUiEffect e m => e -> Double -> m ()
requestScrollTo eid v = queueEffect (ScrollTo eid v)

-- | Adjusts the given element's scroll position by @dv@, clamped to
-- @[0, 1]@, from the next frame onward. Multiple calls in the same frame
-- for the same element accumulate. Callable from 'View' or
-- 'Blink.Update.Update' -- see 'HasUiEffect'.
requestScrollBy :: HasUiEffect e m => e -> Double -> m ()
requestScrollBy eid dv = queueEffect (ScrollBy eid dv)

-- | 'requestScrollBy' as a handler reaction, ignoring the triggering
-- event's own data.
postScrollBy :: e -> Double -> a -> [Effect e msg]
postScrollBy eid dv = const [EffectUi (ScrollBy eid dv)]

-- | Sets the given element's scroll position, clamped to @[0, 1]@,
-- immediately -- visible to a later 'getScrollState' read in this same
-- frame, unlike 'requestScrollTo'/'requestScrollBy', which only take
-- effect from the next frame onward. For a control correcting its own
-- scroll position as a direct, same-frame consequence of what it's about
-- to render (e.g. 'Blink.Controls.List.scrollRowIntoView' keeping a moved
-- selection in view) -- not for reacting to a user gesture like a drag or
-- a wheel event, which should stay deferred so a frame's own reads of
-- "current scroll" stay stable throughout its rendering.
setScrollStateNow :: Ord e => e -> Double -> View e msg ()
setScrollStateNow eid v = modify (writeScrollState eid v)
