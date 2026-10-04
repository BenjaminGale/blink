{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module: Blink.View.Scroll

Per-element scroll position, stored as a pixel offset from the start of the
content: the monadic accessors ('getScrollState', 'requestScrollTo',
'requestScrollBy') and the step that turns a pending request into an
offset when the control draws ('resolveScrollOffset'). See "Blink.View" for
the module overview; import that instead of this module directly.
-}
module Blink.View.Scroll
  ( ScrollState
  , getScrollState
  , contextScrollState
  , requestScrollTo
  , requestScrollBy
  , setScrollStateNow
  , resolveScrollOffset
  ) where

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Blink.View.Context

-- | The given element's scroll offset in pixels. Returns @0@ when no
-- position has been recorded yet.
getScrollState :: Ord e => e -> View e msg Double
getScrollState eid = controlIdOf eid >>= gets . contextScrollState

-- | The given element's scroll offset in pixels, read directly from a
-- 'ViewContext' outside the 'View' monad — e.g. to assert on the result of
-- a completed frame. Returns @0@ when no position has been recorded yet.
contextScrollState :: Ord e => ControlId e -> ViewContext e msg -> Double
contextScrollState eid ctx = scrollOffset (scrollStateOf eid ctx)

scrollStateOf :: Ord e => ControlId e -> ViewContext e msg -> ScrollState
scrollStateOf eid ctx = Map.findWithDefault noScroll eid (elmScrollStates (ctxElements ctx))

-- | Scrolls the given element to @position@ in its scrollable range, from
-- @0@ (the start) to @1@ (the end), when it next draws. Callable from
-- 'View' (queued immediately) or 'Blink.Update.Update' (queued to apply
-- once the frame's messages are folded) -- see 'HasUiEffect'.
requestScrollTo :: (Ord e, Monad m, HasUiEffect e m) => e -> Double -> m ()
requestScrollTo eid v = controlIdFor eid >>= \k -> queueEffect (ScrollTo k (ScrollToFraction v))

-- | Adjusts the given element's scroll offset by @dv@ pixels, from the next
-- frame onward. Multiple calls in the same frame for the same element
-- accumulate. Callable from 'View' or 'Blink.Update.Update' -- see
-- 'HasUiEffect'.
requestScrollBy :: (Ord e, Monad m, HasUiEffect e m) => e -> Double -> m ()
requestScrollBy eid dv = controlIdFor eid >>= \k -> queueEffect (ScrollBy k dv)

-- | Sets the given element's scroll offset to @v@ pixels immediately --
-- visible to a later 'getScrollState' read in this same frame, unlike
-- 'requestScrollTo'/'requestScrollBy', which only take effect from the
-- next frame onward. For a control correcting its own scroll position as a
-- direct, same-frame consequence of what it's about to render (e.g.
-- 'Blink.Controls.List.scrollRowIntoView' keeping a moved selection in
-- view) -- not for reacting to a user gesture like a drag or a wheel event,
-- which should stay deferred so a frame's own reads of "current scroll"
-- stay stable throughout its rendering.
setScrollStateNow :: Ord e => e -> Double -> View e msg ()
setScrollStateNow eid v = controlIdOf eid >>= \k -> modify (writeScrollState k v)

-- | The scroll offset @eid@ draws at, given the largest offset its content
-- allows: applies any pending request, turning an item index into pixels
-- with @itemOffset@ (ignored when 'Nothing'), and clamps the result to
-- @[0, maxOffset]@. Stores the result and @maxOffset@ immediately, so the
-- request is applied once and later scrolling is clamped to the content.
resolveScrollOffset :: Ord e => ControlId e -> Double -> Maybe (Int -> Double) -> View e msg Double
resolveScrollOffset eid maxOffset itemOffset = do
  st <- gets (scrollStateOf eid)
  let limit     = max 0 maxOffset
      requested = case scrollPending st of
        Just (ScrollToFraction f) -> f * limit
        Just (ScrollToItem i)     -> maybe (scrollOffset st) ($ i) itemOffset
        Nothing                   -> scrollOffset st
      resolved  = ScrollState
        { scrollOffset    = max 0 (min limit requested)
        , scrollPending   = Nothing
        , scrollMaxOffset = Just limit
        }
  when (resolved /= st) $ modify (updateScrollState eid (const resolved))
  pure (scrollOffset resolved)
