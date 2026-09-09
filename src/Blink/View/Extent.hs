{- |
Module: Blink.View.Extent

Per-element accumulated offset, in whatever unit the caller gives it: the
pure 'ExtentState' type, plus the monadic accessors ('getExtentState',
'requestExtentBy') built on top of it. See "Blink.View" for the module
overview; import that instead of this module directly.

Unlike 'Blink.View.Scroll.ScrollState', never clamped to @[0, 1]@ --
built for a value like a resized column's width delta, which has no
natural upper bound. A caller wanting bounds of its own (e.g. a minimum
width) applies them when reading the value back.
-}
module Blink.View.Extent
  ( ExtentState
  , getExtentState
  , contextExtentState
  , requestExtentBy
  ) where

import qualified Data.Map.Strict as Map
import Blink.View.Context

-- | The given element's current extent, or @0@ if nothing has adjusted
-- it yet.
getExtentState :: Ord e => e -> View e msg Double
getExtentState eid = gets (contextExtentState eid)

-- | The given element's current extent, or @0@ if nothing has adjusted
-- it yet, read directly from a 'ViewContext' outside the 'View' monad --
-- e.g. to assert on the result of a completed frame.
contextExtentState :: Ord e => e -> ViewContext e msg -> Double
contextExtentState eid ctx =
  extentValue (Map.findWithDefault (ExtentState 0) eid (elmExtentStates (ctxElements ctx)))

-- | Adjusts the given element's extent by @dv@, from the next frame
-- onward. Multiple calls in the same frame for the same element
-- accumulate.
requestExtentBy :: e -> Double -> View e msg ()
requestExtentBy eid dv = emitUi (AdjustExtent eid dv)
