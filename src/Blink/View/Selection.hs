{- |
Module: Blink.View.Selection

A contiguous selection or cursor within a linear sequence: the pure
'Selection' type and helpers built on top of it (reading the low\/high
bound of a range, collapsing it to a cursor, moving its active end), plus
the monadic accessors ('getSelection', 'requestSelectionAt') built on top
of the 'Blink.View.Context.ViewContext' it's threaded through. See
"Blink.View" for the module overview; import that instead of this module
directly.
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
  , getSelection
  , contextSelection
  , requestSelectionAt
  ) where

import Blink.View.Context

-- | The given element's selection, or 'Nothing' if it isn't the element
-- currently holding one.
getSelection :: Eq e => e -> View e msg (Maybe Selection)
getSelection eid = gets (contextSelection eid)

-- | The given element's selection, or 'Nothing' if it isn't the element
-- currently holding one, read directly from a 'ViewContext' outside the 'View'
-- monad.
contextSelection :: Eq e => e -> ViewContext e msg -> Maybe Selection
contextSelection eid ctx = case elmSelection (ctxElements ctx) of
  SelectionAt owner sel | owner == eid -> Just sel
  _                                    -> Nothing

-- | Sets the given element's selection, from the next frame onward.
requestSelectionAt :: e -> Selection -> View e msg ()
requestSelectionAt eid sel = emitUi (SetSelectionAt eid sel)
