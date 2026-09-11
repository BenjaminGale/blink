{- |
Module: Blink.View.Cursor

Per-element "last known list cursor row index" state: the pure
'CursorIndexState' type (in "Blink.View.Context", since 'ViewContext'
embeds it directly), plus the monadic accessors ('getCursorIndex',
'setCursorIndex') built on top of it. See "Blink.View" for the module
overview; import that instead of this module directly.
-}
module Blink.View.Cursor
  ( getCursorIndex
  , contextCursorIndex
  , setCursorIndex
  ) where

import qualified Data.Map.Strict as Map
import Blink.View.Context

-- | The row index a list-like control's cursor most recently held, as of
-- the start of this frame -- 'Nothing' for an element nothing has
-- recorded yet. See 'Blink.Controls.List.listBase'.
getCursorIndex :: Ord e => e -> View e msg (Maybe Int)
getCursorIndex eid = gets (contextCursorIndex eid)

-- | 'getCursorIndex', read directly from a 'ViewContext' outside the
-- 'View' monad -- e.g. to assert on the result of a completed frame.
contextCursorIndex :: Ord e => e -> ViewContext e msg -> Maybe Int
contextCursorIndex eid ctx =
  cursorIndexValue <$> Map.lookup eid (elmCursorIndices (ctxElements ctx))

-- | Records ('Just') or clears ('Nothing') the row index a list-like
-- control's cursor currently holds, from the next frame onward.
setCursorIndex :: e -> Maybe Int -> View e msg ()
setCursorIndex eid mi = emitUi (SetCursorIndex eid mi)
