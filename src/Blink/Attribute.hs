-- | The generic attribute mechanism used throughout Blink's own combinators
-- (widgets in "Blink.Controls", box layout in "Blink.Layout.Box") to let a
-- caller configure something with an ordinary list rather than a single
-- record: build the list with whatever list machinery is convenient
-- (@++@, list comprehensions, @Data.Maybe.catMaybes@, ...), and 'resolve'
-- folds it over a starting default, left to right, so a later entry
-- setting the same field overrides an earlier one.
--
-- An attribute is @'Attribute' cfg@: a function @cfg -> cfg@, wrapped in a
-- newtype so it can be given its own instances. This module only defines
-- the mechanism itself; each config type's own module defines the actual
-- attribute functions (e.g. "Blink.Controls.Control"'s 'Blink.Controls.Control.isFocusable',
-- "Blink.Layout.Box"'s 'Blink.Layout.Box.boxSpacing').
module Blink.Attribute
  ( Attribute (..)
  , resolve
  ) where

import Data.List (foldl')

-- | A single field update on @cfg@, applied by 'resolve'.
newtype Attribute cfg = Attribute { runAttribute :: cfg -> cfg }

-- | Folds a list of attributes over a starting config, left to right -- a
-- later attribute setting the same field overrides an earlier one.
resolve :: cfg -> [Attribute cfg] -> cfg
resolve = foldl' (\cfg (Attribute f) -> f cfg)
