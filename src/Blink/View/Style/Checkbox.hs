{- |
Module: Blink.View.Style.Checkbox

'Blink.View.Controls.Checkbox.checkbox''s registration in
'Blink.View.Style.Defaults.defaultTheme' -- the flat-row look defined
in "Blink.View.Style.Control", since a checkbox doesn't have a shape of
its own.
-}
module Blink.View.Style.Checkbox
  ( defaultStyleEntries
  ) where

import Blink.View.Controls.Checkbox (checkboxStyleKey)
import Blink.View.Style
import Blink.View.Style.Control (flatRowMetrics, flatRowStyle)

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (checkboxStyleKey, (flatRowMetrics, flatRowStyle p)) ]
