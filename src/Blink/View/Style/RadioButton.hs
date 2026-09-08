{- |
Module: Blink.View.Style.RadioButton

'Blink.View.Controls.RadioButton.radioButton''s registration in
'Blink.View.Style.Defaults.defaultTheme' -- the flat-row look defined
in "Blink.View.Style.Control", since a radio button doesn't have a
shape of its own.
-}
module Blink.View.Style.RadioButton
  ( defaultStyleEntries
  ) where

import Blink.View.Controls.RadioButton (radioButtonStyleKey)
import Blink.View.Style
import Blink.View.Style.Control (flatRowMetrics, flatRowStyle)

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (radioButtonStyleKey, (flatRowMetrics, flatRowStyle p)) ]
