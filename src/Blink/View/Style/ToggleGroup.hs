{- |
Module: Blink.View.Style.ToggleGroup

'Blink.View.Controls.ToggleGroup.toggleButtonGroup' and
'Blink.View.Controls.ToggleGroup.radioButtonGroup''s registration in
'Blink.View.Style.Defaults.defaultTheme' -- the plain wrapper look
defined in "Blink.View.Style.Control", since neither has a shape of its
own; the items inside resolve their own look from
"Blink.View.Style.Button"\/"Blink.View.Style.RadioButton".
-}
module Blink.View.Style.ToggleGroup
  ( defaultStyleEntries
  ) where

import Blink.View.Controls.ToggleGroup (radioButtonGroupStyleKey, toggleButtonGroupStyleKey)
import Blink.View.Style
import Blink.View.Style.Control (toggleGroupMetrics, toggleGroupStyle)

-- | This control pair's entries in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (toggleButtonGroupStyleKey, (toggleGroupMetrics, toggleGroupStyle p))
  , (radioButtonGroupStyleKey,  (toggleGroupMetrics, toggleGroupStyle p))
  ]
