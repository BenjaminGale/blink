{- |
Module: Blink.View.Style.ToggleButton

'Blink.View.Controls.ToggleButton.toggleButton''s registration in
'Blink.View.Style.Defaults.defaultTheme' -- the bordered-box look
defined in "Blink.View.Style.Control", since a toggle button doesn't
have a shape of its own.
-}
module Blink.View.Style.ToggleButton
  ( defaultStyleEntries
  ) where

import Blink.View.Controls.ToggleButton (toggleButtonStyleKey)
import Blink.View.Rendering (TextAlign (..))
import Blink.View.Style
import Blink.View.Style.Control (buttonStyle, controlMetrics)

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (toggleButtonStyleKey, (controlMetrics, buttonStyle AlignCenter p)) ]
