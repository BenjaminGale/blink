{- |
Module: Blink.View.Style.TextInput

'Blink.View.Controls.TextInput.textInput''s registration in
'Blink.View.Style.Defaults.defaultTheme' -- the bordered-box look
defined in "Blink.View.Style.Control", left-aligned, since a text input
doesn't have a shape of its own.
-}
module Blink.View.Style.TextInput
  ( defaultStyleEntries
  ) where

import Blink.View.Controls.TextInput (textInputStyleKey)
import Blink.View.Rendering (TextAlign (..))
import Blink.View.Style
import Blink.View.Style.Control (buttonStyle, controlMetrics)

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (textInputStyleKey, (controlMetrics, buttonStyle AlignLeft p)) ]
