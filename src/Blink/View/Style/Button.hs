{- |
Module: Blink.View.Style.Button

'Blink.View.Controls.Button.button''s registration in
'Blink.View.Style.Defaults.defaultTheme' -- the bordered-box look
defined in "Blink.View.Style.Control", since a button doesn't have a
shape of its own.
-}
module Blink.View.Style.Button
  ( defaultStyleEntries
  ) where

import Blink.View.Controls.Button (buttonStyleKey)
import Blink.View.Rendering (TextAlign (..))
import Blink.View.Style
import Blink.View.Style.Control (buttonStyle, controlMetrics)

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (buttonStyleKey, (controlMetrics, buttonStyle AlignCenter p)) ]
