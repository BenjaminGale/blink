{- |
Module: Blink.View.Style.TextInput

'Blink.View.Controls.TextInput.textInput''s registration in
'Blink.View.Style.Defaults.defaultTheme' -- the bordered-box look defined
in "Blink.View.Style.Control", left-aligned, with its own pressed state
(see 'textInputStyle').
-}
module Blink.View.Style.TextInput
  ( textInputStyle
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.View.Controls.TextInput (textInputStyleKey)
import Blink.View.Rendering (TextAlign (..))
import Blink.View.Style
import Blink.View.Style.Control (buttonStyle, controlMetrics)

-- | Like 'Blink.View.Style.Control.buttonStyle' but with a subtler pressed
-- state: a mouse-down on a text input starts a drag-to-select, so filling
-- it with 'paletteAccent' (as a button does) would hide the selection
-- highlight instead of just darkening the background a touch.
textInputStyle :: Palette -> StyleSet
textInputStyle p = base
  { styleOverrides = Map.insert CommonPressed
      (\s -> s { styleBackground = paletteSurfaceHover p })
      (styleOverrides base)
  }
  where
    base = buttonStyle AlignLeft p

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (textInputStyleKey, (controlMetrics, textInputStyle p)) ]
