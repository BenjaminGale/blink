{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.TextInput.Style

'Blink.Controls.TextInput.textInput''s registration in
'Blink.Style.Defaults.defaultTheme' -- the bordered-box look defined
in "Blink.Controls.Style", left-aligned, with its own pressed state
(see 'textInputStyle').
-}
module Blink.Controls.TextInput.Style
  ( textInputStyleKey
  , textInputStyle
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (buttonStyle, controlMetrics)

-- | The 'StyleKey' 'Blink.Controls.TextInput.textInput' resolves its
-- style from unless overridden via 'Blink.Controls.Control.style'.
textInputStyleKey :: StyleKey e
textInputStyleKey = Class "textInput"

-- | Like 'Blink.Controls.Style.buttonStyle' but with a subtler pressed
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

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (textInputStyleKey, (controlMetrics, textInputStyle p)) ]
