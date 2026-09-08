{- |
Module: Blink.View.Style.Defaults

A ready-made 'Theme' for every built-in control, so an app gets a
coherent, usable look out of the box just by supplying a 'Palette' --
see 'defaultTheme'.

Each control shape is also exported on its own
('buttonStyle', 'flatRowStyle', 'progressBarStyle', 'sliderStyle', 'labelStyle'), so an
app that wants the built-in look for every control /except/ one state on
one control doesn't have to rebuild a whole 'StyleSet' by hand -- take the
built-in one and replace a single entry in its sparse 'styleOverrides'
map:

@
myButtonStyle :: StyleSet
myButtonStyle = (buttonStyle AlignCenter myPalette)
  { styleOverrides = Map.insert CommonMouseOver
      (\\s -> s { styleBorderColour = Just myColour })
      (styleOverrides (buttonStyle AlignCenter myPalette))
  }
@

Register it under whichever 'StyleKey' the control resolves to (e.g.
'Blink.View.Controls.Button.buttonStyleKey', or a different 'Class'\/'ElementId'
passed via 'Blink.View.Controls.Control.style') in 'themeElementStyles'.

= Module layout

Each built-in control's own registration in 'defaultTheme' lives in its
own @Blink.View.Style.\<Control\>@ module (e.g. "Blink.View.Style.Checkbox"),
which is also where a control's own shape lives if it has one (e.g.
"Blink.View.Style.ProgressBar"). A shape shared by more than one control
lives in "Blink.View.Style.Control" instead, since no single control's
module could own it. This module only assembles those pieces into one
'Theme' -- see any of the modules above for the actual colours and
metrics.
-}
module Blink.View.Style.Defaults
  ( defaultTheme
  , buttonStyle
  , flatRowStyle
  , progressBarStyle
  , sliderStyle
  , dividerStyle
  , labelStyle
  , toggleGroupStyle
  ) where

import qualified Data.Map.Strict as Map

import Blink.View.Rendering (TextAlign (..))
import Blink.View.Style
import Blink.View.Style.Control (buttonStyle, controlMetrics, flatRowStyle, sliderStyle, toggleGroupStyle)
import Blink.View.Style.Divider (dividerStyle)
import Blink.View.Style.Label (labelStyle)
import Blink.View.Style.ProgressBar (progressBarStyle)

import qualified Blink.View.Style.Button as Button
import qualified Blink.View.Style.Checkbox as Checkbox
import qualified Blink.View.Style.Divider as Divider
import qualified Blink.View.Style.Label as Label
import qualified Blink.View.Style.ProgressBar as ProgressBar
import qualified Blink.View.Style.RadioButton as RadioButton
import qualified Blink.View.Style.ScrollBar as ScrollBar
import qualified Blink.View.Style.Slider as Slider
import qualified Blink.View.Style.TextInput as TextInput
import qualified Blink.View.Style.ToggleButton as ToggleButton
import qualified Blink.View.Style.ToggleGroup as ToggleGroup

-- | A complete 'Theme' for every built-in control, built entirely from
-- @p@. Works for any element type @e@ since every entry is 'Class'-keyed,
-- never 'ElementId'-keyed. 'themeDefaultStyle' falls back to the
-- boxed-control look. See /Module layout/ above for where each entry
-- actually comes from.
defaultTheme :: Ord e => Palette -> Theme e
defaultTheme p = Theme
  { themeElementStyles = Map.fromList (concat
      [ Button.defaultStyleEntries p
      , ToggleButton.defaultStyleEntries p
      , Checkbox.defaultStyleEntries p
      , RadioButton.defaultStyleEntries p
      , TextInput.defaultStyleEntries p
      , ProgressBar.defaultStyleEntries p
      , Slider.defaultStyleEntries p
      , Divider.defaultStyleEntries p
      , Label.defaultStyleEntries p
      , ToggleGroup.defaultStyleEntries p
      , ScrollBar.defaultStyleEntries p
      ])
  , themeDefaultStyle = (controlMetrics, buttonStyle AlignCenter p)
  }
