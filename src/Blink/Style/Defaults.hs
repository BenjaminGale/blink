{- |
Module: Blink.Style.Defaults

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
'Blink.Controls.Button.Style.buttonStyleKey', or a different 'Class'\/'ElementId'
passed via 'Blink.Controls.Control.style') in 'themeElementStyles'.

= Module layout

Each built-in control's own registration in 'defaultTheme' lives in its
own @Blink.Style.\<Control\>@ module (e.g. "Blink.Controls.Checkbox.Style"),
which is also where a control's own shape lives if it has one (e.g.
"Blink.Controls.ProgressBar.Style"). A shape shared by more than one control
lives in "Blink.Controls.Style" instead, since no single control's
module could own it. This module only assembles those pieces into one
'Theme' -- see any of the modules above for the actual colours and
metrics.
-}
module Blink.Style.Defaults
  ( defaultTheme
  , buttonStyle
  , flatRowStyle
  , progressBarStyle
  , sliderStyle
  , dividerStyle
  , labelStyle
  , toggleGroupStyle
  , containerStyle
  ) where

import qualified Data.Map.Strict as Map

import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (buttonStyle, containerStyle, controlMetrics, flatRowStyle, sliderStyle, toggleGroupStyle)
import Blink.Controls.Divider.Style (dividerStyle)
import Blink.Controls.Label.Style (labelStyle)
import Blink.Controls.ProgressBar.Style (progressBarStyle)

import qualified Blink.Controls.Button.Style as Button
import qualified Blink.Controls.Checkbox.Style as Checkbox
import qualified Blink.Controls.Divider.Style as Divider
import qualified Blink.Controls.Label.Style as Label
import qualified Blink.Controls.List.Style as List
import qualified Blink.Controls.ProgressBar.Style as ProgressBar
import qualified Blink.Controls.RadioButton.Style as RadioButton
import qualified Blink.Controls.ScrollBar.Style as ScrollBar
import qualified Blink.Controls.Slider.Style as Slider
import qualified Blink.Controls.TextInput.Style as TextInput
import qualified Blink.Controls.ToggleButton.Style as ToggleButton
import qualified Blink.Controls.ToggleGroup.Style as ToggleGroup
import qualified Blink.Controls.Tree.Style as Tree

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
      , List.defaultStyleEntries p
      , Tree.defaultStyleEntries p
      ])
  , themeDefaultStyle = (controlMetrics, buttonStyle AlignCenter p)
  }
