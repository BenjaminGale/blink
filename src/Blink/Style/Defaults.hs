{- |
Module: Blink.Style.Defaults

A ready-made 'Theme' for every built-in control, so an app gets a
coherent, usable look out of the box just by supplying a 'Palette' --
see 'defaultTheme'.

Each control shape is also exported on its own
('buttonStyle', 'flatRowStyle', 'sliderStyle', 'labelStyle', 'plainStyle'), so an
app that wants the built-in look for every control /except/ one state on
one control doesn't have to rebuild a whole 'StyleSet' by hand -- take the
built-in one and replace a single entry in its sparse 'styleOverrides'
map:

@
myButtonStyle :: StyleSet
myButtonStyle = (buttonStyle AlignCenter myPalette)
  { styleOverrides = Map.insert CommonMouseOver
      (\\s -> s { styleBorder = withBorderColour myColour (styleBorder s) })
      (styleOverrides (buttonStyle AlignCenter myPalette))
  }
@

Register it under whichever 'StyleKey' the control resolves to (e.g.
'Blink.Controls.Button.buttonStyleKey', or a different 'Class'\/'ElementId'
passed via 'Blink.Controls.Control.style') in 'themeElementStyles'.

= Module layout

Each built-in control's own registration in 'defaultTheme' is the
@defaultStyleEntries@ exported from that control's module (e.g.
"Blink.Controls.Checkbox"), which is also where a control's own shape
lives if it has one (e.g. 'Blink.Controls.Slider.sliderStyle').
A shape shared by more than one control lives in "Blink.Controls.Style"
instead, since no single control's module could own it. This module only
assembles those pieces into one 'Theme' -- see any of the modules above
for the actual colours and metrics.
-}
module Blink.Style.Defaults
  ( defaultTheme
  , buttonStyle
  , flatRowStyle
  , sliderStyle
  , labelStyle
  , plainStyle
  , containerStyle
  ) where

import qualified Data.Map.Strict as Map

import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style
  (buttonStyle, containerStyle, controlMetrics, flatRowStyle, iconStyle, iconStyleKey
  , plainStyle, zeroMetrics
  )
import Blink.Controls.Slider (sliderStyle)
import Blink.Controls.Label (labelStyle)

import qualified Blink.Controls.Button as Button
import qualified Blink.Controls.Checkbox as Checkbox
import qualified Blink.Controls.Divider as Divider
import qualified Blink.Controls.Image as Image
import qualified Blink.Controls.Label as Label
import qualified Blink.Controls.List as List
import qualified Blink.Controls.Menu as Menu
import qualified Blink.Controls.MenuBar as MenuBar
import qualified Blink.Controls.MenuButton as MenuButton
import qualified Blink.Controls.ProgressBar as ProgressBar
import qualified Blink.Controls.RadioButton as RadioButton
import qualified Blink.Controls.ScrollBar as ScrollBar
import qualified Blink.Controls.ScrollPanel as ScrollPanel
import qualified Blink.Controls.Slider as Slider
import qualified Blink.Controls.TextInput as TextInput
import qualified Blink.Controls.ToggleButton as ToggleButton
import qualified Blink.Controls.ToggleGroup as ToggleGroup
import qualified Blink.Controls.Table as Table
import qualified Blink.Controls.Tree as Tree

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
      , Image.defaultStyleEntries p
      , Label.defaultStyleEntries p
      , ToggleGroup.defaultStyleEntries p
      , ScrollBar.defaultStyleEntries p
      , ScrollPanel.defaultStyleEntries p
      , List.defaultStyleEntries p
      , Tree.defaultStyleEntries p
      , Table.defaultStyleEntries p
      , Menu.defaultStyleEntries p
      , MenuButton.defaultStyleEntries p
      , MenuBar.defaultStyleEntries p
      , [ (iconStyleKey, (zeroMetrics, iconStyle p)) ]
      ])
  , themeDefaultStyle = (controlMetrics, buttonStyle AlignCenter p)
  }
