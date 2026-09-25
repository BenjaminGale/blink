{-# LANGUAGE OverloadedStrings #-}
-- | A radio button: a glyph and a caption selected together as one control.
-- Built on 'Blink.Controls.ToggleButton.iconToggle' -- see "Blink.Controls.ToggleButton" for how it and every
-- other toggle-like control fit together. A leaf: nothing derives from it,
-- so it has no 'Blink.Controls.Control.ControlConfig'\/'Blink.Controls.Control.ControlInteraction'-style pair of its own
-- beyond 'ToggleConfig'\/'Blink.Controls.ToggleButton.ToggleInteraction', which already have every field
-- it needs.
module Blink.Controls.RadioButton
  ( radioButton
  , radioButtonStyleKey
    -- * Style
  , defaultStyleEntries
  ) where

import Blink.Controls.Control
import Blink.Controls.ToggleButton
  (IconToggleConfig (..), ToggleConfig (..), defaultGlyphToggleConfig, iconToggle)
import Blink.Controls.Style (flatRowMetrics, flatRowStyle)
import Blink.Element (Element)
import Blink.Style

-- | A radio button: a glyph showing whether it's currently selected (see
-- 'Blink.Controls.ToggleButton.isSelected'), beside a caption set via
-- 'Blink.Controls.Label.text', selected together as one control --
-- clicking either the glyph or the caption activates it, the same as
-- 'Blink.Controls.ToggleButton.toggleButton'. Unlike a 'Blink.Controls.ToggleButton.toggleButton'
-- or 'Blink.Controls.Checkbox.checkbox', activating it never deselects it
-- -- only ever moves it from unselected to selected, since a radio button
-- gives up selection by a sibling in its group being selected instead,
-- never by being clicked again itself. See
-- 'Blink.Controls.ToggleButton.onSelectedChanged' for reacting to it. Defaults to
-- sizing itself to its own glyph-plus-caption content on both axes, the
-- same as 'Blink.Controls.Label.label'; override with
-- 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
radioButton :: Ord e => e -> [Attribute (ToggleConfig e msg)] -> Element e msg
radioButton eid attrs = iconToggle eid IconToggleConfig
  { itToggle         = (resolve (defaultGlyphToggleConfig radioButtonStyleKey) attrs) { tgcNext = const True }
  , itIconWidth      = 20
  , itGap            = 6
  , itIconInset      = 0
  , itTrailingSpace  = 0
  , itSelectedIcon   = "assets/icons/radio_button_checked.svg"
  , itUnselectedIcon = "assets/icons/radio_button_unchecked.svg"
  }

-- * Style

-- | The 'StyleKey' 'Blink.Controls.RadioButton.radioButton' resolves
-- its style from unless overridden via 'Blink.Controls.Control.style'.
radioButtonStyleKey :: StyleKey e
radioButtonStyleKey = Class "radioButton"

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (radioButtonStyleKey, (flatRowMetrics, flatRowStyle p)) ]
