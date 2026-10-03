{-# LANGUAGE OverloadedStrings #-}
-- | A checkbox: a glyph and a caption toggled together as one control.
-- Built on 'Blink.Controls.ToggleButton.iconToggle' -- see "Blink.Controls.ToggleButton" for how it and every
-- other toggle-like control fit together. A leaf: nothing derives from it,
-- so it has no 'Blink.Controls.Control.ControlConfig'\/'Blink.Controls.Control.ControlInteraction'-style pair of its own
-- beyond 'ToggleConfig'\/'Blink.Controls.ToggleButton.ToggleInteraction', which already have every field
-- it needs.
module Blink.Controls.Checkbox
  ( checkbox
  , checkboxStyleKey
    -- * Style
  , defaultStyleEntries
  ) where

import Blink.Controls.Control
import Blink.Controls.ToggleButton
  (IconToggleConfig (..), ToggleConfig (..), defaultGlyphToggleConfig, iconToggle)
import Blink.Controls.Style (flatRowMetrics, flatRowStyle)
import Blink.Element (Element)
import Blink.Style

-- | A checkbox: a box-plus-tick icon (see 'Blink.Controls.ToggleButton.isSelected'), beside a caption set via 'Blink.Controls.Label.text', toggled
-- together as one control -- clicking either the box or the caption
-- activates it, the same as 'Blink.Controls.ToggleButton.toggleButton'. Flips
-- every time it's activated; see 'Blink.Controls.ToggleButton.onSelectedChanged' for reacting to it.
-- Defaults to sizing itself to its glyph, caption and the space after it
-- on both axes, the same as 'Blink.Controls.Label.label'; override with
-- 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
checkbox :: Ord e => e -> [Attribute (ToggleConfig e msg)] -> Element e msg
checkbox eid attrs = iconToggle eid IconToggleConfig
  { itToggle         = (resolve (defaultGlyphToggleConfig checkboxStyleKey) attrs) { tgcNext = not }
  , itIconWidth      = 28
  , itGap            = 6
    -- Keeps the box off the caption and the control's own bounds.
  , itIconInset      = 2
    -- Matches the space the box leaves on its left (the column's centring
    -- plus the icon's own transparent margin), so the control looks evenly
    -- padded.
  , itTrailingSpace  = 5
  , itSelectedIcon   = "assets/icons/check_box.svg"
  , itUnselectedIcon = "assets/icons/check_box_outline_blank.svg"
  }

-- * Style

-- | The 'StyleKey' 'Blink.Controls.Checkbox.checkbox' resolves its
-- style from unless overridden via 'Blink.Controls.Control.style'.
checkboxStyleKey :: StyleKey e
checkboxStyleKey = Class "checkbox"

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (checkboxStyleKey, (flatRowMetrics, flatRowStyle p)) ]
