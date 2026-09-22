{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.MenuButton.Style

The default look for the popup list 'Blink.Controls.MenuButton.menuButton'
opens, and its registration in 'Blink.Style.Defaults.defaultTheme'. The
trigger button itself resolves its look from
"Blink.Controls.ToggleButton.Style" instead -- see
'Blink.Controls.MenuButton.defaultMenuButtonConfig'.
-}
module Blink.Controls.MenuButton.Style
  ( menuButtonListStyleKey
  , defaultStyleEntries
  ) where

import Blink.Style
import Blink.Controls.Menu.Style (menuListMetrics)
import Blink.Controls.Style (containerStyle)

-- | The 'StyleKey' the item list resolves its own panel background\/border
-- from unless overridden via 'Blink.Controls.Control.style'. Uses
-- 'containerStyle' -- built for exactly this shape, a composite on
-- 'Blink.View.Focus.withFocusScope' that reads as focused whenever any item
-- inside it does.
menuButtonListStyleKey :: StyleKey e
menuButtonListStyleKey = Class "menuButtonList"

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (menuButtonListStyleKey, (menuListMetrics, containerStyle p)) ]
