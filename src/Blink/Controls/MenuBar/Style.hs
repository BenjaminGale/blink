{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.MenuBar.Style

The default look for 'Blink.Controls.MenuBar.menuBar' itself and the
dropdown list one of its labels opens, and their registration in
'Blink.Style.Defaults.defaultTheme'. Each label resolves its own look from
"Blink.Controls.ToggleButton.Style" instead, the same as
'Blink.Controls.MenuButton.menuButton''s own trigger.
-}
module Blink.Controls.MenuBar.Style
  ( menuBarStyleKey
  , menuBarListStyleKey
  , defaultStyleEntries
  ) where

import Blink.Style
import Blink.Controls.Style (containerStyle, controlMetrics, toggleGroupMetrics, toggleGroupStyle)

-- | The 'StyleKey' 'Blink.Controls.MenuBar.menuBar' resolves its own
-- container chrome from unless overridden via 'Blink.Controls.Control.style'.
-- Uses 'toggleGroupStyle' -- a plain wrapper around its labels, the same as
-- 'Blink.Controls.ToggleGroup.toggleButtonGroup'.
menuBarStyleKey :: StyleKey e
menuBarStyleKey = Class "menuBar"

-- | The 'StyleKey' an open dropdown list resolves its own panel
-- background\/border from unless overridden via 'Blink.Controls.Control.style'.
-- Uses 'containerStyle' -- the same shape
-- 'Blink.Controls.MenuButton.menuButtonListStyleKey' resolves to.
menuBarListStyleKey :: StyleKey e
menuBarListStyleKey = Class "menuBarList"

-- | This control's entries in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (menuBarStyleKey,     (toggleGroupMetrics, toggleGroupStyle p))
  , (menuBarListStyleKey, (controlMetrics, containerStyle p))
  ]
