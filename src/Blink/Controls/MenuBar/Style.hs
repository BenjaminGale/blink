{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.MenuBar.Style

The default look for 'Blink.Controls.MenuBar.menuBar' itself, its own
labels, and the dropdown list one of them opens, plus their registration
in 'Blink.Style.Defaults.defaultTheme'.
-}
module Blink.Controls.MenuBar.Style
  ( menuBarStyleKey
  , menuBarLabelStyleKey
  , menuBarListStyleKey
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Controls.Style (containerStyle, controlMetrics, flatRowMetrics, transparent)
import Blink.Controls.ToggleButton.Style (toggleChecked)
import Blink.Geometry (uniform)
import Blink.Rendering (TextAlign (..))
import Blink.Style

-- | The 'StyleKey' 'Blink.Controls.MenuBar.menuBar' resolves its own
-- container chrome from unless overridden via 'Blink.Controls.Control.style'.
-- A flat strip -- background plus a bottom rule only, the same
-- top\/bottom-only-border idea the sample app's own status bar uses --
-- rather than a fully bordered box, so it reads as a toolbar sitting above
-- the rest of the window instead of a boxed-in panel.
menuBarStyleKey :: StyleKey e
menuBarStyleKey = Class "menuBar"

-- | The 'StyleKey' each of 'Blink.Controls.MenuBar.menuBar''s own labels
-- resolves its look from -- flat at rest (no visible border\/fill), tinted
-- on hover and while its own dropdown is open ('toggleChecked'), the same
-- pseudo-state 'Blink.Controls.ToggleButton.toggleBase' already puts in
-- 'Blink.Controls.Control.ccActiveStates'. Deliberately its own key rather
-- than 'Blink.Controls.ToggleButton.Style.toggleButtonStyleKey' -- a
-- top-level menu label reads as a plain menu-bar item, not a button.
menuBarLabelStyleKey :: StyleKey e
menuBarLabelStyleKey = Class "menuBarLabel"

-- | The 'StyleKey' an open dropdown list resolves its own panel
-- background\/border from unless overridden via 'Blink.Controls.Control.style'.
-- Uses 'containerStyle' -- the same shape
-- 'Blink.Controls.MenuButton.menuButtonListStyleKey' resolves to.
menuBarListStyleKey :: StyleKey e
menuBarListStyleKey = Class "menuBarList"

menuBarMetrics :: Metrics
menuBarMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 4
  , metricsBorderEdges = BorderEdges { edgeTop = 0, edgeRight = 0, edgeBottom = 1, edgeLeft = 0 }
  }

menuBarStyle :: Palette -> StyleSet
menuBarStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = paletteSurface p
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just (paletteBorder p)
      }
  , styleOverrides = Map.empty
  }

menuBarLabelStyle :: Palette -> StyleSet
menuBarLabelStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignCenter
      , styleBorderColour = Just transparent
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover p })
      , (CommonPressed,   \s -> s { styleBackground = paletteSurfaceHover p })
      , (toggleChecked,   \s -> s { styleBackground = paletteSurfaceHover p })
      , (FocusFocused,    \s -> s { styleBorderColour = Just (paletteFocusRing p) })
      ]
  }

-- | This control's entries in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (menuBarStyleKey,      (menuBarMetrics, menuBarStyle p))
  , (menuBarLabelStyleKey, (flatRowMetrics, menuBarLabelStyle p))
  , (menuBarListStyleKey,  (controlMetrics, containerStyle p))
  ]
