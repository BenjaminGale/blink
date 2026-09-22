{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Menu.Style

The default look for the items in a dropdown menu, the spacing of the
dropdown itself, and the items' registration in
'Blink.Style.Defaults.defaultTheme'. Shared by "Blink.Controls.MenuButton"
and "Blink.Controls.MenuBar".
-}
module Blink.Controls.Menu.Style
  ( menuItemStyleKey
  , menuListMetrics
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Controls.Style (transparent)
import Blink.Geometry (Insets (..), uniform)
import Blink.Rendering (TextAlign (..))
import Blink.Style

-- | The 'StyleKey' each item in a dropdown menu resolves its look from,
-- unless overridden via 'Blink.Controls.Control.style' in its attributes.
menuItemStyleKey :: StyleKey e
menuItemStyleKey = Class "menuItem"

-- | The spacing of a dropdown menu's panel.
menuListMetrics :: Metrics
menuListMetrics = Metrics
  { metricsMargin  = uniform 0
  , metricsPadding = Insets { topInset = 4, rightInset = 0, bottomInset = 4, leftInset = 0 }
  }

menuItemMetrics :: Metrics
menuItemMetrics = Metrics
  { metricsMargin  = uniform 0
  , metricsPadding = Insets { topInset = 4, rightInset = 12, bottomInset = 4, leftInset = 12 }
  }

menuItemStyle :: Palette -> StyleSet
menuItemStyle p = StyleSet
  { styleBase = Style
      { styleBackground = transparent
      , styleTextColour = paletteTextPrimary p
      , styleTextAlign  = AlignLeft
      , styleBorder     = []
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover p })
      , (CommonPressed,   \s -> s { styleBackground = paletteSurfaceHover p })
      , (CommonDisabled,  \s -> s { styleTextColour = paletteTextMuted p })
      , (FocusFocused,    \s -> s { styleBackground = paletteSurfaceHover p })
      ]
  }

-- | This module's entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (menuItemStyleKey, (menuItemMetrics, menuItemStyle p)) ]
