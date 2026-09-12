{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Image.Style

The default look for 'Blink.Controls.Image.image', and its registration
in 'Blink.Style.Defaults.defaultTheme'.
-}
module Blink.Controls.Image.Style
  ( imageStyleKey
  , imageStyle
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (uniform)
import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (transparent)

-- | The 'StyleKey' 'Blink.Controls.Image.image' resolves its style from
-- unless overridden via 'Blink.Controls.Control.style'.
imageStyleKey :: StyleKey e
imageStyleKey = Class "image"

-- | No margin\/padding\/border of its own -- an image draws itself with
-- no chrome by default, the same reasoning as
-- 'Blink.Controls.Style.toggleGroupMetrics'.
imageMetrics :: Metrics
imageMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

-- | Fully transparent and borderless -- an app that wants a border or
-- background around an image overrides this via 'Blink.Controls.Control.style',
-- the same as any other control.
imageStyle :: Palette -> StyleSet
imageStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Nothing
      }
  , styleOverrides = Map.empty
  }

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (imageStyleKey, (imageMetrics, imageStyle p)) ]
