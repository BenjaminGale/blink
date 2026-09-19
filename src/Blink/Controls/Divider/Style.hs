{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Divider.Style

The default look for 'Blink.Controls.Divider.divider', and its
registration in 'Blink.Style.Defaults.defaultTheme'.
-}
module Blink.Controls.Divider.Style
  ( dividerStyleKey
  , dividerStyle
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (uniform)
import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (transparent)

-- | The 'StyleKey' 'Blink.Controls.Divider.divider' resolves its
-- style from unless overridden via 'Blink.Controls.Control.style'.
dividerStyleKey :: StyleKey e
dividerStyleKey = Class "divider"

dividerMetrics :: Metrics
dividerMetrics = Metrics
  { metricsMargin      = uniform 4
  , metricsPadding     = uniform 0
  }

-- | A divider's line style: transparent background, 'paletteBorder' for
-- the line itself (drawn via 'styleBorderColour', same as
-- 'Blink.Controls.Slider.Style.sliderStyle's groove). Width 0 so the
-- control's own automatic border draw never fires.
dividerStyle :: Palette -> StyleSet
dividerStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorder       = soloBorder (paletteBorder p) 0
      }
  , styleOverrides = Map.empty
  }

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (dividerStyleKey, (dividerMetrics, dividerStyle p)) ]
