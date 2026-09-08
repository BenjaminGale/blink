{- |
Module: Blink.View.Style.Divider

The default look for 'Blink.View.Controls.Divider.divider', and its
registration in 'Blink.View.Style.Defaults.defaultTheme'.
-}
module Blink.View.Style.Divider
  ( dividerStyle
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (uniform)
import Blink.View.Controls.Divider (dividerStyleKey)
import Blink.View.Rendering (TextAlign (..))
import Blink.View.Style
import Blink.View.Style.Control (transparent)

dividerMetrics :: Metrics
dividerMetrics = Metrics
  { metricsMargin      = uniform 4
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

-- | A divider's line style: transparent background, 'paletteBorder' for
-- the line itself (drawn via 'styleBorderColour', same as
-- 'Blink.View.Style.Slider.sliderStyle's groove).
dividerStyle :: Palette -> StyleSet
dividerStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just (paletteBorder p)
      }
  , styleOverrides = Map.empty
  }

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (dividerStyleKey, (dividerMetrics, dividerStyle p)) ]
