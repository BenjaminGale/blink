{- |
Module: Blink.View.Style.Label

The default look for 'Blink.View.Controls.Label.label', and its
registration in 'Blink.View.Style.Defaults.defaultTheme'.
-}
module Blink.View.Style.Label
  ( labelStyle
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (uniform)
import Blink.View.Controls.Label (labelStyleKey)
import Blink.View.Rendering (TextAlign (..))
import Blink.View.Style
import Blink.View.Style.Control (transparent)

labelMetrics :: Metrics
labelMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 6
  , metricsBorderEdges = noBorder
  }

-- | A plain, transparent label style with no border.
labelStyle :: Palette -> StyleSet
labelStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Nothing
      }
  , styleOverrides = Map.singleton CommonDisabled (\s -> s { styleTextColour = paletteTextMuted p })
  }

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (labelStyleKey, (labelMetrics, labelStyle p)) ]
