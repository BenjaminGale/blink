{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Label.Style

The default look for 'Blink.Controls.Label.label', and its
registration in 'Blink.Style.Defaults.defaultTheme'.
-}
module Blink.Controls.Label.Style
  ( labelStyleKey
  , labelStyle
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (uniform)
import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (transparent)

-- | The 'StyleKey' 'Blink.Controls.Label.label' resolves its style
-- from unless overridden via 'Blink.Controls.Control.style'.
labelStyleKey :: StyleKey e
labelStyleKey = Class "label"

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

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (labelStyleKey, (labelMetrics, labelStyle p)) ]
