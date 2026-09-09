{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Tree.Style

The default look for a 'Blink.Controls.Tree.tree' row's own chevron, and
its registration in 'Blink.Style.Defaults.defaultTheme'. Needed at all
only because 'Blink.Controls.Control.defaultControlConfig' resolves an
unset 'Blink.Controls.Control.ccStyleKey' to
'Blink.Style.Defaults.defaultTheme''s boxed-control fallback look --
without this entry, the chevron would draw with a button's full
border\/background chrome instead of sitting flush in its narrow column.
-}
module Blink.Controls.Tree.Style
  ( treeChevronStyleKey
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (uniform)
import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (transparent)

-- | The 'StyleKey' a 'Blink.Controls.Tree.tree' row's chevron resolves
-- its style from unless overridden via 'Blink.Controls.Control.style'.
treeChevronStyleKey :: StyleKey e
treeChevronStyleKey = Class "tree-chevron"

-- | No margin\/padding\/border -- the chevron already sits in a fixed,
-- narrow column 'Blink.Controls.Tree.tree' reserves for it, so any
-- chrome inset would just crowd its glyph.
treeChevronMetrics :: Metrics
treeChevronMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

-- | A plain, transparent, centred glyph with no border -- the same
-- shape 'Blink.Controls.Label.Style.labelStyle' has, just with no
-- padding of its own.
treeChevronStyle :: Palette -> StyleSet
treeChevronStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignCenter
      , styleBorderColour = Nothing
      }
  , styleOverrides = Map.singleton CommonDisabled (\s -> s { styleTextColour = paletteTextMuted p })
  }

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (treeChevronStyleKey, (treeChevronMetrics, treeChevronStyle p)) ]
