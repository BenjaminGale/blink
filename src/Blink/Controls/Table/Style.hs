{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Table.Style

The default look for a 'Blink.Controls.Table.table''s header row, and
its registration in 'Blink.Style.Defaults.defaultTheme'. Needed at all
only because 'Blink.Controls.Control.defaultControlConfig' resolves an
unset 'Blink.Controls.Control.ccStyleKey' to
'Blink.Style.Defaults.defaultTheme''s boxed-control fallback look --
without this entry, the header would draw with a button's full chrome
instead of a header that reads distinctly from the rows beneath it.
-}
module Blink.Controls.Table.Style
  ( tableHeaderStyleKey
  , tableColumnDividerStyleKey
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (uniform)
import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (flatRowMetrics, transparent)

-- | The 'StyleKey' each of a 'Blink.Controls.Table.table''s header
-- cells resolves its style from unless overridden via
-- 'Blink.Controls.Control.style'.
tableHeaderStyleKey :: StyleKey e
tableHeaderStyleKey = Class "table-header"

-- | The 'StyleKey' a 'Blink.Controls.Table.table''s draggable
-- column-resize handle resolves its style from. Distinct from
-- 'Blink.Controls.Divider.dividerStyleKey' -- that one's default 4px
-- margin would eat the handle's own few pixels of width, since a
-- control's chrome insets both its hit area and its content bounds by
-- margin before running.
tableColumnDividerStyleKey :: StyleKey e
tableColumnDividerStyleKey = Class "table-column-divider"

-- | A shaded strip, no border, tinted on hover -- reuses
-- 'flatRowMetrics' so its padding lines up with a row's own cells. A
-- resize handle (see 'tableColumnDividerStyleKey') between cells (see
-- 'Blink.Controls.Table.table') separates them instead of a border.
tableHeaderStyle :: Palette -> StyleSet
tableHeaderStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = paletteSurface p
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Nothing
      }
  , styleOverrides = Map.singleton CommonMouseOver (\s -> s { styleBackground = paletteSurfaceHover p })
  }

-- | No margin (unlike 'Blink.Controls.Divider.divider') -- the handle's
-- whole few-pixel width has to stay both hittable and drawable.
tableColumnDividerMetrics :: Metrics
tableColumnDividerMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

-- | A vertical line, 'paletteBorder' by default, tinted on hover the
-- same way a header cell is.
tableColumnDividerStyle :: Palette -> StyleSet
tableColumnDividerStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just (paletteBorder p)
      }
  , styleOverrides = Map.singleton CommonMouseOver (\s -> s { styleBorderColour = Just (paletteSurfaceHover p) })
  }

-- | This control's own entries in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (tableHeaderStyleKey, (flatRowMetrics, tableHeaderStyle p))
  , (tableColumnDividerStyleKey, (tableColumnDividerMetrics, tableColumnDividerStyle p))
  ]
