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
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (flatRowMetrics)

-- | The 'StyleKey' each of a 'Blink.Controls.Table.table''s header
-- cells resolves its style from unless overridden via
-- 'Blink.Controls.Control.style'.
tableHeaderStyleKey :: StyleKey e
tableHeaderStyleKey = Class "table-header"

-- | A shaded strip, no border, tinted on hover -- reuses
-- 'flatRowMetrics' so its padding lines up with a row's own cells. A
-- 'Blink.Controls.Divider.divider' between cells (see
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

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (tableHeaderStyleKey, (flatRowMetrics, tableHeaderStyle p)) ]
