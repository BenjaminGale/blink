{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.ProgressBar.Style

The default look for 'Blink.Controls.ProgressBar.progressBar', and
its registration in 'Blink.Style.Defaults.defaultTheme'.
-}
module Blink.Controls.ProgressBar.Style
  ( progressBarStyleKey
  , progressBarStyle
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (progressBarMetrics)

-- | The 'StyleKey' 'Blink.Controls.ProgressBar.progressBar' resolves
-- its style from unless overridden via 'Blink.Controls.Control.style'.
progressBarStyleKey :: StyleKey e
progressBarStyleKey = Class "progressBar"

-- | A progress bar's track/fill style: 'paletteSurface' for the track
-- (background), 'paletteAccent' for the fill (drawn via
-- 'styleTextColour'), no border.
progressBarStyle :: Palette -> StyleSet
progressBarStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = paletteSurface p
      , styleTextColour   = paletteAccent p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Nothing
      }
  , styleOverrides = Map.singleton CommonDisabled (\s -> s { styleTextColour = paletteTextMuted p })
  }

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (progressBarStyleKey, (progressBarMetrics, progressBarStyle p)) ]
