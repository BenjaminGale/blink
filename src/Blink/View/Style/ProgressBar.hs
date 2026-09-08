{- |
Module: Blink.View.Style.ProgressBar

The default look for 'Blink.View.Controls.ProgressBar.progressBar', and
its registration in 'Blink.View.Style.Defaults.defaultTheme'.
-}
module Blink.View.Style.ProgressBar
  ( progressBarStyle
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map

import Blink.View.Controls.ProgressBar (progressBarStyleKey)
import Blink.View.Rendering (TextAlign (..))
import Blink.View.Style
import Blink.View.Style.Control (progressBarMetrics)

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

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (progressBarStyleKey, (progressBarMetrics, progressBarStyle p)) ]
