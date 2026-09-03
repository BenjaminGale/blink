{-# LANGUAGE OverloadedStrings #-}
-- | The sample app's theme.
--
-- Both 'lightTheme' and 'darkTheme' are built from the library's
-- 'Blink.Style.Defaults.defaultTheme', each fed a 'Palette' sampled from a
-- reference screenshot ('lightPalette'\/'darkPalette') -- see
-- 'withStatusBar' for the one entry added on top of what 'defaultTheme'
-- registers ('StatusBar' is an app-specific 'ElementId', not a built-in
-- control class, so 'defaultTheme' can't register it itself).
module Theme
  ( ControlId (..)
  , lightTheme
  , darkTheme
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry
import Blink.Rendering
import Blink.Style
import Blink.Style.Defaults (defaultTheme)

data ControlId = Label
             | StatusBar
             | DarkModeCheckbox
             | EditingCheckbox
             | ClickButton
             | ResetButton
             | ToggleCtl
             | RadioCtl Int
             | TextInputCtl
             | PasswordInputCtl
             | AnimateCheckbox
             | ProgressCtl
             | SliderCtl
             | DividerCtl
             | ButtonsDividerCtl
  deriving (Eq, Ord)

-- | Colours sampled from a light-mode reference screenshot (an inspector
-- panel).
lightPalette :: Palette
lightPalette = Palette
  { paletteAccent          = RGBA 0.290 0.553 0.941 1  -- #4A8DF0
  , paletteFocusRing       = RGBA 0.290 0.553 0.941 1  -- #4A8DF0
  , paletteSurface         = RGBA 0.949 0.949 0.957 1  -- #F2F2F4
  , paletteSurfaceHover    = RGBA 0.906 0.906 0.918 1  -- #E7E7EA
  , paletteSurfaceDisabled = RGBA 0.969 0.969 0.973 1  -- #F7F7F8
  , paletteTextPrimary     = RGBA 0.231 0.231 0.251 1  -- #3B3B40
  , paletteTextMuted       = RGBA 0.663 0.667 0.682 1  -- #A9AAAE
  , paletteTextOnAccent    = RGBA 1.0   1.0   1.0   1  -- #FFFFFF
  , paletteBorder          = RGBA 0.824 0.827 0.839 1  -- #D2D3D6
  , paletteBorderHover     = RGBA 0.725 0.729 0.745 1  -- #B9BABE
  }

-- | Colours sampled from a dark-mode reference screenshot (a control-states
-- demo panel).
darkPalette :: Palette
darkPalette = Palette
  { paletteAccent          = RGBA 0.239 0.435 0.941 1  -- #3D6FF0
  , paletteFocusRing       = RGBA 0.239 0.435 0.941 1  -- #3D6FF0
  , paletteSurface         = RGBA 0.141 0.161 0.220 1  -- #242938
  , paletteSurfaceHover    = RGBA 0.180 0.204 0.267 1  -- #2E3444
  , paletteSurfaceDisabled = RGBA 0.118 0.137 0.188 1  -- #1E2330
  , paletteTextPrimary     = RGBA 0.910 0.918 0.941 1  -- #E8EAF0
  , paletteTextMuted       = RGBA 0.482 0.514 0.592 1  -- #7B8397
  , paletteTextOnAccent    = RGBA 1.0   1.0   1.0   1  -- #FFFFFF
  , paletteBorder          = RGBA 0.200 0.231 0.302 1  -- #333B4D
  , paletteBorderHover     = RGBA 0.290 0.329 0.408 1  -- #4A5468
  }

statusBarMetrics :: Metrics
statusBarMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = BorderEdges { edgeTop = 1, edgeRight = 0, edgeBottom = 0, edgeLeft = 0 }
  }

-- | Inserts the status bar's look -- an 'ElementId'-keyed entry, not a
-- built-in control class, so 'Blink.Style.Defaults.defaultTheme' doesn't
-- (and can't) register it itself.
withStatusBar :: Palette -> Theme ControlId -> Theme ControlId
withStatusBar p thm = thm
  { themeElementStyles = Map.insert (ElementId StatusBar) (statusBarMetrics, style) (themeElementStyles thm) }
  where
    style = StyleSet
      { styleBase = Style
          { styleBackground   = RGBA 0 0 0 0
          , styleTextColour   = paletteTextPrimary p
          , styleTextAlign    = AlignLeft
          , styleBorderColour = Just (paletteBorder p)
          }
      , styleOverrides = Map.empty
      }

lightTheme :: Theme ControlId
lightTheme = withStatusBar lightPalette (defaultTheme lightPalette)

darkTheme :: Theme ControlId
darkTheme = withStatusBar darkPalette (defaultTheme darkPalette)
