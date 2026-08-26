{-# LANGUAGE OverloadedStrings #-}
-- | The sample app's theme.
--
-- = Style contract
--
-- Buttons and text inputs are a bordered box in every state:
-- 'styleBorderColour' is @Just@ even where nothing should be visibly drawn
-- — see 'invisibleBorder'. States that shouldn't show a border use it
-- instead of dropping the border outright, so a control's measured chrome
-- never changes across states and it doesn't visibly resize ("jump") when
-- hovered, focused, or disabled. Checkboxes and radio buttons
-- ('mkFlatRowStyle') follow the same never-drop-the-border rule but are
-- otherwise flat rows, not boxes, so they don't look like buttons.
--
-- A control's /selected/ look (a pressed 'toggleButton', a checked
-- checkbox, a picked radio button) comes from a
-- @Custom \"Toggle\" \"Checked\"@ override (see
-- 'Blink.Controls.Button.toggleChecked') registered directly on the
-- 'StyleSet' each toggle-style 'Class' resolves to below --
-- 'mkControlStyle' gives 'toggleButton' the same bold accent fill as its
-- pressed state; 'mkFlatRowStyle' gives 'checkbox'\/'radioButton' a
-- lighter accent tint appropriate to a flat row. This composes through
-- whichever 'Blink.Style.StyleKey' a control actually resolves to via
-- 'Blink.Controls.Control.style', unlike the old direct-'ElementId'
-- lookup this replaced.
module Theme
  ( Element (..)
  , lightTheme
  , darkTheme
  ) where

import qualified Data.Map.Strict as Map

import Blink.Controls.Button (toggleChecked)
import Blink.Geometry
import Blink.Rendering
import Blink.Style

data Element = Label
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
  deriving (Eq, Ord)

-- | The app's own palette: a library 'Palette' for the fields it covers,
-- plus a few extra tones ('apAccentDark'\/'apAccentLight' for the pressed\/
-- focused accent variants, separate input-surface tones, and the progress
-- bar's track colour) that the shared 'Palette' doesn't model.
data AppPalette = AppPalette
  { apBase :: Palette
  , apAccentDark :: Colour
  , apAccentLight :: Colour
  , apSurfaceInput :: Colour
  , apSurfaceInputHover :: Colour
  , apSurfaceInputDisabled :: Colour
  , apProgressTrack :: Colour
  }

lightPalette :: AppPalette
lightPalette = AppPalette
  { apBase = Palette
      { paletteAccent           = RGBA 0.102 0.435 0.831 1
      , paletteFocusRing        = RGBA 0.102 0.435 0.831 1
      , paletteSurface          = RGBA 0.878 0.878 0.898 1
      , paletteSurfaceHover     = RGBA 0.800 0.800 0.824 1
      , paletteSurfaceDisabled  = RGBA 0.898 0.898 0.910 1
      , paletteTextPrimary      = RGBA 0.11  0.11  0.12  1
      , paletteTextMuted        = RGBA 0.682 0.682 0.698 1
      , paletteTextOnAccent     = RGBA 1.0   1.0   1.0   1
      , paletteBorder           = RGBA 0.600 0.600 0.620 1
      , paletteBorderHover      = RGBA 0.400 0.400 0.420 1
      }
  , apAccentDark            = RGBA 0.071 0.306 0.584 1
  , apAccentLight           = RGBA 0.667 0.769 0.941 1
  , apSurfaceInput          = RGBA 1.0   1.0   1.0   1
  , apSurfaceInputHover     = RGBA 0.97  0.97  0.97  1
  , apSurfaceInputDisabled  = RGBA 0.95  0.95  0.95  1
  , apProgressTrack         = RGBA 0.878 0.878 0.898 1
  }

darkPalette :: AppPalette
darkPalette = AppPalette
  { apBase = Palette
      { paletteAccent           = RGBA 0.055 0.647 0.914 1  -- sky-500
      , paletteFocusRing        = RGBA 0.055 0.647 0.914 1  -- sky-500
      , paletteSurface          = RGBA 0.200 0.255 0.333 1  -- slate-700
      , paletteSurfaceHover     = RGBA 0.278 0.333 0.412 1  -- slate-600
      , paletteSurfaceDisabled  = RGBA 0.118 0.161 0.231 1  -- slate-800
      , paletteTextPrimary      = RGBA 0.945 0.961 0.976 1  -- slate-100
      , paletteTextMuted        = RGBA 0.580 0.639 0.722 1  -- slate-400
      , paletteTextOnAccent     = RGBA 1.0   1.0   1.0   1
      , paletteBorder           = RGBA 0.278 0.333 0.412 1  -- slate-600
      , paletteBorderHover      = RGBA 0.580 0.639 0.722 1  -- slate-400
      }
  , apAccentDark            = RGBA 0.012 0.412 0.631 1  -- sky-700
  , apAccentLight           = RGBA 0.027 0.349 0.522 1  -- sky-800, focused button bg
  , apSurfaceInput          = RGBA 0.118 0.161 0.231 1  -- slate-800
  , apSurfaceInputHover     = RGBA 0.200 0.255 0.333 1  -- slate-700
  , apSurfaceInputDisabled  = RGBA 0.059 0.090 0.165 1  -- slate-900
  , apProgressTrack         = RGBA 0.200 0.255 0.333 1  -- slate-700
  }

controlMargin :: Insets
controlMargin = uniform 3

controlPadding :: Insets
controlPadding = uniform 6

controlMetrics :: Metrics
controlMetrics = Metrics
  { metricsMargin      = controlMargin
  , metricsPadding     = controlPadding
  , metricsBorderEdges = uniformBorder 1
  }

flatRowMetrics :: Metrics
flatRowMetrics = Metrics
  { metricsMargin      = uniform 2
  , metricsPadding     = uniform 4
  , metricsBorderEdges = uniformBorder 1
  }

-- | An invisible border colour (zero alpha) -- see the module header for why
-- every bordered control keeps a border in every state instead of dropping
-- it, using this colour to hide it without shrinking the control's chrome.
invisibleBorder :: Colour
invisibleBorder = RGBA 0 0 0 0

-- | A bordered-box control style: background/border step through
-- hover/press/focus/disabled, with a bold accent fill both on press and
-- while selected (see the module header for 'toggleChecked').
mkControlStyle :: TextAlign -> AppPalette -> StyleSet
mkControlStyle align p = StyleSet
  { styleBase = Style
      { styleBackground   = paletteSurface (apBase p)
      , styleTextColour   = paletteTextPrimary (apBase p)
      , styleTextAlign    = align
      , styleBorderColour = Just (paletteBorder (apBase p))
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover (apBase p), styleBorderColour = Just (paletteBorderHover (apBase p)) })
      , (CommonPressed,   \s -> s { styleBackground = paletteAccent (apBase p), styleTextColour = paletteTextOnAccent (apBase p), styleBorderColour = Just (apAccentDark p) })
      , (CommonDisabled,  \s -> s { styleBackground = paletteSurfaceDisabled (apBase p), styleTextColour = paletteTextMuted (apBase p), styleBorderColour = Just (paletteBorder (apBase p)) })
      , (FocusFocused,    \s -> s { styleBackground = apAccentLight p, styleTextColour = paletteTextPrimary (apBase p), styleBorderColour = Just (paletteFocusRing (apBase p)) })
      , (toggleChecked,   \s -> s { styleBackground = paletteAccent (apBase p), styleTextColour = paletteTextOnAccent (apBase p), styleBorderColour = Just (apAccentDark p) })
      ]
  }

-- | A flat, mostly-invisible row style for checkboxes\/radio buttons: no
-- background or border normally, just a hover tint, a focus ring, and a
-- light accent tint while selected (see the module header), so they read
-- as plain rows rather than buttons.
mkFlatRowStyle :: AppPalette -> StyleSet
mkFlatRowStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = RGBA 0 0 0 0
      , styleTextColour   = paletteTextPrimary (apBase p)
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just invisibleBorder
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover (apBase p) })
      , (CommonPressed,   \s -> s { styleBackground = paletteSurfaceHover (apBase p) })
      , (CommonDisabled,  \s -> s { styleTextColour = paletteTextMuted (apBase p) })
      , (FocusFocused,    \s -> s { styleBorderColour = Just (paletteFocusRing (apBase p)) })
      , (toggleChecked,   \s -> s { styleBackground = apAccentLight p })
      ]
  }

mkTextInputStyle :: AppPalette -> StyleSet
mkTextInputStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = apSurfaceInput p
      , styleTextColour   = paletteTextPrimary (apBase p)
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just (paletteBorder (apBase p))
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = apSurfaceInputHover p, styleBorderColour = Just (paletteBorderHover (apBase p)) })
      , (CommonPressed,   \s -> s { styleBorderColour = Just (paletteAccent (apBase p)) })
      , (CommonDisabled,  \s -> s { styleBackground = apSurfaceInputDisabled p, styleTextColour = paletteTextMuted (apBase p), styleBorderColour = Just (paletteBorder (apBase p)) })
      , (FocusFocused,    \s -> s { styleBorderColour = Just (paletteAccent (apBase p)) })
      ]
  }

mkProgressBarStyle :: AppPalette -> StyleSet
mkProgressBarStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = apProgressTrack p
      , styleTextColour   = paletteAccent (apBase p)
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Nothing
      }
  , styleOverrides = Map.singleton CommonDisabled (\s -> s { styleTextColour = paletteTextMuted (apBase p) })
  }

mkLabelStyle :: AppPalette -> StyleSet
mkLabelStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = RGBA 0 0 0 0
      , styleTextColour   = paletteTextPrimary (apBase p)
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Nothing
      }
  , styleOverrides = Map.singleton CommonDisabled (\s -> s { styleTextColour = paletteTextMuted (apBase p) })
  }

labelMetrics :: Metrics
labelMetrics = Metrics { metricsMargin = uniform 0, metricsPadding = controlPadding, metricsBorderEdges = noBorder }

progressBarMetrics :: Metrics
progressBarMetrics = Metrics { metricsMargin = controlMargin, metricsPadding = uniform 0, metricsBorderEdges = noBorder }

mkStatusBarStyle :: AppPalette -> StyleSet
mkStatusBarStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = RGBA 0 0 0 0
      , styleTextColour   = paletteTextPrimary (apBase p)
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just (paletteBorder (apBase p))
      }
  , styleOverrides = Map.empty
  }

statusBarMetrics :: Metrics
statusBarMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = BorderEdges { edgeTop = 1, edgeRight = 0, edgeBottom = 0, edgeLeft = 0 }
  }

mkTheme :: AppPalette -> Theme Element
mkTheme p = Theme
  { themeElementStyles = Map.fromList
      [ (ElementId StatusBar,  (statusBarMetrics,   mkStatusBarStyle p))
      , (Class "button",       (controlMetrics,     mkControlStyle AlignCenter p))
      , (Class "toggleButton", (controlMetrics,     mkControlStyle AlignCenter p))
      , (Class "checkbox",     (flatRowMetrics,      mkFlatRowStyle p))
      , (Class "radioButton",  (flatRowMetrics,      mkFlatRowStyle p))
      , (Class "textInput",    (controlMetrics,     mkTextInputStyle p))
      , (Class "progressBar",  (progressBarMetrics, mkProgressBarStyle p))
      , (Class "label",        (labelMetrics,       mkLabelStyle p))
      ]
  , themeDefaultStyle = (controlMetrics, mkControlStyle AlignCenter p)
  }

lightTheme :: Theme Element
lightTheme = mkTheme lightPalette

darkTheme :: Theme Element
darkTheme = mkTheme darkPalette
