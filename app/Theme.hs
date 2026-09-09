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
  , Page (..)
  , containerStyleKey
  , lightTheme
  , darkTheme
  ) where

import qualified Data.Map.Strict as Map

import Data.Text (Text)
import Blink.Geometry
import Blink.Controls.List (ListPart)
import Blink.Controls.ScrollBar (ScrollBarPart)
import Blink.Controls.ToggleGroup (ToggleGroupPart)
import Blink.Rendering
import Blink.Style
import Blink.Style.Defaults (containerStyle, defaultTheme)
import Blink.Controls.Divider.Style (dividerStyle)
import Blink.Controls.Style (controlMetrics)

-- | Which of the demo's sidebar-selected pages is showing.
data Page = ControlsPage | ScrollBarsPage | ContinuePage | ContainedPage | ListPage
  deriving (Eq, Ord, Show)

data ControlId = Label
             | FieldLabel ControlId
             | StatusBar
             | SidebarPageButton (ToggleGroupPart Page)
             | DarkModeCheckbox
             | EditingCheckbox
             | ClickButton
             | HoldButton
             | ResetButton
             | ToggleCtl
             | RadioOption (ToggleGroupPart Text)
             | TextInputCtl
             | PasswordInputCtl
             | AnimateCheckbox
             | SliderCtl
             | VScrollCtl ScrollBarPart
             | HScrollCtl ScrollBarPart
             | ContainedWrapRadio Int
             | ContainedRememberCheckbox
             | ContainedBefore
             | ContainedGroup
             | ContainedOption Int
             | ContainedAfter
             | ContinueBefore
             | ContinueGroup
             | ContinueSearchInput
             | ContinueClearButton
             | ContinueAfter
             | FruitList (ListPart Text)
             | GroceryList (ListPart Text)
  deriving (Eq, Ord)

-- | Colours sampled from a light-mode reference screenshot (an inspector
-- panel). 'paletteSurfaceHover' is pulled darker than the sampled value,
-- which was only 2 units from the page background (#E5E5EA) -- too close
-- to read as a hover cue on a checkbox or radio button, which shows that
-- colour directly rather than as a shift from an opaque resting fill.
lightPalette :: Palette
lightPalette = Palette
  { paletteAccent          = RGBA 0.290 0.553 0.941 1  -- #4A8DF0
  , paletteFocusRing       = RGBA 0.290 0.553 0.941 1  -- #4A8DF0
  , paletteSurface         = RGBA 0.949 0.949 0.957 1  -- #F2F2F4
  , paletteSurfaceHover    = RGBA 0.847 0.847 0.867 1  -- #D8D8DD
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
-- (and can't) register it itself. Reuses 'dividerStyle' for the look (a
-- plain line in 'paletteBorder' is exactly what a status bar's own top
-- rule wants), with its own 'Metrics' for the top-only border edge.
withStatusBar :: Palette -> Theme ControlId -> Theme ControlId
withStatusBar p thm = thm
  { themeElementStyles = Map.insert (ElementId StatusBar) (statusBarMetrics, dividerStyle p) (themeElementStyles thm) }

-- | The 'StyleKey' 'Continue'\/'Contained' resolve their own outer
-- container's chrome from (see 'UI.continueGroup'\/'UI.containedGroup')
-- -- a 'Class', not an 'ElementId', since both share this one look.
containerStyleKey :: StyleKey ControlId
containerStyleKey = Class "container"

-- | Inserts the 'FocusScope' container look -- 'containerStyle' paired
-- with 'controlMetrics' (same border width 'buttonStyle' uses, so
-- 'FocusFocused' has somewhere to draw its ring), under 'containerStyleKey'.
withContainer :: Palette -> Theme ControlId -> Theme ControlId
withContainer p thm = thm
  { themeElementStyles = Map.insert containerStyleKey (controlMetrics, containerStyle p) (themeElementStyles thm) }

lightTheme :: Theme ControlId
lightTheme = withContainer lightPalette (withStatusBar lightPalette (defaultTheme lightPalette))

darkTheme :: Theme ControlId
darkTheme = withContainer darkPalette (withStatusBar darkPalette (defaultTheme darkPalette))
