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
  , lightTheme
  , darkTheme
  ) where

import qualified Data.Map.Strict as Map

import Data.Text (Text)
import Blink.Geometry
import Blink.Rendering (TextAlign (..))
import Blink.Controls.List (ListPart)
import Blink.Controls.MenuBar (MenuBarPart)
import Blink.Controls.MenuButton (MenuButtonPart)
import Blink.Controls.ScrollPanel (ScrollPanelPart)
import Blink.Controls.ToggleGroup (ToggleGroupPart)
import Blink.Controls.Table (TablePart)
import Blink.Controls.Tree (TreePart)
import Blink.Controls.TreeTable (TreeTablePart)
import Blink.Style
import Blink.Style.Defaults (defaultTheme)
import Blink.Controls.Divider.Style (dividerStyle)
import Blink.Controls.Style (transparent)

-- | Which of the demo's sidebar-selected pages is showing.
data Page
  = ControlsPage | ListPage | TreePage | TablePage | TreeTablePage
  | BackgroundPage | ImagePage | BordersPage
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
             | FileMenuButton (MenuButtonPart Text)
             | DemoMenuBar (MenuBarPart Text Text)
             | TextInputCtl
             | PasswordInputCtl
             | AnimateCheckbox
             | SliderCtl
             | MainListScroll ScrollPanelPart
             | FruitList (ListPart Text)
             | GroceryList (ListPart Text)
             | LongList (ListPart Int)
             | FileTree (TreePart Text)
             | GroceryTable (TablePart Text)
             | FileSizeTreeTable (TreeTablePart Text)
             | BackgroundStartButton
             | LongListJumpButton
             | ImageFitWidthCheckbox
             | ImageFitWidthSlider
             | ImageFitHeightCheckbox
             | ImageFitHeightSlider
             | ImagePreserveRatioCheckbox
             | LayeredBorderSwatch
             | RoundedBorderSwatch
             | TabBorderSwatch
             | SaveChangesButton
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
  , paletteIcon            = RGBA 0.400 0.400 0.427 1  -- #66666D, softer than paletteTextPrimary
  , paletteIconHover       = RGBA 0.157 0.157 0.176 1  -- #28282D, darker on hover
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
  , paletteIcon            = RGBA 0.239 0.435 0.941 1  -- #3D6FF0, the accent colour, resting
  , paletteIconHover       = RGBA 0.478 0.616 0.965 1  -- #7A9DF6, lighter on hover
  }

statusBarMetrics :: Metrics
statusBarMetrics = Metrics
  { metricsMargin  = uniform 0
  , metricsPadding = uniform 0
  }

-- | Only the top edge visible -- a status bar's own top rule.
topOnly :: EdgeVisibility
topOnly = allEdgesVisible { edgeRightVisible = False, edgeBottomVisible = False, edgeLeftVisible = False }

-- | The colours from 'dividerStyle', with its width-0 border layer (see
-- its doc comment for why) swapped for a real, visible, top-only one.
statusBarStyle :: Palette -> StyleSet
statusBarStyle p = base { styleBase = (styleBase base) { styleBorder = topRule } }
  where
    base    = dividerStyle p
    topRule = map (\l -> l { layerVisible = topOnly }) (soloBorder (paletteBorder p) 1)

-- | Inserts the status bar's look -- an 'ElementId'-keyed entry, not a
-- built-in control class, so 'Blink.Style.Defaults.defaultTheme' doesn't
-- (and can't) register it itself.
withStatusBar :: Palette -> Theme ControlId -> Theme ControlId
withStatusBar p thm = thm
  { themeElementStyles = Map.insert (ElementId StatusBar) (statusBarMetrics, statusBarStyle p) (themeElementStyles thm) }

swatchMetrics :: Metrics
swatchMetrics = Metrics { metricsMargin = uniform 0, metricsPadding = uniform 0 }

-- | A swatch's plain, borderless base look, before 'swatchStyle' gives it
-- its own showcase border. Transparent background -- these are meant to
-- read as an outline sitting on the page, not a filled tile.
swatchBase :: Palette -> StyleSet
swatchBase p = StyleSet
  { styleBase = Style
      { styleBackground = transparent
      , styleTextColour = paletteTextPrimary p
      , styleTextAlign  = AlignCenter
      , styleBorder     = noBorder
      }
  , styleOverrides = Map.empty
  }

swatchStyle :: Palette -> Border -> StyleSet
swatchStyle p border = base { styleBase = (styleBase base) { styleBorder = border } }
  where base = swatchBase p

-- | Rounded top corners only, e.g. for a tab-like open-bottom look.
topRounded :: Double -> CornerRadii
topRounded r = CornerRadii { radiusTopLeft = r, radiusTopRight = r, radiusBottomRight = 0, radiusBottomLeft = 0 }

-- | Paired with 'buttonShowcaseStyle' -- ordinary button padding, so its
-- caption has room to breathe inside the outer ring. Margin is exactly
-- the outer border layer's own reach (its 3px offset plus 2px width),
-- the minimum that keeps the focus ring inside the control's own
-- allocated bounds; padding is kept small since, at this control's
-- 48px height, margin and the border's own space already claim most of
-- it, leaving little room to spare before the caption's own box
-- disappears.
buttonShowcaseMetrics :: Metrics
buttonShowcaseMetrics = Metrics { metricsMargin = uniform 6, metricsPadding = uniform 4 }

-- | Recolours a single layer of a 'Border' stack by its index (outside in),
-- leaving every other layer untouched -- unlike 'withBorderColour', which
-- recolours the whole stack at once. This is what lets
-- 'buttonShowcaseStyle' give its focus ring a layer of its own: a hover\/
-- press override that only ever touches layer 0 can never stomp on
-- whatever a later 'FocusFocused' override wrote to layer 1, regardless of
-- the fixed Common-then-Focus-then-Custom fold order 'resolveStyle' uses.
-- (A single shared border, recoloured wholesale by every state as
-- 'Blink.Controls.Style.buttonStyle' and
-- 'Blink.Controls.ToggleButton.Style.toggleButtonStyleKey' both do, is
-- exactly how a checked-and-focused toggle button loses its focus ring:
-- whichever override runs last -- here, the checked state -- overwrites
-- the colour focus already set.)
recolourLayer :: Int -> Colour -> Border -> Border
recolourLayer i c = zipWith (\j l -> if j == i then l { layerColour = c } else l) [0 ..]

-- | A real, interactive 'Blink.Controls.Button.button''s style: a solid
-- inner border that carries the button's resting\/hover\/pressed look, plus
-- a second, wider-spaced outer layer reserved purely for the focus ring --
-- transparent at rest, so it takes no width in the metrics, and recoloured
-- to 'paletteFocusRing' only by 'FocusFocused'. Keeping that layer
-- dedicated to focus (via 'recolourLayer' rather than 'withBorderColour')
-- means hover\/press can never mask it, which is the fix the demo's
-- button\/description this style backs calls out.
buttonShowcaseStyle :: Palette -> StyleSet
buttonShowcaseStyle p = StyleSet
  { styleBase = Style
      { styleBackground = paletteSurface p
      , styleTextColour = paletteTextPrimary p
      , styleTextAlign  = AlignCenter
      , styleBorder     = base
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover p, styleBorder = recolourLayer 0 (paletteBorderHover p) (styleBorder s) })
      , (CommonPressed,   \s -> s { styleBackground = paletteAccent p, styleTextColour = paletteTextOnAccent p, styleBorder = recolourLayer 0 (paletteAccent p) (styleBorder s) })
      , (CommonDisabled,  \s -> s { styleBackground = paletteSurfaceDisabled p, styleTextColour = paletteTextMuted p, styleBorder = recolourLayer 0 (paletteBorder p) (styleBorder s) })
      , (FocusFocused,    \s -> s { styleBorder = recolourLayer 1 (paletteFocusRing p) (styleBorder s) })
      ]
  }
  where
    base =
      [ BorderLayer (paletteBorder p) 1 0 (uniformRadii 8) allEdgesVisible
      , BorderLayer transparent       2 3 (uniformRadii 11) allEdgesVisible
      ]

-- | Inserts the border-showcase page's entries -- three static swatches
-- (a stacked layered border, one with rounded corners, one with its
-- bottom edge open for a tab-like look) plus a real, styled button that
-- puts the same layering technique to work as a dedicated focus ring --
-- see "UI"'s @bordersPage@.
withBorderShowcase :: Palette -> Theme ControlId -> Theme ControlId
withBorderShowcase p thm = thm
  { themeElementStyles = Map.union (Map.fromList
      [ (ElementId LayeredBorderSwatch, (swatchMetrics, swatchStyle p layered))
      , (ElementId RoundedBorderSwatch, (swatchMetrics, swatchStyle p rounded))
      , (ElementId TabBorderSwatch,     (swatchMetrics, swatchStyle p tab))
      , (ElementId SaveChangesButton, (buttonShowcaseMetrics, buttonShowcaseStyle p))
      ]) (themeElementStyles thm)
  }
  where
    layered =
      [ BorderLayer (paletteBorder p) 2 0 (uniformRadii 0) allEdgesVisible
      , BorderLayer (paletteAccent p) 2 4 (uniformRadii 0) allEdgesVisible
      ]
    rounded = [ BorderLayer (paletteBorder p) 3 0 (uniformRadii 24) allEdgesVisible ]
    tab     = [ BorderLayer (paletteBorder p) 2 0 (topRounded 16) (allEdgesVisible { edgeBottomVisible = False }) ]

lightTheme :: Theme ControlId
lightTheme = withBorderShowcase lightPalette (withStatusBar lightPalette (defaultTheme lightPalette))

darkTheme :: Theme ControlId
darkTheme = withBorderShowcase darkPalette (withStatusBar darkPalette (defaultTheme darkPalette))
