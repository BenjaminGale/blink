{- |
Module: Blink.Style.Defaults

A ready-made 'Theme' for every built-in control, so an app gets a
coherent, usable look out of the box just by supplying a 'Palette' --
see 'defaultTheme'.

Each control shape below is also exported on its own
('buttonStyle', 'flatRowStyle', 'progressBarStyle', 'sliderStyle', 'labelStyle'), so an
app that wants the built-in look for every control /except/ one state on
one control doesn't have to rebuild a whole 'StyleSet' by hand -- take the
built-in one and replace a single entry in its sparse 'styleOverrides'
map:

@
myButtonStyle :: StyleSet
myButtonStyle = (buttonStyle AlignCenter myPalette)
  { styleOverrides = Map.insert CommonMouseOver
      (\\s -> s { styleBorderColour = Just myColour })
      (styleOverrides (buttonStyle AlignCenter myPalette))
  }
@

Register it under whichever 'StyleKey' the control resolves to (e.g.
'Blink.Controls.Button.buttonStyleKey', or a different 'Class'\/'ElementId'
passed via 'Blink.Controls.Control.style') in 'themeElementStyles'.
-}
module Blink.Style.Defaults
  ( defaultTheme
  , buttonStyle
  , flatRowStyle
  , progressBarStyle
  , sliderStyle
  , dividerStyle
  , labelStyle
  ) where

import qualified Data.Map.Strict as Map

import Blink.Controls.Button (buttonStyleKey)
import Blink.Controls.Checkbox (checkboxStyleKey)
import Blink.Controls.Divider (dividerStyleKey)
import Blink.Controls.Label (labelStyleKey)
import Blink.Controls.ProgressBar (progressBarStyleKey)
import Blink.Controls.RadioButton (radioButtonStyleKey)
import Blink.Controls.Slider (sliderStyleKey)
import Blink.Controls.TextInput (textInputStyleKey)
import Blink.Controls.Toggle (toggleButtonStyleKey, toggleChecked)
import Blink.Geometry (uniform)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style

-- | Fully transparent -- used, like 'app/Theme.hs's @invisibleBorder@,
-- as a border colour that's really invisible rather than as 'Nothing':
-- a control whose border becomes visible on another state (e.g.
-- 'FocusFocused') must keep a @Just@ border at rest too, or gaining a
-- real border colour would also change its measured size.
transparent :: Colour
transparent = RGBA 0 0 0 0

controlMetrics :: Metrics
controlMetrics = Metrics
  { metricsMargin      = uniform 3
  , metricsPadding     = uniform 6
  , metricsBorderEdges = uniformBorder 1
  }

flatRowMetrics :: Metrics
flatRowMetrics = Metrics
  { metricsMargin      = uniform 2
  , metricsPadding     = uniform 4
  , metricsBorderEdges = uniformBorder 1
  }

labelMetrics :: Metrics
labelMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 6
  , metricsBorderEdges = noBorder
  }

progressBarMetrics :: Metrics
progressBarMetrics = Metrics
  { metricsMargin      = uniform 3
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

dividerMetrics :: Metrics
dividerMetrics = Metrics
  { metricsMargin      = uniform 4
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

-- | A bordered-box control style: background/border step through
-- hover/press/focus/disabled, with a bold accent fill both on press and
-- while selected (see 'Blink.Controls.Toggle.toggleChecked'). Used for
-- buttons, toggle buttons, and (left-aligned) text inputs.
buttonStyle :: TextAlign -> Palette -> StyleSet
buttonStyle align p = StyleSet
  { styleBase = Style
      { styleBackground   = paletteSurface p
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = align
      , styleBorderColour = Just (paletteBorder p)
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover p, styleBorderColour = Just (paletteBorderHover p) })
      , (CommonPressed,   \s -> s { styleBackground = paletteAccent p, styleTextColour = paletteTextOnAccent p, styleBorderColour = Just (paletteAccent p) })
      , (CommonDisabled,  \s -> s { styleBackground = paletteSurfaceDisabled p, styleTextColour = paletteTextMuted p, styleBorderColour = Just (paletteBorder p) })
      , (FocusFocused,    \s -> s { styleBorderColour = Just (paletteFocusRing p) })
      , (toggleChecked,   \s -> s { styleBackground = paletteAccent p, styleTextColour = paletteTextOnAccent p, styleBorderColour = Just (paletteAccent p) })
      ]
  }

-- | A flat, mostly-invisible row style: no background or border
-- normally, just a hover tint, a focus ring, and an accent tint while
-- selected, so it reads as a plain row rather than a button. Used for
-- checkboxes and radio buttons.
flatRowStyle :: Palette -> StyleSet
flatRowStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just transparent
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover p })
      , (CommonPressed,   \s -> s { styleBackground = paletteSurfaceHover p })
      , (CommonDisabled,  \s -> s { styleTextColour = paletteTextMuted p })
      , (FocusFocused,    \s -> s { styleBorderColour = Just (paletteFocusRing p) })
      , (toggleChecked,   \s -> s { styleBackground = paletteAccent p })
      ]
  }

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

-- | A slider's groove\/fill\/thumb style: transparent background,
-- 'paletteBorder' for the groove (drawn via 'styleBorderColour'),
-- 'paletteAccent' for the filled track and thumb (drawn via
-- 'styleTextColour').
sliderStyle :: Palette -> StyleSet
sliderStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteAccent p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just (paletteBorder p)
      }
  , styleOverrides = Map.singleton CommonDisabled (\s -> s { styleTextColour = paletteTextMuted p })
  }

-- | A divider's line style: transparent background, 'paletteBorder' for
-- the line itself (drawn via 'styleBorderColour', same as
-- 'Blink.Controls.Slider.Slider's groove).
dividerStyle :: Palette -> StyleSet
dividerStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just (paletteBorder p)
      }
  , styleOverrides = Map.empty
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

-- | A complete 'Theme' for every built-in control, built entirely from
-- @p@ -- registers each control's default 'StyleKey' (see each control
-- module's own @*StyleKey@, e.g. 'Blink.Controls.Button.buttonStyleKey')
-- with its shape above. Works for any element type @e@ since every entry
-- is 'Class'-keyed, never 'ElementId'-keyed. 'themeDefaultStyle' falls
-- back to the boxed-control look.
defaultTheme :: Ord e => Palette -> Theme e
defaultTheme p = Theme
  { themeElementStyles = Map.fromList
      [ (buttonStyleKey,       (controlMetrics,     buttonStyle AlignCenter p))
      , (toggleButtonStyleKey, (controlMetrics,     buttonStyle AlignCenter p))
      , (checkboxStyleKey,     (flatRowMetrics,     flatRowStyle p))
      , (radioButtonStyleKey,  (flatRowMetrics,     flatRowStyle p))
      , (textInputStyleKey,    (controlMetrics,     buttonStyle AlignLeft p))
      , (progressBarStyleKey,  (progressBarMetrics, progressBarStyle p))
      , (sliderStyleKey,       (progressBarMetrics, sliderStyle p))
      , (dividerStyleKey,      (dividerMetrics,     dividerStyle p))
      , (labelStyleKey,        (labelMetrics,       labelStyle p))
      ]
  , themeDefaultStyle = (controlMetrics, buttonStyle AlignCenter p)
  }
