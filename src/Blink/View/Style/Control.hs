{- |
Module: Blink.View.Style.Control

The shape vocabulary shared by more than one built-in control -- the
"bordered box" ('buttonStyle') and "flat row" ('flatRowStyle') looks, the
track look ('sliderStyle') shared by a slider and a scrollbar's own
track, and the plain wrapper look ('toggleGroupStyle') shared by a toggle
group, a radio group, and a scrollbar's own container -- plus the
'Metrics' each pairs with in 'Blink.View.Style.Defaults.defaultTheme'.

A shape used by exactly one control lives in that control's own
@Blink.View.Style.\<Control\>@ module instead (e.g.
"Blink.View.Style.ProgressBar", "Blink.View.Style.Divider",
"Blink.View.Style.Label") -- this module is only for shapes more than one
control resolves to. 'buttonStyle' is also 'Blink.View.Style.Defaults.defaultTheme's
'Blink.View.Style.themeDefaultStyle' fallback, making it the library's
one universal default look.
-}
module Blink.View.Style.Control
  ( transparent
  , controlMetrics
  , flatRowMetrics
  , progressBarMetrics
  , toggleGroupMetrics
  , buttonStyle
  , flatRowStyle
  , sliderStyle
  , toggleGroupStyle
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (uniform)
import Blink.View.Controls.ToggleButton (toggleChecked)
import Blink.View.Rendering (Colour (..), TextAlign (..))
import Blink.View.Style

-- | Fully transparent -- used, like 'app/Theme.hs's @invisibleBorder@,
-- as a border colour that's really invisible rather than as 'Nothing':
-- a control whose border becomes visible on another state (e.g.
-- 'FocusFocused') must keep a @Just@ border at rest too, or gaining a
-- real border colour would also change its measured size.
transparent :: Colour
transparent = RGBA 0 0 0 0

-- | Paired with 'buttonStyle' -- a bordered box with visible margin and
-- padding. Used for buttons, toggle buttons, text inputs, and a
-- scrollbar's own buttons.
controlMetrics :: Metrics
controlMetrics = Metrics
  { metricsMargin      = uniform 3
  , metricsPadding     = uniform 6
  , metricsBorderEdges = uniformBorder 1
  }

-- | Paired with 'flatRowStyle' -- a bordered row, tighter than
-- 'controlMetrics'. Used for checkboxes and radio buttons.
flatRowMetrics :: Metrics
flatRowMetrics = Metrics
  { metricsMargin      = uniform 2
  , metricsPadding     = uniform 4
  , metricsBorderEdges = uniformBorder 1
  }

-- | Shared by 'Blink.View.Style.ProgressBar', 'Blink.View.Style.Slider',
-- and 'Blink.View.Style.ScrollBar's track -- despite the name, this is
-- the generic track metrics, not something owned by the progress bar.
progressBarMetrics :: Metrics
progressBarMetrics = Metrics
  { metricsMargin      = uniform 3
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

-- | No margin\/padding\/border of its own -- a
-- 'Blink.View.Controls.ToggleGroup.toggleButtonGroup'\/'Blink.View.Controls.ToggleGroup.radioButtonGroup'
-- is just a plain wrapper around its items; any chrome belongs on the
-- items themselves ('Blink.View.Style.Button.buttonStyleKey'\/'Blink.View.Style.RadioButton.radioButtonStyleKey'),
-- not doubled up on their container. Shared by
-- 'Blink.View.Style.ScrollBar's own outer container for the same reason.
toggleGroupMetrics :: Metrics
toggleGroupMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

-- | A bordered-box control style: background/border step through
-- hover/press/focus/disabled, with a bold accent fill both on press and
-- while selected (see 'Blink.View.Controls.ToggleButton.toggleChecked'). Used for
-- buttons, toggle buttons, text inputs, and a scrollbar's own buttons.
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
-- normally, just a hover tint and a focus ring, so it reads as a plain
-- row rather than a button. Used for checkboxes and radio buttons.
-- No 'toggleChecked' override -- the glyph itself (checkmark or filled
-- dot) already shows selected state, and overriding it here would mask
-- the hover/press tint above whenever a row is selected.
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
      ]
  }

-- | A track/fill style: transparent background, 'paletteBorder' for the
-- groove (drawn via 'styleBorderColour'), 'paletteAccent' for the filled
-- track and thumb (drawn via 'styleTextColour'). Used for a slider (which
-- is focusable by default, hence the focus ring below) and a scrollbar's
-- own track (which is 'Blink.View.Controls.Control.NotFocusable', so the
-- override just never triggers there).
sliderStyle :: Palette -> StyleSet
sliderStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteAccent p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Just (paletteBorder p)
      }
  , styleOverrides = Map.fromList
      [ (CommonDisabled, \s -> s { styleTextColour = paletteTextMuted p })
      , (FocusFocused,   \s -> s { styleBorderColour = Just (paletteFocusRing p) })
      ]
  }

-- | A plain, transparent, borderless style for a group's own container --
-- paired with 'toggleGroupMetrics' alongside it above. Shared by
-- 'Blink.View.Controls.ToggleGroup.toggleButtonGroup', 'Blink.View.Controls.ToggleGroup.radioButtonGroup',
-- and a scrollbar's own outer container; the items inside still resolve
-- their own look from 'buttonStyle'\/'flatRowStyle'.
toggleGroupStyle :: Palette -> StyleSet
toggleGroupStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorderColour = Nothing
      }
  , styleOverrides = Map.empty
  }
