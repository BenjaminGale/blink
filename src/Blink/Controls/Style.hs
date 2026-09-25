{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Style

The shape vocabulary shared by more than one built-in control -- the
"bordered box" ('buttonStyle') and "flat row" ('flatRowStyle') looks, the
fill and thumb looks ('plainFillStyle', 'valueFillStyle', 'thumbStyle')
shared by the parts of a slider, scrollbar and progress bar, and the plain
wrapper look ('toggleGroupStyle') shared by a toggle
group, a radio group, and a scrollbar's own container -- plus the
'Metrics' each pairs with in 'Blink.Style.Defaults.defaultTheme'.
Also 'containerStyle', not registered by 'Blink.Style.Defaults.defaultTheme'
itself (no built-in control needs it) but exported the same way
'buttonStyle' is, for an app to register against its own composites built
on 'Blink.View.Focus.withFocusScope' -- see "Theme"'s @withStatusBar@-style
registration in the sample app.

A shape used by exactly one control lives in that control's own module
instead (e.g. "Blink.Controls.ProgressBar", "Blink.Controls.Divider",
"Blink.Controls.Label") -- this module is only for shapes more than one
control resolves to. 'buttonStyle' is also 'Blink.Style.Defaults.defaultTheme's
'Blink.Style.themeDefaultStyle' fallback, making it the library's
one universal default look.
-}
module Blink.Controls.Style
  ( transparent
  , controlMetrics
  , flatRowMetrics
  , progressBarMetrics
  , toggleGroupMetrics
  , buttonStyle
  , flatRowStyle
  , toggleGroupStyle
  , containerStyle
  , iconStyleKey
  , iconStyle
  , shade
  , plainFillStyle
  , valueFillStyle
  , thumbStyle
  ) where

import qualified Data.Map.Strict as Map

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

-- | Paired with 'buttonStyle' -- a bordered box with visible margin and
-- padding. Used for buttons, toggle buttons, text inputs, and a
-- scrollbar's own buttons.
controlMetrics :: Metrics
controlMetrics = Metrics
  { metricsMargin      = uniform 3
  , metricsPadding     = uniform 6
  }

-- | Paired with 'flatRowStyle' -- a bordered row, tighter than
-- 'controlMetrics'. Used for checkboxes and radio buttons.
flatRowMetrics :: Metrics
flatRowMetrics = Metrics
  { metricsMargin      = uniform 2
  , metricsPadding     = uniform 4
  }

-- | Shared by 'Blink.Controls.ProgressBar', 'Blink.Controls.Slider',
-- and 'Blink.Controls.ScrollBar's track -- despite the name, this is
-- the generic track metrics, not something owned by the progress bar.
progressBarMetrics :: Metrics
progressBarMetrics = Metrics
  { metricsMargin      = uniform 3
  , metricsPadding     = uniform 0
  }

-- | No margin\/padding\/border of its own -- a
-- 'Blink.Controls.ToggleGroup.toggleButtonGroup'\/'Blink.Controls.ToggleGroup.radioButtonGroup'
-- is just a plain wrapper around its items; any chrome belongs on the
-- items themselves ('Blink.Controls.Button.buttonStyleKey'\/'Blink.Controls.RadioButton.radioButtonStyleKey'),
-- not doubled up on their container. Shared by
-- 'Blink.Controls.ScrollBar's own outer container for the same reason.
toggleGroupMetrics :: Metrics
toggleGroupMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  }

-- | A bordered-box control style: background/border step through
-- hover/press/focus/disabled, with a bold accent fill on press. Used for
-- buttons, toggle buttons, text inputs, and a scrollbar's own buttons --
-- see "Blink.Controls.ToggleButton" for the extra accent fill a toggle
-- button adds on top while selected.
buttonStyle :: TextAlign -> Palette -> StyleSet
buttonStyle align p = StyleSet
  { styleBase = Style
      { styleBackground   = paletteSurface p
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = align
      , styleBorder       = soloBorder (paletteBorder p) 1
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover p, styleBorder = withBorderColour (paletteBorderHover p) (styleBorder s) })
      , (CommonPressed,   \s -> s { styleBackground = paletteAccent p, styleTextColour = paletteTextOnAccent p, styleBorder = withBorderColour (paletteAccent p) (styleBorder s) })
      , (CommonDisabled,  \s -> s { styleBackground = paletteSurfaceDisabled p, styleTextColour = paletteTextMuted p, styleBorder = withBorderColour (paletteBorder p) (styleBorder s) })
      , (FocusFocused,    \s -> s { styleBorder = withBorderColour (paletteFocusRing p) (styleBorder s) })
      ]
  }

-- | A flat, mostly-invisible row style: no background or border
-- normally, just a hover tint and a focus ring, so it reads as a plain
-- row rather than a button. Used for checkboxes and radio buttons.
-- No 'Blink.Controls.ToggleButton.toggleChecked' override -- the glyph itself (checkmark or filled
-- dot) already shows selected state, and overriding it here would mask
-- the hover/press tint above whenever a row is selected.
flatRowStyle :: Palette -> StyleSet
flatRowStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorder       = soloBorder transparent 1
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover p })
      , (CommonPressed,   \s -> s { styleBackground = paletteSurfaceHover p })
      , (CommonDisabled,  \s -> s { styleTextColour = paletteTextMuted p })
      , (FocusFocused,    \s -> s { styleBorder = withBorderColour (paletteFocusRing p) (styleBorder s) })
      ]
  }

-- | A plain, transparent, borderless style for a group's own container --
-- paired with 'toggleGroupMetrics' alongside it above. Shared by
-- 'Blink.Controls.ToggleGroup.toggleButtonGroup', 'Blink.Controls.ToggleGroup.radioButtonGroup',
-- and a scrollbar's own outer container; the items inside still resolve
-- their own look from 'buttonStyle'\/'flatRowStyle'. No 'FocusFocused'
-- override either -- unlike 'containerStyle' below, none of these three
-- containers ever holds keyboard focus itself (each is
-- 'Blink.Controls.Control.NotFocusable', fixed), so a ring here
-- would never actually draw.
toggleGroupStyle :: Palette -> StyleSet
toggleGroupStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorder       = noBorder
      }
  , styleOverrides = Map.empty
  }

-- | 'buttonStyle's boxed look with its background held fixed -- hover only
-- steps the border colour, never 'styleBackground', the way a container
-- that merely holds focusable children, rather than being one itself,
-- should read (no fill tint, no press fill). Keeps 'CommonDisabled' and
-- 'FocusFocused' too -- the latter is a state such a container does
-- reach: a composite built on 'Blink.View.Focus.withFocusScope' (unlike
-- 'toggleGroupStyle's wrappers above) reads as focused whenever any child
-- inside it does, so its own border still needs to answer that. Paired
-- with 'controlMetrics' (real border width, so both overrides have
-- something to draw into) rather than 'toggleGroupMetrics'.
containerStyle :: Palette -> StyleSet
containerStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = paletteSurface p
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorder       = soloBorder (paletteBorder p) 1
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBorder = withBorderColour (paletteBorderHover p) (styleBorder s) })
      , (CommonDisabled,  \s -> s { styleTextColour = paletteTextMuted p })
      , (FocusFocused,    \s -> s { styleBorder = withBorderColour (paletteFocusRing p) (styleBorder s) })
      ]
  }

-- | The 'StyleKey' a control resolves an icon's own tint from --
-- independently of whatever 'StyleKey' governs the row it sits in (e.g.
-- 'Blink.Controls.Checkbox.checkboxStyleKey'), so hovering the
-- icon specifically can recolour it without also recolouring that row's
-- caption text. A control resolves this itself (it isn't part of the
-- usual per-control 'Blink.Controls.Control.ccStyleKey'\/'Blink.View.currentStyle'
-- path), against whatever 'Blink.Style.VisualState's are relevant to the icon alone -- see
-- 'Blink.Controls.Checkbox.checkbox' for how.
iconStyleKey :: StyleKey e
iconStyleKey = Class "icon"

-- | Carries only a themed colour, via 'styleTextColour' -- 'paletteIcon'
-- at rest, 'paletteIconHover' while hovered, 'paletteTextMuted' while
-- disabled. No background\/border of its own since nothing about a
-- plain icon uses them.
iconStyle :: Palette -> StyleSet
iconStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteIcon p
      , styleTextAlign    = AlignLeft
      , styleBorder       = noBorder
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleTextColour = paletteIconHover p })
      , (CommonDisabled,  \s -> s { styleTextColour = paletteTextMuted p })
      ]
  }

-- | Darkens @c@'s RGB toward black by @factor@ (in @[0, 1]@; 1 leaves it
-- unchanged), leaving alpha alone. Used to shade a thumb on hover\/drag
-- without needing a dedicated theme colour for each -- see 'thumbStyle'.
shade :: Double -> Colour -> Colour
shade factor (RGBA r g b a) = RGBA (r * factor) (g * factor) (b * factor) a

-- | A flat fill in @colour@, for a part of a control that's only ever a
-- solid region (a groove, a divider's line). No state changes its look.
plainFillStyle :: Palette -> Colour -> StyleSet
plainFillStyle p colour = StyleSet
  { styleBase = Style
      { styleBackground = colour
      , styleTextColour = paletteTextPrimary p
      , styleTextAlign  = AlignLeft
      , styleBorder     = noBorder
      }
  , styleOverrides = Map.empty
  }

-- | 'plainFillStyle' for a part showing a value (a progress bar's or
-- slider's fill), muted while disabled.
valueFillStyle :: Palette -> Colour -> StyleSet
valueFillStyle p colour = (plainFillStyle p colour)
  { styleOverrides = Map.singleton CommonDisabled (\s -> s { styleBackground = paletteTextMuted p }) }

-- | A slider's or scrollbar's thumb: the accent colour, darkened on hover
-- and further while dragged, muted while disabled.
thumbStyle :: Palette -> StyleSet
thumbStyle p = (valueFillStyle p (paletteAccent p))
  { styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = shade 0.85 (paletteAccent p) })
      , (CommonPressed,   \s -> s { styleBackground = shade 0.7 (paletteAccent p) })
      , (CommonDisabled,  \s -> s { styleBackground = paletteTextMuted p })
      ]
  }
