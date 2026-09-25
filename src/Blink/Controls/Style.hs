{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Style

The shape vocabulary shared by more than one built-in control -- the
"bordered box" ('buttonStyle') and "flat row" ('flatRowStyle') looks, the
fill and thumb looks ('plainFillStyle', 'valueFillStyle', 'thumbStyle')
shared by the parts of a slider, scrollbar and progress bar, and the plain
look ('plainStyle') for anything with no look of its own -- plus the
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
  , trackMetrics
  , zeroMetrics
  , buttonStyle
  , flatRowStyle
  , plainStyle
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

-- | A margin with no padding, for a track-shaped control (a progress bar,
-- a slider, a scrollbar's track) whose content runs right up to its
-- chrome.
trackMetrics :: Metrics
trackMetrics = Metrics
  { metricsMargin      = uniform 3
  , metricsPadding     = uniform 0
  }

-- | No margin or padding, for a wrapper whose chrome belongs to what it
-- holds (a toggle group's items, a scrollbar's buttons and track), for a
-- control that sits flush in space its parent reserves for it (a tree
-- chevron, an image), and for every part (see
-- 'Blink.Controls.Control.drawPart'), which never uses metrics.
zeroMetrics :: Metrics
zeroMetrics = Metrics
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

-- | Transparent, borderless, primary-coloured left-aligned text, and no
-- change with state: the look of anything with no look of its own (a
-- group's container, a progress bar or divider whose visible parts are
-- styled separately, an image), and the base other styles adjust.
plainStyle :: Palette -> StyleSet
plainStyle p = StyleSet
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
-- 'plainStyle's wrappers above) reads as focused whenever any child
-- inside it does, so its own border still needs to answer that. Paired
-- with 'controlMetrics' (real border width, so both overrides have
-- something to draw into) rather than 'zeroMetrics'.
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
