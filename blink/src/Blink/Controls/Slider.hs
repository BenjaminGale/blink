{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A continuous-value slider: a thin filled track up to the current
-- value, with a square thumb straddling it at that point -- the
-- traditional thin-bar-plus-thumb slider look, rather than one solid
-- block. A leaf, built directly on 'control' -- nothing derives from
-- it, and it displays no label, so it has no
-- 'Blink.Controls.Label.LabelledConfig' either. The interactive
-- counterpart to 'Blink.Controls.ProgressBar.progressBar': where a
-- progress bar only ever displays a value the application computes, a
-- slider lets the user set one, by dragging the thumb, clicking the
-- track, or (while focused) pressing the arrow keys.
module Blink.Controls.Slider
  ( SliderConfig (..)
  , defaultSliderConfig
  , sliderStyleKey
  , sliderTrackStyleKey
  , sliderFillStyleKey
  , sliderThumbStyleKey
  , sliderStyle
  , slider
  , value
  , step
  , onValueChanged
    -- * Style
  , defaultStyleEntries
  ) where

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), clampFraction)
import Blink.Input (Key (..), KeyEvent (..))
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.View
import Blink.Element (Element (..), HasLayoutConfig (..), noIntrinsicSize, HasStep (..), HasValue (..))
import Blink.Style
import Blink.Rendering (TextAlign (..))
import Blink.Controls.Style (plainFillStyle, trackMetrics, thumbStyle, zeroMetrics, transparent, valueFillStyle)

-- | The height of the thin filled bar drawn along the middle of the
-- control's full bounds -- deliberately much shorter than the thumb, so
-- the two read as a track and a thing sliding along it rather than one
-- solid block.
trackThickness :: Double
trackThickness = 4

-- | The fixed width and height of the square thumb drawn at the current
-- value's position, straddling the track -- bigger than 'trackThickness'
-- so it reads as the thing you grab, not part of the track itself.
thumbSize :: Double
thumbSize = 14

-- | Horizontal margin, on each side, between the inside of the control's
-- 1px border and its track -- so the groove and thumb never touch the
-- border the focus ring is drawn in.
contentInset :: Double
contentInset = 5

-- | @bounds@ narrowed by 'contentInset' on the left and right -- the
-- track's own geometry, and what a mouse position maps to a value
-- against, both live within this rather than the raw control bounds.
trackRect :: Rectangle -> Rectangle
trackRect bounds = bounds
  { rectX     = rectX bounds + contentInset
  , rectWidth = max 0 (rectWidth bounds - 2 * contentInset)
  }

-- | Every capability 'slider' resolves: the wrapped 'ControlConfig', its
-- current value, the increment arrow keys move it by, and its
-- 'onValueChanged' reactions.
data SliderConfig e msg = SliderConfig
  { scControl        :: ControlConfig e msg
  , scValue          :: Double
  , scStep           :: Double
  , scOnValueChanged :: [Double -> [Effect e msg]]
  , scLayout         :: Layout
  }

-- | 'defaultControlConfig' (styled via 'sliderStyleKey', 'CaptureActivated'
-- since dragging the thumb off the track and releasing there has already
-- changed the value -- see 'MouseActivation'), a value of 0, a step of 0.1,
-- no 'onValueChanged' reactions, and @Layout fill fill TopLeft@ (see
-- 'slider').
defaultSliderConfig :: SliderConfig e msg
defaultSliderConfig = SliderConfig
  { scControl        = defaultControlConfig
      { ccStyleKey        = sliderStyleKey
      , ccMouseActivation = CaptureActivated
      }
  , scValue          = 0
  , scStep           = 0.1
  , scOnValueChanged = []
  , scLayout         = Layout fill fill TopLeft
  }

instance HasControlConfig e msg (SliderConfig e msg) where
  overControl = nested scControl (\sc x -> sc { scControl = x })

instance HasEventHandlers (SliderConfig e msg)

instance HasLayoutConfig (SliderConfig e msg) where
  overLayout = nested scLayout (\sc x -> sc { scLayout = x })

-- | Sets the slider's current value, clamped to @[0, 1]@. Defaults to 0.
instance HasValue Double (SliderConfig e msg) where
  value v = Attribute (\sc -> sc { scValue = v })

-- | How much an arrow key press while focused (Left\/Down to decrease,
-- Right\/Up to increase) moves the value by. Has no effect on dragging or
-- clicking the track, which always follow the pointer continuously.
-- Defaults to 0.1.
instance HasStep (SliderConfig e msg) where
  step s = Attribute (\sc -> sc { scStep = s })

-- | Reacts with the new value whenever dragging, clicking the track, or an
-- arrow key press would change it. It's up to the reaction to actually
-- store the new value and pass it back in via 'value' next frame.
onValueChanged :: (Double -> [Effect e msg]) -> Attribute (SliderConfig e msg)
onValueChanged = appendTo scOnValueChanged (\sc hs -> sc { scOnValueChanged = hs })

-- | The @[0, 1]@ fraction along @bounds@ that horizontal position @x@ maps
-- to, clamped to stay within the track even when the pointer has moved
-- outside it -- the same "capture holds past the edge" behaviour dragging
-- a text selection relies on.
fractionAt :: Rectangle -> Double -> Double
fractionAt bounds x
  | rectWidth bounds <= 0 = 0
  | otherwise             = clampFraction ((x - rectX bounds) / rectWidth bounds)

-- | The value an arrow key press this frame moves @v@ to, if any: Left\/Down
-- decrease by @s@, Right\/Up increase by @s@, both clamped to @[0, 1]@.
-- 'Nothing' when neither was pressed.
resolveKeyboardValue :: Double -> [KeyEvent] -> Double -> Maybe Double
resolveKeyboardValue s keyEvts v
  | pressed KeyLeft  || pressed KeyDown = Just (clampFraction (v - s))
  | pressed KeyRight || pressed KeyUp   = Just (clampFraction (v + s))
  | otherwise                           = Nothing
  where
    pressed k = any ((== k) . key) keyEvts

-- | Draws the groove (border colour), the filled bar and thumb (text
-- colour, thumb shaded for hover\/drag), and a focus ring around the whole
-- control when focused.
drawTrack :: Ord e => Rectangle -> Bool -> Bool -> Bool -> Double -> View e msg ()
drawTrack bounds disabled hovered dragging v = do
  withBounds groove (drawPart sliderTrackStyleKey valueState)
  withBounds track  (drawPart sliderFillStyleKey valueState)
  withBounds thumb  (drawPart sliderThumbStyleKey (Set.singleton (commonState disabled dragging hovered)))
  where
    valueState = Set.singleton (commonState disabled False False)
    tr       = trackRect bounds
    clamped  = clampFraction v
    trackY   = rectY tr + (rectHeight tr - trackThickness) / 2
    groove   = Rectangle (rectX tr) trackY (rectWidth tr) trackThickness
    track    = groove { rectWidth = rectWidth tr * clamped }
    thumbX   = rectX tr + clamped * rectWidth tr - thumbSize / 2
    clampedX = max (rectX tr) (min (rectX tr + rectWidth tr - thumbSize) thumbX)
    thumbY   = rectY tr + (rectHeight tr - thumbSize) / 2
    thumb    = Rectangle clampedX thumbY thumbSize thumbSize

-- | A continuous-value slider (see the module header). Dragging the thumb
-- or clicking anywhere on the track jumps the value to the pointer's
-- position, continuously, for as long as the press holds capture -- even
-- once the pointer moves outside the track -- the same way dragging a text
-- selection does. While focused and enabled, Left\/Down and Right\/Up also
-- adjust it by 'step'. Its value is control state owned by the caller,
-- not this control -- see 'onValueChanged' for reacting to a change.
-- Defaults to filling the space it's given on both axes, the same as
-- 'Blink.Controls.ProgressBar.progressBar'; override with
-- 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
slider :: Ord e => e -> [Attribute (SliderConfig e msg)] -> Element e msg
slider eid attrs = controlElement (scLayout cfg) (Element (scLayout cfg) noIntrinsicSize (pure ())) ctrl
  where
    cfg  = resolve defaultSliderConfig attrs
    ctrl = (scControl cfg)
      { ccContent   = body
      , ccElementId = Just eid
      }
    body ci = do
      bounds <- getBounds
      let capturing = ciIsCaptured ci
          value0    = clampFraction (scValue cfg)
          fromKeys  = resolveKeyboardValue (scStep cfg) (ciKeysPressed ci) value0

      fromMouse <-
        if not (ciDisabled ci) && capturing
          then Just . fractionAt (trackRect bounds) . pointX <$> getMousePos
          else pure Nothing

      let newValue = fromMaybe (fromMaybe value0 fromKeys) fromMouse

      when (newValue /= value0) $ runHandlers (scOnValueChanged cfg) newValue

      drawTrack bounds (ciDisabled ci) (ciHovered ci) capturing value0

-- * Style

-- | The 'StyleKey' 'Blink.Controls.Slider.slider' resolves its style
-- from unless overridden via 'Blink.Controls.Control.style'.
sliderStyleKey :: StyleKey e
sliderStyleKey = Class "slider"

-- | The 'StyleKey' the groove the fill runs along resolves its style from.
sliderTrackStyleKey :: StyleKey e
sliderTrackStyleKey = Class "sliderTrack"

-- | The 'StyleKey' the filled part of the track, up to the value, resolves
-- its style from.
sliderFillStyleKey :: StyleKey e
sliderFillStyleKey = Class "sliderFill"

-- | The 'StyleKey' the thumb resolves its style from, with the slider's
-- hover as 'CommonMouseOver' and a drag as 'CommonPressed'.
sliderThumbStyleKey :: StyleKey e
sliderThumbStyleKey = Class "sliderThumb"

-- | No background of its own, and a 1px border that's transparent at rest
-- and draws the focus ring in the accent colour while focused.
sliderStyle :: Palette -> StyleSet
sliderStyle p = StyleSet
  { styleBase = Style
      { styleBackground = transparent
      , styleTextColour = paletteTextPrimary p
      , styleTextAlign  = AlignLeft
      , styleBorder     = soloBorder transparent 1
      }
  , styleOverrides = Map.singleton FocusFocused
      (\s -> s { styleBorder = withBorderColour (paletteAccent p) (styleBorder s) })
  }

-- | This control's entries in 'Blink.Style.Defaults.defaultTheme': its
-- own chrome and each of its parts.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (sliderStyleKey,      (trackMetrics, sliderStyle p))
  , (sliderTrackStyleKey, (zeroMetrics, plainFillStyle p (paletteBorder p)))
  , (sliderFillStyleKey,  (zeroMetrics, valueFillStyle p (paletteAccent p)))
  , (sliderThumbStyleKey, (zeroMetrics, thumbStyle p))
  ]
