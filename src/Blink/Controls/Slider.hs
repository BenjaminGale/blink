{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A continuous-value slider: a filled track up to the current value, with
-- a thumb drawn at its edge. A leaf, built directly on 'controlBase' --
-- nothing derives from it, and it displays no label, so it has no
-- 'Blink.Controls.Label.LabelledConfig' either. The interactive counterpart
-- to 'Blink.Controls.ProgressBar.progressBar': where a progress bar only
-- ever displays a value the application computes, a slider lets the user
-- set one, by dragging the thumb, clicking the track, or (while focused)
-- pressing the arrow keys.
module Blink.Controls.Slider
  ( SliderConfig (..)
  , defaultSliderConfig
  , sliderStyleKey
  , slider
  , value
  , step
  , onValueChanged
  ) where

import Control.Monad (void, when)
import Data.Maybe (fromMaybe)

import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..))
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Layout.Constraints (HasLayoutConfig (..), Layout (..), fill)
import Blink.Style (Style (..))
import Blink.UI
import Blink.UI.Element (Element (..), noIntrinsicSize)

-- | The fixed width of the thumb drawn at the current value's edge.
thumbWidth :: Double
thumbWidth = 8

-- | Every capability 'slider' resolves: the wrapped 'ControlConfig', its
-- current value, the increment arrow keys move it by, and its
-- 'onValueChanged' reactions.
data SliderConfig e msg = SliderConfig
  { scControl        :: ControlConfig e msg
  , scValue          :: Double
  , scStep           :: Double
  , scOnValueChanged :: [Double -> [Out e msg]]
  , scLayout         :: Layout
  }

-- | The 'StyleKey' 'slider' resolves its style from unless overridden via
-- 'style'.
sliderStyleKey :: StyleKey e
sliderStyleKey = Class "slider"

-- | 'defaultControlConfig' (styled via 'sliderStyleKey'), a value of 0, a
-- step of 0.1, no 'onValueChanged' reactions, and @Layout fill fill
-- TopLeft@ (see 'slider').
defaultSliderConfig :: SliderConfig e msg
defaultSliderConfig = SliderConfig
  { scControl        = defaultControlConfig { ccStyleKey = sliderStyleKey }
  , scValue          = 0
  , scStep           = 0.1
  , scOnValueChanged = []
  , scLayout         = Layout fill fill TopLeft
  }

instance HasElementConfig e msg (SliderConfig e msg) where
  overElement attr = Attribute (\sc -> sc { scControl = runAttribute (overElement attr) (scControl sc) })

instance HasControlConfig e msg (SliderConfig e msg) where
  overControl attr = Attribute (\sc -> sc { scControl = runAttribute attr (scControl sc) })

instance HasLayoutConfig (SliderConfig e msg) where
  overLayout attr = Attribute (\sc -> sc { scLayout = runAttribute attr (scLayout sc) })

-- | Sets the slider's current value, clamped to @[0, 1]@. Defaults to 0.
value :: Double -> Attribute (SliderConfig e msg)
value v = Attribute (\sc -> sc { scValue = v })

-- | How much an arrow key press while focused (Left\/Down to decrease,
-- Right\/Up to increase) moves the value by. Has no effect on dragging or
-- clicking the track, which always follow the pointer continuously.
-- Defaults to 0.1.
step :: Double -> Attribute (SliderConfig e msg)
step s = Attribute (\sc -> sc { scStep = s })

-- | Reacts with the new value whenever dragging, clicking the track, or an
-- arrow key press would change it. It's up to the reaction to actually
-- store the new value and pass it back in via 'value' next frame.
onValueChanged :: (Double -> [Out e msg]) -> Attribute (SliderConfig e msg)
onValueChanged f = Attribute (\sc -> sc { scOnValueChanged = scOnValueChanged sc ++ [f] })

-- | Clamps a value to @[0, 1]@.
clamp01 :: Double -> Double
clamp01 = max 0 . min 1

-- | The @[0, 1]@ fraction along @bounds@ that horizontal position @x@ maps
-- to, clamped to stay within the track even when the pointer has moved
-- outside it -- the same "capture holds past the edge" behaviour dragging
-- a text selection relies on.
fractionAt :: Rectangle -> Double -> Double
fractionAt bounds x
  | rectWidth bounds <= 0 = 0
  | otherwise             = clamp01 ((x - rectX bounds) / rectWidth bounds)

-- | The value an arrow key press this frame moves @v@ to, if any: Left\/Down
-- decrease by @s@, Right\/Up increase by @s@, both clamped to @[0, 1]@.
-- 'Nothing' when neither was pressed.
resolveKeyboardValue :: Double -> [KeyEvent] -> Double -> Maybe Double
resolveKeyboardValue s keyEvts v
  | pressed KeyLeft  || pressed KeyDown = Just (clamp01 (v - s))
  | pressed KeyRight || pressed KeyUp   = Just (clamp01 (v + s))
  | otherwise                           = Nothing
  where
    pressed k = any ((== k) . key) keyEvts

-- | Draws the filled track up to @v@, then the thumb centred on that same
-- point (clamped so it never draws outside the track), both in the
-- resolved style's text colour -- the same "foreground" role
-- 'Blink.Controls.ProgressBar.progressBar's fill and
-- 'Blink.Controls.Checkbox.checkbox's tick already use.
drawTrack :: Style -> Rectangle -> Double -> UI e msg ()
drawTrack s bounds v = do
  withBounds filled $ fillRect (styleTextColour s)
  withBounds thumb $ fillRect (styleTextColour s)
  where
    clamped  = clamp01 v
    filled   = bounds { rectWidth = rectWidth bounds * clamped }
    thumbX   = rectX bounds + clamped * rectWidth bounds - thumbWidth / 2
    clampedX = max (rectX bounds) (min (rectX bounds + rectWidth bounds - thumbWidth) thumbX)
    thumb    = bounds { rectX = clampedX, rectWidth = thumbWidth }

-- | A continuous-value slider (see the module header). Dragging the thumb
-- or clicking anywhere on the track jumps the value to the pointer's
-- position, continuously, for as long as the press holds capture -- even
-- once the pointer moves outside the track -- the same way dragging a text
-- selection does. While focused and enabled, Left\/Down and Right\/Up also
-- adjust it by 'step'. Its value is control state owned by the caller, not
-- this control -- see 'onValueChanged' for reacting to a change. Defaults
-- to filling the space it's given on both axes, the same as
-- 'Blink.Controls.ProgressBar.progressBar'; override with
-- 'Blink.Layout.Constraints.width'\/'Blink.Layout.Constraints.height'\/'Blink.Layout.Constraints.align'.
slider :: Ord e => e -> [Attribute (SliderConfig e msg)] -> Element e msg
slider eid attrs = Element
  { elLayout  = scLayout cfg
  , elMeasure = measureChrome (ccStyleKey ctrl) (Element (scLayout cfg) noIntrinsicSize (pure ()))
  , elRun     = void (controlBase eid ctrl)
  }
  where
    cfg  = resolve defaultSliderConfig attrs
    ctrl = (scControl cfg) { ccContent = body }
    body = do
      s         <- currentStyle
      bounds    <- getBounds
      disabled  <- isDisabled
      focused   <- isFocused eid
      capturing <- isDragging eid
      input     <- getInput

      let value0   = clamp01 (scValue cfg)
          keyEvts  = if not disabled && focused then inputKeyEvents input else []
          fromKeys = resolveKeyboardValue (scStep cfg) keyEvts value0

      fromMouse <-
        if not disabled && capturing
          then Just . fractionAt bounds . pointX <$> getMousePos
          else pure Nothing

      let newValue = fromMaybe (fromMaybe value0 fromKeys) fromMouse

      when (newValue /= value0) $ runHandlers (scOnValueChanged cfg) newValue

      drawTrack s bounds value0
