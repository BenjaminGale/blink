{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A continuous-value slider: a thin filled track up to the current
-- value, with a square thumb straddling it at that point -- the
-- traditional thin-bar-plus-thumb slider look, rather than one solid
-- block. A leaf, built directly on 'controlBase' -- nothing derives from
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
  , slider
  , value
  , step
  , onValueChanged
  ) where

import Control.Monad (forM_, void, when)
import Data.Maybe (fromMaybe)

import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), uniformBorder)
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Layout.Constraints (HasLayoutConfig (..), Layout (..), fill)
import Blink.Rendering (Colour (..))
import Blink.Style (Style (..))
import Blink.UI
import Blink.UI.Element (Element (..), noIntrinsicSize)

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

-- | The width of the focus ring drawn around the whole control while
-- focused.
focusRingWidth :: Double
focusRingWidth = 1

-- | Horizontal margin, on each side, between the control's own bounds and
-- its track -- so the groove and thumb never touch the control's edge (or
-- its focus ring).
contentInset :: Double
contentInset = 6

-- | @bounds@ narrowed by 'contentInset' on the left and right -- the
-- track's own geometry, and what a mouse position maps to a value
-- against, both live within this rather than the raw control bounds.
trackRect :: Rectangle -> Rectangle
trackRect bounds = bounds
  { rectX     = rectX bounds + contentInset
  , rectWidth = max 0 (rectWidth bounds - 2 * contentInset)
  }

-- | Darkens @c@'s RGB toward black by @factor@ (in @[0, 1]@; 1 leaves it
-- unchanged), leaving alpha alone. Used to shade the thumb on hover\/drag
-- without needing a dedicated theme colour for each -- see 'thumbColourFor'.
shade :: Double -> Colour -> Colour
shade factor (RGBA r g b a) = RGBA (r * factor) (g * factor) (b * factor) a

-- | The thumb's own colour for this frame: darkened while a drag is in
-- progress (checked first, since a drag can continue after the pointer
-- has moved off the thumb entirely), a lighter darkening on hover, or
-- @accent@ unchanged otherwise. Only ever applied to the thumb -- the
-- groove and the filled track stay @accent@ regardless, so hovering or
-- dragging never recolours anything but the thing being grabbed.
thumbColourFor :: Bool -> Bool -> Colour -> Colour
thumbColourFor dragging hovered accent
  | dragging  = shade 0.7 accent
  | hovered   = shade 0.85 accent
  | otherwise = accent

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

-- | Draws the groove (border colour), the filled bar and thumb (text
-- colour, thumb shaded for hover\/drag), and a focus ring around the whole
-- control when focused.
drawTrack :: Style -> Rectangle -> Bool -> Bool -> Bool -> Double -> UI e msg ()
drawTrack s bounds focused hovered dragging v = do
  forM_ (styleBorderColour s) $ \c -> withBounds groove (fillRect c)
  withBounds track $ fillRect accent
  withBounds thumb $ fillRect (thumbColourFor dragging hovered accent)
  when focused $ strokeRect accent (uniformBorder focusRingWidth)
  where
    accent   = styleTextColour s
    tr       = trackRect bounds
    clamped  = clamp01 v
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
      -- One frame behind the real hit test (see 'wasMouseOverLastFrame'),
      -- since 'body' has no access to this frame's own hover reading --
      -- imperceptible for a cosmetic thumb tint.
      hovered   <- wasMouseOverLastFrame eid
      input     <- getInput

      let value0   = clamp01 (scValue cfg)
          keyEvts  = if not disabled && focused then inputKeyEvents input else []
          fromKeys = resolveKeyboardValue (scStep cfg) keyEvts value0

      fromMouse <-
        if not disabled && capturing
          then Just . fractionAt (trackRect bounds) . pointX <$> getMousePos
          else pure Nothing

      let newValue = fromMaybe (fromMaybe value0 fromKeys) fromMouse

      when (newValue /= value0) $ runHandlers (scOnValueChanged cfg) newValue

      drawTrack s bounds focused hovered capturing value0
