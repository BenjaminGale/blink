{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A scrollbar: a composite of two repeating arrow buttons (built on
-- 'Blink.View.Controls.RepeatButton.repeatButton') straddling a draggable
-- track, all three driving a single external value the same way
-- 'Blink.View.Controls.Slider.slider' owns its own -- clicking or dragging
-- the track jumps\/follows the pointer the same way a slider's thumb does,
-- and holding either arrow steps the value by 'step', repeating for as long
-- as it's held.
--
-- Like 'Blink.View.Controls.ToggleGroup.toggleGroup', the composite's own
-- id is never a keyboard focus target -- 'Blink.View.Controls.Control.NotFocusable'
-- fixed, not attr-settable. Unlike 'Blink.View.Controls.ToggleGroup.toggleGroup',
-- whose items stay independently focusable, neither arrow button is a focus
-- target either -- fixed the same way, not attr-settable -- since a
-- scrollbar (native or otherwise) is never itself part of Tab order; only
-- the content it scrolls is.
--
-- @
-- control --> hBox\/vBox --> repeatButton (decrement)
--                        --> control (track)
--                        --> repeatButton (increment)
-- @
module Blink.View.Controls.ScrollBar
  ( ScrollBarConfig (..)
  , ScrollBarPart (..)
  , RepeatState (..)
  , defaultScrollBarConfig
  , initialRepeatState
  , scrollBarStyleKey
  , scrollBarButtonStyleKey
  , scrollBarTrackStyleKey
  , scrollBar
  , scrollBarOrientation
  , value
  , visibleFraction
  , step
  , onValueChanged
  , decrementRepeatState
  , incrementRepeatState
  , onDecrementRepeatStateChanged
  , onIncrementRepeatStateChanged
  ) where

import Control.Monad (forM_, void, when)

import Blink.View.Controls.Button (onActivated)
import Blink.View.Controls.Control
import Blink.View.Controls.Label (text)
import Blink.View.Controls.RepeatButton
  (firedCount, onFiredCountChanged, onPressEnded, onPressStarted, pressStartedAt, repeatButton)
import Blink.Geometry (Alignment (TopLeft), Orientation (..), Point (..), Rectangle (..))
import Blink.View.Layout.Box (children, hBox, vBox)
import Blink.View.Layout.Constraints (HasLayoutConfig (..), Layout (..), exactly, fill, height, width)
import Blink.View.Rendering (Colour (..))
import Blink.View.Style (Style (..))
import Blink.View
import Blink.View.Drawing (fillRect)
import Blink.View.Element (Element (..), noIntrinsicSize, runElement)

-- | The thickness (cross-axis extent) of the whole control, and of each
-- arrow button's extent along the main axis. Both fixed rather than
-- attr-settable, the same way 'Blink.View.Controls.Slider.thumbSize' is --
-- override the resolved chrome via 'style' instead.
scrollBarThickness :: Double
scrollBarThickness = 16

-- | The minimum length the thumb ever draws at, regardless of
-- 'visibleFraction' -- without this, a very small fraction would shrink the
-- thumb to the point of being unreachable\/invisible.
minThumbLength :: Double
minThumbLength = 20

-- | Identifies one part of a 'scrollBar' for the purpose of minting element
-- ids -- the draggable track, and the two arrow buttons.
data ScrollBarPart
  = ScrollBarTrack
  | ScrollBarDecrement
  | ScrollBarIncrement
  | ScrollBarRoot
  deriving (Eq, Ord, Show)

-- | The caller-owned handoff behind one arrow button's repeat cadence --
-- exactly the two numbers 'Blink.View.Controls.RepeatButton.repeatButton'
-- itself needs (see its module header), bundled into one value since
-- 'scrollBar' has two such buttons to track at once. Fed back in via
-- 'decrementRepeatState'\/'incrementRepeatState', reported via
-- 'onDecrementRepeatStateChanged'\/'onIncrementRepeatStateChanged'.
data RepeatState = RepeatState
  { rsPressStartedAt :: Maybe Double
  , rsFiredCount     :: Int
  } deriving (Eq, Show)

-- | No press in progress, fired count zero -- the state to feed back once
-- 'onDecrementRepeatStateChanged'\/'onIncrementRepeatStateChanged' reports a
-- press ending.
initialRepeatState :: RepeatState
initialRepeatState = RepeatState Nothing 0

-- | Every capability 'scrollBar' resolves: the wrapped 'ControlConfig', its
-- own size request, which axis it runs along, its current value and the
-- proportion of the track its thumb covers, the step each arrow moves it
-- by, its 'onValueChanged' reactions, and the repeat-cadence handoff for
-- each arrow button (see 'RepeatState').
data ScrollBarConfig e msg = ScrollBarConfig
  { sbControl                  :: ControlConfig e msg
  , sbLayout                   :: Layout
  , sbOrientation               :: Orientation
  , sbValue                     :: Double
  , sbVisibleFraction           :: Double
  , sbStep                      :: Double
  , sbOnValueChanged            :: [Double -> [Out e msg]]
  , sbDecrementRepeat           :: RepeatState
  , sbIncrementRepeat           :: RepeatState
  , sbOnDecrementRepeatChanged  :: RepeatState -> [Out e msg]
  , sbOnIncrementRepeatChanged  :: RepeatState -> [Out e msg]
  }

-- | The default 'Layout' for a scrollbar running along @o@: fills the space
-- it's given along that axis, and sizes itself to 'scrollBarThickness'
-- across it. Set via 'scrollBarOrientation'.
layoutFor :: Orientation -> Layout
layoutFor Horizontal = Layout fill (exactly scrollBarThickness) TopLeft
layoutFor Vertical   = Layout (exactly scrollBarThickness) fill TopLeft

-- | 'defaultControlConfig' (styled via 'scrollBarStyleKey'), 'Vertical', a
-- value of 0, a visible fraction of 0.2, a step of 0.05, no
-- 'onValueChanged' reactions, and no repeat in progress on either arrow.
defaultScrollBarConfig :: ScrollBarConfig e msg
defaultScrollBarConfig = ScrollBarConfig
  { sbControl                  = defaultControlConfig { ccStyleKey = scrollBarStyleKey }
  , sbLayout                   = layoutFor Vertical
  , sbOrientation               = Vertical
  , sbValue                     = 0
  , sbVisibleFraction           = 0.2
  , sbStep                      = 0.05
  , sbOnValueChanged            = []
  , sbDecrementRepeat           = initialRepeatState
  , sbIncrementRepeat           = initialRepeatState
  , sbOnDecrementRepeatChanged  = const []
  , sbOnIncrementRepeatChanged  = const []
  }

instance HasControlConfig e msg (ScrollBarConfig e msg) where
  overControl attr = Attribute (\sc -> sc { sbControl = runAttribute attr (sbControl sc) })

instance HasLayoutConfig (ScrollBarConfig e msg) where
  overLayout attr = Attribute (\sc -> sc { sbLayout = runAttribute attr (sbLayout sc) })

-- | 'StyleKey's 'scrollBar' resolves its own chrome, its arrow buttons, and
-- its track from unless overridden via 'Blink.View.Controls.Control.style'.
scrollBarStyleKey, scrollBarButtonStyleKey, scrollBarTrackStyleKey :: StyleKey e
scrollBarStyleKey       = Class "scrollBar"
scrollBarButtonStyleKey = Class "scrollBarButton"
scrollBarTrackStyleKey  = Class "scrollBarTrack"

-- | Which axis the bar runs along: 'Horizontal' arranges the arrows and
-- track left-to-right, 'Vertical' (the default) top-to-bottom. Resets the
-- default 'Layout' the new axis implies -- apply this before any
-- 'Blink.View.Layout.Constraints.width'\/'Blink.View.Layout.Constraints.height'
-- override in the attribute list, or it will clobber them.
scrollBarOrientation :: Orientation -> Attribute (ScrollBarConfig e msg)
scrollBarOrientation o = Attribute (\sc -> sc { sbOrientation = o, sbLayout = layoutFor o })

-- | The current scroll position, as a fraction of the track the thumb's
-- leading edge can travel across -- @0@ pins it to the start, @1@ to the
-- end. Clamped to @[0, 1]@. Defaults to 0.
value :: Double -> Attribute (ScrollBarConfig e msg)
value v = Attribute (\sc -> sc { sbValue = v })

-- | How much of the scrollable content is visible at once, as a fraction of
-- the whole -- sets the thumb's length as that fraction of the track,
-- clamped to never draw shorter than the minimum grabbable length. Defaults
-- to 0.2.
visibleFraction :: Double -> Attribute (ScrollBarConfig e msg)
visibleFraction v = Attribute (\sc -> sc { sbVisibleFraction = v })

-- | How much each arrow button moves the value by, once per activation
-- (including each repeat while held -- see 'Blink.View.Controls.RepeatButton.repeatButton').
-- Defaults to 0.05.
step :: Double -> Attribute (ScrollBarConfig e msg)
step s = Attribute (\sc -> sc { sbStep = s })

-- | Reacts with the new value whenever dragging or clicking the track, or
-- an arrow button, would change it. It's up to the reaction to actually
-- store the new value and pass it back in via 'value' next frame.
onValueChanged :: (Double -> [Out e msg]) -> Attribute (ScrollBarConfig e msg)
onValueChanged f = Attribute (\sc -> sc { sbOnValueChanged = sbOnValueChanged sc ++ [f] })

-- | Hands the decrement arrow's current repeat state back to 'scrollBar',
-- exactly as last reported via 'onDecrementRepeatStateChanged'. Without
-- this, holding the arrow down never repeats past its first activation --
-- see 'Blink.View.Controls.RepeatButton.pressStartedAt'. Defaults to
-- 'initialRepeatState'.
decrementRepeatState :: RepeatState -> Attribute (ScrollBarConfig e msg)
decrementRepeatState s = Attribute (\sc -> sc { sbDecrementRepeat = s })

-- | The increment arrow's equivalent of 'decrementRepeatState'.
incrementRepeatState :: RepeatState -> Attribute (ScrollBarConfig e msg)
incrementRepeatState s = Attribute (\sc -> sc { sbIncrementRepeat = s })

-- | Reacts whenever the decrement arrow's own repeat state changes -- a
-- fresh press, a new repeat firing, or the press ending (reported as
-- 'initialRepeatState'). Store it and feed it back via
-- 'decrementRepeatState' next frame.
onDecrementRepeatStateChanged :: (RepeatState -> [Out e msg]) -> Attribute (ScrollBarConfig e msg)
onDecrementRepeatStateChanged f = Attribute (\sc -> sc { sbOnDecrementRepeatChanged = f })

-- | The increment arrow's equivalent of 'onDecrementRepeatStateChanged'.
onIncrementRepeatStateChanged :: (RepeatState -> [Out e msg]) -> Attribute (ScrollBarConfig e msg)
onIncrementRepeatStateChanged f = Attribute (\sc -> sc { sbOnIncrementRepeatChanged = f })

-- | Clamps a value to @[0, 1]@.
clamp01 :: Double -> Double
clamp01 = max 0 . min 1

-- | @bounds@'s own extent along @o@ -- width for 'Horizontal', height for
-- 'Vertical'.
axisLength :: Orientation -> Rectangle -> Double
axisLength Horizontal = rectWidth
axisLength Vertical   = rectHeight

-- | @bounds@'s own origin along @o@ -- left edge for 'Horizontal', top edge
-- for 'Vertical'.
axisOrigin :: Orientation -> Rectangle -> Double
axisOrigin Horizontal = rectX
axisOrigin Vertical   = rectY

-- | @p@'s own coordinate along @o@.
pointMain :: Orientation -> Point -> Double
pointMain Horizontal = pointX
pointMain Vertical   = pointY

-- | A rectangle spanning @bounds@'s full cross-axis extent, positioned at
-- @origin@ and sized to @len@ along @o@ -- the thumb's own geometry.
mainRect :: Orientation -> Rectangle -> Double -> Double -> Rectangle
mainRect Horizontal bounds origin len = bounds { rectX = origin, rectWidth  = len }
mainRect Vertical   bounds origin len = bounds { rectY = origin, rectHeight = len }

-- | The thumb's own length along @o@: @frac@ of @bounds@'s extent, floored
-- at 'minThumbLength' and capped at the track's own length.
thumbLengthFor :: Orientation -> Rectangle -> Double -> Double
thumbLengthFor o bounds frac =
  let len = axisLength o bounds
  in min len (max minThumbLength (clamp01 frac * len))

-- | The @[0, 1]@ value that centres the thumb (of @thumbLen@) under pointer
-- position @p@ -- clamped so the thumb never travels past either end of the
-- track, and reading as @0@ when the track has no room for the thumb to
-- travel at all.
fractionAt :: Orientation -> Rectangle -> Double -> Point -> Double
fractionAt o bounds thumbLen p
  | travel <= 0 = 0
  | otherwise   = clamp01 ((pointMain o p - axisOrigin o bounds - thumbLen / 2) / travel)
  where
    travel = axisLength o bounds - thumbLen

-- | Darkens @c@'s RGB toward black by @factor@ (in @[0, 1]@; 1 leaves it
-- unchanged), leaving alpha alone -- see 'Blink.View.Controls.Slider.shade',
-- which this replicates for the same reason (thumb hover\/drag shading
-- without a dedicated theme colour for each).
shade :: Double -> Colour -> Colour
shade factor (RGBA r g b a) = RGBA (r * factor) (g * factor) (b * factor) a

-- | The thumb's own colour for this frame -- see
-- 'Blink.View.Controls.Slider.thumbColourFor', which this replicates.
thumbColourFor :: Bool -> Bool -> Colour -> Colour
thumbColourFor dragging hovered accent
  | dragging  = shade 0.7 accent
  | hovered   = shade 0.85 accent
  | otherwise = accent

-- | Draws the full-length groove (border colour, if set) and the thumb
-- (text colour, shaded for hover\/drag) at its position for @v@.
drawTrack :: Orientation -> Style -> Rectangle -> Bool -> Bool -> Double -> Double -> View e msg ()
drawTrack o s bounds hovered dragging frac v = do
  forM_ (styleBorderColour s) $ \c -> withBounds bounds (fillRect c)
  withBounds thumb $ fillRect (thumbColourFor dragging hovered accent)
  where
    accent   = styleTextColour s
    thumbLen = thumbLengthFor o bounds frac
    travel   = max 0 (axisLength o bounds - thumbLen)
    origin   = axisOrigin o bounds + clamp01 v * travel
    thumb    = mainRect o bounds origin thumbLen

-- | The new value an arrow's activation moves @cfg@'s current value to by
-- @delta@ (negative for the decrement arrow), reported via
-- 'onValueChanged' only when it actually changes.
stepValue :: ScrollBarConfig e msg -> Double -> [Out e msg]
stepValue cfg delta =
  let v0 = clamp01 (sbValue cfg)
      v1 = clamp01 (v0 + delta)
  in if v1 /= v0 then concatMap ($ v1) (sbOnValueChanged cfg) else []

-- | Each arrow button's fixed size along the main axis, filling the cross
-- axis -- the same shape regardless of which arrow it is.
arrowLayoutAttrs :: HasLayoutConfig cfg => Orientation -> [Attribute cfg]
arrowLayoutAttrs Horizontal = [width (exactly scrollBarThickness), height fill]
arrowLayoutAttrs Vertical   = [width fill, height (exactly scrollBarThickness)]

-- | A scrollbar (see the module header). Clicking or dragging the track
-- moves the thumb to (and keeps it centred under) the pointer, the same way
-- 'Blink.View.Controls.Slider.slider' does; holding either arrow steps the
-- value by 'step', repeating for as long as it's held. Its value is control
-- state owned by the caller, not this control -- see 'onValueChanged' for
-- reacting to a change, and 'decrementRepeatState'\/'incrementRepeatState'
-- for the arrows' own repeat-cadence handoff.
--
-- @tag@ mints each part's element id from a 'ScrollBarPart' -- the caller
-- never writes a per-part id by hand.
scrollBar :: Ord e => (ScrollBarPart -> e) -> [Attribute (ScrollBarConfig e msg)] -> Element e msg
scrollBar tag attrs = Element
  { elLayout  = sbLayout cfg
  , elMeasure = measureChrome (ccStyleKey (sbControl cfg)) box
  , elRun     = void (control ctrl)
  }
  where
    cfg = resolve defaultScrollBarConfig attrs
    o   = sbOrientation cfg

    box = (if o == Horizontal then hBox else vBox) [children [decrementBtn, trackEl, incrementBtn]]

    decrementBtn = repeatButton (tag ScrollBarDecrement) $
      [ text (if o == Horizontal then "\9664" else "\9650") -- ◀ / ▲
      , style scrollBarButtonStyleKey
      , focusPolicy NotFocusable
      , pressStartedAt (rsPressStartedAt (sbDecrementRepeat cfg))
      , firedCount (rsFiredCount (sbDecrementRepeat cfg))
      , onPressStarted (\t -> sbOnDecrementRepeatChanged cfg (RepeatState (Just t) 0))
      , onFiredCountChanged (\n -> sbOnDecrementRepeatChanged cfg (RepeatState (rsPressStartedAt (sbDecrementRepeat cfg)) n))
      , onPressEnded (sbOnDecrementRepeatChanged cfg initialRepeatState)
      , onActivated (const (stepValue cfg (negate (sbStep cfg))))
      ] ++ arrowLayoutAttrs o

    incrementBtn = repeatButton (tag ScrollBarIncrement) $
      [ text (if o == Horizontal then "\9654" else "\9660") -- ▶ / ▼
      , style scrollBarButtonStyleKey
      , focusPolicy NotFocusable
      , pressStartedAt (rsPressStartedAt (sbIncrementRepeat cfg))
      , firedCount (rsFiredCount (sbIncrementRepeat cfg))
      , onPressStarted (\t -> sbOnIncrementRepeatChanged cfg (RepeatState (Just t) 0))
      , onFiredCountChanged (\n -> sbOnIncrementRepeatChanged cfg (RepeatState (rsPressStartedAt (sbIncrementRepeat cfg)) n))
      , onPressEnded (sbOnIncrementRepeatChanged cfg initialRepeatState)
      , onActivated (const (stepValue cfg (sbStep cfg)))
      ] ++ arrowLayoutAttrs o

    trackEl = Element
      { elLayout  = Layout fill fill TopLeft
      , elMeasure = measureChrome scrollBarTrackStyleKey (Element (Layout fill fill TopLeft) noIntrinsicSize (pure ()))
      , elRun     = void (control trackCtrl)
      }

    trackCtrl = defaultControlConfig
      { ccElementId       = Just (tag ScrollBarTrack)
      , ccStyleKey        = scrollBarTrackStyleKey
      , ccFocusPolicy     = NotFocusable
      , ccMouseActivation = CaptureActivated
      , ccContent         = const trackBody
      }

    trackBody = do
      s         <- currentStyle
      bounds    <- getBounds
      disabled  <- isDisabled
      let trackId = tag ScrollBarTrack
      capturing <- isDragging trackId
      hovered   <- wasMouseOverLastFrame trackId
      let value0   = clamp01 (sbValue cfg)
          thumbLen = thumbLengthFor o bounds (sbVisibleFraction cfg)
      fromMouse <-
        if not disabled && capturing
          then Just . fractionAt o bounds thumbLen <$> getMousePos
          else pure Nothing
      forM_ fromMouse $ \newValue ->
        when (newValue /= value0) $ runHandlers (sbOnValueChanged cfg) newValue
      drawTrack o s bounds hovered capturing (sbVisibleFraction cfg) value0

    ctrl = (sbControl cfg)
      { ccElementId   = Just (tag ScrollBarRoot)
      , ccFocusPolicy = NotFocusable
      , ccContent     = const (runElement box)
      }
