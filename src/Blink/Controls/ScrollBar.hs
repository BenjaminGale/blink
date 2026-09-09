{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A scrollbar: a composite of two repeating arrow buttons (built on
-- 'Blink.Controls.RepeatButton.repeatButton') straddling a draggable
-- track. Its position is control state, not application data -- 'scrollBar'
-- reads and writes it itself via 'Blink.View.getScrollState'\/'Blink.View.requestScrollTo'\/
-- 'Blink.View.requestScrollBy', keyed by its own element id, the same way
-- 'Blink.Controls.TextInput.textInput' owns its own scroll offset
-- rather than asking the caller to thread it through. Clicking or dragging
-- the track jumps\/follows the pointer the same way
-- 'Blink.Controls.Slider.slider''s thumb does; holding either arrow
-- steps it by 'step', repeating for as long as it's held.
--
-- A caller that needs to know the current position too -- to offset the
-- content being scrolled, say -- reads it the same way, via
-- 'Blink.View.getScrollState' passed the identical element id
-- (@tag 'ScrollBar'@); no attribute\/reaction pair is needed to expose
-- it, the same way none is needed to read a text input's own scroll
-- offset from outside it.
--
-- Like 'Blink.Controls.ToggleGroup.toggleGroup', the composite's own
-- id is never a keyboard focus target -- 'Blink.Controls.Control.NotFocusable'
-- fixed, not attr-settable. Unlike 'Blink.Controls.ToggleGroup.toggleGroup',
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
module Blink.Controls.ScrollBar
  ( ScrollBarConfig (..)
  , ScrollBarPart (..)
  , defaultScrollBarConfig
  , scrollBarStyleKey
  , scrollBarButtonStyleKey
  , scrollBarTrackStyleKey
  , scrollBar
  , scrollBarOrientation
  , visibleFraction
  , step
  ) where

import Control.Monad (forM_, void, when)

import Blink.Controls.Button (onActivated)
import Blink.Controls.Control
import Blink.Controls.Label (text)
import Blink.Controls.RepeatButton (repeatButton)
import Blink.Controls.ScrollBar.Style (scrollBarButtonStyleKey, scrollBarStyleKey, scrollBarTrackStyleKey)
import Blink.Geometry (Alignment (TopLeft), Orientation (..), Point (..), Rectangle (..))
import Blink.Layout.Box (children, hBox, vBox)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..))
import Blink.Style (Style (..))
import Blink.View
import Blink.View.Drawing (fillRect)
import Blink.Element (Element (..), HasLayoutConfig (..), height, noIntrinsicSize, runElement, width)

-- | The thickness (cross-axis extent) of the whole control, and of each
-- arrow button's extent along the main axis. Both fixed rather than
-- attr-settable, the same way 'Blink.Controls.Slider.thumbSize' is --
-- override the resolved chrome via 'style' instead.
scrollBarThickness :: Double
scrollBarThickness = 16

-- | The minimum length the thumb ever draws at, regardless of
-- 'visibleFraction' -- without this, a very small fraction would shrink the
-- thumb to the point of being unreachable\/invisible.
minThumbLength :: Double
minThumbLength = 20

-- | Identifies one part of a 'scrollBar' for the purpose of building element
-- ids -- the draggable track, the two arrow buttons, and the composite's
-- own root, which doubles as the 'Blink.View.ScrollState' key its position
-- is stored under (see the module header).
data ScrollBarPart
  = ScrollBarTrack
  | ScrollBarDecrement
  | ScrollBarIncrement
  | ScrollBar
  deriving (Eq, Ord, Show)

-- | Every capability 'scrollBar' resolves: the wrapped 'ControlConfig', its
-- own size request, which axis it runs along, the proportion of the track
-- its thumb covers, and the step each arrow moves the position by.
data ScrollBarConfig e msg = ScrollBarConfig
  { sbControl         :: ControlConfig e msg
  , sbLayout          :: Layout
  , sbOrientation     :: Orientation
  , sbVisibleFraction :: Double
  , sbStep            :: Double
  }

-- | The default 'Layout' for a scrollbar running along @o@: fills the space
-- it's given along that axis, and sizes itself to 'scrollBarThickness'
-- across it. Set via 'scrollBarOrientation'.
layoutFor :: Orientation -> Layout
layoutFor Horizontal = Layout fill (exactly scrollBarThickness) TopLeft
layoutFor Vertical   = Layout (exactly scrollBarThickness) fill TopLeft

-- | 'defaultControlConfig' (styled via 'scrollBarStyleKey'), 'Vertical', a
-- visible fraction of 0.2, and a step of 0.05.
defaultScrollBarConfig :: ScrollBarConfig e msg
defaultScrollBarConfig = ScrollBarConfig
  { sbControl         = defaultControlConfig { ccStyleKey = scrollBarStyleKey }
  , sbLayout          = layoutFor Vertical
  , sbOrientation     = Vertical
  , sbVisibleFraction = 0.2
  , sbStep            = 0.05
  }

instance HasControlConfig e msg (ScrollBarConfig e msg) where
  overControl attr = Attribute (\sc -> sc { sbControl = runAttribute attr (sbControl sc) })

instance HasLayoutConfig (ScrollBarConfig e msg) where
  overLayout attr = Attribute (\sc -> sc { sbLayout = runAttribute attr (sbLayout sc) })

-- | Which axis the bar runs along: 'Horizontal' arranges the arrows and
-- track left-to-right, 'Vertical' (the default) top-to-bottom. Resets the
-- default 'Layout' the new axis implies -- apply this before any
-- 'Blink.Element.width'\/'Blink.Element.height'
-- override in the attribute list, or it will clobber them.
scrollBarOrientation :: Orientation -> Attribute (ScrollBarConfig e msg)
scrollBarOrientation o = Attribute (\sc -> sc { sbOrientation = o, sbLayout = layoutFor o })

-- | How much of the scrollable content is visible at once, as a fraction of
-- the whole -- sets the thumb's length as that fraction of the track,
-- clamped to never draw shorter than the minimum grabbable length. Defaults
-- to 0.2.
visibleFraction :: Double -> Attribute (ScrollBarConfig e msg)
visibleFraction v = Attribute (\sc -> sc { sbVisibleFraction = v })

-- | How much each arrow button moves the position by, once per activation
-- (including each repeat while held -- see 'Blink.Controls.RepeatButton.repeatButton').
-- Defaults to 0.05.
step :: Double -> Attribute (ScrollBarConfig e msg)
step s = Attribute (\sc -> sc { sbStep = s })

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
-- unchanged), leaving alpha alone -- see 'Blink.Controls.Slider.shade',
-- which this replicates for the same reason (thumb hover\/drag shading
-- without a dedicated theme colour for each).
shade :: Double -> Colour -> Colour
shade factor (RGBA r g b a) = RGBA (r * factor) (g * factor) (b * factor) a

-- | The thumb's own colour for this frame -- see
-- 'Blink.Controls.Slider.thumbColourFor', which this replicates.
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

-- | Each arrow button's fixed size along the main axis, filling the cross
-- axis -- the same shape regardless of which arrow it is.
arrowLayoutAttrs :: HasLayoutConfig cfg => Orientation -> [Attribute cfg]
arrowLayoutAttrs Horizontal = [width (exactly scrollBarThickness), height fill]
arrowLayoutAttrs Vertical   = [width fill, height (exactly scrollBarThickness)]

-- | A scrollbar (see the module header). Clicking or dragging the track
-- moves the thumb to (and keeps it centred under) the pointer, the same way
-- 'Blink.Controls.Slider.slider' does; holding either arrow steps the
-- position by 'step', repeating for as long as it's held.
--
-- @tag@ builds each part's element id from a 'ScrollBarPart' -- the caller
-- never writes a per-part id by hand. @tag 'ScrollBar'@ doubles as the
-- 'Blink.View.ScrollState' key -- see the module header for reading it
-- from elsewhere.
scrollBar :: Ord e => (ScrollBarPart -> e) -> [Attribute (ScrollBarConfig e msg)] -> Element e msg
scrollBar tag attrs = Element
  { elLayout  = sbLayout cfg
  , elMeasure = measureChrome (ccStyleKey (sbControl cfg)) box
  , elRun     = void (control ctrl)
  }
  where
    cfg       = resolve defaultScrollBarConfig attrs
    o         = sbOrientation cfg
    scrollEid = tag ScrollBar

    box = (if o == Horizontal then hBox else vBox) [children [decrementBtn, trackEl, incrementBtn]]

    decrementBtn = repeatButton (tag ScrollBarDecrement) $
      [ text (if o == Horizontal then "\9664" else "\9650") -- ◀ / ▲
      , style scrollBarButtonStyleKey
      , focusPolicy NotFocusable
      , onActivated (postScrollBy scrollEid (negate (sbStep cfg)))
      ] ++ arrowLayoutAttrs o

    incrementBtn = repeatButton (tag ScrollBarIncrement) $
      [ text (if o == Horizontal then "\9654" else "\9660") -- ▶ / ▼
      , style scrollBarButtonStyleKey
      , focusPolicy NotFocusable
      , onActivated (postScrollBy scrollEid (sbStep cfg))
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
      value0    <- getScrollState scrollEid
      let thumbLen = thumbLengthFor o bounds (sbVisibleFraction cfg)
      when (not disabled && capturing) $ do
        newValue <- fractionAt o bounds thumbLen <$> getMousePos
        when (newValue /= value0) $ requestScrollTo scrollEid newValue
      drawTrack o s bounds hovered capturing (sbVisibleFraction cfg) value0

    ctrl = (sbControl cfg)
      { ccElementId   = Just scrollEid
      , ccFocusPolicy = NotFocusable
      , ccContent     = const (runElement box)
      }
