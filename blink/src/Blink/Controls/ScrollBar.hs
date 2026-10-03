{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A scrollbar: a composite of two repeating arrow buttons (drawing an
-- icon rather than a caption, so built on
-- 'Blink.Controls.RepeatButton.repeatButtonBase' rather than
-- 'Blink.Controls.RepeatButton.repeatButton' itself -- see @arrowButton@)
-- straddling a draggable track. Its position is control state, not application data -- 'scrollBar'
-- reads and writes it itself via 'Blink.View.getScrollState'\/'Blink.View.requestScrollTo'\/
-- 'Blink.View.requestScrollBy', keyed by its own element id, the same way
-- 'Blink.Controls.TextInput.textInput' owns its own scroll offset
-- rather than asking the caller to thread it through. Clicking the track
-- jumps the thumb there, centred under the click, the same way
-- 'Blink.Controls.Slider.slider''s thumb does; grabbing the thumb drags
-- it, tracking the pointer without recentring; holding either arrow steps
-- it by 'step', repeating for as long as it's held.
--
-- A caller that needs to know the current position too -- to offset the
-- content being scrolled, say -- reads it the same way, via
-- 'Blink.View.getScrollState' passed the identical element id
-- (the one given to 'scrollBar'); no attribute\/reaction pair is needed to expose
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
-- control --> hBox\/vBox --> arrowButton (decrement)
--                        --> control (track)
--                        --> arrowButton (increment)
-- @
module Blink.Controls.ScrollBar
  ( ScrollBarConfig (..)
  , defaultScrollBarConfig
  , scrollBarStyleKey
  , scrollBarButtonStyleKey
  , scrollBarTrackStyleKey
  , scrollBarThumbStyleKey
  , scrollBar
  , orientation
  , scrollBarThickness
  , visibleFraction
  , step
    -- * Scrollable viewports
  , ScrollViewportConfig (..)
  , scrollViewport
  , scrollViewportTo
  , verticalBarOf
    -- * Style
  , defaultStyleEntries
  ) where

import Control.Monad (void, when)
import qualified Data.Set as Set

import Blink.Controls.Button (ButtonConfig (..), HasButtonConfig (..))
import Blink.Controls.RepeatButton (RepeatButtonConfig (..), defaultRepeatButtonConfig, repeatButtonBase)
import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft), Orientation (..), Point (..), Rectangle (..), Size (..), clampFraction, insetRect, uniform)
import Blink.Layout.Box (children, hBox, vBox)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (ImagePath)
import Blink.View
import Blink.View.Context (Effect (..), UiEffect (..), gets)
import Blink.View.Scroll (contextScrollState)
import Blink.View.Drawing (drawImage, withClip)
import Blink.Element (Element (..), HasLayoutConfig (..), elementWithLayout, height, noIntrinsicSize, part, runElement, width, HasOrientation (..), HasStep (..))
import Blink.Style
import Blink.Controls.Style (iconStyle, plainFillStyle, trackMetrics, thumbStyle, zeroMetrics, plainStyle)

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

-- | The parts a 'scrollBar' is made of, besides itself.
data ScrollBarPart = Decrement | Increment | Track
  deriving Show

-- | The scroll bars a 'scrollViewport' can show.
data ViewportPart = VerticalBar | HorizontalBar
  deriving Show

-- | Every capability 'scrollBar' resolves: the wrapped 'ControlConfig', the
-- caller's layout attributes (applied over the default layout for its axis
-- when the scrollbar is built), which axis it runs along, the proportion of
-- the track its thumb covers, and the step each arrow moves the position by.
data ScrollBarConfig e msg = ScrollBarConfig
  { sbControl         :: ControlConfig e msg
  , sbLayoutAttrs     :: [Attribute Layout]
  , sbOrientation     :: Orientation
  , sbVisibleFraction :: Double
  , sbStep            :: Double
  }

-- | The default 'Layout' for a scrollbar running along @o@: fills the space
-- it's given along that axis, and sizes itself to 'scrollBarThickness'
-- across it.
layoutFor :: Orientation -> Layout
layoutFor Horizontal = Layout fill (exactly scrollBarThickness) TopLeft
layoutFor Vertical   = Layout (exactly scrollBarThickness) fill TopLeft

-- | 'defaultControlConfig' (styled via 'scrollBarStyleKey'), 'Vertical', a
-- visible fraction of 0.2, and a step of 0.05.
defaultScrollBarConfig :: ScrollBarConfig e msg
defaultScrollBarConfig = ScrollBarConfig
  { sbControl         = defaultControlConfig { ccStyleKey = scrollBarStyleKey }
  , sbLayoutAttrs     = []
  , sbOrientation     = Vertical
  , sbVisibleFraction = 0.2
  , sbStep            = 0.05
  }

instance HasControlConfig e msg (ScrollBarConfig e msg) where
  overControl = nested sbControl (\sc x -> sc { sbControl = x })

instance HasEventHandlers (ScrollBarConfig e msg)

instance HasLayoutConfig (ScrollBarConfig e msg) where
  overLayout = appendTo sbLayoutAttrs (\sc as -> sc { sbLayoutAttrs = as })

-- | The scrollbar's layout: the default for its 'orientation', with the
-- caller's 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'
-- applied over it, in whatever order they were given relative to
-- 'orientation'.
scrollBarLayout :: ScrollBarConfig e msg -> Layout
scrollBarLayout sc = resolve (layoutFor (sbOrientation sc)) (sbLayoutAttrs sc)

-- | Which axis the bar runs along: 'Horizontal' arranges the arrows and
-- track left-to-right, 'Vertical' (the default) top-to-bottom. Picks the
-- default layout for that axis; 'Blink.Element.width'\/'Blink.Element.height'
-- still override it, wherever they appear.
instance HasOrientation (ScrollBarConfig e msg) where
  orientation o = Attribute (\sc -> sc { sbOrientation = o })

-- | How much of the scrollable content is visible at once, as a fraction of
-- the whole -- sets the thumb's length as that fraction of the track,
-- clamped to never draw shorter than the minimum grabbable length. Defaults
-- to 0.2.
visibleFraction :: Double -> Attribute (ScrollBarConfig e msg)
visibleFraction v = Attribute (\sc -> sc { sbVisibleFraction = v })

-- | How much each arrow button moves the position by, once per activation
-- (including each repeat while held -- see @arrowButton@). Defaults to 0.05.
instance HasStep (ScrollBarConfig e msg) where
  step s = Attribute (\sc -> sc { sbStep = s })

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
  in min len (max minThumbLength (clampFraction frac * len))

-- | The pixel offset along @o@ of the thumb's own leading edge (of length
-- @thumbLen@) for scroll position @v@ within @bounds@ -- the near end of
-- the track when @v@ is 0, the far end (minus the thumb's own length) when
-- @v@ is 1.
thumbOriginFor :: Orientation -> Rectangle -> Double -> Double -> Double
thumbOriginFor o bounds thumbLen v = axisOrigin o bounds + clampFraction v * travel
  where
    travel = max 0 (axisLength o bounds - thumbLen)

-- | The @[0, 1]@ scroll position whose thumb (of length @thumbLen@) would
-- sit with its leading edge at pixel position @originMain@ along @o@ within
-- @bounds@ -- the inverse of 'thumbOriginFor'. Reads as @0@ when the track
-- has no room for the thumb to travel at all.
fractionForOrigin :: Orientation -> Rectangle -> Double -> Double -> Double
fractionForOrigin o bounds thumbLen originMain
  | travel <= 0 = 0
  | otherwise   = clampFraction ((originMain - axisOrigin o bounds) / travel)
  where
    travel = axisLength o bounds - thumbLen

-- | The pixel distance from the thumb's leading edge (@thumbOrigin0@, of
-- length @thumbLen@) to @mouseMain@, if @mouseMain@ falls within the
-- thumb; otherwise the thumb's own centre, so a track click still jumps
-- the thumb to be centred under it.
grabOffsetAt :: Double -> Double -> Double -> Double
grabOffsetAt thumbOrigin0 thumbLen mouseMain
  | mouseMain >= thumbOrigin0 && mouseMain <= thumbOrigin0 + thumbLen = mouseMain - thumbOrigin0
  | otherwise = thumbLen / 2

-- | Draws the full-length groove (border colour, if set) and the thumb
-- (text colour, shaded for hover\/drag) at its position for @v@.
drawThumb :: Ord e => Orientation -> Rectangle -> Bool -> Bool -> Bool -> Double -> Double -> View e msg ()
drawThumb o bounds disabled hovered dragging frac v =
  withBounds thumb (drawPart scrollBarThumbStyleKey (Set.singleton (commonState disabled dragging hovered)))
  where
    thumbLen = thumbLengthFor o bounds frac
    thumb    = mainRect o bounds (thumbOriginFor o bounds thumbLen v) thumbLen

-- | Each arrow button's fixed size along the main axis, filling the cross
-- axis -- the same shape regardless of which arrow it is.
arrowLayoutAttrs :: HasLayoutConfig cfg => Orientation -> [Attribute cfg]
arrowLayoutAttrs Horizontal = [width (exactly scrollBarThickness), height fill]
arrowLayoutAttrs Vertical   = [width fill, height (exactly scrollBarThickness)]

-- | An arrow button: 'Blink.Controls.RepeatButton.repeatButtonBase''s
-- press-then-hold-repeat behaviour, drawing @path@'s icon (tinted by the
-- resolved style's text colour) instead of a caption.
arrowButton :: Ord e => e -> ImagePath -> [Attribute (RepeatButtonConfig e msg)] -> Element e msg
arrowButton eid path attrs =
  chromeElement (bcLayout btn) (ccStyleKey (bcControl btn)) (elementWithLayout (bcLayout btn) (pure ()))
    (void (repeatButtonBase eid cfg { rbButton = btn { bcControl = ctrl } }))
  where
    cfg  = resolve defaultRepeatButtonConfig attrs
    btn  = rbButton cfg
    ctrl = (bcControl btn) { ccContent = const drawArrow }

    -- | Draws a couple of pixels past the button's own bounds on every
    -- side -- with zero chrome inset of its own (see
    -- 'Blink.Controls.Style.iconStyle', which 'scrollBarButtonStyleKey'
    -- resolves to), the icon otherwise reads a little small against the
    -- button's full, edge-to-edge box.
    drawArrow = do
      s      <- currentStyle
      bounds <- getBounds
      withBounds (insetRect (uniform (-2)) bounds) (drawImage (styleTextColour s) path)

-- | A scrollbar (see the module header). Clicking the bare track jumps the
-- thumb to (and centres it under) the pointer, the same way
-- 'Blink.Controls.Slider.slider' does; grabbing the thumb itself drags it,
-- tracking the pointer without recentring under it; holding either arrow
-- steps the position by 'step', repeating for as long as it's held.
--
-- The arrows and the track are parts of @eid@ (see 'Blink.Element.part'),
-- named by the private @ScrollBarPart@. The position is stored
-- under @eid@ itself -- see the module header for reading it from
-- elsewhere.
scrollBar :: Ord e => e -> [Attribute (ScrollBarConfig e msg)] -> Element e msg
scrollBar eid attrs = controlElement (scrollBarLayout cfg) (box (Control eid)) ctrl
  where
    cfg = resolve defaultScrollBarConfig attrs
    o   = sbOrientation cfg

    -- @barId@ is the bar's own id as stored this frame. Its parts can't
    -- look it up themselves: inside a part, @eid@ means that part.
    box barId = (if o == Horizontal then hBox else vBox)
      [children [decrementBtn barId, trackEl barId, incrementBtn barId]]

    decrementBtn barId = part eid (partName Decrement) $ arrowButton eid
      (if o == Horizontal then "assets/icons/arrow_left.svg" else "assets/icons/arrow_drop_up.svg")
      ( [ style scrollBarButtonStyleKey
        , overControl (focusPolicy NotFocusable)
        , scrollOnActivated barId (negate (sbStep cfg))
        ] ++ arrowLayoutAttrs o
      )

    incrementBtn barId = part eid (partName Increment) $ arrowButton eid
      (if o == Horizontal then "assets/icons/arrow_right.svg" else "assets/icons/arrow_drop_down.svg")
      ( [ style scrollBarButtonStyleKey
        , overControl (focusPolicy NotFocusable)
        , scrollOnActivated barId (sbStep cfg)
        ] ++ arrowLayoutAttrs o
      )

    scrollOnActivated barId dv =
      overButton (appendTo bcOnActivated (\bc hs -> bc { bcOnActivated = hs }) (EffectUi (ScrollBy barId dv)))

    trackEl barId = part eid (partName Track) $
      controlElement (Layout fill fill TopLeft) (Element (Layout fill fill TopLeft) noIntrinsicSize (pure ())) (trackCtrl barId)

    trackCtrl barId = defaultControlConfig
      { ccElementId       = Just eid
      , ccStyleKey        = scrollBarTrackStyleKey
      , ccFocusPolicy     = NotFocusable
      , ccMouseActivation = CaptureActivated
      , ccContent         = trackBody barId
      }

    trackBody barId ci = do
      bounds <- getBounds
      value0 <- gets (contextScrollState barId)
      let thumbLen = thumbLengthFor o bounds (sbVisibleFraction cfg)
      when (not (ciDisabled ci) && ciIsCaptured ci) $ do
        mouseMain <- pointMain o <$> getMousePos
        current   <- getExtentState eid
        -- 'requestExtentBy' accumulates, so the delta zeroes out whatever
        -- the last drag on this track left behind before fixing this
        -- drag's own offset.
        grabOffset <-
          if ciCaptureStarted ci
            then do
              let offset = grabOffsetAt (thumbOriginFor o bounds thumbLen value0) thumbLen mouseMain
              requestExtentBy eid (offset - current)
              pure offset
            else pure current
        let newValue = fractionForOrigin o bounds thumbLen (mouseMain - grabOffset)
        when (newValue /= value0) $ emitUi (ScrollTo barId newValue)
      drawThumb o bounds (ciDisabled ci) (ciHovered ci) (ciIsCaptured ci) (sbVisibleFraction cfg) value0

    ctrl = (sbControl cfg)
      { ccElementId   = Just eid
      , ccFocusPolicy = NotFocusable
      , ccContent     = const (controlIdOf eid >>= runElement . box)
      }

-- * Scrollable viewports

-- | Every capability 'scrollViewport' resolves: how far a wheel notch
-- scrolls, the content's full size, and how to draw it.
data ScrollViewportConfig e msg = ScrollViewportConfig
  { svWheelStep   :: Double
    -- ^ Pixels one mouse-wheel notch scrolls.
  , svContentSize :: Size
    -- ^ The content's full size. Along an axis where it fits, the content
    -- is laid out at the viewport's own size instead.
  , svContent     :: Rectangle -> View e msg ()
    -- ^ Draws the content into the current bounds (its full, scrolled
    -- rectangle), given the part of it currently in view, in the content's
    -- own coordinates.
  }

-- | A clipped area showing @cfg@'s content, with a 'scrollBar' along each
-- axis the content overflows (and a blank corner where both meet). The
-- mouse wheel scrolls the vertical axis while it overflows, otherwise the
-- horizontal one. The bars are parts of @vid@, named by the private
-- @ViewportPart@; see 'scrollViewportTo' to move the vertical
-- one from elsewhere.
scrollViewport :: Ord e => e -> ScrollViewportConfig e msg -> View e msg ()
scrollViewport vid cfg = do
  bounds <- getBounds
  -- A scrollbar shown on one axis takes space from the other, which can
  -- itself tip that axis into overflow -- so the overflow check runs
  -- twice: once against the full bounds, once against what's left after
  -- the first pass's own bar(s).
  let viewportW0 = rectWidth bounds
      viewportH0 = rectHeight bounds
      showV0     = overflows contentH viewportH0
      showH0     = overflows contentW viewportW0
      viewportW  = viewportW0 - (if showV0 then scrollBarThickness else 0)
      viewportH  = viewportH0 - (if showH0 then scrollBarThickness else 0)
      showV      = overflows contentH viewportH
      showH      = overflows contentW viewportW
  if not showV && not showH
    then svContent cfg (Rectangle 0 0 viewportW0 viewportH0)
    else runElement (scrollableArea viewportW viewportH showV showH)
  where
    Size contentW contentH = svContentSize cfg

    overflows content viewport = content > max 0 viewport

    barState bar = do
      self <- controlIdOf vid
      gets (contextScrollState (partId self (partName bar)))

    scrollableArea viewportW viewportH showV showH = vBox
      [ children
          ( hBox
              [ children
                  ( elementWithLayout (Layout fill fill TopLeft) (clippedContent viewportW viewportH showV showH)
                    : [ vBar | showV ]
                  )
              ]
            : [ hBox
                  [ height (exactly scrollBarThickness)
                  , children (hBar : [ corner | showV ])
                  ]
              | showH
              ]
          )
      ]
      where
        vBar = part vid (partName VerticalBar) $ scrollBar vid
          [ orientation Vertical, height fill, visibleFraction (viewportH / contentH) ]
        hBar = part vid (partName HorizontalBar) $ scrollBar vid
          [ orientation Horizontal, width fill, visibleFraction (viewportW / contentW) ]
        corner = elementWithLayout (Layout (exactly scrollBarThickness) (exactly scrollBarThickness) TopLeft) (pure ())

    -- 'withClip' must capture this bounds -- the viewport's own, not yet
    -- offset -- before the content moves within it.
    clippedContent viewportW viewportH showV showH = do
      applyWheel viewportW viewportH showV showH
      bounds <- getBounds
      hFrac  <- if showH then barState HorizontalBar else pure 0
      vFrac  <- if showV then barState VerticalBar else pure 0
      let offsetX = if showH then hFrac * (contentW - viewportW) else 0
          offsetY = if showV then vFrac * (contentH - viewportH) else 0
          contentBounds = bounds
            { rectX      = rectX bounds - offsetX
            , rectY      = rectY bounds - offsetY
            , rectWidth  = if showH then contentW else rectWidth bounds
            , rectHeight = if showV then contentH else rectHeight bounds
            }
          inView = Rectangle offsetX offsetY (rectWidth bounds) (rectHeight bounds)
      withClip $ withBounds contentBounds (svContent cfg inView)

    -- Only a vertical wheel delta exists in the input model, so it drives
    -- whichever axis actually scrolls, favouring vertical. Checked against
    -- the viewport's own (unscrolled) bounds, and deferred like every
    -- other user gesture.
    applyWheel viewportW viewportH showV showH = do
      wheel <- getWheelDelta
      when (wheel /= 0) $ do
        over <- isRegionHit
        when over $ case (showV, showH) of
          (True, _)      -> scrollBy VerticalBar (contentH - viewportH) wheel
          (False, True)  -> scrollBy HorizontalBar (contentW - viewportW) wheel
          (False, False) -> pure ()

    scrollBy bar maxOffset wheel =
      when (maxOffset > 0) $ do
        self <- controlIdOf vid
        emitUi (ScrollBy (partId self (partName bar)) (wheel * svWheelStep cfg / maxOffset))

-- | The id of the vertical scroll bar of the viewport @self@ -- where
-- that viewport's vertical position is stored.
verticalBarOf :: ControlId e -> ControlId e
verticalBarOf self = partId self (partName VerticalBar)

-- | Scrolls the vertical scroll bar of the viewport @vid@ to @position@,
-- from @0@ (top) to @1@ (bottom), from the next frame onward. Callable
-- from 'View' or 'Blink.Update.Update'.
scrollViewportTo :: (Ord e, Monad m, HasUiEffect e m) => e -> Double -> m ()
scrollViewportTo vid position = do
  self <- controlIdFor vid
  queueEffect (ScrollTo (verticalBarOf self) position)

-- * Style

-- | 'StyleKey's 'Blink.Controls.ScrollBar.scrollBar' resolves its own
-- chrome, its arrow buttons, and its track (whose background is the
-- groove) from unless overridden via 'Blink.Controls.Control.style'.
scrollBarStyleKey, scrollBarButtonStyleKey, scrollBarTrackStyleKey :: StyleKey e
scrollBarStyleKey       = Class "scrollBar"
scrollBarButtonStyleKey = Class "scrollBarButton"
scrollBarTrackStyleKey  = Class "scrollBarTrack"

-- | The 'StyleKey' the thumb resolves its style from, with the track's
-- hover as 'CommonMouseOver' and a drag as 'CommonPressed'.
scrollBarThumbStyleKey :: StyleKey e
scrollBarThumbStyleKey = Class "scrollBarThumb"

-- | This control's entries in 'Blink.Style.Defaults.defaultTheme': its
-- outer container reuses the plain wrapper look, its arrow buttons the
-- plain-icon look (just the icon, recolouring on hover), its track a flat
-- groove, and its thumb the same look as a slider's.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (scrollBarStyleKey,       (zeroMetrics, plainStyle p))
  , (scrollBarButtonStyleKey, (zeroMetrics, iconStyle p))
  , (scrollBarTrackStyleKey,  (trackMetrics, plainFillStyle p (paletteBorder p)))
  , (scrollBarThumbStyleKey,  (zeroMetrics, thumbStyle p))
  ]
