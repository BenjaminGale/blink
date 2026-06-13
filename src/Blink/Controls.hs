{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls

Standard UI controls built on top of "Blink.UI". Each control takes an element
ID of type @e@ used for styling and interaction tracking; see "Blink.UI" for an
explanation of element IDs, commands, and the render loop.

= Value-callback pattern

Controls that edit application data receive the current value and a function
producing a state modifier from an updated value, dispatched whenever the
user makes a change:

@
textInput NameInput (userName s) (\\t st -> st { userName = t })
@

The host applies the modifier once the frame completes; the control reads the
new value back from the application state on the next frame. This keeps all
application data outside the UI tree.

= Control state

Controls whose state is presentational rather than application data — a
scrollbar's position, for example — read and write it directly through the
primitives in "Blink.UI" ('getScrollState', 'setScrollState', and the
selection counterparts). State is keyed by element ID, populates lazily on
first write, and persists across frames inside the 'UIContext'. The application
never sees the traffic.
-}
module Blink.Controls
  ( -- * Display
    label
  , ProgressValue (..)
  , progressBar
    -- * Input
  , button
  , checkbox
  , radioGroup
  , textInput
    -- * Scroll
  , ScrollBarPart (..)
  , scrollBar
  , readScrollPos
  , writeScrollPos
  , scrollThumbRect
  , scrollPosFromMouse
    -- * Scrollable regions
  , ScrollRegionPart (..)
  , scrollableRegion
  , scrollableDynamic
  , scrollRegionBarSize
    -- * Slider
  , SliderPart (..)
  , slider
  , sliderThumbRect
    -- * Building controls
  , control
  , renderControl
  , isActivatedBy
  , whenFocused
  , isKeyPressed
  ) where

import Control.Monad (when, forM_)
import Data.List (foldl', find)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Blink.Geometry (Alignment (..), Orientation (..), Point (..), Rectangle (..), insetRect)
import Blink.Input (ButtonState (..), Key (..), KeyEvent (..), Modifier (..), InputState (..))
import Blink.Layout (Layout (..), Length (..), BoxConfig (..), hBox, vBox, defaultBoxConfig)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI

-- | Read-only text display. Renders @text@ within the element's content
-- rectangle using the active style. Does not participate in interaction or
-- keyboard navigation.
label :: (Eq e, Ord e) => e -> Text -> UI e s ()
label eid text = renderControl eid $ do
  style <- getStyle eid
  drawText (styleTextColour style) (styleTextAlign style) text

-- | The value passed to 'progressBar'.
data ProgressValue
  = Progress Double
    -- ^ A determinate value in @[0, 1]@, clamped and rendered as a filled bar.
  | Indeterminate
    -- ^ Unknown progress: a band animates continuously across the bar.
  deriving (Eq, Show)

-- | A read-only progress indicator. Pass 'Progress' for a determinate bar or
-- 'Indeterminate' for a continuously animating band indicating activity of
-- unknown duration. The animation runs only on ticker frames; 'requiresAnimation'
-- keeps the ticker active while an 'Indeterminate' bar is visible.
progressBar :: (Eq e, Ord e) => e -> ProgressValue -> UI e s ()
progressBar eid (Progress value) = renderControl eid $ do
  style <- getStyle eid
  r     <- getBounds
  let clamped  = max 0 (min 1 value)
      fillRect' = r { rectWidth = rectWidth r * clamped }
  withBounds fillRect' $ fillRect (styleTextColour style)
progressBar eid Indeterminate = do
  requiresAnimation
  renderControl eid $ do
    r       <- getBounds
    style   <- getStyle eid
    elapsed <- getAnimElapsed
    let t     = realToFrac elapsed * (0.5 :: Double)
        phase = t - fromIntegral (floor t :: Int)
        bandW = rectWidth r * 0.3
        left  = rectX r - bandW + (rectWidth r + bandW) * phase
    withBounds (r { rectX = left, rectWidth = bandW }) $
      fillRect (styleTextColour style)

checkboxMark :: (Eq e, Ord e) => e -> Bool -> (Bool -> s -> s) -> UI e s ()
checkboxMark boxId checked onToggle = control boxId $ do
  style     <- getStyle boxId
  activated <- isActivatedBy [KeyReturn, KeySpace] boxId
  when checked   $ drawText (styleTextColour style) AlignCenter "✓"
  when activated $ dispatch (onToggle (not checked))

checkboxLabel :: Style -> Text -> UI e s ()
checkboxLabel style = drawText (styleTextColour style) AlignLeft

-- | A togglable checkbox with an adjacent label. Dispatches the state modifier
-- @onToggle (not checked)@ when activated by a click or the Enter key.
checkbox :: (Eq e, Ord e) => e -> Text -> Bool -> (Bool -> s -> s) -> UI e s ()
checkbox boxId text checked onToggle = do
  style <- getStyle boxId
  hBox (defaultBoxConfig { boxSpacing = 4, boxFillCross = False })
    [ (Layout (Exactly 20) (Exactly 20) MiddleLeft, checkboxMark boxId checked onToggle)
    , (Layout Fill Fill MiddleLeft, checkboxLabel style text)
    ]
  whenFocused boxId $ do
    styleSet <- getStyleSet boxId
    let s = styleSetFocused styleSet
    case styleBorderColour s of
      Just c  -> strokeRect c (styleBorderWidth s)
      Nothing -> pure ()

-- | A group of mutually exclusive options. Each item renders as a radio mark
-- and label; activating one — by click, Enter, or Space — dispatches
-- @onChange value@. Multiple groups on screen each bind to their own
-- application-state field; no shared state is required.
radioGroup :: (Eq e, Ord e, Eq a)
           => (Int -> e)     -- ^ maps item index to an element ID
           -> [(a, Text)]    -- ^ @(value, label)@ pairs
           -> a              -- ^ currently selected value
           -> (a -> s -> s)
           -> UI e s ()
radioGroup mkId items selected onChange = do
  initialFocus <- getFocus
  vBox defaultBoxConfig (zipWith (mkItem initialFocus) [0..] items)
  where
    lastIdx = length items - 1
    mkItem initialFocus idx (val, lbl) =
      let eid = mkId idx
      in ( Layout Fill Fill TopLeft
         , control eid $ do
             style     <- getStyle eid
             activated <- isActivatedBy [KeyReturn, KeySpace] eid
             drawText (styleTextColour style) AlignLeft $
               (if selected == val then "● " else "○ ") <> lbl
             when activated $ dispatch (onChange val)
             when (initialFocus == Just eid) $ do
               upPressed   <- isKeyPressed eid KeyUp
               downPressed <- isKeyPressed eid KeyDown
               when upPressed   $ setFocus (mkId (max 0 (idx - 1)))
               when downPressed $ setFocus (mkId (min lastIdx (idx + 1)))
         )

-- | A clickable button labelled @txt@. Returns 'True' on the frame the button
-- is activated — by a left-click or by pressing Enter while focused.
button :: (Eq e, Ord e) => e -> Text -> UI e s Bool
button eid txt = do
  control eid $ do
    style <- getStyle eid
    drawText (styleTextColour style) (styleTextAlign style) txt
  isActivatedBy [KeyReturn] eid

-- | A single-line text entry field. Supports click-to-place cursor, drag
-- selection, Shift+arrow extension, and selection-aware editing.
textInput :: (Eq e, Ord e) => e -> Text -> (Text -> s -> s) -> UI e s ()
textInput eid value onChange = do
  wasCapturing <- isDragging eid
  control eid $ do
    style    <- getStyle eid
    hasFocus <- isFocused eid
    disabled <- isDisabled
    bounds   <- getBounds
    input    <- getInput
    sel      <- getSelection eid

    let defPos  = T.length value
        anchor0 = maybe defPos selAnchor sel
        active0 = maybe defPos selActive sel

    -- Mouse: click sets both ends; drag extends active only.
    (anchor1, active1) <-
      if hasFocus && not disabled then do
        isCapturing <- isDragging eid
        if isCapturing then do
          mousePos <- getMousePos
          let localX = realToFrac (pointX mousePos - rectX bounds) :: Float
          clickedPos <- charAtOffsetUI value localX
          pure $ if not wasCapturing
            then (clickedPos, clickedPos)
            else (anchor0, clickedPos)
        else pure (anchor0, active0)
      else pure (anchor0, active0)

    -- Keyboard: Shift+arrows extend selection; plain arrows collapse it.
    let keyEvts    = inputKeyEvents input
        len        = T.length value
        hasSel1    = anchor1 /= active1
        selLo1     = min anchor1 active1
        selHi1     = max anchor1 active1
        shiftLeft  = hasFocus && any (\e -> key e == KeyLeft  && Shift `elem`    modifiers e) keyEvts
        shiftRight = hasFocus && any (\e -> key e == KeyRight && Shift `elem`    modifiers e) keyEvts
        plainLeft  = hasFocus && any (\e -> key e == KeyLeft  && Shift `notElem` modifiers e) keyEvts
        plainRight = hasFocus && any (\e -> key e == KeyRight && Shift `notElem` modifiers e) keyEvts
        (anchor2, active2)
          | shiftLeft  = (anchor1, max 0    (active1 - 1))
          | shiftRight = (anchor1, min len  (active1 + 1))
          | plainLeft  = let p = if hasSel1 then selLo1 else max 0   (active1 - 1) in (p, p)
          | plainRight = let p = if hasSel1 then selHi1 else min len (active1 + 1) in (p, p)
          | otherwise  = (anchor1, active1)

    when (hasFocus && not disabled) $
      setSelection eid (Selection anchor2 active2)

    -- Editing: typed text and backspace, selection-aware.
    when (hasFocus && not disabled) $ do
      let backspace = any (\e -> key e == KeyBackspace) keyEvts
          typed     = foldl' (<>) T.empty (inputTypedText input)
          hasTyped  = not (T.null typed)
          hasSel2   = anchor2 /= active2
          selLo2    = min anchor2 active2
          selHi2    = max anchor2 active2
      when (backspace || hasTyped) $ do
        let (newText, newCursor)
              | hasSel2 && backspace =
                  (T.take selLo2 value <> T.drop selHi2 value, selLo2)
              | hasSel2 =
                  (T.take selLo2 value <> typed <> T.drop selHi2 value, selLo2 + T.length typed)
              | backspace && active2 > 0 =
                  (T.take (active2 - 1) value <> T.drop active2 value, active2 - 1)
              | hasTyped =
                  (T.take active2 value <> typed <> T.drop active2 value, active2 + T.length typed)
              | otherwise = (value, active2)
        when (newText /= value) $ dispatch (onChange newText)
        setSelection eid (Selection newCursor newCursor)

    -- Drawing: selection highlight, text, then cursor.
    finalSel <- getSelection eid
    let finalAnchor = maybe defPos selAnchor finalSel
        finalActive = maybe defPos selActive finalSel
        finalLo     = min finalAnchor finalActive
        finalHi     = max finalAnchor finalActive

    when (hasFocus && finalLo < finalHi) $ do
      loX <- charOffsetUI value finalLo
      hiX <- charOffsetUI value finalHi
      let selRect = Rectangle
            (rectX bounds + realToFrac loX)
            (rectY bounds)
            (realToFrac (hiX - loX))
            (rectHeight bounds)
      withBounds selRect $ fillRect (RGBA 0.3 0.5 1.0 0.4)

    drawText (styleTextColour style) (styleTextAlign style) value

    when (hasFocus && not disabled) $ do
      curX <- charOffsetUI value finalActive
      let cursorRect = Rectangle
            (rectX bounds + realToFrac curX)
            (rectY bounds)
            1
            (rectHeight bounds)
      withBounds cursorRect $ fillRect (styleTextColour style)

-- | Sub-parts of a scrollbar, used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | VScroll ScrollBarPart
-- scrollBar VScroll Vertical ratio
-- @
data ScrollBarPart
  = ScrollTrack   -- ^ The track area behind the thumb.
  | ScrollThumb   -- ^ The draggable thumb.
  | ScrollDecrBtn -- ^ The decrement arrow button.
  | ScrollIncrBtn -- ^ The increment arrow button.
  deriving (Eq, Ord, Show)

-- | A scrollbar with decrement\/increment buttons flanking a draggable thumb.
-- The scroll position in @[0, 1]@ is stored in the 'UIContext', keyed by
-- @mkId ScrollTrack@; the control reads and writes it itself. @thumbRatio@ is
-- the fraction of the track the thumb fills (visible \/ total), also in
-- @[0, 1]@. Button clicks step by @thumbRatio@; dragging centres the thumb on
-- the cursor.
scrollBar :: (Eq e, Ord e)
          => (ScrollBarPart -> e)
          -> Orientation
          -> Double
          -> UI e s ()
scrollBar mkId ori thumbRatio = do
  bounds <- getBounds
  pos <- readPos
  let pos'      = max 0 (min 1 pos)
      ratio'    = max 0 (min 1 thumbRatio)
      btnLayout = case ori of
        Vertical   -> Layout Fill (Exactly (rectWidth bounds))  TopLeft
        Horizontal -> Layout (Exactly (rectHeight bounds)) Fill TopLeft
  layoutFn defaultBoxConfig
    [ (btnLayout, decrBtn pos' ratio')
    , (Layout Fill Fill TopLeft, track pos' ratio')
    , (btnLayout, incrBtn pos' ratio')
    ]
  where
    trackId = mkId ScrollTrack

    readPos = getScrollState trackId

    writePos v = setScrollState trackId v

    layoutFn = case ori of
      Vertical   -> vBox
      Horizontal -> hBox

    decrSym = case ori of
      Vertical   -> "▲"
      Horizontal -> "◀"
    incrSym = case ori of
      Vertical   -> "▼"
      Horizontal -> "▶"

    decrBtn pos' ratio' = do
      clicked <- button (mkId ScrollDecrBtn) decrSym
      when clicked $ writePos (max 0 (pos' - ratio'))

    incrBtn pos' ratio' = do
      clicked <- button (mkId ScrollIncrBtn) incrSym
      when clicked $ writePos (min 1 (pos' + ratio'))

    track pos' ratio' = do
      slotBounds <- getBounds
      styleSet   <- getStyleSet trackId
      let normalStyle = styleSetNormal styleSet
          bgRect      = insetRect (styleMargin normalStyle) slotBounds
          contentRect = insetRect (stylePadding normalStyle) bgRect
          thumbR      = scrollThumbRect ori pos' ratio' contentRect
      control trackId $
        withBounds thumbR $ renderControl (mkId ScrollThumb) $ pure ()
      dragging <- isDragging trackId
      when dragging $ do
        mousePos <- getMousePos
        writePos (scrollPosFromMouse ori ratio' contentRect mousePos)

-- | Read the current scroll position for the scrollbar keyed by @trackId@,
-- returning @0@ if no position has been recorded yet. Use this to
-- programmatically observe scroll state — for example, to show a "Back to
-- top" button only when the user has scrolled down.
readScrollPos :: Ord e => e -> UI e s Double
readScrollPos = getScrollState

-- | Overwrite the scroll position for the scrollbar keyed by @trackId@. The
-- value is clamped to @[0, 1]@. Use this to drive scroll position from
-- application logic — for example, a "Scroll to top" button or resetting
-- position when the content changes.
writeScrollPos :: Ord e => e -> Double -> UI e s ()
writeScrollPos trackId v = setScrollState trackId (max 0 (min 1 v))

-- | Computes the bounding rectangle of a scrollbar thumb within a track.
-- Exported for callers that build custom scroll surfaces or need to hit-test
-- the thumb independently of the standard 'scrollBar' widget. @pos@ and
-- @ratio@ are both in @[0, 1]@; the result is a sub-rectangle of @r@.
scrollThumbRect :: Orientation -> Double -> Double -> Rectangle -> Rectangle
scrollThumbRect Vertical pos ratio r =
  let h = rectHeight r * ratio
  in r { rectY = rectY r + (rectHeight r - h) * pos, rectHeight = h }
scrollThumbRect Horizontal pos ratio r =
  let w = rectWidth r * ratio
  in r { rectX = rectX r + (rectWidth r - w) * pos, rectWidth = w }

-- | Converts a mouse position to a scroll position in @[0, 1]@, centring the
-- thumb on the cursor. This is the inverse of 'scrollThumbRect' and is
-- exported for the same reason: callers building custom drag handlers can
-- reuse it rather than duplicating the clamping arithmetic. Returns @0@ when
-- the thumb fills the track (@ratio = 1@) and there is no range to scroll.
scrollPosFromMouse :: Orientation -> Double -> Rectangle -> Point -> Double
scrollPosFromMouse Vertical ratio r mouse =
  let thumbH = rectHeight r * ratio
      range  = rectHeight r - thumbH
  in if range <= 0 then 0
     else max 0 (min 1 ((pointY mouse - rectY r - thumbH / 2) / range))
scrollPosFromMouse Horizontal ratio r mouse =
  let thumbW = rectWidth r * ratio
      range  = rectWidth r - thumbW
  in if range <= 0 then 0
     else max 0 (min 1 ((pointX mouse - rectX r - thumbW / 2) / range))

-- | Sub-parts of a scrollable region's element ID hierarchy. Wraps
-- 'ScrollBarPart' for the horizontal and vertical scrollbars:
--
-- @
-- data Element = ... | MyRegion ScrollRegionPart
-- scrollableRegion MyRegion 600 400 content
-- @
data ScrollRegionPart
  = ScrollRegionH ScrollBarPart -- ^ A part of the horizontal scrollbar.
  | ScrollRegionV ScrollBarPart -- ^ A part of the vertical scrollbar.
  deriving (Eq, Ord, Show)

-- | The pixel width of a scrollbar strip used by 'scrollableRegion' and
-- 'scrollableDynamic'. Exported so callers that compose a scrollable region
-- inside their own layout can account for the strip in their geometry without
-- hard-coding the value.
scrollRegionBarSize :: Double
scrollRegionBarSize = 16

-- | A scrollable region with a known virtual content size. Scrollbars appear
-- automatically on axes where the content exceeds the viewport. The content
-- action runs with virtual bounds — the full content rectangle translated so
-- the scrolled portion aligns with the viewport — clipped to the visible area.
-- Mouse interaction works naturally because translated bounds are in window
-- coordinates; the clip region hides the rest.
scrollableRegion
  :: (Eq e, Ord e)
  => (ScrollRegionPart -> e)  -- ^ maps region parts to element IDs
  -> Double                    -- ^ virtual content width
  -> Double                    -- ^ virtual content height
  -> UI e s ()                 -- ^ content
  -> UI e s ()
scrollableRegion mkId cw ch content = do
  outer <- getBounds
  let ow      = rectWidth outer
      oh      = rectHeight outer
      -- Two-pass: check V with full height to determine reduced width, then H,
      -- then re-check V with reduced height.
      needsV1 = ch > oh
      vpW1    = if needsV1 then ow - scrollRegionBarSize else ow
      needsH  = cw > vpW1
      vpH     = if needsH  then oh - scrollRegionBarSize else oh
      needsV  = ch > vpH
      vpW     = if needsV  then ow - scrollRegionBarSize else ow
      hThumb  = max 0 (min 1 (vpW / cw))
      vThumb  = max 0 (min 1 (vpH / ch))
      vpRect  = outer { rectWidth = vpW,                    rectHeight = vpH }
      hBar    = outer { rectY = rectY outer + vpH,          rectHeight = scrollRegionBarSize, rectWidth = vpW }
      vBar    = outer { rectX = rectX outer + vpW,          rectWidth  = scrollRegionBarSize, rectHeight = vpH }
  -- Render scrollbars first so position updates take effect before the content
  -- offset is computed.
  when needsH $ withBounds hBar $ scrollBar (mkId . ScrollRegionH) Horizontal hThumb
  when needsV $ withBounds vBar $ scrollBar (mkId . ScrollRegionV) Vertical   vThumb
  hPos <- if needsH then getScrollState (mkId (ScrollRegionH ScrollTrack)) else pure 0
  vPos <- if needsV then getScrollState (mkId (ScrollRegionV ScrollTrack)) else pure 0
  let offsetX    = hPos * max 0 (cw - vpW)
      offsetY    = vPos * max 0 (ch - vpH)
      virtBounds = outer
        { rectX      = rectX outer - offsetX
        , rectY      = rectY outer - offsetY
        , rectWidth  = cw
        , rectHeight = ch
        }
  withBounds vpRect $ clipToCurrent $
    withBounds virtBounds content

-- | A scrollable region where the caller controls content rendering. Renders
-- scrollbars for the axes where a thumb ratio is supplied, then calls
-- @content hFrac vFrac@ with the current scroll fractions in @[0, 1]@. The
-- content runs within the viewport rectangle (full bounds minus scrollbar
-- strips) so @getBounds@ returns the available content area.
--
-- Pass 'Nothing' to suppress a scrollbar on an axis entirely. A typical thumb
-- ratio is @viewportSize / contentSize@; the caller uses the returned fractions
-- to determine which portion of the virtual content to render.
scrollableDynamic
  :: (Eq e, Ord e)
  => (ScrollRegionPart -> e)           -- ^ maps region parts to element IDs
  -> Maybe Double                       -- ^ horizontal scrollbar thumb ratio
  -> Maybe Double                       -- ^ vertical scrollbar thumb ratio
  -> (Double -> Double -> UI e s ())   -- ^ @content hFrac vFrac@
  -> UI e s ()
scrollableDynamic mkId hThumb vThumb content = do
  outer <- getBounds
  let ow     = rectWidth outer
      oh     = rectHeight outer
      vpW    = maybe ow (\_ -> ow - scrollRegionBarSize) vThumb
      vpH    = maybe oh (\_ -> oh - scrollRegionBarSize) hThumb
      vpRect = outer { rectWidth = vpW,               rectHeight = vpH }
      hBar   = outer { rectY = rectY outer + vpH,     rectHeight = scrollRegionBarSize, rectWidth = vpW }
      vBar   = outer { rectX = rectX outer + vpW,     rectWidth  = scrollRegionBarSize, rectHeight = vpH }
  case hThumb of
    Nothing -> pure ()
    Just r  -> withBounds hBar $ scrollBar (mkId . ScrollRegionH) Horizontal r
  case vThumb of
    Nothing -> pure ()
    Just r  -> withBounds vBar $ scrollBar (mkId . ScrollRegionV) Vertical r
  hPos <- maybe (pure 0) (\_ -> getScrollState (mkId (ScrollRegionH ScrollTrack))) hThumb
  vPos <- maybe (pure 0) (\_ -> getScrollState (mkId (ScrollRegionV ScrollTrack))) vThumb
  withBounds vpRect $ clipToCurrent $ content hPos vPos

-- | Sub-parts of a slider, used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | HSlider SliderPart
-- slider HSlider Horizontal value (\\v s -> s { volume = v })
-- @
data SliderPart
  = SliderTrack -- ^ The track area behind the thumb.
  | SliderThumb -- ^ The draggable thumb.
  deriving (Eq, Ord, Show)

-- | A slider mapping a draggable thumb to a value in @[0, 1]@. Dispatches
-- @onChange newValue@ when the user drags, clicks on the track, or nudges
-- with arrow keys (Left\/Right for 'Horizontal', Up\/Down for 'Vertical').
-- The thumb is square: its side equals the cross-axis of the track's content
-- rectangle. Arrow-key steps are 0.05.
slider :: (Eq e, Ord e)
       => (SliderPart -> e)
       -> Orientation
       -> Double
       -> (Double -> s -> s)
       -> UI e s ()
slider mkId ori value onChange = do
  let trackId = mkId SliderTrack
      clamped = max 0 (min 1 value)
  slotBounds <- getBounds
  styleSet   <- getStyleSet trackId
  let normalStyle = styleSetNormal styleSet
      bgRect      = insetRect (styleMargin normalStyle) slotBounds
      contentRect = insetRect (stylePadding normalStyle) bgRect
      (crossSz, mainSz) = case ori of
        Horizontal -> (rectHeight contentRect, rectWidth contentRect)
        Vertical   -> (rectWidth contentRect,  rectHeight contentRect)
      thumbRatio  = if mainSz > 0 then crossSz / mainSz else 0
      thumbR      = sliderThumbRect ori clamped contentRect
  control trackId $
    withBounds thumbR $ renderControl (mkId SliderThumb) $ pure ()
  dragging <- isDragging trackId
  when dragging $ do
    mousePos <- getMousePos
    dispatch (onChange (scrollPosFromMouse ori thumbRatio contentRect mousePos))
  let step = 0.05
      (decrKey, incrKey) = case ori of
        Horizontal -> (KeyLeft,  KeyRight)
        Vertical   -> (KeyUp,    KeyDown)
  decrPressed <- isKeyPressed trackId decrKey
  incrPressed <- isKeyPressed trackId incrKey
  when decrPressed $ dispatch (onChange (max 0 (clamped - step)))
  when incrPressed $ dispatch (onChange (min 1 (clamped + step)))

-- | Computes the bounding rectangle of a slider thumb within a track. The
-- thumb is square: its side equals the cross-axis of @r@. Exported alongside
-- 'scrollThumbRect' for callers building custom slider rendering or hit-testing
-- outside the standard 'slider' widget. @pos@ is in @[0, 1]@.
sliderThumbRect :: Orientation -> Double -> Rectangle -> Rectangle
sliderThumbRect Horizontal pos r =
  let sz    = rectHeight r
      range = max 0 (rectWidth r - sz)
  in r { rectX = rectX r + range * pos, rectWidth = sz }
sliderThumbRect Vertical pos r =
  let sz    = rectWidth r
      range = max 0 (rectHeight r - sz)
  in r { rectY = rectY r + range * pos, rectHeight = sz }

-- | Style-aware rendering for a control. Applies the element's margin, draws
-- its background and border, and runs @content@ within the padded content
-- rectangle. Does not perform hover detection, focus management, or tab
-- navigation — use this for display-only elements that should not participate
-- in interaction. See 'control' for the interactive counterpart.
renderControl :: Ord e => e -> UI e s () -> UI e s ()
renderControl eid content = do
  style <- getStyle eid
  r     <- getBounds
  let bgRect      = insetRect (styleMargin style) r
      contentRect = insetRect (stylePadding style) bgRect
      inner       = withBounds contentRect $ clipToCurrent content
  withBounds bgRect $
    withBackground (styleBackground style) $
    case styleBorderColour style of
      Just c  -> withBorder c (styleBorderWidth style) inner
      Nothing -> inner

-- | The standard entry point for interactive controls. Applies hover detection,
-- focus management, and Tab\/Shift-Tab navigation, then delegates to
-- 'renderControl' for style-aware rendering. @content@ runs inside the padded
-- content rectangle.
--
-- @
-- control eid $ do
--   style <- getStyle eid
--   drawText (styleTextColour style) (styleTextAlign style) label
-- @
control :: (Eq e, Ord e) => e -> UI e s () -> UI e s ()
control eid content = do
  applyHover eid
  applyFocus eid
  applyTabNavigation eid
  renderControl eid content

applyHover :: (Eq e, Ord e) => e -> UI e s ()
applyHover eid = do
  whenEnabled $ do
    free     <- isMouseFree
    dragging <- isDragging eid
    when (free || dragging) $ do
      s <- getStyle eid
      r <- getBounds
      let bgRect = insetRect (styleMargin s) r
      isHit <- withBounds bgRect regionHit
      when isHit $ setHovered eid

applyFocus :: (Eq e, Ord e) => e -> UI e s ()
applyFocus eid = do
  whenEnabled $ do
    currentFocus <- getFocus
    isHit        <- isHovered eid
    btn          <- getLeftButton
    captured     <- getCapturedElement
    let nothingIsFocused = isNothing currentFocus
        isRetainingFocus = currentFocus == Just eid
        -- A drag release is when the button is released over a different
        -- element than the one that was captured. Focus should not transfer
        -- in that case — the drag origin retains focus.
        isDragRelease = btn == ButtonReleased && isJust captured && captured /= Just eid
        wasClicked    = isHit && btn == ButtonReleased && not isDragRelease
    setFocusWhen ((nothingIsFocused || isRetainingFocus || wasClicked) && not isDragRelease) eid

applyTabNavigation :: (Eq e, Ord e) => e -> UI e s ()
applyTabNavigation eid = do
  hasFocus <- isFocused eid
  input    <- getInput
  prevCtrl <- getPreviousTabStop
  let tabKey          = find (\e -> key e == KeyTab) (inputKeyEvents input)
      tabPressed      = maybe False (\e -> Shift `notElem` modifiers e) tabKey
      shiftTabPressed = maybe False (\e -> Shift `elem`    modifiers e) tabKey
  when (hasFocus && tabPressed) $ do
    clearFocus
    consumeKey KeyTab
  when (hasFocus && shiftTabPressed) $
    forM_ prevCtrl $ \prev -> do
      setFocus prev
      consumeKey KeyTab
  whenEnabled $ do
    setPreviousTabStop eid

-- | 'True' when the element is clicked or any of the given keys are pressed
-- while it is focused, and the element is not disabled. Use this to implement
-- the activation behaviour of interactive controls.
isActivatedBy :: (Eq e, Ord e) => [Key] -> e -> UI e s Bool
isActivatedBy keys eid = do
  clicked  <- isClicked eid
  keyPress <- or <$> mapM (isKeyPressed eid) keys
  disabled <- isDisabled
  return (not disabled && (clicked || keyPress))

-- | Runs an action only when the given element holds keyboard focus.
whenFocused :: Eq e => e -> UI e s () -> UI e s ()
whenFocused eid action = isFocused eid >>= \f -> when f action

-- | 'True' when the element holds focus and a key event for @k@ is present
-- in the current frame's input queue.
isKeyPressed :: Eq e => e -> Key -> UI e s Bool
isKeyPressed eid k = do
  hasFoc  <- isFocused eid
  pressed <- any (\e -> key e == k) . inputKeyEvents <$> getInput
  return (hasFoc && pressed)
