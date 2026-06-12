{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
scrollbar's position, for example — keep it in 'StandardControls', a record
of maps keyed by element ID and stored in the UI state record @u@ (see
"Blink.UI"). Per-instance state is keyed by a representative element of the
control, so every instance gets its own slot automatically; absent keys read
as the control's initial state and the maps populate lazily on first write.

Applications that need no other control state use 'StandardControls' directly
as @u@:

@
demoApp = App { initialUIState = emptyStandardControls, ... }
@

Applications with additional custom control state embed the standard record
alongside their own and provide a 'HasStandardControls' instance pointing at
it:

@
data MyControls e = MyControls
  { myStandard :: StandardControls e
  , myCustom :: Map e MyCustomState
  }

instance HasStandardControls e (MyControls e) where
  getStandardControls = myStandard
  setStandardControls sc c = c { myStandard = sc }
@
-}
module Blink.Controls
  ( -- * Control state
    StandardControls (..)
  , emptyStandardControls
  , HasStandardControls (..)
  , ScrollState (..)
    -- * Display
  , label
  , progressBar
  , indeterminateProgressBar
    -- * Input
  , button
  , checkbox
  , textInput
    -- * Scroll
  , ScrollBarPart (..)
  , scrollBar
    -- * Slider
  , SliderPart (..)
  , slider
  ) where

import Control.Monad (when)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Blink.Geometry (Alignment (..), Orientation (..), Point (..), Rectangle (..), insetRect)
import Blink.Input (Key (..), InputState (..))
import Blink.Layout (Layout (..), Length (..), BoxConfig (..), hBox, vBox, defaultBoxConfig)
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI

-- | Per-instance presentation state for a scrollbar: its position in @[0, 1]@.
newtype ScrollState = ScrollState { scrollPosition :: Double }
  deriving (Eq, Show)

-- | The presentation state needed by the standard controls, keyed by element
-- ID. Serves directly as the UI state record @u@ for applications with no
-- custom control state; see the module introduction for embedding it
-- alongside custom state.
data StandardControls e = StandardControls
  { scScrollStates :: Map e ScrollState
  , scAnimPhases   :: Map e Double
    -- ^ Animation phase in @[0, 1)@ for each animated control instance,
    -- keyed by element ID. Populated lazily on first write.
  }

-- | A 'StandardControls' with no per-control state recorded yet.
emptyStandardControls :: StandardControls e
emptyStandardControls = StandardControls Map.empty Map.empty

-- | Grants the standard controls access to their state within the
-- user-supplied UI state record @u@.
class HasStandardControls e u where
  getStandardControls :: u -> StandardControls e
  setStandardControls :: StandardControls e -> u -> u

instance HasStandardControls e (StandardControls e) where
  getStandardControls = id
  setStandardControls sc _ = sc

-- | Read-only text display. Renders @text@ within the element's content
-- rectangle using the active style. Does not participate in interaction or
-- keyboard navigation.
label :: (Eq e, Ord e) => e -> Text -> UI e u s ()
label eid text = renderControl eid $ do
  style <- getStyle eid
  drawText (styleTextColour style) (styleTextAlign style) text

-- | Read-only progress indicator. @value@ is clamped to @[0, 1]@ and rendered
-- as a filled bar scaled to that fraction of the content width.
progressBar :: (Eq e, Ord e) => e -> Double -> UI e u s ()
progressBar eid value = renderControl eid $ do
  style <- getStyle eid
  r     <- getBounds
  let clamped  = max 0 (min 1 value)
      fillRect' = r { rectWidth = rectWidth r * clamped }
  withBounds fillRect' $ fillRect (styleTextColour style)

-- | An indeterminate progress indicator that animates continuously. A band
-- moves across the control's content width to indicate ongoing activity of
-- unknown duration. The animation runs only on ticker frames; 'requiresAnimation'
-- keeps the ticker active while the control is visible.
indeterminateProgressBar :: (Eq e, Ord e, HasStandardControls e u) => e -> UI e u s ()
indeterminateProgressBar eid = do
  requiresAnimation
  withAnimationFrame $ do
    delta <- getAnimDelta
    modifyUIState $ \u ->
      let sc     = getStandardControls u
          phase  = Map.findWithDefault 0 eid (scAnimPhases sc)
          p      = phase + realToFrac delta * 0.5
          phase' = p - fromIntegral (floor p :: Int)
      in setStandardControls (sc { scAnimPhases = Map.insert eid phase' (scAnimPhases sc) }) u
  renderControl eid $ do
    r     <- getBounds
    sc    <- getStandardControls <$> getUIState
    style <- getStyle eid
    let phase = Map.findWithDefault 0 eid (scAnimPhases sc)
        bandW = rectWidth r * 0.3
        left  = rectX r - bandW + (rectWidth r + bandW) * phase
    withBounds (r { rectX = left, rectWidth = bandW }) $
      fillRect (styleTextColour style)

checkboxMark :: (Eq e, Ord e) => e -> Bool -> (Bool -> s -> s) -> UI e u s ()
checkboxMark boxId checked onToggle = control boxId $ do
  style     <- getStyle boxId
  activated <- isActivatedBy [KeyReturn, KeySpace] boxId
  when checked   $ drawText (styleTextColour style) AlignCenter "✓"
  when activated $ dispatch (onToggle (not checked))

checkboxLabel :: Style -> Text -> UI e u s ()
checkboxLabel style = drawText (styleTextColour style) AlignLeft

-- | A togglable checkbox with an adjacent label. Dispatches the state modifier
-- @onToggle (not checked)@ when activated by a click or the Enter key.
checkbox :: (Eq e, Ord e) => e -> Text -> Bool -> (Bool -> s -> s) -> UI e u s ()
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

-- | A clickable button labelled @txt@. Returns 'True' on the frame the button
-- is activated — by a left-click or by pressing Enter while focused.
button :: (Eq e, Ord e) => e -> Text -> UI e u s Bool
button eid txt = do
  control eid $ do
    style <- getStyle eid
    drawText (styleTextColour style) (styleTextAlign style) txt
  isActivatedBy [KeyReturn] eid

-- | A single-line text entry field. Displays a cursor when focused. Dispatches
-- the state modifier @onChange newValue@ when the text changes via typed
-- characters or Backspace; the control reads the new value back from the
-- application state on the next frame.
textInput :: (Eq e, Ord e) => e -> Text -> (Text -> s -> s) -> UI e u s ()
textInput eid value onChange = control eid $ do
  style     <- getStyle eid
  hasFocus  <- isFocused eid
  disabled  <- isDisabled
  backspace <- isKeyPressed eid KeyBackspace
  let displayed = if hasFocus && not disabled then value <> "|" else value
  drawText (styleTextColour style) (styleTextAlign style) displayed
  when hasFocus $ whenEnabled $ do
    input <- getInput
    let appended = foldl' (<>) value (inputTypedText input)
        result    = if backspace && not (T.null appended)
                    then T.init appended
                    else appended
    when (result /= value) $ do
      dispatch (onChange result)

-- | Sub-parts of a scrollbar, used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | VScroll ScrollBarPart
-- scrollBar VScroll Vertical ratio
-- @
data ScrollBarPart
  = ScrollTrack
  | ScrollThumb
  | ScrollDecrBtn
  | ScrollIncrBtn
  deriving (Eq, Ord, Show)

-- | A scrollbar with decrement\/increment buttons flanking a draggable thumb.
-- The scroll position in @[0, 1]@ lives in 'scScrollStates', keyed by
-- @mkId ScrollTrack@; the control reads and writes it itself. @thumbRatio@ is
-- the fraction of the track the thumb fills (visible \/ total), also in
-- @[0, 1]@. Button clicks step by @thumbRatio@; dragging centres the thumb on
-- the cursor.
scrollBar :: (Eq e, Ord e, HasStandardControls e u)
          => (ScrollBarPart -> e)
          -> Orientation
          -> Double
          -> UI e u s ()
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

    -- The scroll state slot for this instance; an absent key reads as 0.
    readPos = do
      sc <- getStandardControls <$> getUIState
      pure (scrollPosition (Map.findWithDefault (ScrollState 0) trackId (scScrollStates sc)))

    writePos v = modifyUIState $ \u ->
      let sc = getStandardControls u
      in setStandardControls (sc { scScrollStates = Map.insert trackId (ScrollState v) (scScrollStates sc) }) u

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

scrollThumbRect :: Orientation -> Double -> Double -> Rectangle -> Rectangle
scrollThumbRect Vertical pos ratio r =
  let h = rectHeight r * ratio
  in r { rectY = rectY r + (rectHeight r - h) * pos, rectHeight = h }
scrollThumbRect Horizontal pos ratio r =
  let w = rectWidth r * ratio
  in r { rectX = rectX r + (rectWidth r - w) * pos, rectWidth = w }

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

-- | Sub-parts of a slider, used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | HSlider SliderPart
-- slider HSlider Horizontal value (\\v s -> s { volume = v })
-- @
data SliderPart = SliderTrack | SliderThumb
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
       -> UI e u s ()
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

sliderThumbRect :: Orientation -> Double -> Rectangle -> Rectangle
sliderThumbRect Horizontal pos r =
  let sz    = rectHeight r
      range = max 0 (rectWidth r - sz)
  in r { rectX = rectX r + range * pos, rectWidth = sz }
sliderThumbRect Vertical pos r =
  let sz    = rectWidth r
      range = max 0 (rectHeight r - sz)
  in r { rectY = rectY r + range * pos, rectHeight = sz }
