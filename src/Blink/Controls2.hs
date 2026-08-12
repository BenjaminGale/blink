{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls2
  ( Attr
  , configure
  , fire
  , captureOuts
  , onAny
  , configAny
  , ControlEvent (..)
  , HasControlEvent (..)
  , onFocusGained
  , onFocusLost
  , onMouseEnter
  , onMouseExit
  , FocusOnClick (..)
  , tabStop
  , focusOnClick
  , isMouseOver
  , renderChrome
  , measureChrome
  , applyMouseOver
  , applyFocus
  , control
  , isKeyPressed
  , whenFocused
  , isActivatedBy
  , activatable
  , LabelEvent
  , label
  , ProgressValue (..)
  , progressBar
  , bandSpeed
  , button
  , ButtonEvent (Clicked)
  , onClick
  , onClickTo
  , CheckboxPart (..)
  , CheckboxEvent (Toggled)
  , onToggle
  , renderCheckboxGlyph
  , checkbox
  , TextEvent (Edited, Submitted)
  , onInput
  , onSubmit
  , inputFilter
  , displayFilter
  , textInputControl
  , RangeEvent (RangeChanged)
  , onRangeChange
  , rangeControl
  , thumbRect
  , mouseToTrackPos
  , SliderPart (..)
  , SliderEvent (Changed)
  , onChange
  , arrowStep
  , slider
  ) where

import Control.Monad (forM_, when)
import Data.List (find, foldl')
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)

import Blink.Geometry (Alignment (..), Insets (..), Orientation (..), Point (..), Rectangle (..), borderInsets, insetRect, noBorder)
import Blink.Input (Key (..), KeyEvent (..), Modifier (..), InputState (..))
import Blink.Layout (BoxConfig (..), Layout (..), Length (..), defaultBoxConfig, hBox)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI

data ControlEvent
  = FocusGained
  | FocusLost
  | MouseEntered
  | MouseExited
  deriving (Eq, Show)

class HasControlEvent ev where
  liftControl  :: ControlEvent -> ev
  matchControl :: ev -> Maybe ControlEvent

data FocusOnClick e
  = FocusSelf
  | FocusTarget e
  | NoFocus
  deriving (Eq, Show)

-- | Configuration shared by every control, regardless of that control's own
-- 'Config': whether Tab lands on it ('tabStop') and what clicking it does to
-- focus ('focusOnClick').
data ControlConfig e = ControlConfig
  { ccTabStop      :: Bool
  , ccFocusOnClick :: FocusOnClick e
  }

defaultControlConfig :: ControlConfig e
defaultControlConfig = ControlConfig { ccTabStop = True, ccFocusOnClick = FocusSelf }

data Attr e ev msg cfg
  = On (ev -> [Out e msg])
  | Config (cfg -> cfg)
  | Shared (ControlConfig e -> ControlConfig e)

configure :: cfg -> [Attr e ev msg cfg] -> cfg
configure = foldl' apply
  where
    apply cfg (Config f) = f cfg
    apply cfg _          = cfg

controlConfig :: [Attr e ev msg cfg] -> ControlConfig e
controlConfig = foldl' apply defaultControlConfig
  where
    apply cc (Shared f) = f cc
    apply cc _          = cc

-- | The outs a set of attrs' handlers would produce for one event, without
-- executing any of them — the pure half of 'fire'. Lets a composite bridge
-- an inner control's events into its own attrs (see 'rangeControl') by
-- capturing what firing against them would do, then handing that result
-- back through its own dispatch instead of triggering it directly.
captureOuts :: [Attr e ev msg cfg] -> ev -> [Out e msg]
captureOuts attrs ev = concatMap ($ ev) [h | On h <- attrs]

fire :: [Attr e ev msg cfg] -> [ev] -> UI e msg ()
fire attrs evs = forM_ evs $ \ev -> mapM_ dispatch (captureOuts attrs ev)
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

onAny :: (ev -> [Out e msg]) -> Attr e ev msg cfg
onAny = On

configAny :: (cfg -> cfg) -> Attr e ev msg cfg
configAny = Config

onFocusGained :: HasControlEvent ev => msg -> Attr e ev msg cfg
onFocusGained msg = onAny $ \ev -> [OutMsg msg | matchControl ev == Just FocusGained]

onFocusLost :: HasControlEvent ev => msg -> Attr e ev msg cfg
onFocusLost msg = onAny $ \ev -> [OutMsg msg | matchControl ev == Just FocusLost]

onMouseEnter :: HasControlEvent ev => msg -> Attr e ev msg cfg
onMouseEnter msg = onAny $ \ev -> [OutMsg msg | matchControl ev == Just MouseEntered]

onMouseExit :: HasControlEvent ev => msg -> Attr e ev msg cfg
onMouseExit msg = onAny $ \ev -> [OutMsg msg | matchControl ev == Just MouseExited]

tabStop :: Bool -> Attr e ev msg cfg
tabStop b = Shared $ \cc -> cc { ccTabStop = b }

focusOnClick :: FocusOnClick e -> Attr e ev msg cfg
focusOnClick foc = Shared $ \cc -> cc { ccFocusOnClick = foc }

-- | 'True' when the mouse is over the element's background rectangle (bounds
-- inset by its margin) — the control-specific hit area. Built on 'isRegionHit';
-- a pure geometric test, so unlike the legacy single-owner hover field, many
-- elements can each independently be "over" in the same frame.
isMouseOver :: Ord e => e -> UI e msg Bool
isMouseOver eid = do
  s <- getStyle eid
  r <- getBounds
  withBounds (insetRect (styleMargin s) r) isRegionHit

-- | Registers the element as moused over and as hot (for drag continuation)
-- when the mouse is within its bounds, and fires 'onMouseEnter'\/'onMouseExit'
-- by comparing against last frame's result.
applyMouseOver :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg cfg] -> UI e msg ()
applyMouseOver eid attrs = do
  wasOver  <- wasMouseOverLastFrame eid
  disabled <- isDisabled
  free     <- isMouseFree
  dragging <- isDragging eid
  hit      <- isMouseOver eid
  let isOver = not disabled && (free || dragging) && hit
  when isOver $ do
    registerMouseOver eid
    setHot eid
  fire attrs $ concat
    [ [liftControl MouseEntered | not wasOver && isOver]
    , [liftControl MouseExited  | wasOver && not isOver]
    ]

-- | Applies focus rules (take focus, hand off to a target, or leave it, per
-- 'focusOnClick') together with Tab\/Shift-Tab navigation (per 'tabStop'),
-- firing 'onFocusGained'\/'onFocusLost'. These are one primitive, not two,
-- because a Tab press's effect on this element's own focus must be visible
-- in the same before\/after bracket as the click\/retain logic for those
-- events to fire correctly — split into two separately-callable primitives,
-- a Tab-driven loss would go undetected: 'applyFocus' alone would fire
-- nothing (it returns before the loss happens), and by the time anything
-- calls it again, this same element's own auto-claim (nothing is focused,
-- so I'll take it) would have silently reclaimed focus first, masking the
-- transition entirely.
applyFocus :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg cfg] -> UI e msg ()
applyFocus eid attrs = do
  wasFocused <- isFocused eid
  applyFocusRules
  applyTabKeys
  nowFocused <- isFocused eid
  fire attrs $ concat
    [ [liftControl FocusGained | not wasFocused && nowFocused]
    , [liftControl FocusLost   | wasFocused && not nowFocused]
    ]
  where
    cc = controlConfig attrs

    -- Takes focus on a click, hands it to a target, or leaves it, per 'focusOnClick'.
    applyFocusRules = whenEnabled $ do
      currentFocus <- getFocus
      isHit        <- isMouseOver eid
      released     <- isButtonReleased
      captured     <- getCapturedElement
      let nothingIsFocused = isNothing currentFocus
          isRetainingFocus = currentFocus == Just eid
          isDragRelease    = released && isJust captured && captured /= Just eid
          wasClicked       = isHit && released && not isDragRelease
          autoClaim        = case ccFocusOnClick cc of
            FocusSelf -> nothingIsFocused && not isDragRelease
            _         -> False
      if isRetainingFocus || autoClaim
        then setFocus eid
        else when (wasClicked && not isDragRelease) $ case ccFocusOnClick cc of
          FocusSelf     -> setFocus eid
          FocusTarget t -> setFocus t
          NoFocus       -> pure ()

    -- Handles Tab and Shift-Tab and registers the element as a tab stop,
    -- subject to 'tabStop'. An element with @tabStop False@ still gives up
    -- focus normally on Tab if it happens to hold it, but is skipped by
    -- Shift-Tab from whatever comes after it, since it never records itself
    -- as the previous tab stop.
    applyTabKeys = whenEnabled $ do
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
      when (ccTabStop cc) $ setPreviousTabStop eid

measureChrome :: Ord e => e -> UI e msg (Length, Length)
measureChrome eid = do
  style <- getStyle eid
  let m  = styleMargin style
      p  = stylePadding style
      be = case styleBorderColour style of
             Just _  -> borderInsets (styleBorderEdges style)
             Nothing -> borderInsets noBorder
      dw = leftInset m + rightInset m + leftInset be + rightInset be + leftInset p + rightInset p
      dh = topInset m  + bottomInset m  + topInset be + bottomInset be + topInset p  + bottomInset p
  pure (Exactly dw, Exactly dh)

renderChrome :: Ord e => e -> UI e msg () -> UI e msg ()
renderChrome eid content = do
  style <- getStyle eid
  r     <- getBounds
  let bg          = insetRect (styleMargin style) r
      borderRect  = case styleBorderColour style of
                      Just _  -> insetRect (borderInsets (styleBorderEdges style)) bg
                      Nothing -> bg
      contentRect = insetRect (stylePadding style) borderRect
      inner       = withBounds contentRect $ clipToCurrent content
  withBounds bg $
    withBackground (styleBackground style) $
    case styleBorderColour style of
      Just c  -> withBorder c (styleBorderEdges style) inner
      Nothing -> inner

-- | The standard entry point for interactive controls: applies mouse-over
-- (with hot/drag-continuation), focus and tab navigation, then renders
-- chrome around @content@.
control :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg cfg] -> UI e msg () -> UI e msg ()
control eid attrs content = do
  applyMouseOver eid attrs
  applyFocus eid attrs
  renderChrome eid content

-- | 'True' when the element holds focus and a key event for @k@ is present
-- in the current frame's input queue.
isKeyPressed :: Eq e => e -> Key -> UI e msg Bool
isKeyPressed eid k = do
  hasFoc  <- isFocused eid
  pressed <- any (\e -> key e == k) . inputKeyEvents <$> getInput
  pure (hasFoc && pressed)

-- | Runs @action@ only when the given element holds keyboard focus.
whenFocused :: Eq e => e -> UI e msg () -> UI e msg ()
whenFocused eid action = isFocused eid >>= \f -> when f action

-- | 'True' when the element is a valid click target this frame: not
-- disabled, eligible for mouse-over (mouse free, or this element itself
-- holds capture), geometrically hit, and the button was just released.
-- Mirrors 'applyMouseOver's own gating, so a click can't activate a control
-- while a different control is mid-drag. Not exported: 'isMouseOver' is
-- geometric and has no registration state of its own to query directly the
-- way the legacy @isHovered@ did, so this is assembled fresh here rather
-- than reused from "Blink.UI".
isClickedOver :: Ord e => e -> UI e msg Bool
isClickedOver eid = do
  disabled <- isDisabled
  free     <- isMouseFree
  dragging <- isDragging eid
  hit      <- isMouseOver eid
  released <- isButtonReleased
  pure (not disabled && (free || dragging) && hit && released)

-- | 'True' when the element was clicked, or one of @keys@ was pressed while
-- it held focus, and it is not disabled.
isActivatedBy :: Ord e => e -> [Key] -> UI e msg Bool
isActivatedBy eid keys = do
  clicked  <- isClickedOver eid
  keyPress <- or <$> mapM (isKeyPressed eid) keys
  disabled <- isDisabled
  pure (not disabled && (clicked || keyPress))

-- | 'control' plus 'isActivatedBy': runs @draw@ as a normal interactive
-- control, then reports whether it was activated (a click or one of @keys@)
-- this frame.
activatable :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg cfg] -> [Key] -> UI e msg () -> UI e msg Bool
activatable eid attrs keys draw = do
  control eid attrs draw
  isActivatedBy eid keys

-- | Events reported by 'label': just a lifecycle event via 'LabelControl'
-- (see 'ControlEvent') — 'label' has no domain events of its own, but still
-- needs a concrete event type to be a 'control' and raise the shared ones.
newtype LabelEvent = LabelControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent LabelEvent where
  liftControl = LabelControl
  matchControl (LabelControl ce) = Just ce

-- | Text in the resolved style. A full 'control', so it registers
-- mouse-over and honours 'tabStop'\/'focusOnClick' (both default to
-- off-the-beaten-path uses — a plain label has no reason to take focus —
-- but a composite like a labelled field can use 'focusOnClick' to redirect
-- a click on the caption onto its input).
label :: Ord e => e -> Text -> [Attr e LabelEvent msg cfg] -> UI e msg ()
label eid text attrs = control eid attrs $ do
  style <- getStyle eid
  drawText (styleTextColour style) (styleTextAlign style) text

-- | The value passed to 'progressBar'.
data ProgressValue
  = Progress Double
    -- ^ A determinate value in @[0, 1]@, clamped and rendered as a filled bar.
  | Indeterminate
    -- ^ Unknown progress: a band animates continuously across the bar.
  deriving (Eq, Show)

-- | Configuration for 'progressBar', set via 'bandSpeed'.
newtype ProgressBarConfig = ProgressBarConfig { configBandSpeed :: Double }

defaultProgressBarConfig :: ProgressBarConfig
defaultProgressBarConfig = ProgressBarConfig { configBandSpeed = 0.5 }

-- | How fast the band sweeps across an 'Indeterminate' bar, in bar-widths
-- per second. Defaults to 0.5.
bandSpeed :: Double -> Attr e ev msg ProgressBarConfig
bandSpeed v = configAny $ \cfg -> cfg { configBandSpeed = v }

-- | A read-only progress indicator. Pass 'Progress' for a determinate bar or
-- 'Indeterminate' for a continuously animating band. Not interactive — no
-- mouse-over, focus, or tab stop, so it takes no 'tabStop'\/'focusOnClick'
-- and never needs a real event type; 'Void' rules out anything being fired.
progressBar :: Ord e => e -> ProgressValue -> [Attr e Void msg ProgressBarConfig] -> UI e msg ()
progressBar eid (Progress value) _attrs = renderChrome eid $ do
  style <- getStyle eid
  r     <- getBounds
  let clamped   = max 0 (min 1 value)
      fillRect' = r { rectWidth = rectWidth r * clamped }
  withBounds fillRect' $ fillRect (styleTextColour style)
progressBar eid Indeterminate attrs = do
  requiresAnimation
  renderChrome eid $ do
    r       <- getBounds
    style   <- getStyle eid
    elapsed <- getAnimElapsed
    let speed = configBandSpeed (configure defaultProgressBarConfig attrs)
        t     = realToFrac elapsed * speed
        phase = t - fromIntegral (floor t :: Int)
        bandW = rectWidth r * 0.3
        left  = rectX r - bandW + (rectWidth r + bandW) * phase
    withBounds (r { rectX = left, rectWidth = bandW }) $
      fillRect (styleTextColour style)

-- | Events reported by 'button': 'Clicked' when activated, or a lifecycle
-- event via 'Control' (see 'ControlEvent'). 'Control' is not exported —
-- 'onFocusGained'\/'onFocusLost'\/'onMouseEnter'\/'onMouseExit' already cover
-- it generically — but 'Clicked' is, since 'onAny' needs to pattern-match
-- on it directly to build a custom handler ('onClick'\/'onClickTo' each only
-- cover one output; combining a message and a 'UiEffect' on the same click
-- needs 'onAny' with 'Clicked' in scope).
data ButtonEvent = Clicked | Control ControlEvent
  deriving (Eq, Show)

instance HasControlEvent ButtonEvent where
  liftControl = Control
  matchControl (Control ce) = Just ce
  matchControl _             = Nothing

-- | Emits @msg@ when the button is 'Clicked'.
onClick :: msg -> Attr e ButtonEvent msg cfg
onClick msg = onAny $ \ev -> case ev of
  Clicked -> [OutMsg msg]
  _       -> []

-- | Queues a 'UiEffect' when the button is 'Clicked', for effects that don't
-- have a @msg@ to emit.
onClickTo :: UiEffect e -> Attr e ButtonEvent msg cfg
onClickTo eff = onAny $ \ev -> case ev of
  Clicked -> [OutUi eff]
  _       -> []

-- | A clickable button labelled @txt@. Fires 'Clicked' — via 'onClick' or
-- 'onClickTo' — on the frame it's activated, by a left-click or by pressing
-- Enter while focused.
button :: Ord e => e -> Text -> [Attr e ButtonEvent msg cfg] -> UI e msg ()
button eid txt attrs = do
  activated <- activatable eid attrs [KeyReturn] draw
  when activated $ fire attrs [Clicked]
  where
    draw = do
      style <- getStyle eid
      drawText (styleTextColour style) (styleTextAlign style) txt

-- | Sub-parts of a 'checkbox', used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | NotifyMe CheckboxPart
-- checkbox NotifyMe "Notify me by email" (notifyMe model) [onToggle NotifyMeChanged]
-- @
data CheckboxPart
  = CheckboxBox   -- ^ The checkbox as a whole: chrome, hit region, focus, activation.
  | CheckboxGlyph -- ^ The checkmark glyph.
  | CheckboxLabel -- ^ The label beside it — an ordinary 'label'.
  deriving (Eq, Ord, Show)

-- | Events reported by 'checkbox': 'Toggled' with the new checked state when
-- activated, or a lifecycle event via 'CheckboxControl' (see 'ControlEvent').
data CheckboxEvent = Toggled Bool | CheckboxControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent CheckboxEvent where
  liftControl = CheckboxControl
  matchControl (CheckboxControl ce) = Just ce
  matchControl _                    = Nothing

-- | Emits @f newChecked@ when 'Toggled'.
onToggle :: (Bool -> msg) -> Attr e CheckboxEvent msg cfg
onToggle f = onAny $ \ev -> case ev of
  Toggled b -> [OutMsg (f b)]
  _         -> []

-- | Draws a checkbox glyph: a checkmark centred in the current bounds when
-- @checked@, nothing otherwise, in the resolved style's text colour. A bare
-- rendering action with no interactive behaviour of its own — reusable by
-- anything that wants to look like a checkbox glyph without taking on
-- 'checkbox''s toggle behaviour.
renderCheckboxGlyph :: Ord e => e -> Bool -> UI e msg ()
renderCheckboxGlyph eid checked = do
  style <- getStyle eid
  when checked $ drawText (styleTextColour style) AlignCenter "✓"

-- | Attrs shared by a checkbox's glyph and label sub-parts: neither is a tab
-- stop, and clicking either redirects focus to the checkbox itself.
-- Committing to 'LabelEvent' here (rather than leaving it polymorphic) is
-- what lets 'checkbox' use the same value at both the glyph's 'control' call
-- and the label's 'label' call without either site leaving its event type
-- ambiguous.
checkboxSubPartAttrs :: e -> [Attr e LabelEvent msg cfg]
checkboxSubPartAttrs boxId = [tabStop False, focusOnClick (FocusTarget boxId)]

-- | A togglable checkbox with an adjacent label, as one unit: the glyph and
-- label are purely visual sub-parts (each moused-over and styled on its own
-- bounds, neither a tab stop, a click on either redirecting focus to the
-- checkbox) — all interaction lives on the checkbox itself, whose own hit
-- region spans the glyph, the label, and the gap between them. Fires
-- 'Toggled' with the new checked state when activated by a click anywhere
-- in that region, or by Enter or Space while focused.
--
-- @
-- checkbox NotifyMe "Notify me by email" (notifyMe model) [onToggle NotifyMeChanged]
-- @
checkbox :: Ord e => (CheckboxPart -> e) -> Text -> Bool -> [Attr e CheckboxEvent msg cfg] -> UI e msg ()
checkbox mkId text checked attrs = do
  activated <- activatable (mkId CheckboxBox) attrs [KeyReturn, KeySpace] draw
  when activated $ fire attrs [Toggled (not checked)]
  where
    glyphId  = mkId CheckboxGlyph
    subAttrs = checkboxSubPartAttrs (mkId CheckboxBox)
    draw =
      hBox (defaultBoxConfig { boxSpacing = 4, boxFillCross = False })
        [ (Layout (Exactly 20) (Exactly 20) MiddleLeft, control glyphId subAttrs (renderCheckboxGlyph glyphId checked))
        , (Layout Fill Fill MiddleLeft, label (mkId CheckboxLabel) text subAttrs)
        ]

-- | Click sets both selection ends at the clicked character; dragging
-- extends only the active end, keeping the anchor from before the drag
-- started. Assumes the caller has already checked the control is focused
-- and enabled.
resolveMouseSelection
  :: Ord e
  => e           -- ^ element ID
  -> Rectangle   -- ^ control's bounds
  -> Bool        -- ^ control was already being dragged last frame
  -> Bool        -- ^ control just gained focus this frame via a click
  -> Text        -- ^ displayed value (post-@displayFilter@)
  -> Double      -- ^ current horizontal scroll offset
  -> (Int, Int)  -- ^ current @(anchor, active)@ selection
  -> UI e msg (Int, Int)
resolveMouseSelection eid bounds wasCapturing justFocused value scrollX (anchor0, active0) = do
  isCapturing <- isDragging eid
  if isCapturing
    then do
      mousePos <- getMousePos
      let localX = realToFrac (pointX mousePos - rectX bounds) + realToFrac scrollX :: Float
      clickedPos <- charAtOffset value localX
      pure $ if not wasCapturing || justFocused
        then (clickedPos, clickedPos)
        else (anchor0, clickedPos)
    else pure (anchor0, active0)

-- | Shift+Left\/Right extend the selection; plain Left\/Right collapse an
-- existing selection to its near end, or step by one otherwise.
resolveKeyboardSelection
  :: Bool         -- ^ control has keyboard focus
  -> [KeyEvent]   -- ^ this frame's key events
  -> Int          -- ^ length of the underlying value
  -> (Int, Int)   -- ^ current @(anchor, active)@ selection
  -> (Int, Int)
resolveKeyboardSelection hasFocus keyEvts len (anchor1, active1)
  | shiftLeft  = (anchor1, max 0    (active1 - 1))
  | shiftRight = (anchor1, min len  (active1 + 1))
  | plainLeft  = let p = if hasSel1 then selLo1 else max 0   (active1 - 1) in (p, p)
  | plainRight = let p = if hasSel1 then selHi1 else min len (active1 + 1) in (p, p)
  | otherwise  = (anchor1, active1)
  where
    hasSel1    = anchor1 /= active1
    selLo1     = min anchor1 active1
    selHi1     = max anchor1 active1
    shiftLeft  = hasFocus && any (\e -> key e == KeyLeft  && Shift `elem`    modifiers e) keyEvts
    shiftRight = hasFocus && any (\e -> key e == KeyRight && Shift `elem`    modifiers e) keyEvts
    plainLeft  = hasFocus && any (\e -> key e == KeyLeft  && Shift `notElem` modifiers e) keyEvts
    plainRight = hasFocus && any (\e -> key e == KeyRight && Shift `notElem` modifiers e) keyEvts

-- | Backspace and typed text edit the value, selection-aware; returns the
-- new value alongside the new cursor position when the text actually
-- changed. @inputFilter@ is applied to the newly typed text before
-- insertion, letting callers reject or transform keystrokes (e.g. digits
-- only). Assumes the caller has already checked the control is focused and
-- enabled. Pure — the caller decides how (or whether) to report the change.
applyEdit :: (Text -> Text)   -- ^ @inputFilter@, applied to newly typed text before insertion
          -> Text             -- ^ current value
          -> InputState       -- ^ this frame's input
          -> (Int, Int)       -- ^ current @(anchor, active)@ selection
          -> ((Int, Int), Maybe Text) -- ^ new @(anchor, active)@, and the new value if it changed
applyEdit inputFilterFn value input (anchor2, active2)
  | backspace || hasTyped =
      ((newCursor, newCursor), if newText /= value then Just newText else Nothing)
  | otherwise = ((anchor2, active2), Nothing)
  where
    keyEvts   = inputKeyEvents input
    backspace = any (\e -> key e == KeyBackspace) keyEvts
    typed     = inputFilterFn (foldl' (<>) T.empty (inputTypedText input))
    hasTyped  = not (T.null typed)
    hasSel2   = anchor2 /= active2
    selLo2    = min anchor2 active2
    selHi2    = max anchor2 active2
    (newText, newCursor)
      | hasSel2 && backspace =
          (T.take selLo2 value <> T.drop selHi2 value, selLo2)
      | hasSel2 =
          (T.take selLo2 value <> typed <> T.drop selHi2 value, selLo2 + T.length typed)
      | backspace && active2 > 0 =
          (T.take (active2 - 1) value <> T.drop active2 value, active2 - 1)
      | hasTyped =
          (T.take active2 value <> typed <> T.drop active2 value, active2 + T.length typed)
      | otherwise = (value, active2)

-- | The scroll offset needed to keep a cursor at @cursorAbs@ visible within
-- a viewport of width @w@ currently scrolled to @scrollX@. Pixels in, pixels
-- out — 'scrollFraction'\/'scrollPixels' convert at the boundary with
-- 'getScrollState'\/'ScrollTo' so the stored value stays in the same
-- @[0, 1]@ convention every other scroll-state consumer uses.
resolveScroll
  :: Double  -- ^ viewport width
  -> Double  -- ^ current scroll offset
  -> Double  -- ^ cursor position to keep visible
  -> Double
resolveScroll w scrollX cursorAbs
  | cursorAbs < scrollX         = cursorAbs
  | cursorAbs > scrollX + w - 1 = max 0 (cursorAbs - w + 1)
  | otherwise                   = scrollX

-- | The largest pixel offset worth scrolling by: zero once the content
-- already fits within the viewport.
maxScrollPixels :: Double -> Double -> Double
maxScrollPixels contentW viewportW = max 0 (contentW - viewportW)

-- | Converts a pixel scroll offset to the @[0, 1]@ fraction 'ScrollState'
-- stores, given the max offset from 'maxScrollPixels'. @0@ when there's
-- nothing to scroll.
scrollFraction :: Double -> Double -> Double
scrollFraction maxPx px
  | maxPx > 0 = clampScrollPos (px / maxPx)
  | otherwise = 0

-- | The inverse of 'scrollFraction': converts a stored @[0, 1]@ fraction
-- back to a pixel offset, given the max offset from 'maxScrollPixels'.
scrollPixels :: Double -> Double -> Double
scrollPixels maxPx frac = frac * maxPx

-- | Draws the selection highlight (focused with a non-empty selection), the
-- text itself, and the cursor (focused and enabled), all offset by the
-- current horizontal scroll.
drawTextInputContent
  :: Ord e
  => Style       -- ^ active style
  -> Rectangle   -- ^ control's bounds
  -> Text        -- ^ displayed value (post-@displayFilter@)
  -> Bool        -- ^ control has keyboard focus
  -> Bool        -- ^ control is focused and not disabled
  -> Double      -- ^ current horizontal scroll offset
  -> (Int, Int)  -- ^ current @(anchor, active)@ selection
  -> UI e msg ()
drawTextInputContent style bounds value hasFocus enabled ox (anchor3, active3) = do
  when (hasFocus && drawLo < drawHi) $ do
    loX <- charOffset value drawLo
    hiX <- charOffset value drawHi
    let selRect = Rectangle
          (rectX bounds + realToFrac loX - ox)
          (rectY bounds)
          (realToFrac (hiX - loX))
          (rectHeight bounds)
    withBounds selRect $ fillRect (RGBA 0.3 0.5 1.0 0.4)

  let textBounds = bounds { rectX = rectX bounds - ox }
  withBounds textBounds $ drawText (styleTextColour style) AlignLeft value

  when enabled $ do
    curX <- charOffset value active3
    let cursorRect = Rectangle
          (rectX bounds + realToFrac curX - ox)
          (rectY bounds)
          1
          (rectHeight bounds)
    withBounds cursorRect $ fillRect (styleTextColour style)
  where
    drawLo = min anchor3 active3
    drawHi = max anchor3 active3

-- | Events reported by 'textInputControl': 'Edited' with the new value
-- whenever a keystroke changes it, 'Submitted' when Enter is pressed while
-- focused and enabled, or a lifecycle event via 'TextControl' (see
-- 'ControlEvent').
data TextEvent = Edited Text | Submitted | TextControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent TextEvent where
  liftControl = TextControl
  matchControl (TextControl ce) = Just ce
  matchControl _                = Nothing

-- | Emits @f newValue@ on every 'Edited'.
onInput :: (Text -> msg) -> Attr e TextEvent msg cfg
onInput f = onAny $ \ev -> case ev of
  Edited t -> [OutMsg (f t)]
  _        -> []

-- | Emits @msg@ when Enter is pressed while the field is focused ('Submitted').
onSubmit :: msg -> Attr e TextEvent msg cfg
onSubmit msg = onAny $ \ev -> case ev of
  Submitted -> [OutMsg msg]
  _         -> []

-- | Configuration for 'textInputControl', set via 'inputFilter' and
-- 'displayFilter'.
data TextInputConfig = TextInputConfig
  { configInputFilter   :: Text -> Text
  , configDisplayFilter :: Text -> Text
  }

defaultTextInputConfig :: TextInputConfig
defaultTextInputConfig = TextInputConfig
  { configInputFilter   = id
  , configDisplayFilter = id
  }

-- | Applied to newly typed text before it's inserted, letting callers
-- restrict which keystrokes are accepted (e.g. @T.filter isDigit@ for a
-- digits-only field). Reformatting the value itself (e.g. inserting
-- punctuation as the user types) is an application concern, not this
-- control's — do it in an 'onInput' handler and pass the already-formatted
-- value back in on the next frame. Defaults to 'id'.
inputFilter :: (Text -> Text) -> Attr e ev msg TextInputConfig
inputFilter f = configAny $ \cfg -> cfg { configInputFilter = f }

-- | Applied to the value everywhere it is measured or drawn — the rendered
-- text, and every character-offset calculation used for cursor placement,
-- click hit-testing, and auto-scroll — so what's on screen and where the
-- cursor lands always agree. It must be length- and position-preserving
-- (e.g. @T.map (const '\8226')@ to mask each character of a password); the
-- underlying value edited by 'inputFilter'\/'onInput' is never affected by
-- it. Defaults to 'id'.
displayFilter :: (Text -> Text) -> Attr e ev msg TextInputConfig
displayFilter f = configAny $ \cfg -> cfg { configDisplayFilter = f }

-- | A single-line text entry field. Supports click-to-place cursor, drag
-- selection, Shift+arrow extension, and selection-aware editing. Long text
-- scrolls horizontally to keep the cursor visible. 'inputFilter' and
-- 'displayFilter' attrs turn this into a digits-only or password-style
-- field.
--
-- Cursor position and selection are control state, not application data —
-- 'textInputControl' reads and writes them itself via 'getSelection' and
-- 'getScrollState', keyed by @eid@, writing through 'emitUi' with
-- 'SetSelectionAt' and 'ScrollTo'. The scroll position is stored as the same
-- @[0, 1]@ fraction every other scroll-state consumer uses — see
-- 'scrollFraction'\/'scrollPixels' — converted to and from pixels locally,
-- since the selection\/cursor\/auto-scroll math below is naturally pixel-based.
textInputControl :: Ord e => e -> Text -> [Attr e TextEvent msg TextInputConfig] -> UI e msg ()
textInputControl eid value attrs = do
  wasFocused   <- isFocused eid
  wasCapturing <- isDragging eid
  let cfg = configure defaultTextInputConfig attrs
  control eid attrs $ do
    style    <- getStyle eid
    hasFocus <- isFocused eid
    disabled <- isDisabled
    bounds   <- getBounds
    input    <- getInput
    sel      <- getSelection eid
    frac     <- getScrollState eid

    let displayValue = configDisplayFilter cfg value
        w           = rectWidth bounds
        defPos      = T.length value
        anchor0     = maybe defPos selectionAnchor sel
        active0     = maybe defPos selectionActive sel
        -- Focus was gained by a click this frame (e.g. clicking from another
        -- element). Treat as a fresh click rather than a drag continuation so
        -- the old anchor is not inherited.
        justFocused = hasFocus && not wasFocused
        enabled     = hasFocus && not disabled

    contentW <- realToFrac <$> charOffset displayValue (T.length displayValue)
    let maxScrollPx = maxScrollPixels contentW w
        scrollX     = scrollPixels maxScrollPx frac

    (anchor1, active1) <-
      if enabled
        then resolveMouseSelection eid bounds wasCapturing justFocused displayValue scrollX (anchor0, active0)
        else pure (anchor0, active0)

    let (anchor2, active2) =
          resolveKeyboardSelection hasFocus (inputKeyEvents input) (T.length value) (anchor1, active1)

        ((anchor3, active3), edited)
          | enabled   = applyEdit (configInputFilter cfg) value input (anchor2, active2)
          | otherwise = ((anchor2, active2), Nothing)

        submitted = enabled && any (\e -> key e == KeyReturn) (inputKeyEvents input)

    fire attrs ([Submitted | submitted] ++ [Edited t | Just t <- [edited]])

    when enabled $ emitUi (SetSelectionAt eid (Selection anchor3 active3))

    -- Computed locally rather than re-read via 'getScrollState': scroll
    -- writes are deferred (applied between frames), so a same-frame re-read
    -- would still see the pre-write value and the cursor would lag the
    -- auto-scroll by one frame.
    effectiveScrollX <-
      if enabled
        then do
          curX <- charOffset displayValue active3
          let newScrollX = resolveScroll w scrollX (realToFrac curX)
          when (newScrollX /= scrollX) $ emitUi (ScrollTo eid (scrollFraction maxScrollPx newScrollX))
          pure newScrollX
        else pure scrollX

    drawTextInputContent style bounds displayValue hasFocus enabled effectiveScrollX (anchor3, active3)

contentRectFor :: StyleSet -> Rectangle -> Rectangle
contentRectFor ss r =
  let s = styleSetNormal ss
  in insetRect (stylePadding s) (insetRect (styleMargin s) r)

-- | The content rectangle of a track-style element (its slot bounds inset by
-- margin and padding), used by both 'scrollBar' and 'slider' to size and
-- place their thumb.
trackContentRect :: Ord e => e -> UI e msg Rectangle
trackContentRect trackId = do
  bounds   <- getBounds
  styleSet <- getStyleSet trackId
  pure (contentRectFor styleSet bounds)

-- | While @trackId@ is being dragged with the button held, returns the track
-- position under the cursor; 'Nothing' otherwise. Shared drag-handling for
-- 'scrollBar' and 'slider', both of which map a thumb drag to a position via
-- 'mouseToTrackPos'.
dragToTrackPos :: Ord e => e -> Orientation -> Double -> Rectangle -> UI e msg (Maybe Double)
dragToTrackPos trackId ori ratio contentRect = do
  dragging <- isDragging trackId
  btnDown  <- isButtonDown
  if dragging && btnDown
    then Just . mouseToTrackPos ori ratio contentRect <$> getMousePos
    else pure Nothing

-- | Events reported by 'rangeControl': 'RangeChanged' with the new position
-- while being dragged, or a lifecycle event via 'RangeControl' (see
-- 'ControlEvent'). 'RangeControl' is not exported — the shared
-- 'onFocusGained'\/etc. combinators already cover it — but 'RangeChanged'
-- is, for the same reason 'ButtonEvent'\'s 'Clicked' is: a caller bridging
-- this into a wrapping control's own events (see 'captureOuts') needs to
-- pattern-match on it directly.
data RangeEvent = RangeChanged Double | RangeControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent RangeEvent where
  liftControl = RangeControl
  matchControl (RangeControl ce) = Just ce
  matchControl _                 = Nothing

-- | Emits @f newPosition@ on every 'RangeChanged'.
onRangeChange :: (Double -> msg) -> Attr e RangeEvent msg cfg
onRangeChange f = onAny $ \ev -> case ev of
  RangeChanged v -> [OutMsg (f v)]
  _              -> []

-- | A track with a draggable thumb, positioned within @[0, 1]@ by @pos@ and
-- sized within the track by @ratio@ (visible \/ total, also @[0, 1]@).
-- @trackId@ is the interactive element — it receives chrome, hover, focus,
-- and tab navigation via 'control' — and @thumbId@ is a purely decorative
-- child positioned inside it. Fires 'RangeChanged' with the new position
-- while @trackId@ is being dragged; a caller wanting a different reporting
-- channel (an effect rather than a message, say) bridges it via 'onAny' and
-- 'captureOuts' against its own attrs — see 'slider'. Shared by 'scrollBar'
-- and 'slider'.
rangeControl :: Ord e => e -> e -> Orientation -> Double -> Double -> [Attr e RangeEvent msg cfg] -> UI e msg ()
rangeControl trackId thumbId ori pos ratio attrs = do
  contentRect <- trackContentRect trackId
  let thumbR = thumbRect ori pos ratio contentRect
  control trackId attrs $
    withBounds thumbR $ renderChrome thumbId $ pure ()
  newPos <- dragToTrackPos trackId ori ratio contentRect
  fire attrs [RangeChanged v | Just v <- [newPos]]

-- | Computes the bounding rectangle of a thumb within a track. @pos@ is the
-- position along the track and @ratio@ is the fraction of the track the
-- thumb fills (visible \/ total); both are in @[0, 1]@. The result is a
-- sub-rectangle of @r@.
thumbRect :: Orientation -> Double -> Double -> Rectangle -> Rectangle
thumbRect Vertical pos ratio r =
  let h = rectHeight r * ratio
  in r { rectY = rectY r + (rectHeight r - h) * pos, rectHeight = h }
thumbRect Horizontal pos ratio r =
  let w = rectWidth r * ratio
  in r { rectX = rectX r + (rectWidth r - w) * pos, rectWidth = w }

-- | Converts a mouse position to a track position in @[0, 1]@, centring the
-- thumb on the cursor. This is the inverse of 'thumbRect': exported for
-- callers building custom drag handlers. Returns @0@ when the thumb fills
-- the track (@ratio = 1@) and there is no range to move.
mouseToTrackPos :: Orientation -> Double -> Rectangle -> Point -> Double
mouseToTrackPos Vertical ratio r mouse =
  let thumbH = rectHeight r * ratio
      range  = rectHeight r - thumbH
  in if range <= 0 then 0
     else max 0 (min 1 ((pointY mouse - rectY r - thumbH / 2) / range))
mouseToTrackPos Horizontal ratio r mouse =
  let thumbW = rectWidth r * ratio
      range  = rectWidth r - thumbW
  in if range <= 0 then 0
     else max 0 (min 1 ((pointX mouse - rectX r - thumbW / 2) / range))

-- | Sub-parts of a slider, used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | HSlider SliderPart
-- slider HSlider Horizontal value [onChange VolumeChanged]
-- @
data SliderPart
  = SliderTrack -- ^ The track area behind the thumb.
  | SliderThumb -- ^ The draggable thumb.
  deriving (Eq, Ord, Show)

-- | Events reported by 'slider': 'Changed' with the new value when the user
-- drags, clicks on the track, or nudges with arrow keys, or a lifecycle
-- event via 'SliderControl' (see 'ControlEvent').
data SliderEvent = Changed Double | SliderControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent SliderEvent where
  liftControl = SliderControl
  matchControl (SliderControl ce) = Just ce
  matchControl _                  = Nothing

-- | Emits @f newValue@ on every 'Changed'.
onChange :: (Double -> msg) -> Attr e SliderEvent msg cfg
onChange f = onAny $ \ev -> case ev of
  Changed v -> [OutMsg (f v)]
  _         -> []

-- | Configuration for 'slider', set via 'arrowStep'.
newtype SliderConfig = SliderConfig { configArrowStep :: Double }

defaultSliderConfig :: SliderConfig
defaultSliderConfig = SliderConfig { configArrowStep = 0.05 }

-- | The amount an arrow-key press (Left\/Right for 'Horizontal', Up\/Down
-- for 'Vertical') changes the value by. Defaults to @0.05@.
arrowStep :: Double -> Attr e ev msg SliderConfig
arrowStep v = configAny $ \cfg -> cfg { configArrowStep = v }

-- | A slider mapping a draggable thumb to a value in @[0, 1]@. Fires
-- 'Changed' with the new value when the user drags, clicks on the track, or
-- nudges with arrow keys (Left\/Right for 'Horizontal', Up\/Down for
-- 'Vertical', by 'arrowStep'). The thumb is square: its side equals the
-- cross-axis of the track's content rectangle.
slider :: Ord e => (SliderPart -> e) -> Orientation -> Double -> [Attr e SliderEvent msg SliderConfig] -> UI e msg ()
slider mkId ori value attrs = do
  let trackId = mkId SliderTrack
      clamped = max 0 (min 1 value)
      step    = configArrowStep (configure defaultSliderConfig attrs)
  contentRect <- trackContentRect trackId
  let (crossSz, mainSz) = case ori of
        Horizontal -> (rectHeight contentRect, rectWidth contentRect)
        Vertical   -> (rectWidth contentRect,  rectHeight contentRect)
      thumbRatio = if mainSz > 0 then crossSz / mainSz else 0
      bridgeAttrs = [onAny $ \ev -> case ev of
        RangeChanged v  -> captureOuts attrs (Changed v)
        RangeControl ce -> captureOuts attrs (liftControl ce)]
  rangeControl trackId (mkId SliderThumb) ori clamped thumbRatio bridgeAttrs
  let (decrKey, incrKey) = case ori of
        Horizontal -> (KeyLeft,  KeyRight)
        Vertical   -> (KeyUp,    KeyDown)
  disabled  <- isDisabled
  decrKeyed <- isKeyPressed trackId decrKey
  incrKeyed <- isKeyPressed trackId incrKey
  let decrPressed = not disabled && decrKeyed
      incrPressed = not disabled && incrKeyed
      changes = concat
        [ [Changed (max 0 (clamped - step)) | decrPressed]
        , [Changed (min 1 (clamped + step)) | incrPressed]
        ]
  fire attrs changes
