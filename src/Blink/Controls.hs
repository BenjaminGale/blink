{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls

Standard UI controls built on top of "Blink.UI". Each control takes an element
ID of type @e@ used for styling and interaction tracking; see "Blink.UI" for an
explanation of element IDs, commands, and the render loop.

= Value-callback pattern

Stateful controls receive the current value and a function that wraps an
updated value in an application command, dispatched whenever the user makes a
change:

@
textInput NameField currentName NameChanged
-- dispatches: NameChanged newValue
@

The application retrieves 'NameChanged' via 'getCommands', stores the new
value, and passes it back to the control on the next frame. This keeps all
state outside the UI tree.

= Builder pattern

Controls whose state is presentational rather than application data — a
scrollbar's position, for example — manage it themselves through a 'Field'
into the user-supplied UI state record (see "Blink.UI"). These controls are
exported as @fooBuilder@ functions taking the 'Field' as their first argument,
so an application configures each instance once by partial application:

@
vScrollBar = scrollBarBuilder (Field vScroll (\\v u -> u { vScroll = v })) VScroll
@

The application's update function never sees this state change hands.

== Per-instance state from a map

Binding each instance to a dedicated record field, as above, suits a fixed
handful of controls. When many controls of the same kind share one map in the
UI state record, a single wrapper serves all of them: build the 'Field' on the
fly from the same tagging function the control already takes, keying the map
by a representative element such as @mkId ScrollTrack@.

@
data MyUIState = MyUIState { scrollPositions :: Map Element Double }

scrollFieldFor :: Element -> Field MyUIState Double
scrollFieldFor eid = Field
  (Map.findWithDefault 0 eid . scrollPositions)
  (\\v u -> u { scrollPositions = Map.insert eid v (scrollPositions u) })

scrollBar :: (ScrollBarPart -> Element) -> Orientation -> Double -> UI Element MyUIState Command ()
scrollBar mkId = scrollBarBuilder (scrollFieldFor (mkId ScrollTrack)) mkId
@

Every call site — @scrollBar VScroll Vertical 0.3@, @scrollBar HScroll
Horizontal 0.3@ — then gets its own slot in the map automatically. The
getter's @findWithDefault@ supplies the initial position, so absent keys read
as 0 and the map populates lazily on first write.
-}
module Blink.Controls
  ( -- * Display
    label
  , progressBar
    -- * Input
  , button
  , checkbox
  , textInput
    -- * Scroll
  , ScrollBarPart (..)
  , scrollBarBuilder
  ) where

import Control.Monad (when)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Blink.Geometry (Alignment (..), Orientation (..), Point (..), Rectangle (..), insetRect)
import Blink.Input (Key (..), InputState (..))
import Blink.Layout (RectConstraint (..), Constraint (..), BoxConfig (..), hBox, vBox, defaultBoxConfig)
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI

isActivated :: (Eq e, Ord e) => e -> UI e u c Bool
isActivated eid = do
  clicked    <- isClicked eid
  enterPress <- isKeyPressed eid KeyReturn
  disabled   <- isDisabled
  return (not disabled && (clicked || enterPress))

-- | Read-only text display. Renders @text@ within the element's content
-- rectangle using the active style. Does not participate in interaction or
-- keyboard navigation.
label :: (Eq e, Ord e) => e -> Text -> UI e u c ()
label eid text = renderControl eid $ do
  style <- getStyle eid
  drawText (styleTextColour style) (styleTextAlign style) text

-- | Read-only progress indicator. @value@ is clamped to @[0, 1]@ and rendered
-- as a filled bar scaled to that fraction of the content width.
progressBar :: (Eq e, Ord e) => e -> Double -> UI e u c ()
progressBar eid value = renderControl eid $ do
  style <- getStyle eid
  r     <- getBounds
  let clamped  = max 0 (min 1 value)
      fillRect' = r { rectWidth = rectWidth r * clamped }
  withBounds fillRect' $ fillRect (styleTextColour style)

checkboxMark :: (Eq e, Ord e) => e -> Bool -> (Bool -> c) -> UI e u c ()
checkboxMark boxId checked mkCmd = control boxId $ do
  style     <- getStyle boxId
  activated <- isActivated boxId
  when checked   $ drawText (styleTextColour style) AlignCenter "✓"
  when activated $ dispatch (mkCmd (not checked))

checkboxLabel :: Style -> Text -> UI e u c ()
checkboxLabel style text = drawText (styleTextColour style) AlignLeft text

-- | A togglable checkbox with an adjacent label. Dispatches @mkCmd (not checked)@
-- when activated by a click or the Enter key.
checkbox :: (Eq e, Ord e) => e -> Text -> Bool -> (Bool -> c) -> UI e u c ()
checkbox boxId text checked mkCmd = do
  style <- getStyle boxId
  hBox (defaultBoxConfig { boxSpacing = 4, boxFillCross = False })
    [ (RectConstraint (Exactly 20) (Exactly 20) MiddleLeft, checkboxMark boxId checked mkCmd)
    , (RectConstraint Fill Fill MiddleLeft, checkboxLabel style text)
    ]
  whenFocused boxId $ do
    styleSet <- getStyleSet boxId
    let s = styleSetFocused styleSet
    case styleBorderColour s of
      Just c  -> strokeRect c (styleBorderWidth s)
      Nothing -> pure ()

-- | A clickable button labelled @txt@. Returns 'True' on the frame the button
-- is activated — by a left-click or by pressing Enter while focused.
button :: (Eq e, Ord e) => e -> Text -> UI e u c Bool
button eid txt = do
  control eid $ do
    style <- getStyle eid
    drawText (styleTextColour style) (styleTextAlign style) txt
  isActivated eid

-- | A single-line text entry field. Displays a cursor when focused. Dispatches
-- @mkCmd newValue@ when the text changes via typed characters or Backspace. The
-- application is responsible for storing and passing back @value@ each frame.
textInput :: (Eq e, Ord e) => e -> Text -> (Text -> c) -> UI e u c ()
textInput eid value mkCmd = control eid $ do
  style     <- getStyle eid
  hasFocus  <- isFocused eid
  isDisabl  <- isDisabled
  backspace <- isKeyPressed eid KeyBackspace
  let displayed = if hasFocus && not isDisabl then value <> "|" else value
  drawText (styleTextColour style) (styleTextAlign style) displayed
  when hasFocus $ whenEnabled $ do
    input <- getInput
    let withTyped = foldl' (<>) value (inputTypedText input)
        result    = if backspace && not (T.null withTyped)
                    then T.init withTyped
                    else withTyped
    when (result /= value) $ do
      dispatch (mkCmd result)

-- | Sub-parts of a scrollbar, used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | VScroll ScrollBarPart
-- scrollBarBuilder posField VScroll Vertical ratio
-- @
data ScrollBarPart
  = ScrollTrack
  | ScrollThumb
  | ScrollDecrBtn
  | ScrollIncrBtn
  deriving (Eq, Ord, Show)

-- | A scrollbar with decrement\/increment buttons flanking a draggable thumb.
-- @posField@ is a 'Field' into the UI state record holding the scroll position
-- in @[0, 1]@; the control reads and writes it itself. @thumbRatio@ is the
-- fraction of the track the thumb fills (visible \/ total), also in @[0, 1]@.
-- Button clicks step by @thumbRatio@; dragging centres the thumb on the cursor.
scrollBarBuilder :: (Eq e, Ord e)
                 => Field u Double
                 -> (ScrollBarPart -> e)
                 -> Orientation
                 -> Double
                 -> UI e u c ()
scrollBarBuilder posField mkId ori thumbRatio = do
  bounds <- getBounds
  pos    <- useField posField
  let p    = max 0 (min 1 pos)
      r    = max 0 (min 1 thumbRatio)
      btnC = case ori of
        Vertical   -> RectConstraint Fill (Exactly (rectWidth bounds))  TopLeft
        Horizontal -> RectConstraint (Exactly (rectHeight bounds)) Fill TopLeft
  layoutFn defaultBoxConfig
    [ (btnC,                              decrBtn p r)
    , (RectConstraint Fill Fill TopLeft,  track p r)
    , (btnC,                              incrBtn p r)
    ]
  where
    layoutFn = case ori of
      Vertical   -> vBox
      Horizontal -> hBox

    (decrSym, incrSym) = case ori of
      Vertical   -> ("▲", "▼")
      Horizontal -> ("◀", "▶")

    decrBtn p r = do
      clicked <- button (mkId ScrollDecrBtn) decrSym
      when clicked $ setField posField (max 0 (p - r))

    incrBtn p r = do
      clicked <- button (mkId ScrollIncrBtn) incrSym
      when clicked $ setField posField (min 1 (p + r))

    track p r = do
      slotBounds <- getBounds
      styleSet   <- getStyleSet (mkId ScrollTrack)
      let norm        = styleSetNormal styleSet
          bgRect      = insetRect (styleMargin norm) slotBounds
          contentRect = insetRect (stylePadding norm) bgRect
          thumbR      = scrollThumbRect ori p r contentRect
      control (mkId ScrollTrack) $
        withBounds thumbR $ renderControl (mkId ScrollThumb) $ pure ()
      pressed  <- isPressed (mkId ScrollTrack)
      captured <- isCapturedBy (mkId ScrollTrack)
      when pressed $ captureElement (mkId ScrollTrack)
      when (pressed || captured) $ do
        mousePos <- getMousePos
        setField posField (scrollPosFromMouse ori r contentRect mousePos)

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
