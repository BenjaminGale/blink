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
  , scrollBar
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

isActivated :: (Eq e, Ord e) => e -> UI e c Bool
isActivated eid = do
  clicked    <- isClicked eid
  enterPress <- isKeyPressed eid KeyReturn
  disabled   <- isDisabled
  return (not disabled && (clicked || enterPress))

-- | Read-only text display. Renders @text@ within the element's content
-- rectangle using the active style. Does not participate in interaction or
-- keyboard navigation.
label :: (Eq e, Ord e) => e -> Text -> UI e c ()
label eid text = renderControl eid $ do
  style <- getStyle eid
  drawText (styleTextColour style) (styleTextAlign style) text

-- | Read-only progress indicator. @value@ is clamped to @[0, 1]@ and rendered
-- as a filled bar scaled to that fraction of the content width.
progressBar :: (Eq e, Ord e) => e -> Double -> UI e c ()
progressBar eid value = renderControl eid $ do
  style <- getStyle eid
  r     <- getBounds
  let clamped  = max 0 (min 1 value)
      fillRect' = r { rectWidth = rectWidth r * clamped }
  withBounds fillRect' $ fillRect (styleTextColour style)

checkboxMark :: (Eq e, Ord e) => e -> Bool -> (Bool -> c) -> UI e c ()
checkboxMark boxId checked mkCmd = control boxId $ do
  style     <- getStyle boxId
  activated <- isActivated boxId
  when checked   $ drawText (styleTextColour style) AlignCenter "✓"
  when activated $ dispatch (mkCmd (not checked))

checkboxLabel :: Style -> Text -> UI e c ()
checkboxLabel style text = drawText (styleTextColour style) AlignLeft text

-- | A togglable checkbox with an adjacent label. Dispatches @mkCmd (not checked)@
-- when activated by a click or the Enter key.
checkbox :: (Eq e, Ord e) => e -> Text -> Bool -> (Bool -> c) -> UI e c ()
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
button :: (Eq e, Ord e) => e -> Text -> UI e c Bool
button eid txt = do
  control eid $ do
    style <- getStyle eid
    drawText (styleTextColour style) (styleTextAlign style) txt
  isActivated eid

-- | A single-line text entry field. Displays a cursor when focused. Dispatches
-- @mkCmd newValue@ when the text changes via typed characters or Backspace. The
-- application is responsible for storing and passing back @value@ each frame.
textInput :: (Eq e, Ord e) => e -> Text -> (Text -> c) -> UI e c ()
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

-- | Sub-parts of a 'scrollBar', used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | VScroll ScrollBarPart
-- scrollBar VScroll Vertical pos ratio ScrollMoved
-- @
data ScrollBarPart
  = ScrollTrack
  | ScrollThumb
  | ScrollDecrBtn
  | ScrollIncrBtn
  deriving (Eq, Ord, Show)

-- | A scrollbar with decrement\/increment buttons flanking a draggable thumb.
-- @pos@ is the current scroll position in @[0, 1]@; @thumbRatio@ is the
-- fraction of the track the thumb fills (visible \/ total), also in @[0, 1]@.
-- Dispatches @mkCmd newPos@ when the user drags the track or clicks a button.
-- Button clicks step by @thumbRatio@; dragging centres the thumb on the cursor.
scrollBar :: (Eq e, Ord e)
          => (ScrollBarPart -> e)
          -> Orientation
          -> Double
          -> Double
          -> (Double -> c)
          -> UI e c ()
scrollBar mkId ori pos thumbRatio mkCmd = do
  bounds <- getBounds
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
      when clicked $ dispatch (mkCmd (max 0 (p - r)))

    incrBtn p r = do
      clicked <- button (mkId ScrollIncrBtn) incrSym
      when clicked $ dispatch (mkCmd (min 1 (p + r)))

    track p r = do
      slotBounds <- getBounds
      styleSet   <- getStyleSet (mkId ScrollTrack)
      let norm        = styleSetNormal styleSet
          bgRect      = insetRect (styleMargin norm) slotBounds
          contentRect = insetRect (stylePadding norm) bgRect
          thumbR      = scrollThumbRect ori p r contentRect
      control (mkId ScrollTrack) $
        withBounds thumbR $ renderControl (mkId ScrollThumb) $ pure ()
      pressed <- isPressed (mkId ScrollTrack)
      when pressed $ do
        mousePos <- getMousePos
        dispatch (mkCmd (scrollPosFromMouse ori r contentRect mousePos))

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
