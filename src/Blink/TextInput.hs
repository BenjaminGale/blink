{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A single-line text entry field: click-to-place cursor, drag selection,
-- Shift+arrow extension, and selection-aware editing. Long text scrolls
-- horizontally to keep the cursor visible.
module Blink.TextInput
  ( TextInputAttributes
  , TextInputConfig
  , textInput
  , textInputStyleKey
  , text
  , inputFilter
  , displayFilter
  , onInput
  , onSubmit
  , isFocusable
  , isEnabled
  , style
  , StyleKey (..)
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost
  ) where

import Control.Monad (forM_, when)
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Blink.Control
import Blink.Geometry (Point (..), Rectangle (..))
import Blink.Input (Key (..), KeyEvent (..), Modifier (..), InputState (..))
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI

-- | 'Blink.TextInput.textInput'\'s own closed attrs type: the common
-- capabilities every control has, plus 'text' (the field's current value),
-- 'inputFilter', 'displayFilter', 'onInput', and 'onSubmit'.
data TextInputAttributes e msg
  = TextInputIsFocusable Bool
  | TextInputIsEnabled Bool
  | TextInputStyle (StyleKey e)
  | TextInputTabNavigation NavigationMode
  | TextInputIsArrowNavigationEnabled Bool
  | TextInputOnClicked (() -> [Out e msg])
  | TextInputOnFocusGained (() -> [Out e msg])
  | TextInputOnFocusLost (() -> [Out e msg])
  | TextInputOnMouseEntered (() -> [Out e msg])
  | TextInputOnMouseExited (() -> [Out e msg])
  | TextInputOnMouseDown (() -> [Out e msg])
  | TextInputOnMouseUp (() -> [Out e msg])
  | TextInputOnKeyPressed (KeyEvent -> [Out e msg])
  | TextInputText Text
  | TextInputInputFilter (Text -> Text)
  | TextInputDisplayFilter (Text -> Text)
  | TextInputOnInput (Text -> [Out e msg])
  | TextInputOnSubmit (() -> [Out e msg])

instance HasControlConfig e (TextInputAttributes e msg) where
  mkIsFocusable = TextInputIsFocusable
  matchIsFocusable (TextInputIsFocusable b) = Just b
  matchIsFocusable _ = Nothing
  mkIsEnabled = TextInputIsEnabled
  matchIsEnabled (TextInputIsEnabled b) = Just b
  matchIsEnabled _ = Nothing
  mkStyle = TextInputStyle
  matchStyle (TextInputStyle k) = Just k
  matchStyle _ = Nothing
  mkTabNavigation = TextInputTabNavigation
  matchTabNavigation (TextInputTabNavigation m) = Just m
  matchTabNavigation _ = Nothing
  mkIsArrowNavigationEnabled = TextInputIsArrowNavigationEnabled
  matchIsArrowNavigationEnabled (TextInputIsArrowNavigationEnabled b) = Just b
  matchIsArrowNavigationEnabled _ = Nothing

instance HasElementEvents e msg (TextInputAttributes e msg) where
  mkOnClicked = TextInputOnClicked
  matchOnClicked (TextInputOnClicked f) = Just f
  matchOnClicked _ = Nothing
  mkOnFocusGained = TextInputOnFocusGained
  matchOnFocusGained (TextInputOnFocusGained f) = Just f
  matchOnFocusGained _ = Nothing
  mkOnFocusLost = TextInputOnFocusLost
  matchOnFocusLost (TextInputOnFocusLost f) = Just f
  matchOnFocusLost _ = Nothing
  mkOnMouseEntered = TextInputOnMouseEntered
  matchOnMouseEntered (TextInputOnMouseEntered f) = Just f
  matchOnMouseEntered _ = Nothing
  mkOnMouseExited = TextInputOnMouseExited
  matchOnMouseExited (TextInputOnMouseExited f) = Just f
  matchOnMouseExited _ = Nothing
  mkOnMouseDown = TextInputOnMouseDown
  matchOnMouseDown (TextInputOnMouseDown f) = Just f
  matchOnMouseDown _ = Nothing
  mkOnMouseUp = TextInputOnMouseUp
  matchOnMouseUp (TextInputOnMouseUp f) = Just f
  matchOnMouseUp _ = Nothing
  mkOnKeyPressed = TextInputOnKeyPressed
  matchOnKeyPressed (TextInputOnKeyPressed f) = Just f
  matchOnKeyPressed _ = Nothing

instance HasTextConfig (TextInputAttributes e msg) where
  mkText = TextInputText
  matchText (TextInputText t) = Just t
  matchText _ = Nothing

-- | Applied to newly typed text before it's inserted, letting callers
-- restrict which keystrokes are accepted (e.g. @T.filter isDigit@ for a
-- digits-only field). Reformatting the value itself (e.g. inserting
-- punctuation as the user types) is an application concern, not this
-- control's -- do it in an 'onInput' handler and pass the already-formatted
-- value back in on the next frame. Defaults to 'id'.
inputFilter :: (Text -> Text) -> TextInputAttributes e msg
inputFilter = TextInputInputFilter

-- | Applied to the value everywhere it is measured or drawn -- the rendered
-- text, and every character-offset calculation used for cursor placement,
-- click hit-testing, and auto-scroll -- so what's on screen and where the
-- cursor lands always agree. It must be length- and position-preserving
-- (e.g. @T.map (const '\8226')@ to mask each character of a password); the
-- underlying value edited by 'inputFilter'\/'onInput' is never affected by
-- it. Defaults to 'id'.
displayFilter :: (Text -> Text) -> TextInputAttributes e msg
displayFilter = TextInputDisplayFilter

-- | Reacts with the new value whenever a keystroke changes it.
onInput :: (Text -> [Out e msg]) -> TextInputAttributes e msg
onInput = TextInputOnInput

-- | Reacts when Enter is pressed while the field is focused and enabled.
onSubmit :: (() -> [Out e msg]) -> TextInputAttributes e msg
onSubmit = TextInputOnSubmit

-- | Configuration for 'textInput', resolved from a
-- @['TextInputAttributes' e msg]@.
data TextInputConfig e msg = TextInputConfig
  { ticfgValue         :: Text
  , ticfgInputFilter   :: Text -> Text
  , ticfgDisplayFilter :: Text -> Text
  , ticfgOnInput       :: [Text -> [Out e msg]]
  , ticfgOnSubmit      :: [() -> [Out e msg]]
  }

-- | The 'StyleKey' 'textInput' resolves its style from unless overridden
-- via 'style'.
textInputStyleKey :: StyleKey e
textInputStyleKey = Class "textInput"

defaultTextInputConfig :: TextInputConfig e msg
defaultTextInputConfig = TextInputConfig
  { ticfgValue = "", ticfgInputFilter = id, ticfgDisplayFilter = id, ticfgOnInput = [], ticfgOnSubmit = [] }

resolveTextInputConfig :: [TextInputAttributes e msg] -> TextInputConfig e msg
resolveTextInputConfig = foldl' apply defaultTextInputConfig
  where
    apply cfg (TextInputText t)           = cfg { ticfgValue = t }
    apply cfg (TextInputInputFilter f)    = cfg { ticfgInputFilter = f }
    apply cfg (TextInputDisplayFilter f)  = cfg { ticfgDisplayFilter = f }
    apply cfg (TextInputOnInput f)        = cfg { ticfgOnInput = ticfgOnInput cfg ++ [f] }
    apply cfg (TextInputOnSubmit f)       = cfg { ticfgOnSubmit = ticfgOnSubmit cfg ++ [f] }
    apply cfg _                           = cfg

toTextInputControlAttr :: TextInputAttributes e msg -> Maybe (ControlAttrs e msg)
toTextInputControlAttr (TextInputText _)          = Nothing
toTextInputControlAttr (TextInputInputFilter _)   = Nothing
toTextInputControlAttr (TextInputDisplayFilter _) = Nothing
toTextInputControlAttr (TextInputOnInput _)       = Nothing
toTextInputControlAttr (TextInputOnSubmit _)      = Nothing
toTextInputControlAttr a                          = translateCommon a

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
  -> Text        -- ^ displayed value (post-'displayFilter')
  -> Double      -- ^ current horizontal scroll offset
  -> Selection   -- ^ current selection
  -> UI e msg Selection
resolveMouseSelection eid bounds wasCapturing justFocused displayValue scrollX sel = do
  isCapturing <- isDragging eid
  if isCapturing
    then do
      mousePos <- getMousePos
      let localX = realToFrac (pointX mousePos - rectX bounds) + realToFrac scrollX :: Float
      clickedPos <- charAtOffset displayValue localX
      pure $ if not wasCapturing || justFocused
        then cursor clickedPos
        else extendActive (const clickedPos) sel
    else pure sel

-- | Shift+Left\/Right extend the selection; plain Left\/Right collapse an
-- existing selection to its near end, or step by one otherwise.
resolveKeyboardSelection :: Bool -> [KeyEvent] -> Int -> Selection -> Selection
resolveKeyboardSelection canEdit keyEvts len sel@(Selection _ active)
  | shiftLeft  = extendActive (\a -> max 0   (a - 1)) sel
  | shiftRight = extendActive (\a -> min len (a + 1)) sel
  | plainLeft  = cursor (if hasSel then selLo else max 0   (active - 1))
  | plainRight = cursor (if hasSel then selHi else min len (active + 1))
  | otherwise  = sel
  where
    hasSel     = selectionHasExtent sel
    selLo      = selectionLow sel
    selHi      = selectionHigh sel
    pressed k withShift = canEdit && any (\e -> key e == k && (Shift `elem` modifiers e) == withShift) keyEvts
    shiftLeft  = pressed KeyLeft  True
    shiftRight = pressed KeyRight True
    plainLeft  = pressed KeyLeft  False
    plainRight = pressed KeyRight False

-- | Backspace and typed text edit the value, selection-aware; returns the
-- new selection alongside the new value when it actually changed.
-- 'inputFilter' is applied to the newly typed text before insertion,
-- letting callers reject or transform keystrokes (e.g. digits only).
-- Assumes the caller has already checked the control is focused and
-- enabled. Pure -- the caller decides how (or whether) to report the
-- change.
applyEdit :: (Text -> Text) -> Text -> InputState -> Selection -> (Selection, Maybe Text)
applyEdit inputFilterFn currentValue input sel@(Selection _ active)
  | backspace || hasTyped =
      (cursor newCursor, if newText /= currentValue then Just newText else Nothing)
  | otherwise = (sel, Nothing)
  where
    keyEvts   = inputKeyEvents input
    backspace = any (\e -> key e == KeyBackspace) keyEvts
    typed     = inputFilterFn (foldl (<>) T.empty (inputTypedText input))
    hasTyped  = not (T.null typed)
    hasSel    = selectionHasExtent sel
    selLo     = selectionLow sel
    selHi     = selectionHigh sel
    (newText, newCursor)
      | hasSel && backspace =
          (T.take selLo currentValue <> T.drop selHi currentValue, selLo)
      | hasSel =
          (T.take selLo currentValue <> typed <> T.drop selHi currentValue, selLo + T.length typed)
      | backspace && active > 0 =
          (T.take (active - 1) currentValue <> T.drop active currentValue, active - 1)
      | hasTyped =
          (T.take active currentValue <> typed <> T.drop active currentValue, active + T.length typed)
      | otherwise = (currentValue, active)

-- | The scroll offset needed to keep a cursor at @cursorAbs@ visible within
-- a viewport of width @w@ currently scrolled to @scrollX@. Pixels in,
-- pixels out -- @scrollFraction@\/@scrollPixels@ convert at the boundary
-- with 'getScrollState'\/'ScrollTo' so the stored value stays in the same
-- @[0, 1]@ convention every other scroll-state consumer uses.
resolveScroll :: Double -> Double -> Double -> Double
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
  | maxPx > 0 = max 0 (min 1 (px / maxPx))
  | otherwise = 0

-- | The inverse of 'scrollFraction': converts a stored @[0, 1]@ fraction
-- back to a pixel offset, given the max offset from 'maxScrollPixels'.
scrollPixels :: Double -> Double -> Double
scrollPixels maxPx frac = frac * maxPx

-- | Draws the selection highlight and the cursor (both focused and
-- enabled), and the text itself, all offset by the current horizontal
-- scroll.
drawTextInputContent :: Ord e => Style -> Rectangle -> Text -> Bool -> Double -> Selection -> UI e msg ()
drawTextInputContent s bounds displayValue canEdit ox sel@(Selection _ active) = do
  when (canEdit && drawLo < drawHi) $ do
    loX <- charOffset displayValue drawLo
    hiX <- charOffset displayValue drawHi
    let selRect = Rectangle
          (rectX bounds + realToFrac loX - ox)
          (rectY bounds)
          (realToFrac (hiX - loX))
          (rectHeight bounds)
    withBounds selRect $ fillRect (RGBA 0.3 0.5 1.0 0.4)

  let textBounds = bounds { rectX = rectX bounds - ox }
  withBounds textBounds $ drawText (styleTextColour s) AlignLeft displayValue

  when canEdit $ do
    curX <- charOffset displayValue active
    let cursorRect = Rectangle
          (rectX bounds + realToFrac curX - ox)
          (rectY bounds)
          1
          (rectHeight bounds)
    withBounds cursorRect $ fillRect (styleTextColour s)
  where
    drawLo = selectionLow sel
    drawHi = selectionHigh sel

-- | A single-line text entry field (see the module header). Cursor
-- position and selection are control state, not application data --
-- 'textInput' reads and writes them itself via 'getSelection' and
-- 'getScrollState', keyed by the element ID.
textInput :: Ord e => e -> [TextInputAttributes e msg] -> UI e msg ()
textInput eid attrs = do
  wasFocused   <- isFocused eid
  wasCapturing <- isDragging eid
  control eid (mapMaybe toTextInputControlAttr attrs ++ [focusOnClick FocusSelf, content (body wasFocused wasCapturing)])
  where
    cfg          = resolveTextInputConfig attrs
    currentValue = ticfgValue cfg

    body wasFocused wasCapturing = do
      s        <- currentStyle
      hasFocus <- isFocused eid
      disabled <- isDisabled
      bounds   <- getBounds
      input    <- getInput
      sel      <- getSelection eid
      frac     <- getScrollState eid

      let displayValue = ticfgDisplayFilter cfg currentValue
          w           = rectWidth bounds
          selInit     = fromMaybe (cursor (T.length currentValue)) sel
          -- Focus was gained by a click this frame (e.g. clicking from
          -- another element). Treat as a fresh click rather than a drag
          -- continuation so the old anchor is not inherited.
          justFocused = hasFocus && not wasFocused
          canEdit     = hasFocus && not disabled

      contentW <- realToFrac <$> charOffset displayValue (T.length displayValue)
      let maxScrollPx = maxScrollPixels contentW w
          scrollX     = scrollPixels maxScrollPx frac

      selAfterMouse <-
        if canEdit
          then resolveMouseSelection eid bounds wasCapturing justFocused displayValue scrollX selInit
          else pure selInit

      let selAfterKeys = resolveKeyboardSelection canEdit (inputKeyEvents input) (T.length currentValue) selAfterMouse

          (selFinal, edited)
            | canEdit   = applyEdit (ticfgInputFilter cfg) currentValue input selAfterKeys
            | otherwise = (selAfterKeys, Nothing)

          submitted = canEdit && any (\e -> key e == KeyReturn) (inputKeyEvents input)

      when submitted $ runHandlers (ticfgOnSubmit cfg) ()
      forM_ edited $ \t -> runHandlers (ticfgOnInput cfg) t

      when canEdit $ emitUi (SetSelectionAt eid selFinal)

      -- Computed locally rather than re-read via 'getScrollState': scroll
      -- writes are deferred (applied between frames), so a same-frame
      -- re-read would still see the pre-write value and the cursor would lag
      -- the auto-scroll by one frame.
      effectiveScrollX <-
        if canEdit
          then do
            curX <- charOffset displayValue (selectionActive selFinal)
            let newScrollX = resolveScroll w scrollX (realToFrac curX)
            when (newScrollX /= scrollX) $ emitUi (ScrollTo eid (scrollFraction maxScrollPx newScrollX))
            pure newScrollX
          else pure scrollX

      drawTextInputContent s bounds displayValue canEdit effectiveScrollX selFinal
