{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A single-line text entry field: click-to-place cursor, drag selection,
-- Shift+arrow extension, and selection-aware editing. Long text scrolls
-- horizontally to keep the cursor visible. A leaf, built directly on
-- 'control'. Its own value isn't a 'Blink.Controls.Label.LabelledConfig'
-- field -- it's edited, not just displayed -- so it has its own 'value'
-- attribute rather than 'Blink.Controls.Label.text'.
module Blink.Controls.TextInput
  ( TextInputConfig (..)
  , defaultTextInputConfig
  , textInputStyleKey
  , textInput
  , value
  , placeholder
  , inputFilter
  , displayFilter
  , onInput
  , onSubmit
  ) where

import Control.Monad (forM_, void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Blink.Controls.Control
import Blink.Controls.Label (captionElement)
import Blink.Controls.TextInput.Style (textInputStyleKey)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..))
import Blink.Input (Key (..), KeyEvent (..), Modifier (..), InputState (..))
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..))
import Blink.View
import Blink.View.Drawing (fillRect, drawText)
import Blink.View.Selection (selectionHasExtent, selectionLow, selectionHigh, cursor, extendActive)
import Blink.Element (Element (..), HasLayoutConfig (..))

-- | Every capability 'textInput' resolves: the wrapped 'ControlConfig',
-- its current value, 'placeholder', 'inputFilter'\/'displayFilter', and
-- its 'onInput'\/'onSubmit' reactions.
data TextInputConfig e msg = TextInputConfig
  { ticControl       :: ControlConfig e msg
  , ticValue         :: Text
  , ticPlaceholder   :: Text
  , ticInputFilter   :: Text -> Text
  , ticDisplayFilter :: Text -> Text
  , ticOnInput       :: [Text -> [Effect e msg]]
  , ticOnSubmit      :: [EventHandler e msg]
  , ticLayout        :: Layout
  }

-- | 'defaultControlConfig' (styled via 'textInputStyleKey'), an empty
-- value and placeholder, identity filters, no 'onInput'\/'onSubmit'
-- reactions, and @Layout fill fitContent TopLeft@ (see 'textInput').
defaultTextInputConfig :: TextInputConfig e msg
defaultTextInputConfig = TextInputConfig
  { ticControl       = defaultControlConfig { ccStyleKey = textInputStyleKey }
  , ticValue         = ""
  , ticPlaceholder   = ""
  , ticInputFilter   = id
  , ticDisplayFilter = id
  , ticOnInput       = []
  , ticOnSubmit      = []
  , ticLayout        = Layout fill fitContent TopLeft
  }

instance HasControlConfig e msg (TextInputConfig e msg) where
  overControl attr = Attribute (\tc -> tc { ticControl = runAttribute attr (ticControl tc) })

instance HasLayoutConfig (TextInputConfig e msg) where
  overLayout attr = Attribute (\tc -> tc { ticLayout = runAttribute attr (ticLayout tc) })

-- | Sets the field's current value. Defaults to @\"\"@ when not given.
value :: Text -> Attribute (TextInputConfig e msg)
value t = Attribute (\tc -> tc { ticValue = t })

-- | Text shown, muted, in place of the value whenever that value is
-- empty -- drawn directly, never passed through 'inputFilter' or
-- 'displayFilter' since it isn't user data. Defaults to @\"\"@, which
-- shows nothing.
placeholder :: Text -> Attribute (TextInputConfig e msg)
placeholder t = Attribute (\tc -> tc { ticPlaceholder = t })

-- | Applied to newly typed text before it's inserted, letting callers
-- restrict which keystrokes are accepted (e.g. @T.filter isDigit@ for a
-- digits-only field). Reformatting the value itself (e.g. inserting
-- punctuation as the user types) is an application concern, not this
-- control's -- do it in an 'onInput' handler and pass the already-formatted
-- value back in on the next frame. Defaults to 'id'.
inputFilter :: (Text -> Text) -> Attribute (TextInputConfig e msg)
inputFilter f = Attribute (\tc -> tc { ticInputFilter = f })

-- | Applied to the value everywhere it is measured or drawn -- the rendered
-- text, and every character-offset calculation used for cursor placement,
-- click hit-testing, and auto-scroll -- so what's on screen and where the
-- cursor lands always agree. It must be length- and position-preserving
-- (e.g. @T.map (const '\8226')@ to mask each character of a password); the
-- underlying value edited by 'inputFilter'\/'onInput' is never affected by
-- it. Defaults to 'id'.
displayFilter :: (Text -> Text) -> Attribute (TextInputConfig e msg)
displayFilter f = Attribute (\tc -> tc { ticDisplayFilter = f })

-- | Reacts with the new value whenever a keystroke changes it.
onInput :: (Text -> [Effect e msg]) -> Attribute (TextInputConfig e msg)
onInput f = Attribute (\tc -> tc { ticOnInput = ticOnInput tc ++ [f] })

-- | Reacts when Enter is pressed while the field is focused and enabled.
onSubmit :: EventHandler e msg -> Attribute (TextInputConfig e msg)
onSubmit f = Attribute (\tc -> tc { ticOnSubmit = ticOnSubmit tc ++ [f] })

-- | Whether a click/drag this frame is continuing a drag already in
-- progress, or arriving alongside a fresh focus change -- the two facts
-- 'resolveMouseSelection' needs from before the frame started, since
-- neither is recoverable from this frame's own state alone.
data SelectionGesture = SelectionGesture
  { sgWasCapturing :: Bool  -- ^ control was already being dragged last frame
  , sgJustFocused  :: Bool  -- ^ control just gained focus this frame
  }

-- | Click sets both selection ends at the clicked character; dragging
-- extends only the active end, keeping the anchor from before the drag
-- started. Gaining focus with no drag in progress -- Tab\/Shift-Tab, or the
-- default focus a control gets when nothing else is focused -- selects the
-- entire value. Assumes the caller has already checked the control is
-- focused and enabled.
resolveMouseSelection
  :: Ord e
  => e                -- ^ element ID
  -> Rectangle        -- ^ control's bounds
  -> SelectionGesture
  -> Text             -- ^ displayed value (post-'displayFilter')
  -> Double           -- ^ current horizontal scroll offset
  -> Selection        -- ^ current selection
  -> View e msg Selection
resolveMouseSelection eid bounds gesture displayValue scrollX sel = do
  isCapturing <- isDragging eid
  if isCapturing
    then do
      mousePos <- getMousePos
      let localX = realToFrac (pointX mousePos - rectX bounds) + realToFrac scrollX :: Float
      clickedPos <- charAtOffset displayValue localX
      pure $ if not (sgWasCapturing gesture) || sgJustFocused gesture
        then cursor clickedPos
        else extendActive (const clickedPos) sel
    else pure $ if sgJustFocused gesture
      then Selection 0 (T.length displayValue)
      else sel

-- | Ctrl+A selects the entire value; Shift+Left\/Right extend the
-- selection; plain Left\/Right collapse an existing selection to its near
-- end, or step by one otherwise.
resolveKeyboardSelection :: Bool -> [KeyEvent] -> Int -> Selection -> Selection
resolveKeyboardSelection canEdit keyEvts len sel@(Selection _ active)
  | selectAll  = Selection 0 len
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
    selectAll  = canEdit && any (\e -> key e == KeyA && Ctrl `elem` modifiers e) keyEvts

-- | Backspace, Delete, and typed text edit the value, selection-aware;
-- returns the new selection alongside the new value when it actually
-- changed. Each removes the whole selection when there is one; with
-- none, Backspace removes the character before the cursor and Delete
-- the one after it. 'inputFilter' is applied to newly typed text before
-- insertion. Assumes the caller has already checked the control is
-- focused and enabled.
applyEdit :: (Text -> Text) -> Text -> InputState -> Selection -> (Selection, Maybe Text)
applyEdit inputFilterFn currentValue input sel@(Selection _ active)
  | backspace || delete || hasTyped =
      (cursor newCursor, if newText /= currentValue then Just newText else Nothing)
  | otherwise = (sel, Nothing)
  where
    keyEvts   = inputKeyEvents input
    backspace = any (\e -> key e == KeyBackspace) keyEvts
    delete    = any (\e -> key e == KeyDelete) keyEvts
    typed     = inputFilterFn (foldl (<>) T.empty (inputTypedText input))
    hasTyped  = not (T.null typed)
    hasSel    = selectionHasExtent sel
    selLo     = selectionLow sel
    selHi     = selectionHigh sel
    (newText, newCursor)
      | hasSel && (backspace || delete) =
          (T.take selLo currentValue <> T.drop selHi currentValue, selLo)
      | hasSel =
          (T.take selLo currentValue <> typed <> T.drop selHi currentValue, selLo + T.length typed)
      | backspace && active > 0 =
          (T.take (active - 1) currentValue <> T.drop active currentValue, active - 1)
      | delete && active < T.length currentValue =
          (T.take active currentValue <> T.drop (active + 1) currentValue, active)
      | hasTyped =
          (T.take active currentValue <> typed <> T.drop active currentValue, active + T.length typed)
      | otherwise = (currentValue, active)

-- | Resolves the frame's selection changes (mouse, then keyboard) and any
-- resulting edit, firing 'onSubmit'\/'onInput' reactions and, when the
-- selection actually moved, writing it back via 'requestSelectionAt'.
-- Returns the final selection for the caller to draw and auto-scroll
-- against.
resolveSelectionAndEdit
  :: Ord e
  => TextInputConfig e msg
  -> e
  -> Rectangle
  -> Bool              -- ^ canEdit
  -> SelectionGesture
  -> Text              -- ^ current value
  -> Text              -- ^ displayed value (post-'displayFilter')
  -> Double            -- ^ current horizontal scroll offset
  -> InputState
  -> Selection         -- ^ selection at the start of the frame
  -> View e msg Selection
resolveSelectionAndEdit cfg eid bounds canEdit gesture currentValue displayValue scrollX input selInit = do
  selAfterMouse <-
    if canEdit
      then resolveMouseSelection eid bounds gesture displayValue scrollX selInit
      else pure selInit

  let selAfterKeys = resolveKeyboardSelection canEdit (inputKeyEvents input) (T.length currentValue) selAfterMouse

      (selFinal, edited)
        | canEdit   = applyEdit (ticInputFilter cfg) currentValue input selAfterKeys
        | otherwise = (selAfterKeys, Nothing)

      submitted = canEdit && any (\e -> key e == KeyReturn) (inputKeyEvents input)

  when submitted $ runHandlers (ticOnSubmit cfg) ()
  forM_ edited $ \t -> runHandlers (ticOnInput cfg) t

  when (canEdit && selFinal /= selInit) $ requestSelectionAt eid selFinal

  pure selFinal

-- | The scroll offset needed to keep a cursor at @cursorAbs@ visible within
-- a viewport of width @w@ currently scrolled to @scrollX@. Pixels in,
-- pixels out -- @scrollFraction@\/@scrollPixels@ convert at the boundary
-- with 'getScrollState'\/'requestScrollTo' so the stored value stays in the same
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

-- | Mixes @text@'s RGB 60% of the way toward @bg@, leaving alpha alone --
-- how the placeholder is muted against whatever text/background colours
-- the current style resolves to, without needing a dedicated theme
-- colour for it. Mixing toward the background rather than toward a
-- fixed white keeps the placeholder muted in both a light theme (dark
-- text faded toward a light background) and a dark one (light text
-- faded toward a dark background), where mixing toward white would
-- instead make it stand out more than the value text. RGB rather than
-- alpha, since text is rasterized to a texture that may not
-- alpha-blend on copy -- see 'Blink.Controls.Slider.shade' for the
-- same trick run with a fixed factor instead of a target colour.
muted :: Colour -> Colour -> Colour
muted (RGBA tr tg tb a) (RGBA br bg bb _) = RGBA (mix tr br) (mix tg bg) (mix tb bb) a
  where mix t b = t + (b - t) * 0.6

-- | Draws the selection highlight and the cursor (both focused and
-- enabled), and the text itself, all offset by the current horizontal
-- scroll -- the placeholder, muted, in place of the value when that
-- value is empty.
drawTextInputContent :: Ord e => Style -> Rectangle -> Text -> Text -> Bool -> Double -> Selection -> View e msg ()
drawTextInputContent s bounds displayValue placeholderText canEdit ox sel@(Selection _ active) = do
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
  if T.null displayValue && not (T.null placeholderText)
    then withBounds textBounds $ drawText (muted (styleTextColour s) (styleBackground s)) AlignLeft placeholderText
    else withBounds textBounds $ drawText (styleTextColour s) AlignLeft displayValue

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
-- 'getScrollState', keyed by the element ID. Defaults to filling the width
-- it's given and sizing its height to one line of text plus chrome -- never
-- its own value's width, which would make the field resize as it's typed
-- into. Override with 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
textInput :: Ord e => e -> [Attribute (TextInputConfig e msg)] -> Element e msg
textInput eid attrs = Element
  { elLayout  = ticLayout cfg
  , elMeasure = measureChrome (ccStyleKey (ticControl cfg)) (lineHeightElement (ticValue cfg))
  , elRun     = do
      wasFocused <- isFocused eid
      let ctrl = (ticControl cfg)
            { ccContent   = body wasFocused
            , ccElementId = Just eid
            }
      void (control ctrl)
  }
  where
    cfg = resolve defaultTextInputConfig attrs

    -- A single line of the current value's text, for height purposes only
    -- ('elLayout' never asks for 'fitContent' width) -- falls back to a
    -- single space when empty so an untouched field doesn't collapse to
    -- zero height.
    lineHeightElement t = captionElement (if T.null t then " " else t)

    body wasFocused ci = do
      let currentValue = ticValue cfg
      s        <- currentStyle
      let hasFocus = ciFocused ci
      disabled <- isDisabled
      bounds   <- getBounds
      input    <- getInput
      sel      <- getSelection eid
      frac     <- getScrollState eid
      let gained      = ciFocusGained ci
          wasCapturing = ciWasDragging ci

      let displayValue = ticDisplayFilter cfg currentValue
          w           = rectWidth bounds
          selInit     = fromMaybe (cursor (T.length currentValue)) sel
          -- True on the one frame focus arrives, whether as an immediate
          -- same-frame claim (auto-claim, or Tab landing here) or a @Focus@
          -- effect applied between frames (a click on this control, or
          -- Shift-Tab) -- 'hasFocus'\/'wasFocused' alone catch the former;
          -- 'ciFocusGained' reports the latter on the frame it takes effect.
          justFocused = (hasFocus && not wasFocused) || gained
          canEdit     = hasFocus && not disabled

      contentW <- realToFrac <$> charOffset displayValue (T.length displayValue)
      let maxScrollPx = maxScrollPixels contentW w
          scrollX     = scrollPixels maxScrollPx frac

      selFinal <- resolveSelectionAndEdit cfg eid bounds canEdit
        (SelectionGesture wasCapturing justFocused) currentValue displayValue scrollX input selInit

      -- Computed locally rather than re-read via 'getScrollState': scroll
      -- writes are deferred (applied between frames), so a same-frame
      -- re-read would still see the pre-write value and the cursor would lag
      -- the auto-scroll by one frame.
      effectiveScrollX <-
        if canEdit
          then do
            curX <- charOffset displayValue (selectionActive selFinal)
            let newScrollX = resolveScroll w scrollX (realToFrac curX)
            when (newScrollX /= scrollX) $ requestScrollTo eid (scrollFraction maxScrollPx newScrollX)
            pure newScrollX
          else pure scrollX

      drawTextInputContent s bounds displayValue (ticPlaceholder cfg) canEdit effectiveScrollX selFinal
