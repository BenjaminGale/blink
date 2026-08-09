{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls

= Purpose

The standard widget library, built on top of "Blink.UI". Where "Blink.UI"
provides the render loop, input state, and low-level drawing primitives, and
"Blink.Layout" provides sizing and arrangement, this module fuses the two
with interaction (hover, focus, tab order, drag) and style-driven chrome
rendering to produce ready-to-use controls — buttons, checkboxes, sliders,
scrollable regions, and so on.

This module is organised in three tiers, each building on the last. Read
them in order:

  1. /Concepts/ (this section) — shared vocabulary used throughout the rest
     of the module.
  2. /Building blocks/ — the primitives every control below is assembled
     from. Reach for these when composing a custom control that isn't
     provided directly.
  3. /Controls/ — the ready-to-use widgets themselves, grouped by kind. Each
     one's Haddock notes which building blocks it's made of.

== Element ID hierarchy

Every control takes an element ID of type @e@, used as the key for styling
("Blink.Style"), interaction state (hover, focus, drag), and persisted
control state (scroll position, text selection). A simple screen might use
one sum-type constructor per control:

@
data Element = NameInput | SaveButton | VolumeSlider
  deriving (Eq, Ord, Show)
@

Composite controls — those made of several interactive parts, like
'scrollBar' (a track plus two buttons) or 'selector' (one entry per item) —
take a /tagging function/ that maps a part to its own element ID, rather
than a single ID. By convention this parameter is named @mkId@. This lets
each part be styled, hit-tested, and focused independently while keeping the
whole composite addressable through one sum-type branch of the caller's
element type:

@
data Element = ... | VScroll ScrollBarPart
              | ItemList Int

scrollBar VScroll Vertical 0.3          -- mkId = VScroll
selector  ItemList items selected onChange renderItem  -- mkId = ItemList
@

The element ID space therefore forms a tree that mirrors the widget tree:
each composite control's sub-parts (see 'ScrollBarPart', 'SliderPart',
'ViewportPart') nest inside the caller's own element type the same way
the widgets they identify nest inside the caller's UI tree. A 'viewport'
nested inside a custom panel, for example, produces IDs like
@MyPanel (ViewportV ScrollThumb)@.

== Chrome and the box model

/Chrome/ is the style-driven decoration around a control's content: margin,
border, background, and padding, applied in that order from the outside in,
the same as the CSS box model.

>  +----------------- margin -----------------+
>  |  +-------------- border --------------+  |
>  |  |  +---------- padding -----------+  |  |
>  |  |  |                              |  |  |
>  |  |  |           content            |  |  |
>  |  |  |                              |  |  |
>  |  |  +------------------------------+  |  |
>  |  +------------------------------------+  |
>  +------------------------------------------+

These insets and colours come from a control's 'StyleSet' (see
"Blink.Style"), which holds a normal and focused variant. 'renderChrome'
applies margin, background, and border, then runs its content action within
the padded content rectangle — this is what @content@ means for every
control below. 'measureChrome' returns the total width\/height overhead this
adds, for callers that need to size a control from the inside out.

== Activation pattern

Input controls treat "the user chose this" uniformly: a left-click, or one
of a designated set of keys pressed while the control holds keyboard focus —
provided the control isn't disabled. 'isActivatedBy' implements this test;
'activatable' pairs it with 'control' so a control's draw action and its
activation result are produced together. 'button' is the direct expression
of this pattern; 'checkboxMark' and 'selector''s per-item handling build on
it too.

== Value-callback pattern

Controls that edit application data receive the current value and a function
producing a state modifier from an updated value, dispatched whenever the
user makes a change:

@
textField NameInput (userName s) (\\t st -> st { userName = t })
@

The host applies the modifier once the frame completes; the control reads the
new value back from the application state on the next frame. This keeps all
application data outside the UI tree.

'button' is the deliberate exception — see its Haddock for why it returns a
'Bool' instead.

== Control state

Controls whose state is presentational rather than application data — a
scrollbar's position, for example — read and write it directly through the
primitives in "Blink.UI" ('getScrollState', 'setScrollState', and the
selection counterparts). State is keyed by element ID, populates lazily on
first write, and persists across frames inside the 'UIContext'. The application
never sees the traffic.
-}
module Blink.Controls
  ( -- * Building controls
    control
  , renderChrome
  , measureChrome
  , activatable
  , rangeControl
  , focusRing
  , isActivatedBy
  , isControlHit
  , whenFocused
  , isKeyPressed
    -- * Display
  , label
  , ProgressValue (..)
  , progressBar
    -- * Input
  , button
  , checkbox
  , checkboxMark
  , radioGroup
  , selector
  , textInputControl
  , textField
  , numberField
  , passwordField
    -- * Scroll
  , ScrollBarPart (..)
  , scrollBar
  , thumbRect
  , mouseToTrackPos
    -- * Viewport
  , ViewportPart (..)
  , viewport
  , scrollRegionBarSize
    -- * Slider
  , SliderPart (..)
  , slider
    -- * Virtualized content
  , virtualContent
  , ListBoxPart (..)
  , listBox
  ) where

import Control.Monad (when, forM_)
import Data.Char (isDigit)
import Data.List (foldl', find)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Blink.Geometry (Alignment (..), Insets (..), Orientation (..), Point (..), Rectangle (..), Size (..), borderInsets, insetRect, noBorder)
import Blink.Input (Key (..), KeyEvent (..), Modifier (..), InputState (..))
import Blink.Layout (Layout (..), Length (..), BoxConfig (..), hBox, vBox, defaultBoxConfig)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI

-- | Read-only text display. Renders @text@ within the element's content
-- rectangle using the active style. Does not participate in interaction or
-- keyboard navigation.
label :: Ord e
      => e     -- ^ element ID
      -> Text  -- ^ text to display
      -> UI e s ()
label eid text = renderChrome eid $ do
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
progressBar :: Ord e
            => e             -- ^ element ID
            -> ProgressValue -- ^ determinate or indeterminate value
            -> UI e s ()
progressBar eid (Progress value) = renderChrome eid $ do
  style <- getStyle eid
  r     <- getBounds
  let clamped  = max 0 (min 1 value)
      fillRect' = r { rectWidth = rectWidth r * clamped }
  withBounds fillRect' $ fillRect (styleTextColour style)
progressBar eid Indeterminate = do
  requiresAnimation
  renderChrome eid $ do
    r       <- getBounds
    style   <- getStyle eid
    elapsed <- getAnimElapsed
    -- Band width (0.3) and speed (0.5) are fixed; expose as parameters if
    -- callers need to distinguish multiple simultaneous indeterminate bars.
    let t     = realToFrac elapsed * (0.5 :: Double)
        phase = t - fromIntegral (floor t :: Int)
        bandW = rectWidth r * 0.3
        left  = rectX r - bandW + (rectWidth r + bandW) * phase
    withBounds (r { rectX = left, rectWidth = bandW }) $
      fillRect (styleTextColour style)

-- | The interactive checkmark box on its own, with no adjacent label. An
-- 'activatable' control (click, Enter, or Space) that draws a checkmark when
-- @checked@. Exported separately from 'checkbox' so callers that need
-- non-standard label placement can still get the standard toggle behaviour.
checkboxMark :: Ord e
             => e                -- ^ element ID
             -> Bool             -- ^ current checked state
             -> (Bool -> s -> s) -- ^ state modifier given the new checked state
             -> UI e s ()
checkboxMark boxId checked onToggle = do
  activated <- activatable boxId draw [KeyReturn, KeySpace]
  when activated $ dispatch (onToggle (not checked))
  where
    draw = do
      style <- getStyle boxId
      when checked $ drawText (styleTextColour style) AlignCenter "✓"

-- | Draws left-aligned label text in the given style. A small helper shared
-- by 'checkbox' so its label rendering is a one-line call.
checkboxLabel
  :: Style  -- ^ style to draw with
  -> Text   -- ^ label text
  -> UI e s ()
checkboxLabel style = drawText (styleTextColour style) AlignLeft

-- | A togglable checkbox with an adjacent label. Dispatches the state modifier
-- @onToggle (not checked)@ when activated by a click or the Enter key.
-- Composed from 'checkboxMark' (the interactive box) laid out beside
-- 'checkboxLabel' (plain text) via 'hBox', with 'focusRing' drawn around the
-- pair since neither half alone spans the whole composite.
--
-- @
-- checkbox NotifyMe "Notify me by email" (notifyMe s) $ \\v st -> st { notifyMe = v }
-- @
checkbox :: Ord e
         => e                -- ^ element ID
         -> Text             -- ^ label text
         -> Bool             -- ^ current checked state
         -> (Bool -> s -> s) -- ^ state modifier given the new checked state
         -> UI e s ()
checkbox boxId text checked onToggle = do
  style <- getStyle boxId
  hBox (defaultBoxConfig { boxSpacing = 4, boxFillCross = False })
    [ (Layout (Exactly 20) (Exactly 20) MiddleLeft, checkboxMark boxId checked onToggle)
    , (Layout Fill Fill MiddleLeft, checkboxLabel style text)
    ]
  focusRing boxId

-- | A vertical list of items, each activated by click, Enter, or Space, with
-- arrow-key navigation between items and one item designated as selected.
-- Dispatches @onChange value@ when a different item is activated.
-- @renderItem eid isSelected item@ draws each item's content; @selector@
-- itself owns the selection comparison, activation, and Up\/Down navigation.
-- Multiple selectors on screen each bind to their own application-state
-- field; no shared state is required. See 'radioGroup' for the radio-mark
-- rendering built on top of this. Each item is a 'control' internally, so
-- it gets its own hover, focus, and tab-stop; 'selector' layers Up\/Down
-- navigation and the shared selection value on top.
--
-- @
-- data Element = ... | SizeItem Int
--
-- selector SizeItem [(Small, "Small"), (Medium, "Medium"), (Large, "Large")]
--   (size s) (\\v st -> st { size = v }) $ \\eid isSelected (_, lbl) -> do
--     style <- getStyle eid
--     drawText (styleTextColour style) AlignLeft (if isSelected then "> " <> lbl else lbl)
-- @
selector :: (Eq e, Ord e, Eq a)
         => (Int -> e)                          -- ^ maps item index to an element ID
         -> [(a, Text)]                         -- ^ @(value, label)@ pairs
         -> a                                   -- ^ currently selected value
         -> (a -> s -> s)
         -> (e -> Bool -> (a, Text) -> UI e s ()) -- ^ @eid isSelected item@
         -> UI e s ()
selector mkId items selected onChange renderItem = do
  initialFocus <- getFocus
  vBox defaultBoxConfig (zipWith (mkItem initialFocus) [0..] items)
  where
    lastIdx = length items - 1
    mkItem initialFocus idx item@(val, _) =
      let eid = mkId idx
      in ( Layout Fill Fill TopLeft
         , control eid $ do
             clicked      <- isClicked eid
             keyActivated <- or <$> mapM (isKeyPressed eid) [KeyReturn, KeySpace]
             disabled     <- isDisabled
             let activated = not disabled && (clicked || keyActivated)
             renderItem eid (selected == val) item
             when activated $ dispatch (onChange val)
             -- Use initialFocus (captured before any item renders) to prevent
             -- cascade: an item that gained focus via setFocus earlier in this
             -- same vBox pass must not also fire navigation. Allow same-frame
             -- clicks as an additional trigger since initialFocus predates applyFocus.
             whenEnabled $ when (initialFocus == Just eid || clicked) $ do
               upPressed   <- isKeyPressed eid KeyUp
               downPressed <- isKeyPressed eid KeyDown
               when upPressed   $ setFocus (mkId (max 0 (idx - 1)))
               when downPressed $ setFocus (mkId (min lastIdx (idx + 1)))
         )

-- | A group of mutually exclusive options, rendered as a radio mark and
-- label per item. A thin wrapper over 'selector' supplying the radio-mark
-- rendering; see 'selector' for the selection, activation, and navigation
-- behaviour.
radioGroup :: (Eq e, Ord e, Eq a)
           => (Int -> e)     -- ^ maps item index to an element ID
           -> [(a, Text)]    -- ^ @(value, label)@ pairs
           -> a              -- ^ currently selected value
           -> (a -> s -> s)
           -> UI e s ()
radioGroup mkId items selected onChange =
  selector mkId items selected onChange $ \eid isSelected (_, lbl) -> do
    style <- getStyle eid
    drawText (styleTextColour style) AlignLeft $
      (if isSelected then "● " else "○ ") <> lbl

-- | A clickable button labelled @txt@. Returns 'True' on the frame the button
-- is activated — by a left-click or by pressing Enter while focused.
--
-- Unlike the other input controls, @button@ returns a 'Bool' rather than
-- accepting a value-callback. This is intentional: button clicks often trigger
-- UI actions ('setFocus', opening a dialog) that cannot be expressed as a pure
-- @s -> s@ state modifier, so the caller dispatches the response directly:
--
-- @
-- clicked <- button eid "Save"
-- when clicked $ do
--   dispatch (\\s -> s { dirty = False })
--   setFocus ConfirmDialog
-- @
button :: Ord e
       => e     -- ^ element ID
       -> Text  -- ^ button label
       -> UI e s Bool
button eid txt = activatable eid draw [KeyReturn]
  where
    draw = do
      style <- getStyle eid
      drawText (styleTextColour style) (styleTextAlign style) txt

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
  -> UI e s (Int, Int)
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

-- | Backspace and typed text edit the value, selection-aware; dispatches
-- @onChange@ when the text actually changes. @inputFilter@ is applied to the
-- newly typed text before insertion, letting callers reject or transform
-- keystrokes (e.g. digits only). Assumes the caller has already checked the
-- control is focused and enabled.
applyEdit :: Ord e
          => (Text -> Text)   -- ^ @inputFilter@, applied to newly typed text before insertion
          -> (Text -> s -> s) -- ^ @onChange@, state modifier given the new value
          -> Text             -- ^ current value
          -> InputState       -- ^ this frame's input
          -> (Int, Int)       -- ^ current @(anchor, active)@ selection
          -> UI e s (Int, Int)
applyEdit inputFilter onChange value input (anchor2, active2)
  | backspace || hasTyped = do
      when (newText /= value) $ dispatch (onChange newText)
      pure (newCursor, newCursor)
  | otherwise = pure (anchor2, active2)
  where
    keyEvts   = inputKeyEvents input
    backspace = any (\e -> key e == KeyBackspace) keyEvts
    typed     = inputFilter (foldl' (<>) T.empty (inputTypedText input))
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
-- a viewport of width @w@ currently scrolled to @scrollX@.
resolveScroll
  :: Double  -- ^ viewport width
  -> Double  -- ^ current scroll offset
  -> Double  -- ^ cursor position to keep visible
  -> Double
resolveScroll w scrollX cursorAbs
  | cursorAbs < scrollX         = cursorAbs
  | cursorAbs > scrollX + w - 1 = max 0 (cursorAbs - w + 1)
  | otherwise                   = scrollX

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
  -> UI e s ()
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

-- | A single-line text entry field, and the base every other text-entry
-- control ('textField', 'numberField', 'passwordField') is built on.
-- Supports click-to-place cursor, drag selection, Shift+arrow extension, and
-- selection-aware editing. Long text scrolls horizontally to keep the cursor
-- visible.
--
-- @inputFilter@ is applied to newly typed text before it's inserted, letting
-- callers restrict which keystrokes are accepted (e.g. digits only).
-- Reformatting the value itself (e.g. inserting punctuation as the user
-- types) is application concern, not this control's — do it in @onChange@
-- and pass the already-formatted value back in on the next frame, same as
-- any other value-callback control.
--
-- @displayFilter@ is applied to the value everywhere it is measured or
-- drawn — the rendered text, and every character-offset calculation used for
-- cursor placement, click hit-testing, and auto-scroll — so what's on screen
-- and where the cursor lands always agree. It must be length- and
-- position-preserving (e.g. masking each character of a password with @•@);
-- the underlying value edited by @inputFilter@\/@onChange@ is never affected
-- by it.
--
-- Cursor position and selection are control state (see "Blink.Style" and the
-- Concepts section above), not application data — 'textInputControl' reads
-- and writes them itself via 'getSelection'\/'setSelection' and
-- 'getScrollState'\/'setScrollState', keyed by @eid@.
textInputControl :: Ord e
                  => (Text -> Text)   -- ^ @inputFilter@, applied to newly typed text before insertion
                  -> (Text -> Text)   -- ^ @displayFilter@, applied wherever the value is measured or drawn
                  -> e                -- ^ element ID
                  -> Text             -- ^ current value
                  -> (Text -> s -> s) -- ^ @onChange@, state modifier given the new value
                  -> UI e s ()
textInputControl inputFilter displayFilter eid value onChange = do
  wasFocused   <- isFocused eid
  wasCapturing <- isDragging eid
  control eid $ do
    style    <- getStyle eid
    hasFocus <- isFocused eid
    disabled <- isDisabled
    bounds   <- getBounds
    input    <- getInput
    sel      <- getSelection eid
    scrollX  <- getScrollState eid

    let displayValue = displayFilter value
        w           = rectWidth bounds
        defPos      = T.length value
        anchor0     = maybe defPos selectionAnchor sel
        active0     = maybe defPos selectionActive sel
        -- Focus was gained by a click this frame (e.g. clicking from another
        -- element). Treat as a fresh click rather than a drag continuation so
        -- the old anchor is not inherited.
        justFocused = hasFocus && not wasFocused
        enabled     = hasFocus && not disabled

    (anchor1, active1) <-
      if enabled
        then resolveMouseSelection eid bounds wasCapturing justFocused displayValue scrollX (anchor0, active0)
        else pure (anchor0, active0)

    let (anchor2, active2) =
          resolveKeyboardSelection hasFocus (inputKeyEvents input) (T.length value) (anchor1, active1)

    (anchor3, active3) <-
      if enabled
        then applyEdit inputFilter onChange value input (anchor2, active2)
        else pure (anchor2, active2)

    when enabled $ setSelection eid (Selection anchor3 active3)

    when enabled $ do
      curX <- charOffset displayValue active3
      let newScrollX = resolveScroll w scrollX (realToFrac curX)
      when (newScrollX /= scrollX) $ setScrollState eid newScrollX

    scrollX' <- getScrollState eid
    drawTextInputContent style bounds displayValue hasFocus enabled scrollX' (anchor3, active3)

-- | A plain single-line text entry field, with no keystroke filtering or
-- display masking.
--
-- @
-- textField NameInput (userName s) (\\t st -> st { userName = t })
-- @
textField :: Ord e
          => e                -- ^ element ID
          -> Text             -- ^ current value
          -> (Text -> s -> s) -- ^ state modifier given the new value
          -> UI e s ()
textField = textInputControl id id

-- | A text field that only accepts digit keystrokes; all other typed
-- characters are silently dropped. Built on 'textInputControl'.
numberField :: Ord e
            => e                -- ^ element ID
            -> Text             -- ^ current value
            -> (Text -> s -> s) -- ^ state modifier given the new value
            -> UI e s ()
numberField = textInputControl (T.filter isDigit) id

-- | A text field that masks its displayed value with @•@, one per character,
-- while editing the real underlying text as normal. Built on
-- 'textInputControl'.
passwordField :: Ord e
              => e                -- ^ element ID
              -> Text             -- ^ current value
              -> (Text -> s -> s) -- ^ state modifier given the new value
              -> UI e s ()
passwordField = textInputControl id (T.map (const '•'))

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

contentRectFor :: StyleSet -> Rectangle -> Rectangle
contentRectFor ss r =
  let s = styleSetNormal ss
  in insetRect (stylePadding s) (insetRect (styleMargin s) r)

-- | The content rectangle of a track-style element (its slot bounds inset by
-- margin and padding), used by both 'scrollBar' and 'slider' to size and
-- place their thumb.
trackContentRect :: Ord e
                  => e  -- ^ track's element ID
                  -> UI e s Rectangle
trackContentRect trackId = do
  bounds   <- getBounds
  styleSet <- getStyleSet trackId
  pure (contentRectFor styleSet bounds)

-- | While @trackId@ is being dragged with the button held, returns the track
-- position under the cursor; 'Nothing' otherwise. Shared drag-handling for
-- 'scrollBar' and 'slider', both of which map a thumb drag to a position via
-- 'mouseToTrackPos'.
dragToTrackPos :: Ord e
               => e            -- ^ track's element ID
               -> Orientation  -- ^ track orientation
               -> Double       -- ^ thumb ratio (visible / total), in @[0, 1]@
               -> Rectangle    -- ^ track's content rectangle
               -> UI e s (Maybe Double)
dragToTrackPos trackId ori ratio contentRect = do
  dragging <- isDragging trackId
  btnDown  <- isButtonDown
  if dragging && btnDown
    then Just . mouseToTrackPos ori ratio contentRect <$> getMousePos
    else pure Nothing

-- | A track with a draggable thumb, positioned within @[0, 1]@ by @pos@ and
-- sized within the track by @ratio@ (visible \/ total, also @[0, 1]@).
-- @trackId@ is the interactive element — it receives chrome, hover, focus,
-- and tab navigation via 'control' — and @thumbId@ is a purely decorative
-- child positioned inside it. Returns the new position while @trackId@ is
-- being dragged, 'Nothing' otherwise; the caller decides how to store or
-- dispatch it. Shared by 'scrollBar' and 'slider'.
rangeControl :: Ord e
             => e            -- ^ track's element ID
             -> e            -- ^ thumb's element ID
             -> Orientation  -- ^ track orientation
             -> Double       -- ^ thumb position within the track, in @[0, 1]@
             -> Double       -- ^ thumb ratio (visible / total), in @[0, 1]@
             -> UI e s (Maybe Double)
rangeControl trackId thumbId ori pos ratio = do
  contentRect <- trackContentRect trackId
  let thumbR = thumbRect ori pos ratio contentRect
  control trackId $
    withBounds thumbR $ renderChrome thumbId $ pure ()
  dragToTrackPos trackId ori ratio contentRect

-- | A scrollbar with decrement\/increment buttons flanking a draggable thumb.
-- The scroll position in @[0, 1]@ is stored in the 'UIContext', keyed by
-- @mkId ScrollTrack@; the control reads and writes it itself. @thumbRatio@ is
-- the fraction of the track the thumb fills (visible \/ total), also in
-- @[0, 1]@. Button clicks step by @thumbRatio@; dragging centres the thumb on
-- the cursor.
scrollBar :: Ord e
          => (ScrollBarPart -> e)  -- ^ maps scrollbar parts to element IDs
          -> Orientation           -- ^ scrollbar orientation
          -> Double                -- ^ thumb ratio (visible / total), in @[0, 1]@
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
      newPos <- rangeControl trackId (mkId ScrollThumb) ori pos' ratio'
      forM_ newPos writePos

-- | Computes the bounding rectangle of a thumb within a track. @pos@ is the
-- position along the track and @ratio@ is the fraction of the track the thumb
-- fills (visible \/ total); both are in @[0, 1]@. The result is a
-- sub-rectangle of @r@.
thumbRect
  :: Orientation  -- ^ track orientation
  -> Double       -- ^ thumb position along the track, in @[0, 1]@
  -> Double       -- ^ thumb ratio (visible / total), in @[0, 1]@
  -> Rectangle    -- ^ track rectangle to place the thumb within
  -> Rectangle
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
mouseToTrackPos
  :: Orientation  -- ^ track orientation
  -> Double       -- ^ thumb ratio (visible / total), in @[0, 1]@
  -> Rectangle    -- ^ track rectangle
  -> Point        -- ^ mouse position
  -> Double
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

-- | Sub-parts of a viewport's element ID hierarchy. Wraps 'ScrollBarPart'
-- for the horizontal and vertical scrollbars:
--
-- @
-- data Element = ... | MyRegion ViewportPart
-- viewport MyRegion (Size 600 400) content
-- @
data ViewportPart
  = ViewportH ScrollBarPart -- ^ A part of the horizontal scrollbar.
  | ViewportV ScrollBarPart -- ^ A part of the vertical scrollbar.
  deriving (Eq, Ord, Show)

-- | The pixel width of a scrollbar strip used by 'viewport'. Exported so
-- callers that compose a viewport inside their own layout can account for
-- the strip in their geometry without hard-coding the value.
scrollRegionBarSize :: Double
scrollRegionBarSize = 16

-- | A scrollable window onto a fixed-size virtual content area. Scrollbars
-- appear automatically on axes where the content exceeds the viewport, and
-- are drawn first so that a drag or click this frame is reflected
-- immediately. The content action then runs with virtual bounds — the full
-- content rectangle translated so the scrolled portion aligns with the
-- viewport — clipped to the visible area. Mouse interaction works naturally
-- because translated bounds are in window coordinates; the clip region
-- hides the rest.
--
-- For large, uniform item collections where building the whole content
-- every frame would be wasteful, or where scrolling needs to be driven by
-- keyboard selection, use 'listBox' instead — it manages its own scroll
-- state directly rather than wrapping content in a viewport.
viewport
  :: Ord e
  => (ViewportPart -> e)  -- ^ maps viewport parts to element IDs
  -> Size                  -- ^ virtual content size
  -> UI e s ()             -- ^ content
  -> UI e s ()
viewport mkId (Size cw ch) content = do
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
      hThumb  = if needsH then Just (max 0 (min 1 (vpW / cw))) else Nothing
      vThumb  = if needsV then Just (max 0 (min 1 (vpH / ch))) else Nothing
      vpRect  = outer { rectWidth = vpW, rectHeight = vpH }
      hBar    = outer { rectY = rectY outer + vpH, rectHeight = scrollRegionBarSize, rectWidth = vpW }
      vBar    = outer { rectX = rectX outer + vpW, rectWidth  = scrollRegionBarSize, rectHeight = vpH }
  case hThumb of
    Nothing -> pure ()
    Just r  -> withBounds hBar $ scrollBar (mkId . ViewportH) Horizontal r
  case vThumb of
    Nothing -> pure ()
    Just r  -> withBounds vBar $ scrollBar (mkId . ViewportV) Vertical r
  hPos <- maybe (pure 0) (\_ -> getScrollState (mkId (ViewportH ScrollTrack))) hThumb
  vPos <- maybe (pure 0) (\_ -> getScrollState (mkId (ViewportV ScrollTrack))) vThumb
  let offsetX    = hPos * max 0 (cw - vpW)
      offsetY    = vPos * max 0 (ch - vpH)
      virtBounds = outer
        { rectX      = rectX outer - offsetX
        , rectY      = rectY outer - offsetY
        , rectWidth  = cw
        , rectHeight = ch
        }
  withBounds vpRect $ clipToCurrent $ withBounds virtBounds content

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
slider :: Ord e
       => (SliderPart -> e)   -- ^ maps slider parts to element IDs
       -> Orientation         -- ^ slider orientation
       -> Double              -- ^ current value, in @[0, 1]@
       -> (Double -> s -> s)  -- ^ state modifier given the new value
       -> UI e s ()
slider mkId ori value onChange = do
  let trackId = mkId SliderTrack
      clamped = max 0 (min 1 value)
  contentRect <- trackContentRect trackId
  let (crossSz, mainSz) = case ori of
        Horizontal -> (rectHeight contentRect, rectWidth contentRect)
        Vertical   -> (rectWidth contentRect,  rectHeight contentRect)
      thumbRatio  = if mainSz > 0 then crossSz / mainSz else 0
  newPos <- rangeControl trackId (mkId SliderThumb) ori clamped thumbRatio
  forM_ newPos $ \p -> dispatch (onChange p)
  let step = 0.05
      (decrKey, incrKey) = case ori of
        Horizontal -> (KeyLeft,  KeyRight)
        Vertical   -> (KeyUp,    KeyDown)
  disabled  <- isDisabled
  decrKeyed <- isKeyPressed trackId decrKey
  incrKeyed <- isKeyPressed trackId incrKey
  let decrPressed = not disabled && decrKeyed
      incrPressed = not disabled && incrKeyed
  when decrPressed $ dispatch (onChange (max 0 (clamped - step)))
  when incrPressed $ dispatch (onChange (min 1 (clamped + step)))

-- | Renders a windowed slice of a uniform-height item list: given the current
-- scroll position (in pixels), the height of one item, and the total item
-- count, calls @renderItem i@ only for the items that intersect the current
-- bounds, clipped to those bounds. The first and\/or last rendered item may
-- be partially clipped if the scroll position doesn't land on an item
-- boundary — this is correct: no virtual canvas or coordinate translation is
-- needed, since only the content that will actually be visible is ever
-- generated, at the right position.
--
-- Does not draw a scrollbar or manage scroll state itself — pair with
-- 'scrollBar' and 'getScrollState'\/'setScrollState' (see 'listBox').
virtualContent
  :: Double              -- ^ current scroll position, in pixels
  -> Double              -- ^ height of one item, in pixels
  -> Int                 -- ^ total item count
  -> (Int -> UI e s ())  -- ^ renders the item at the given index
  -> UI e s ()
virtualContent scrollPos itemHeight itemCount renderItem = do
  vp <- getBounds
  let firstIdx = floor (scrollPos / itemHeight) :: Int
      subOffset = scrollPos - fromIntegral firstIdx * itemHeight
      visibleN = ceiling ((rectHeight vp + subOffset) / itemHeight) :: Int
  clipToCurrent $ forM_ [0 .. visibleN - 1] $ \j ->
    let i       = firstIdx + j
        itemRect = vp { rectY = rectY vp + fromIntegral j * itemHeight - subOffset, rectHeight = itemHeight }
    in when (i >= 0 && i < itemCount) $ withBounds itemRect (renderItem i)

-- | Sub-parts of a 'listBox': individual items, and the scrollbar's own
-- parts, tagged together so both can be addressed through one composite
-- element ID.
data ListBoxPart
  = ListBoxItem Int
  | ListBoxScroll ScrollBarPart
  deriving (Eq, Ord, Show)

-- | A vertically scrolling list of items, one selected, with keyboard
-- navigation between them — the composite of 'selector'-style navigation,
-- 'scrollBar', and 'virtualContent', wired together so that only the
-- currently-visible items are ever rendered. Moving the current item off
-- the visible window with the arrow keys scrolls it back into view.
--
-- As with 'selector', the /current/ item (keyboard focus, tracked here) is
-- distinct from the /selected/ item (application state, via @selected@\/
-- @onChange@): arrow keys move the current item without dispatching; Enter,
-- Space, or a click activates it and dispatches @onChange@.
listBox :: (Eq e, Ord e, Eq a)
        => (ListBoxPart -> e)                     -- ^ maps list-box parts to element IDs
        -> Double                                  -- ^ item height, in pixels
        -> [(a, Text)]                             -- ^ @(value, label)@ pairs
        -> a                                       -- ^ currently selected value
        -> (a -> s -> s)
        -> (e -> Bool -> (a, Text) -> UI e s ())  -- ^ @eid isSelected item@
        -> UI e s ()
listBox mkId itemHeight items selected onChange renderItem = do
  initialFocus <- getFocus
  hBox defaultBoxConfig
    [ (Layout Fill Fill TopLeft, itemsArea initialFocus)
    , (Layout (Exactly scrollRegionBarSize) Fill TopLeft, scrollBarArea)
    ]
  where
    itemCount = length items
    lastIdx   = itemCount - 1
    trackId   = mkId (ListBoxScroll ScrollTrack)
    contentH  = fromIntegral itemCount * itemHeight

    -- scrollBar persists position as a [0, 1] fraction (same convention as
    -- every other scroll-state consumer in this module); virtualContent and
    -- the scroll-to-current math below both work in pixels, so the fraction
    -- is converted on the way in and out.
    itemsArea initialFocus = do
      vp <- getBounds
      let vpH       = rectHeight vp
          maxScroll = max 0 (contentH - vpH)
      frac <- getScrollState trackId
      let scrollPx = frac * maxScroll
      virtualContent scrollPx itemHeight itemCount $ \idx ->
        mkItem initialFocus vpH maxScroll scrollPx idx

    mkItem initialFocus vpH maxScroll scrollPx idx =
      let item@(val, _) = items !! idx
          eid            = mkId (ListBoxItem idx)
      in control eid $ do
           clicked      <- isClicked eid
           keyActivated <- or <$> mapM (isKeyPressed eid) [KeyReturn, KeySpace]
           disabled     <- isDisabled
           let activated = not disabled && (clicked || keyActivated)
           renderItem eid (selected == val) item
           when activated $ dispatch (onChange val)
           -- Same same-frame-cascade guard as 'selector': only the item that
           -- already held focus before this frame (or was just clicked) may
           -- move focus.
           whenEnabled $ when (initialFocus == Just eid || clicked) $ do
             upPressed   <- isKeyPressed eid KeyUp
             downPressed <- isKeyPressed eid KeyDown
             when upPressed   $ scrollToCurrent vpH maxScroll scrollPx (max 0 (idx - 1))
             when downPressed $ scrollToCurrent vpH maxScroll scrollPx (min lastIdx (idx + 1))

    scrollToCurrent vpH maxScroll scrollPx newIdx = do
      setFocus (mkId (ListBoxItem newIdx))
      let itemTop    = fromIntegral newIdx * itemHeight
          itemBottom = itemTop + itemHeight
          newScrollPx
            | itemTop < scrollPx          = itemTop
            | itemBottom > scrollPx + vpH = itemBottom - vpH
            | otherwise                   = scrollPx
          clampedPx = max 0 (min maxScroll newScrollPx)
          newFrac   = if maxScroll > 0 then clampedPx / maxScroll else 0
      when (clampedPx /= scrollPx) $ setScrollState trackId newFrac

    scrollBarArea = do
      vp <- getBounds
      let vpH        = rectHeight vp
          thumbRatio = if contentH > 0 then max 0 (min 1 (vpH / contentH)) else 1
      scrollBar (mkId . ListBoxScroll) Vertical thumbRatio

-- | Returns the @(width, height)@ overhead consumed by a control's margin and
-- padding as 'Length' constraints. Add these to a content size with 'addLength'
-- to get the total size needed for the control to display without clipping.
measureChrome :: Ord e
              => e  -- ^ element ID
              -> UI e s (Length, Length)
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

-- | Style-aware rendering for a control. Applies the element's margin, draws
-- its background and border, and runs @content@ within the padded content
-- rectangle. Does not perform hover detection, focus management, or tab
-- navigation — use this for display-only elements that should not participate
-- in interaction. See 'control' for the interactive counterpart.
renderChrome :: Ord e
             => e          -- ^ element ID
             -> UI e s ()  -- ^ content, run within the padded content rectangle
             -> UI e s ()
renderChrome eid content = do
  style <- getStyle eid
  r     <- getBounds
  let bgRect      = insetRect (styleMargin style) r
      borderRect  = case styleBorderColour style of
                      Just _  -> insetRect (borderInsets (styleBorderEdges style)) bgRect
                      Nothing -> bgRect
      contentRect = insetRect (stylePadding style) borderRect
      inner       = withBounds contentRect $ clipToCurrent content
  withBounds bgRect $
    withBackground (styleBackground style) $
    case styleBorderColour style of
      Just c  -> withBorder c (styleBorderEdges style) inner
      Nothing -> inner

-- | The standard entry point for interactive controls. Applies hover detection,
-- focus management, and Tab\/Shift-Tab navigation, then delegates to
-- 'renderChrome' for style-aware rendering. @content@ runs inside the padded
-- content rectangle.
--
-- @
-- control eid $ do
--   style <- getStyle eid
--   drawText (styleTextColour style) (styleTextAlign style) label
-- @
control :: Ord e
        => e          -- ^ element ID
        -> UI e s ()  -- ^ content, run within the padded content rectangle
        -> UI e s ()
control eid content = do
  applyHover eid
  applyFocus eid
  applyTabNavigation eid
  renderChrome eid content

-- | 'True' when the mouse is over the element's background rectangle (bounds
-- inset by its margin) — the control-specific hit area. Built on 'isRegionHit';
-- use this rather than reimplementing the margin geometry in custom controls.
isControlHit :: Ord e
             => e  -- ^ element ID
             -> UI e s Bool
isControlHit eid = do
  s <- getStyle eid
  r <- getBounds
  withBounds (insetRect (styleMargin s) r) isRegionHit

applyHover :: Ord e => e -> UI e s ()
applyHover eid = do
  whenEnabled $ do
    free     <- isMouseFree
    dragging <- isDragging eid
    when (free || dragging) $ do
      isHit <- isControlHit eid
      when isHit $ setHovered eid

applyFocus :: Ord e => e -> UI e s ()
applyFocus eid = do
  whenEnabled $ do
    currentFocus <- getFocus
    isHit        <- isHovered eid
    released     <- isButtonReleased
    captured     <- getCapturedElement
    let nothingIsFocused = isNothing currentFocus
        isRetainingFocus = currentFocus == Just eid
        -- A drag release is when the button is released over a different
        -- element than the one that was captured. Focus should not transfer
        -- in that case — the drag origin retains focus.
        isDragRelease = released && isJust captured && captured /= Just eid
        wasClicked    = isHit && released && not isDragRelease
    setFocusWhen (isRetainingFocus || ((nothingIsFocused || wasClicked) && not isDragRelease)) eid

applyTabNavigation :: Ord e => e -> UI e s ()
applyTabNavigation eid = whenEnabled $ do
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
  setPreviousTabStop eid

-- | 'True' when the element is clicked or any of the given keys are pressed
-- while it is focused, and the element is not disabled. Use this to implement
-- the activation behaviour of interactive controls.
isActivatedBy :: Ord e
              => e      -- ^ element ID
              -> [Key]  -- ^ keys that also activate, in addition to a click
              -> UI e s Bool
isActivatedBy eid keys = do
  clicked  <- isClicked eid
  keyPress <- or <$> mapM (isKeyPressed eid) keys
  disabled <- isDisabled
  return (not disabled && (clicked || keyPress))

-- | 'control' plus 'isActivatedBy': runs @draw@ as a normal interactive
-- control, then reports whether it was activated (a click or one of @keys@)
-- this frame. The shape shared by simple activatable controls — a button, a
-- checkbox's mark — whose draw action does not itself depend on whether
-- activation occurred. This is 'button' with a fixed set of keys and a
-- fixed draw action; most custom activatable controls are exactly this
-- shape with the specifics filled in:
--
-- @
-- starRating :: Ord e => e -> Bool -> UI e s Bool
-- starRating eid lit = activatable eid draw [KeyReturn, KeySpace]
--   where
--     draw = do
--       style <- getStyle eid
--       drawText (styleTextColour style) AlignCenter (if lit then "\9733" else "\9734")
-- @
activatable :: Ord e
            => e          -- ^ element ID
            -> UI e s ()  -- ^ draw action
            -> [Key]      -- ^ keys that also activate, in addition to a click
            -> UI e s Bool
activatable eid draw keys = do
  control eid draw
  isActivatedBy eid keys

-- | Runs an action only when the given element holds keyboard focus.
whenFocused :: Eq e
            => e          -- ^ element ID
            -> UI e s ()  -- ^ action to run when focused
            -> UI e s ()
whenFocused eid action = isFocused eid >>= \f -> when f action

-- | Draws @eid@'s focused-style border around the current bounds when @eid@
-- holds focus, and nothing otherwise. Composite controls made of several
-- non-'control' pieces (a checkbox's mark and label, say) have no single
-- element whose own chrome can show a focus indicator spanning the whole
-- composite; call this after laying out the composite's children, while the
-- current bounds are still the composite's own outer bounds.
focusRing :: Ord e
          => e  -- ^ element ID
          -> UI e s ()
focusRing eid = whenFocused eid $ do
  styleSet <- getStyleSet eid
  let s = styleSetFocused styleSet
  case styleBorderColour s of
    Just c  -> strokeRect c (styleBorderEdges s)
    Nothing -> pure ()

-- | 'True' when the element holds focus and a key event for @k@ is present
-- in the current frame's input queue.
isKeyPressed :: Eq e
             => e    -- ^ element ID
             -> Key  -- ^ key to test for
             -> UI e s Bool
isKeyPressed eid k = do
  hasFoc  <- isFocused eid
  pressed <- any (\e -> key e == k) . inputKeyEvents <$> getInput
  return (hasFoc && pressed)
