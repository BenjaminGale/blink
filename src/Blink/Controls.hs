{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Module: Blink.Controls

Ready-made interactive controls — 'button', 'checkbox', 'textInputControl',
'slider', 'scrollBar', 'viewport' — and the 'label'\/'progressBar'
display-only ones, all built from the primitives in "Blink.UI".

= Element IDs

Every control call starts with an element ID — a value of your application's
own @e@ type (see /Element identity/ in "Blink.UI") that identifies it for
styling, focus, and hover tracking:

@
button OkButton [text "OK", onClick (post Confirmed)]
@

A control made of a single piece, like 'button', takes one @e@ value
directly. A control made of several visually or interactively distinct
pieces — 'checkbox' (its glyph vs. its label), 'slider'\/'scrollBar' (track
vs. thumb), 'viewport' (its scroll bars vs. its content) — instead takes a
/tagging function/: a function from a small "part" type to @e@, so you supply
one @e@-shaped constructor and the control derives a distinct, stylable ID
for each of its pieces from it:

@
data Elem = NotifyMe CheckboxPart
  deriving (Eq, Ord)

checkbox NotifyMe [checked (notifyMe model), text "Notify me by email", onToggle NotifyMeChanged]
@

= Attributes

Every control value — text, checked state, current value, orientation,
selection, and so on — is set via a list of attributes passed after the ID,
not positional arguments:

@
checkbox NotifyMe [checked (notifyMe model), text "Notify me by email", onToggle NotifyMeChanged]
@

Every attribute has a sensible default, so nothing is mandatory beyond the
element ID (or tagging function) itself, and later entries in the list
override earlier ones that set the same thing — so a caller can always
override a control's default. Attributes come in three flavours:

  * Value attributes like 'text', 'checked', and 'value' set one field of
    the control's own configuration — what it displays or holds.
  * Reaction attributes like 'onClick', 'onToggle', and 'onChange' (see
    /Reactions/ below) say what happens when the control's state changes.
  * 'tabStop' \/ 'focusOnClick' configure focus behaviour shared by every
    control (see /Focus and tab order/ below).

Under the hood, each attribute list carries a hidden type, 'Attr'; value and
reaction attributes are both built from a pair of general-purpose smart
constructors, 'configAny' and 'onEvent'. Reach for those directly only when
assembling a new control of your own — see /Building composites/ below.

= Reactions

A handler built with 'onEvent' — or one of the per-control @onX@ helpers —
runs a /reaction/: a function from whatever data the triggering event
carried to @['Out' e msg]@, the same currency 'fire' dispatches through.
Four small combinators build one without needing to know that:

  * 'post' \/ 'postWith' emit a message, ignoring or using the event's data.
  * 'perform' \/ 'performWith' queue a 'UiEffect' instead, for controls (like
    'scrollBar') that write their own presentation state directly to the
    'UIContext' rather than routing it through the app's model.
  * 'forward' \/ 'translate' \/ 'translateWith' re-raise an event against a
    /different/ attrs list — how a composite (like 'scrollBar', built on
    'slider') mirrors an inner control's lifecycle events as its own.

Reactions are just functions into a list, so combining more than one is
ordinary 'Monoid' composition:

@
onClick (post SavedClicked \<\> perform (ScrollTo ResultsList 0))
@

= Lifecycle events

Every control reports focus and hover changes the same way, via
'ControlEvent' and the 'HasControlEvent' class each control's own event type
instantiates. 'onFocusGained' \/ 'onFocusLost' \/ 'onMouseEnter' \/
'onMouseExit' are written once, generically, against any @ev@ with a
'HasControlEvent' instance, rather than once per control.

= Focus and tab order

'control' — the standard entry point for an interactive element — combines
mouse-over ('applyMouseOver'), focus and Tab\/Shift-Tab navigation
('applyFocus'), and style-driven chrome ('renderChrome'). An element takes
focus automatically when nothing else holds it and retains it on click, but
only while 'tabStop' is 'True' — 'tabStop' 'False' removes it from Tab order
entirely, including this auto-claim. 'focusOnClick' overrides what a click
does to focus — 'FocusTarget' redirects it elsewhere (used by a caption to
focus the input beside it instead of itself), and 'NoFocus' makes clicking a
no-op for focus.

= Building composites

'activatable' (click or a listed key activates), 'isActivatedBy', and
'renderCheckboxGlyph' are the smaller building blocks 'button' and
'checkbox' are made from, exported for anyone assembling a custom control
with the same shape. 'thumbRect' \/ 'mouseToTrackPos' are the drag-track
geometry 'slider' and 'scrollBar' share.

= Items and selection

'itemsLayout' is a primitive building block, not a control: it has no
element id and draws no chrome, the same way 'virtualContent' doesn't — it
just renders a list of plain data values via a caller-supplied template,
stacked horizontally or vertically. It exists to be composed inside real
controls.

'selectionControl' is such a composite: it layers a single selected item
over 'itemsLayout', resolving 'items' and a 'SelectedItem' choice into a
per-item 'SelectionState', handing each item to the caller's
'itemContainer' template, and detecting clicks on items so it can report
which one was activated via 'onSelect'. It "is" a control in the sense
this module uses the word — it has an id (one per item, via its tagging
function) and behaviour (click detection) — but still draws no chrome of
its own; that stays the parent's decision, same as 'itemsLayout'.

Neither control owns interactive focus over its items — a caller that
wants keyboard navigation or a fully interactive item builds it into its
own template using its own element ids, the same way any composed
'Blink.UI.UI' content does.
-}
module Blink.Controls
  ( Attr
  , configure
  , fire
  , onEvent
  , configAny
  , post
  , postWith
  , perform
  , performWith
  , forward
  , translate
  , translateWith
  , ControlEvent (..)
  , HasControlEvent (..)
  , onFocusGained
  , onFocusLost
  , onMouseEnter
  , onMouseExit
  , FocusOnClick (..)
  , tabStop
  , focusOnClick
  , HasTextConfig (..)
  , text
  , isMouseOver
  , isPressed
  , getStyle
  , renderChrome
  , measureChrome
  , applyMouseOver
  , applyFocus
  , control
  , isKeyPressed
  , whenFocused
  , isClickedOver
  , isActivatedBy
  , activatable
  , LabelEvent
  , LabelConfig
  , label
  , ProgressValue (..)
  , ProgressBarConfig
  , progressBar
  , bandSpeed
  , progress
  , button
  , ButtonEvent (Clicked)
  , ButtonConfig
  , onClick
  , CheckboxPart (..)
  , CheckboxEvent (Toggled)
  , CheckboxConfig
  , onToggle
  , checked
  , renderCheckboxGlyph
  , checkbox
  , TextEvent (Edited, Submitted)
  , TextInputConfig
  , onInput
  , onSubmit
  , inputFilter
  , displayFilter
  , textInputControl
  , thumbRect
  , mouseToTrackPos
  , SliderPart (..)
  , SliderEvent (Changed)
  , SliderConfig
  , onChange
  , arrowStep
  , HasThumbRatioConfig (..)
  , thumbRatio
  , HasOrientationConfig (..)
  , orientation
  , value
  , slider
  , ScrollBarPart (..)
  , ScrollBarEvent
  , ScrollBarConfig
  , scrollBar
  , ViewportPart (..)
  , ViewportConfig
  , scrollRegionBarSize
  , contentSize
  , viewport
  , virtualContent
  , HasItemsConfig (..)
  , HasItemsPanelConfig (..)
  , items
  , itemsPanel
  , ItemTemplate
  , CompositeControlConfig
  , itemTemplate
  , itemsLayout
  , CompositeEvent
  , compositeControl
  , SelectionState (..)
  , SelectedItem (..)
  , SelectionItemTemplate
  , SelectionEvent (..)
  , SelectionConfig
  , itemContainer
  , selected
  , selectedIndex
  , onSelect
  , selectionControl
  ) where

import Control.Monad (forM_, guard, when)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find, foldl')
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)

import Blink.Geometry (Alignment (..), Insets (..), Orientation (..), Point (..), Rectangle (..), Size (..), borderInsets, insetRect, noBorder)
import Blink.Input (Key (..), KeyEvent (..), Modifier (..), InputState (..))
import Blink.Layout (BoxConfig (..), Layout (..), Length (..), defaultBoxConfig, hBox, vBox)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI

-- | The lifecycle events every control can fire, regardless of its own
-- specific event type — see 'HasControlEvent'. Raised by 'applyFocus' and
-- 'applyMouseOver'; handled with 'onFocusGained', 'onFocusLost',
-- 'onMouseEnter', and 'onMouseExit'.
data ControlEvent
  = FocusGained
  | FocusLost
  | MouseEntered
  | MouseExited
  deriving (Eq, Show)

-- | Lets a control's own event type carry the generic 'ControlEvent's
-- alongside its control-specific ones (e.g. 'ButtonEvent' carries 'Clicked'
-- and, via this class, the focus\/hover lifecycle too), so a single attrs
-- list can react to both with 'onEvent' and friends.
class HasControlEvent ev where
  liftControl  :: ControlEvent -> ev
  matchControl :: ev -> Maybe ControlEvent

-- | What clicking a control does to focus, set with 'focusOnClick'.
data FocusOnClick e
  = FocusSelf
    -- ^ The control takes focus itself (the default for interactive controls).
  | FocusTarget e
    -- ^ The control hands focus to a different element instead of taking it
    -- itself — e.g. a checkbox's label redirecting focus onto the checkbox.
  | NoFocus
    -- ^ Clicking the control has no effect on focus at all.
  deriving (Eq, Show)

-- | Configuration shared by every control, regardless of that control's own
-- @cfg@: whether Tab lands on it ('tabStop') and what clicking it does to
-- focus ('focusOnClick').
data ControlConfig e = ControlConfig
  { ccTabStop      :: Bool
  , ccFocusOnClick :: FocusOnClick e
  }

defaultControlConfig :: ControlConfig e
defaultControlConfig = ControlConfig { ccTabStop = True, ccFocusOnClick = FocusSelf }

-- | Whether a control is eligible to claim focus purely by rendering first
-- while nothing else holds it: opted into keyboard focus at all ('tabStop')
-- and configured to take focus itself on click ('focusOnClick').
autoClaimsFocus :: Eq e => ControlConfig e -> Bool
autoClaimsFocus cc = ccTabStop cc && ccFocusOnClick cc == FocusSelf

-- | One entry in a control's attrs list — either a reaction to an event
-- ('onEvent' and the combinators built on it), a change to the control's own
-- @cfg@ ('configAny' and the smart constructors built on it), or shared
-- focus\/tab-stop config ('tabStop', 'focusOnClick'). Opaque: built and
-- consumed only through the functions this module exports.
data Attr e ev msg cfg
  = On (ev -> [Out e msg])
  | Config (cfg -> cfg)
  | Shared (ControlConfig e -> ControlConfig e)

-- | Resolves a control's final @cfg@ by folding every @Config@ attr in the
-- list over the default, left to right — so later attrs override earlier
-- ones that touch the same field.
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

-- | Raises each event in turn against every @On@ reaction in the attrs list,
-- dispatching the resulting 'Out's (emitting messages, queuing effects).
-- Used internally by 'applyFocus' and 'applyMouseOver' to fire the generic
-- lifecycle events, and by each control to fire its own.
fire :: [Attr e ev msg cfg] -> [ev] -> UI e msg ()
fire attrs evs = forM_ evs $ \ev -> mapM_ dispatch (concatMap ($ ev) [h | On h <- attrs])
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

-- | The raw escape hatch: react to an event with an arbitrary function to
-- 'Out's. Prefer the more specific combinators ('post', 'postWith',
-- 'perform', 'performWith', 'onFocusGained', ...) where they fit — reach for
-- this when a reaction needs to inspect the event and choose between several
-- different 'Out's.
onEvent :: (ev -> [Out e msg]) -> Attr e ev msg cfg
onEvent = On

-- | The raw escape hatch for changing a control's own @cfg@. The named smart
-- constructors ('text', 'checked', 'value', ...) are built on this — reach
-- for it directly only when writing a new one.
configAny :: (cfg -> cfg) -> Attr e ev msg cfg
configAny = Config

-- | Emits @msg@, ignoring whatever data the triggering event carried.
post :: msg -> a -> [Out e msg]
post msg = const [OutMsg msg]

-- | Emits @f a@ — uses the triggering event's own data to build the message.
postWith :: (a -> msg) -> a -> [Out e msg]
postWith f a = [OutMsg (f a)]

-- | Queues a 'UiEffect', ignoring whatever data the triggering event
-- carried. For controls (like 'scrollBar') that write their own state
-- directly to the 'UIContext' rather than routing it through the app's
-- model.
perform :: UiEffect e -> a -> [Out e msg]
perform eff = const [OutUi eff]

-- | Queues @f a@ as a 'UiEffect' — uses the triggering event's own data to
-- build it.
performWith :: (a -> UiEffect e) -> a -> [Out e msg]
performWith f a = [OutUi (f a)]

-- | Re-raises a sub-control's lifecycle event against another attrs list
-- under the same name — e.g. @onFocusLost (forward attrs)@ calls whatever
-- 'onFocusLost' handler (if any) is in @attrs@. Lets a composite (like
-- 'scrollBar') mirror an inner control's lifecycle events as its own,
-- reusing whatever focus\/hover tracking the inner control already did
-- instead of redoing it.
forward :: HasControlEvent ev => [Attr e ev msg cfg] -> ControlEvent -> [Out e msg]
forward attrs ce = concatMap ($ liftControl ce) [h | On h <- attrs]

-- | Re-raises @ev@ against another attrs list, ignoring whatever data the
-- triggering event carried — for triggering a *different* event on the
-- parent than the one that actually fired.
translate :: [Attr e ev msg cfg] -> ev -> a -> [Out e msg]
translate attrs ev = const (concatMap ($ ev) [h | On h <- attrs])

-- | Maps the triggering event through @f@ and raises the result against
-- another attrs list — for translating a sub-control's event, using its own
-- data, into a different event on the parent (unlike 'translate', which
-- always raises the same fixed event regardless of what triggered it).
translateWith :: (subEv -> ev) -> [Attr e ev msg cfg] -> subEv -> [Out e msg]
translateWith f attrs subEv = concatMap ($ f subEv) [h | On h <- attrs]

-- | Reacts when the control gains focus. Fired by 'applyFocus' for any
-- control built on 'control'.
onFocusGained :: HasControlEvent ev => (ControlEvent -> [Out e msg]) -> Attr e ev msg cfg
onFocusGained reaction = onEvent $ \ev -> case matchControl ev of
  Just FocusGained -> reaction FocusGained
  _                -> []

-- | Reacts when the control loses focus. Fired by 'applyFocus' for any
-- control built on 'control'.
onFocusLost :: HasControlEvent ev => (ControlEvent -> [Out e msg]) -> Attr e ev msg cfg
onFocusLost reaction = onEvent $ \ev -> case matchControl ev of
  Just FocusLost -> reaction FocusLost
  _              -> []

-- | Reacts when the mouse moves over the control. Fired by 'applyMouseOver'
-- for any control built on 'control'.
onMouseEnter :: HasControlEvent ev => (ControlEvent -> [Out e msg]) -> Attr e ev msg cfg
onMouseEnter reaction = onEvent $ \ev -> case matchControl ev of
  Just MouseEntered -> reaction MouseEntered
  _                 -> []

-- | Reacts when the mouse moves off the control. Fired by 'applyMouseOver'
-- for any control built on 'control'.
onMouseExit :: HasControlEvent ev => (ControlEvent -> [Out e msg]) -> Attr e ev msg cfg
onMouseExit reaction = onEvent $ \ev -> case matchControl ev of
  Just MouseExited -> reaction MouseExited
  _                -> []

-- | Whether this control participates in keyboard focus at all: Tab\/
-- Shift-Tab cycling onto it, and auto-claiming focus by rendering first
-- while nothing else holds it. 'False' excludes it from both. Defaults to
-- 'True' for interactive controls; 'label' defaults it to 'False' (see
-- 'focusOnClick' too).
tabStop :: Bool -> Attr e ev msg cfg
tabStop b = Shared $ \cc -> cc { ccTabStop = b }

-- | What clicking this control does to focus — see 'FocusOnClick'. Defaults
-- to 'FocusSelf' for interactive controls; 'label' defaults it to 'NoFocus'
-- but can be given 'FocusTarget' to redirect focus onto another element
-- (e.g. a checkbox's label onto its checkbox).
focusOnClick :: FocusOnClick e -> Attr e ev msg cfg
focusOnClick foc = Shared $ \cc -> cc { ccFocusOnClick = foc }

-- | Implemented by any control's own config type that carries displayed
-- text, letting 'text' work uniformly across them (same pattern as
-- 'HasControlEvent' for the shared lifecycle events) rather than needing a
-- differently-named attribute per control.
class HasTextConfig cfg where
  setText :: Text -> cfg -> cfg

-- | Sets the text a control displays — a caption for 'label'\/'button'\/
-- 'checkbox', or the current value for 'textInputControl'. Defaults to
-- @\"\"@ when not given.
text :: HasTextConfig cfg => Text -> Attr e ev msg cfg
text t = configAny (setText t)

-- | 'True' when the mouse is over the element's background rectangle (bounds
-- inset by its margin) — the control-specific hit area. Built on 'isRegionHit';
-- a pure geometric test, so any number of elements can each independently be
-- "over" in the same frame.
--
-- Uses the element's /normal/ margin, not the margin of whichever style
-- variant is currently active: 'getStyle' (below) determines which variant
-- is active by calling this function, so depending on the resolved style
-- here would be circular. Margin defining the hit region's own boundary
-- shouldn't depend on the interaction state the hit test is being used to
-- determine anyway — real themes don't vary margin by state.
isMouseOver :: Ord e => e -> UI e msg Bool
isMouseOver eid = do
  ss <- getStyleSet eid
  r  <- getBounds
  withBounds (insetRect (styleMargin (styleSetNormal ss)) r) isRegionHit

-- | 'True' when the element is hovered (per geometric 'isMouseOver') and the
-- left button is held down, and the element is not disabled.
isPressed :: Ord e => e -> UI e msg Bool
isPressed eid = do
  disabled <- isDisabled
  hit      <- isMouseOver eid
  down     <- isButtonDown
  pure (not disabled && hit && down)

-- | Resolves the active 'Style' for an element given its current interaction
-- state, using geometric 'isMouseOver'\/'isPressed' to determine hover and
-- press. Priority: disabled > pressed > hovered > focused > normal.
getStyle :: Ord e => e -> UI e msg Style
getStyle eid = do
  styles <- getStyleSet eid
  isDis  <- isDisabled
  isHov  <- isMouseOver eid
  isFoc  <- isFocused eid
  isPrs  <- isPressed eid
  let candidates =
        [ guard isDis $> styleSetDisabled styles
        , guard isPrs $> styleSetPressed  styles
        , guard isHov $> styleSetHovered  styles
        , guard isFoc $> styleSetFocused  styles
        ]
  pure $ fromMaybe (styleSetNormal styles) (asum candidates)

-- | Registers the element as moused over and as hot (for drag continuation)
-- when the mouse is within its bounds, and fires 'onMouseEnter'\/'onMouseExit'
-- by comparing against last frame's result. Purely geometric plus a disabled
-- check — no exclusion for a different element's drag being in progress,
-- since geometric hover has no shared slot for one element's drag to
-- contend over. 'setHot' still guards its own capture acquisition
-- independently, so a drag can't be stolen.
applyMouseOver :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg cfg] -> UI e msg ()
applyMouseOver eid attrs = do
  wasOver  <- wasMouseOverLastFrame eid
  disabled <- isDisabled
  hit      <- isMouseOver eid
  let isOver = not disabled && hit
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
  applyTabKeys wasFocused
  nowFocused <- isFocused eid
  fire attrs $ concat
    [ [liftControl FocusGained | not wasFocused && nowFocused]
    , [liftControl FocusLost   | wasFocused && not nowFocused]
    ]
  where
    cc = controlConfig attrs

    -- Takes focus on a click, hands it to a target, or leaves it, per 'focusOnClick'.
    applyFocusRules = whenEnabled $ do
      nothingIsFocused <- isNothingFocused <$> getFocus
      isRetainingFocus <- isFocused eid
      isHit            <- isMouseOver eid
      released         <- isButtonReleased
      captured         <- getCapturedElement
      let isDragRelease = released && isJust captured && captured /= Just eid
          wasClicked     = isHit && released && not isDragRelease
          autoClaim      = autoClaimsFocus cc && nothingIsFocused && not isDragRelease
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
    --
    -- Uses 'wasFocused' (captured before 'applyFocusRules' ran, not a fresh
    -- 'isFocused' read) so a same-frame auto-claim — nothing was focused,
    -- this element just took it — isn't immediately undone by the very Tab
    -- press that made nothing-was-focused true in the first place. Without
    -- this, wraparound could never complete: Tab off the last control
    -- clears focus with nothing left this frame to auto-claim it; next
    -- frame's Tab press lets the first control auto-claim (nothing is
    -- focused) — but a fresh 'isFocused' read here would then see that
    -- claim and immediately clear it again, forever.
    applyTabKeys wasFocused = whenEnabled $ do
      input    <- getInput
      prevCtrl <- getPreviousTabStop
      let tabKey          = find (\e -> key e == KeyTab) (inputKeyEvents input)
          tabPressed      = maybe False (\e -> Shift `notElem` modifiers e) tabKey
          shiftTabPressed = maybe False (\e -> Shift `elem`    modifiers e) tabKey
      when (wasFocused && tabPressed) $ do
        clearFocus
        consumeKey KeyTab
      when (wasFocused && shiftTabPressed) $
        forM_ prevCtrl $ \prev -> do
          setFocus prev
          consumeKey KeyTab
      when (ccTabStop cc) $ setPreviousTabStop eid

-- | The extra width\/height a control's margin, border, and padding add
-- around its content, from the resolved style. Added to a control's own
-- content size when reporting its size to layout.
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

-- | Draws a control's background and border from the resolved style, then
-- runs @content@ clipped to the remaining space inside the padding. The
-- shared rendering half of every control built on 'control'.
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
-- while a different control is mid-drag. Not exported: 'isMouseOver' is a
-- geometric, on-demand hit test with no registration state of its own, so
-- this is assembled fresh here from the primitives above rather than
-- queried as a single stored value.
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

-- | Events reported by 'label': just a lifecycle event via @LabelControl@
-- (see 'ControlEvent') — 'label' has no domain events of its own, but still
-- needs a concrete event type to be a 'control' and raise the shared ones.
newtype LabelEvent = LabelControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent LabelEvent where
  liftControl = LabelControl
  matchControl (LabelControl ce) = Just ce

-- | Configuration for 'label', set via 'text'. Defaults to @\"\"@.
newtype LabelConfig = LabelConfig { labelConfigText :: Text }

defaultLabelConfig :: LabelConfig
defaultLabelConfig = LabelConfig { labelConfigText = "" }

instance HasTextConfig LabelConfig where
  setText t cfg = cfg { labelConfigText = t }

-- | Text in the resolved style, set via 'text'. A full 'control', so it
-- registers mouse-over — but unlike every other control here, it never
-- takes keyboard focus itself: defaults to @'tabStop' False@ and
-- @'focusOnClick' 'NoFocus'@, since a plain label has no reason to hold
-- focus. It can still hand focus /elsewhere/ — a composite like a labelled
-- field can pass @'focusOnClick' ('FocusTarget' input)@ to redirect a click
-- on the caption onto its input — an explicit attr always overrides these
-- defaults (they're consulted first, so a later attr in the list wins).
label :: Ord e => e -> [Attr e LabelEvent msg LabelConfig] -> UI e msg ()
label eid attrs = control eid (tabStop False : focusOnClick NoFocus : attrs) $ do
  style <- getStyle eid
  let cfg = configure defaultLabelConfig attrs
  drawText (styleTextColour style) (styleTextAlign style) (labelConfigText cfg)

-- | The value passed to 'progressBar'.
data ProgressValue
  = Progress Double
    -- ^ A determinate value in @[0, 1]@, clamped and rendered as a filled bar.
  | Indeterminate
    -- ^ Unknown progress: a band animates continuously across the bar.
  deriving (Eq, Show)

-- | Configuration for 'progressBar', set via 'bandSpeed' and 'progress'.
data ProgressBarConfig = ProgressBarConfig
  { configBandSpeed     :: Double
  , progressBarConfigValue :: ProgressValue
  }

defaultProgressBarConfig :: ProgressBarConfig
defaultProgressBarConfig = ProgressBarConfig { configBandSpeed = 0.5, progressBarConfigValue = Progress 0 }

-- | How fast the band sweeps across an 'Indeterminate' bar, in bar-widths
-- per second. Defaults to 0.5.
bandSpeed :: Double -> Attr e ev msg ProgressBarConfig
bandSpeed v = configAny $ \cfg -> cfg { configBandSpeed = v }

-- | Sets the bar to 'Progress' (determinate) or 'Indeterminate'. Defaults
-- to @'Progress' 0@.
progress :: ProgressValue -> Attr e ev msg ProgressBarConfig
progress p = configAny $ \cfg -> cfg { progressBarConfigValue = p }

-- | A read-only progress indicator, set via 'progress' to 'Progress' for a
-- determinate bar or 'Indeterminate' for a continuously animating band. Not
-- interactive — no mouse-over, focus, or tab stop, so it takes no
-- 'tabStop'\/'focusOnClick' and never needs a real event type; 'Void' rules
-- out anything being fired.
progressBar :: Ord e => e -> [Attr e Void msg ProgressBarConfig] -> UI e msg ()
progressBar eid attrs = do
  let cfg = configure defaultProgressBarConfig attrs
  case progressBarConfigValue cfg of
    Progress progressValue -> renderChrome eid $ do
      style <- getStyle eid
      r     <- getBounds
      let clamped   = max 0 (min 1 progressValue)
          fillRect' = r { rectWidth = rectWidth r * clamped }
      withBounds fillRect' $ fillRect (styleTextColour style)
    Indeterminate -> do
      requiresAnimation
      renderChrome eid $ do
        r       <- getBounds
        style   <- getStyle eid
        elapsed <- getAnimElapsed
        let speed = configBandSpeed cfg
            t     = realToFrac elapsed * speed
            phase = t - fromIntegral (floor t :: Int)
            bandW = rectWidth r * 0.3
            left  = rectX r - bandW + (rectWidth r + bandW) * phase
        withBounds (r { rectX = left, rectWidth = bandW }) $
          fillRect (styleTextColour style)

-- | Events reported by 'button': 'Clicked' when activated, or a lifecycle
-- event via @Control@ (see 'ControlEvent'). @Control@ is not exported —
-- 'onFocusGained'\/'onFocusLost'\/'onMouseEnter'\/'onMouseExit' already cover
-- it generically — but 'Clicked' is, so a fully custom handler can still be
-- built with 'onEvent' when 'onClick' isn't enough.
data ButtonEvent = Clicked | Control ControlEvent
  deriving (Eq, Show)

instance HasControlEvent ButtonEvent where
  liftControl = Control
  matchControl (Control ce) = Just ce
  matchControl _             = Nothing

-- | Runs a reaction when the button is 'Clicked' — e.g. @onClick (post msg)@
-- to emit a message, @onClick (perform eff)@ to queue a 'UiEffect', or
-- @onClick (post msg \<\> perform eff)@ to do both.
onClick :: (() -> [Out e msg]) -> Attr e ButtonEvent msg cfg
onClick reaction = onEvent $ \ev -> case ev of
  Clicked -> reaction ()
  _       -> []

-- | Configuration for 'button', set via 'text'. Defaults to @\"\"@.
newtype ButtonConfig = ButtonConfig { buttonConfigText :: Text }

defaultButtonConfig :: ButtonConfig
defaultButtonConfig = ButtonConfig { buttonConfigText = "" }

instance HasTextConfig ButtonConfig where
  setText t cfg = cfg { buttonConfigText = t }

-- | A clickable button labelled via 'text'. Fires 'Clicked' — handled with
-- 'onClick' — on the frame it's activated, by a left-click or by pressing
-- Enter while focused.
button :: Ord e => e -> [Attr e ButtonEvent msg ButtonConfig] -> UI e msg ()
button eid attrs = do
  activated <- activatable eid attrs [KeyReturn] draw
  when activated $ fire attrs [Clicked]
  where
    draw = do
      style <- getStyle eid
      let cfg = configure defaultButtonConfig attrs
      drawText (styleTextColour style) (styleTextAlign style) (buttonConfigText cfg)

-- | Sub-parts of a 'checkbox', used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | NotifyMe CheckboxPart
-- checkbox NotifyMe [text "Notify me by email", checked (notifyMe model), onToggle NotifyMeChanged]
-- @
data CheckboxPart
  = CheckboxBox   -- ^ The checkbox as a whole: chrome, hit region, focus, activation.
  | CheckboxGlyph -- ^ The checkmark glyph.
  | CheckboxLabel -- ^ The label beside it — an ordinary 'label'.
  deriving (Eq, Ord, Show)

-- | Events reported by 'checkbox': 'Toggled' with the new checked state when
-- activated, or a lifecycle event via @CheckboxControl@ (see 'ControlEvent').
data CheckboxEvent = Toggled Bool | CheckboxControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent CheckboxEvent where
  liftControl = CheckboxControl
  matchControl (CheckboxControl ce) = Just ce
  matchControl _                    = Nothing

-- | Runs a reaction with the new checked state when 'Toggled'.
onToggle :: (Bool -> [Out e msg]) -> Attr e CheckboxEvent msg cfg
onToggle reaction = onEvent $ \ev -> case ev of
  Toggled b -> reaction b
  _         -> []

-- | Draws a checkbox glyph: a checkmark centred in the current bounds when
-- @checked@, nothing otherwise, in the resolved style's text colour. A bare
-- rendering action with no interactive behaviour of its own — reusable by
-- anything that wants to look like a checkbox glyph without taking on the
-- toggle behaviour of 'checkbox'.
renderCheckboxGlyph :: Ord e => e -> Bool -> UI e msg ()
renderCheckboxGlyph eid isChecked = do
  style <- getStyle eid
  when isChecked $ drawText (styleTextColour style) AlignCenter "✓"

-- | Configuration for 'checkbox', set via 'checked' and 'text'. Defaults to
-- unchecked with an empty label.
data CheckboxConfig = CheckboxConfig
  { checkboxConfigChecked :: Bool
  , checkboxConfigText    :: Text
  }

defaultCheckboxConfig :: CheckboxConfig
defaultCheckboxConfig = CheckboxConfig { checkboxConfigChecked = False, checkboxConfigText = "" }

instance HasTextConfig CheckboxConfig where
  setText t cfg = cfg { checkboxConfigText = t }

-- | Sets whether the checkbox is checked. Defaults to 'False'.
checked :: Bool -> Attr e ev msg CheckboxConfig
checked b = configAny $ \cfg -> cfg { checkboxConfigChecked = b }

-- | Attrs shared by a checkbox's glyph and label sub-parts: neither is a tab
-- stop, and clicking either redirects focus to the checkbox itself.
-- Committing to 'LabelEvent' here (rather than leaving it polymorphic) is
-- what lets 'checkbox' use the same value at both the glyph's 'control' call
-- and the label's 'label' call without either site leaving its event type
-- ambiguous.
checkboxSubPartAttrs :: e -> [Attr e LabelEvent msg cfg]
checkboxSubPartAttrs boxId = [tabStop False, focusOnClick (FocusTarget boxId)]

-- | A togglable checkbox with an adjacent label, as one unit, set via
-- 'checked' and 'text': the glyph and label are purely visual sub-parts
-- (each moused-over and styled on its own bounds, neither a tab stop, a
-- click on either redirecting focus to the checkbox) — all interaction
-- lives on the checkbox itself, whose own hit region spans the glyph, the
-- label, and the gap between them. Fires 'Toggled' with the new checked
-- state when activated by a click anywhere in that region, or by Enter or
-- Space while focused.
--
-- @
-- checkbox NotifyMe [checked (notifyMe model), text "Notify me by email", onToggle NotifyMeChanged]
-- @
checkbox :: Ord e => (CheckboxPart -> e) -> [Attr e CheckboxEvent msg CheckboxConfig] -> UI e msg ()
checkbox mkId attrs = do
  let cfg = configure defaultCheckboxConfig attrs
      isChecked = checkboxConfigChecked cfg
  activated <- activatable (mkId CheckboxBox) attrs [KeyReturn, KeySpace] (draw isChecked (checkboxConfigText cfg))
  when activated $ fire attrs [Toggled (not isChecked)]
  where
    glyphId  = mkId CheckboxGlyph
    subAttrs = checkboxSubPartAttrs (mkId CheckboxBox)
    draw isChecked txt =
      hBox (defaultBoxConfig { boxSpacing = 4, boxFillCross = False })
        [ (Layout (Exactly 20) (Exactly 20) MiddleLeft, control glyphId subAttrs (renderCheckboxGlyph glyphId isChecked))
        , (Layout Fill Fill MiddleLeft, label (mkId CheckboxLabel) (text txt : subAttrs))
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
resolveMouseSelection eid bounds wasCapturing justFocused displayValue scrollX (anchor, active) = do
  isCapturing <- isDragging eid
  if isCapturing
    then do
      mousePos <- getMousePos
      let localX = realToFrac (pointX mousePos - rectX bounds) + realToFrac scrollX :: Float
      clickedPos <- charAtOffset displayValue localX
      pure $ if not wasCapturing || justFocused
        then (clickedPos, clickedPos)
        else (anchor, clickedPos)
    else pure (anchor, active)

-- | Shift+Left\/Right extend the selection; plain Left\/Right collapse an
-- existing selection to its near end, or step by one otherwise.
resolveKeyboardSelection
  :: Bool         -- ^ control has keyboard focus
  -> [KeyEvent]   -- ^ this frame's key events
  -> Int          -- ^ length of the underlying value
  -> (Int, Int)   -- ^ current @(anchor, active)@ selection
  -> (Int, Int)
resolveKeyboardSelection hasFocus keyEvts len (anchor, active)
  | shiftLeft  = (anchor, max 0    (active - 1))
  | shiftRight = (anchor, min len  (active + 1))
  | plainLeft  = let p = if hasSel then selLo else max 0   (active - 1) in (p, p)
  | plainRight = let p = if hasSel then selHi else min len (active + 1) in (p, p)
  | otherwise  = (anchor, active)
  where
    hasSel     = anchor /= active
    selLo      = min anchor active
    selHi      = max anchor active
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
applyEdit inputFilterFn currentValue input (anchor, active)
  | backspace || hasTyped =
      ((newCursor, newCursor), if newText /= currentValue then Just newText else Nothing)
  | otherwise = ((anchor, active), Nothing)
  where
    keyEvts   = inputKeyEvents input
    backspace = any (\e -> key e == KeyBackspace) keyEvts
    typed     = inputFilterFn (foldl' (<>) T.empty (inputTypedText input))
    hasTyped  = not (T.null typed)
    hasSel    = anchor /= active
    selLo     = min anchor active
    selHi     = max anchor active
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
-- a viewport of width @w@ currently scrolled to @scrollX@. Pixels in, pixels
-- out — @scrollFraction@\/@scrollPixels@ convert at the boundary with
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

-- | The inverse of @scrollFraction@: converts a stored @[0, 1]@ fraction
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
drawTextInputContent style bounds displayValue hasFocus enabled ox (anchor, active) = do
  when (hasFocus && drawLo < drawHi) $ do
    loX <- charOffset displayValue drawLo
    hiX <- charOffset displayValue drawHi
    let selRect = Rectangle
          (rectX bounds + realToFrac loX - ox)
          (rectY bounds)
          (realToFrac (hiX - loX))
          (rectHeight bounds)
    withBounds selRect $ fillRect (RGBA 0.3 0.5 1.0 0.4)

  let textBounds = bounds { rectX = rectX bounds - ox }
  withBounds textBounds $ drawText (styleTextColour style) AlignLeft displayValue

  when enabled $ do
    curX <- charOffset displayValue active
    let cursorRect = Rectangle
          (rectX bounds + realToFrac curX - ox)
          (rectY bounds)
          1
          (rectHeight bounds)
    withBounds cursorRect $ fillRect (styleTextColour style)
  where
    drawLo = min anchor active
    drawHi = max anchor active

-- | Events reported by 'textInputControl': 'Edited' with the new value
-- whenever a keystroke changes it, 'Submitted' when Enter is pressed while
-- focused and enabled, or a lifecycle event via @TextControl@ (see
-- 'ControlEvent').
data TextEvent = Edited Text | Submitted | TextControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent TextEvent where
  liftControl = TextControl
  matchControl (TextControl ce) = Just ce
  matchControl _                = Nothing

-- | Runs a reaction with the new value on every 'Edited'.
onInput :: (Text -> [Out e msg]) -> Attr e TextEvent msg cfg
onInput reaction = onEvent $ \ev -> case ev of
  Edited t -> reaction t
  _        -> []

-- | Runs a reaction when Enter is pressed while the field is focused
-- ('Submitted').
onSubmit :: (() -> [Out e msg]) -> Attr e TextEvent msg cfg
onSubmit reaction = onEvent $ \ev -> case ev of
  Submitted -> reaction ()
  _         -> []

-- | Configuration for 'textInputControl', set via 'inputFilter',
-- 'displayFilter', and 'text' (the field's current value). Defaults to @\"\"@.
data TextInputConfig = TextInputConfig
  { configInputFilter    :: Text -> Text
  , configDisplayFilter  :: Text -> Text
  , textInputConfigValue :: Text
  }

defaultTextInputConfig :: TextInputConfig
defaultTextInputConfig = TextInputConfig
  { configInputFilter    = id
  , configDisplayFilter  = id
  , textInputConfigValue = ""
  }

instance HasTextConfig TextInputConfig where
  setText t cfg = cfg { textInputConfigValue = t }

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
-- @scrollFraction@\/@scrollPixels@ — converted to and from pixels locally,
-- since the selection\/cursor\/auto-scroll math below is naturally pixel-based.
textInputControl :: Ord e => e -> [Attr e TextEvent msg TextInputConfig] -> UI e msg ()
textInputControl eid attrs = do
  wasFocused   <- isFocused eid
  wasCapturing <- isDragging eid
  let cfg          = configure defaultTextInputConfig attrs
      currentValue = textInputConfigValue cfg
  control eid attrs $ do
    style    <- getStyle eid
    hasFocus <- isFocused eid
    disabled <- isDisabled
    bounds   <- getBounds
    input    <- getInput
    sel      <- getSelection eid
    frac     <- getScrollState eid

    let displayValue = configDisplayFilter cfg currentValue
        w           = rectWidth bounds
        defPos        = T.length currentValue
        anchorInit    = maybe defPos selectionAnchor sel
        activeInit    = maybe defPos selectionActive sel
        -- Focus was gained by a click this frame (e.g. clicking from another
        -- element). Treat as a fresh click rather than a drag continuation so
        -- the old anchor is not inherited.
        justFocused = hasFocus && not wasFocused
        enabled     = hasFocus && not disabled

    contentW <- realToFrac <$> charOffset displayValue (T.length displayValue)
    let maxScrollPx = maxScrollPixels contentW w
        scrollX     = scrollPixels maxScrollPx frac

    (anchorAfterMouse, activeAfterMouse) <-
      if enabled
        then resolveMouseSelection eid bounds wasCapturing justFocused displayValue scrollX (anchorInit, activeInit)
        else pure (anchorInit, activeInit)

    let (anchorAfterKeys, activeAfterKeys) =
          resolveKeyboardSelection hasFocus (inputKeyEvents input) (T.length currentValue) (anchorAfterMouse, activeAfterMouse)

        ((anchorFinal, activeFinal), edited)
          | enabled   = applyEdit (configInputFilter cfg) currentValue input (anchorAfterKeys, activeAfterKeys)
          | otherwise = ((anchorAfterKeys, activeAfterKeys), Nothing)

        submitted = enabled && any (\e -> key e == KeyReturn) (inputKeyEvents input)

    fire attrs ([Submitted | submitted] ++ [Edited t | Just t <- [edited]])

    when enabled $ emitUi (SetSelectionAt eid (Selection anchorFinal activeFinal))

    -- Computed locally rather than re-read via 'getScrollState': scroll
    -- writes are deferred (applied between frames), so a same-frame re-read
    -- would still see the pre-write value and the cursor would lag the
    -- auto-scroll by one frame.
    effectiveScrollX <-
      if enabled
        then do
          curX <- charOffset displayValue activeFinal
          let newScrollX = resolveScroll w scrollX (realToFrac curX)
          when (newScrollX /= scrollX) $ emitUi (ScrollTo eid (scrollFraction maxScrollPx newScrollX))
          pure newScrollX
        else pure scrollX

    drawTextInputContent style bounds displayValue hasFocus enabled effectiveScrollX (anchorFinal, activeFinal)

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

-- | Clamps a value to the @[0, 1]@ range shared by thumb positions and
-- ratios.
clampUnit :: Double -> Double
clampUnit = max 0 . min 1

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
-- event via @SliderControl@ (see 'ControlEvent').
data SliderEvent = Changed Double | SliderControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent SliderEvent where
  liftControl = SliderControl
  matchControl (SliderControl ce) = Just ce
  matchControl _                  = Nothing

-- | Runs a reaction with the new value on every 'Changed'.
onChange :: (Double -> [Out e msg]) -> Attr e SliderEvent msg cfg
onChange reaction = onEvent $ \ev -> case ev of
  Changed v -> reaction v
  _         -> []

-- | Configuration for 'slider', set via 'arrowStep', 'thumbRatio',
-- 'orientation', and 'value'. Defaults to a horizontal slider at 0.
data SliderConfig = SliderConfig
  { configArrowStep         :: Double
  , configThumbRatio        :: Maybe Double
  , sliderConfigOrientation :: Orientation
  , sliderConfigValue       :: Double
  }

defaultSliderConfig :: SliderConfig
defaultSliderConfig = SliderConfig
  { configArrowStep         = 0.05
  , configThumbRatio        = Nothing
  , sliderConfigOrientation = Horizontal
  , sliderConfigValue       = 0
  }

-- | The amount an arrow-key press (Left\/Right for 'Horizontal', Up\/Down
-- for 'Vertical') changes the value by. Defaults to @0.05@.
arrowStep :: Double -> Attr e ev msg SliderConfig
arrowStep v = configAny $ \cfg -> cfg { configArrowStep = v }

-- | Implemented by any control's own config type that carries a thumb
-- ratio, letting 'thumbRatio' work uniformly across them (same pattern as
-- 'HasTextConfig').
class HasThumbRatioConfig cfg where
  setThumbRatio :: Double -> cfg -> cfg

-- | Overrides the thumb's size (visible \/ total, in @[0, 1]@) instead of a
-- control's own default sizing. 'slider' defaults to a square thumb (side
-- equal to the track's cross-axis) when not given; 'scrollBar' defaults to
-- a full-track thumb (i.e. nothing to scroll).
thumbRatio :: HasThumbRatioConfig cfg => Double -> Attr e ev msg cfg
thumbRatio v = configAny (setThumbRatio v)

instance HasThumbRatioConfig SliderConfig where
  setThumbRatio v cfg = cfg { configThumbRatio = Just v }

-- | Implemented by any control's own config type that carries an
-- orientation, letting 'orientation' work uniformly across them (same
-- pattern as 'HasTextConfig').
class HasOrientationConfig cfg where
  setOrientation :: Orientation -> cfg -> cfg

-- | Sets a control's orientation. Defaults to 'Horizontal'.
orientation :: HasOrientationConfig cfg => Orientation -> Attr e ev msg cfg
orientation o = configAny (setOrientation o)

instance HasOrientationConfig SliderConfig where
  setOrientation o cfg = cfg { sliderConfigOrientation = o }

-- | Sets the slider's current value, in @[0, 1]@. Defaults to @0@.
value :: Double -> Attr e ev msg SliderConfig
value v = configAny $ \cfg -> cfg { sliderConfigValue = v }

-- | A slider mapping a draggable thumb to a value in @[0, 1]@, set via
-- 'value' and 'orientation'. Fires 'Changed' with the new value when the
-- user drags, clicks on the track, or nudges with arrow keys (Left\/Right
-- for 'Horizontal', Up\/Down for 'Vertical', by 'arrowStep'). The thumb is
-- square by default; override with 'thumbRatio'.
slider :: Ord e => (SliderPart -> e) -> [Attr e SliderEvent msg SliderConfig] -> UI e msg ()
slider mkId attrs = do
  let trackId = mkId SliderTrack
      thumbId = mkId SliderThumb
      cfg     = configure defaultSliderConfig attrs
      ori     = sliderConfigOrientation cfg
      clamped = clampUnit (sliderConfigValue cfg)
      step    = configArrowStep cfg
  contentRect <- trackContentRect trackId
  let (crossSz, mainSz) = case ori of
        Horizontal -> (rectHeight contentRect, rectWidth contentRect)
        Vertical   -> (rectWidth contentRect,  rectHeight contentRect)
      autoRatio = if mainSz > 0 then crossSz / mainSz else 0
      ratio     = clampUnit (fromMaybe autoRatio (configThumbRatio cfg))
      thumbR    = thumbRect ori clamped ratio contentRect
  control trackId attrs $
    withBounds thumbR $ renderChrome thumbId $ pure ()
  newPos <- dragToTrackPos trackId ori ratio contentRect
  let (decrKey, incrKey) = case ori of
        Horizontal -> (KeyLeft,  KeyRight)
        Vertical   -> (KeyUp,    KeyDown)
  disabled  <- isDisabled
  decrKeyed <- isKeyPressed trackId decrKey
  incrKeyed <- isKeyPressed trackId incrKey
  let decrPressed = not disabled && decrKeyed
      incrPressed = not disabled && incrKeyed
      changes = concat
        [ [Changed v | Just v <- [newPos]]
        , [Changed (max 0 (clamped - step)) | decrPressed && step > 0]
        , [Changed (min 1 (clamped + step)) | incrPressed && step > 0]
        ]
  fire attrs changes

-- | Sub-parts of a scrollbar, used as the inner tag when building the
-- control's element IDs via a tagging function:
--
-- @
-- data Element = ... | VScroll ScrollBarPart
-- scrollBar VScroll [orientation Vertical, thumbRatio 0.3]
-- @
data ScrollBarPart
  = ScrollTrack   -- ^ The track area behind the thumb.
  | ScrollThumb   -- ^ The draggable thumb.
  | ScrollDecrBtn -- ^ The decrement arrow button.
  | ScrollIncrBtn -- ^ The increment arrow button.
  deriving (Eq, Ord, Show)

-- | Events reported by 'scrollBar': a lifecycle event via @ScrollBarControl@
-- (see 'ControlEvent'). The scroll position itself is control state, not
-- reported to the caller — it lives in the 'UIContext', keyed by the
-- track's element ID, and is read with 'getScrollState'.
newtype ScrollBarEvent = ScrollBarControl ControlEvent
  deriving (Eq, Show)

instance HasControlEvent ScrollBarEvent where
  liftControl = ScrollBarControl
  matchControl (ScrollBarControl ce) = Just ce

-- | Configuration for 'scrollBar', set via 'orientation' and 'thumbRatio'.
-- Defaults to a horizontal bar with a full-track thumb (i.e. nothing to
-- scroll) — unlike 'slider', a scrollbar's thumb size is meaningless
-- without a caller-supplied visible\/total fraction, so there's no
-- auto-square fallback to reach for.
data ScrollBarConfig = ScrollBarConfig
  { scrollBarConfigOrientation :: Orientation
  , scrollBarConfigThumbRatio  :: Double
  }

defaultScrollBarConfig :: ScrollBarConfig
defaultScrollBarConfig = ScrollBarConfig
  { scrollBarConfigOrientation = Horizontal
  , scrollBarConfigThumbRatio  = 1
  }

instance HasOrientationConfig ScrollBarConfig where
  setOrientation o cfg = cfg { scrollBarConfigOrientation = o }

instance HasThumbRatioConfig ScrollBarConfig where
  setThumbRatio v cfg = cfg { scrollBarConfigThumbRatio = v }

-- | A scrollbar with decrement\/increment buttons flanking a draggable
-- thumb, set via 'orientation' and 'thumbRatio' (the fraction of the track
-- the thumb fills, visible \/ total, in @[0, 1]@). The track and thumb are
-- a 'slider' underneath — dragging, clicking the track, and arrow-key
-- nudging while focused all move the position, written directly to the
-- 'UIContext' at the track's element ID via 'ScrollTo'; nothing is reported
-- back to the caller through 'ScrollBarEvent' beyond lifecycle events.
-- Button clicks step by 'thumbRatio'.
scrollBar :: Ord e => (ScrollBarPart -> e) -> [Attr e ScrollBarEvent msg ScrollBarConfig] -> UI e msg ()
scrollBar mkId attrs = do
  bounds <- getBounds
  pos    <- getScrollState trackId
  let btnLayout = case ori of
        Vertical   -> Layout Fill (Exactly (rectWidth bounds))  TopLeft
        Horizontal -> Layout (Exactly (rectHeight bounds)) Fill TopLeft
  layoutFn defaultBoxConfig
    [ (btnLayout, decrBtn)
    , (Layout Fill Fill TopLeft, track pos)
    , (btnLayout, incrBtn)
    ]
  where
    cfg   = configure defaultScrollBarConfig attrs
    ori   = scrollBarConfigOrientation cfg
    ratio = clampUnit (scrollBarConfigThumbRatio cfg)

    trackId = mkId ScrollTrack
    thumbId = mkId ScrollThumb

    layoutFn = case ori of
      Vertical   -> vBox
      Horizontal -> hBox

    decrSym = case ori of
      Vertical   -> "▲"
      Horizontal -> "◀"
    incrSym = case ori of
      Vertical   -> "▼"
      Horizontal -> "▶"

    decrBtn = button (mkId ScrollDecrBtn) [text decrSym, onClick (perform (ScrollBy trackId (negate ratio)))]
    incrBtn = button (mkId ScrollIncrBtn) [text incrSym, onClick (perform (ScrollBy trackId ratio))]

    sliderPartId SliderTrack = trackId
    sliderPartId SliderThumb = thumbId

    track pos' =
      slider sliderPartId
        [ value pos'
        , orientation ori
        , thumbRatio ratio
        , onChange (performWith (ScrollTo trackId))
        , onFocusGained (forward attrs)
        , onFocusLost   (forward attrs)
        , onMouseEnter  (forward attrs)
        , onMouseExit   (forward attrs)
        ]

-- | Sub-parts of a viewport's element ID hierarchy. Wraps 'ScrollBarPart'
-- for the horizontal and vertical scrollbars:
--
-- @
-- data Element = ... | MyRegion ViewportPart
-- viewport MyRegion [contentSize (Size 600 400)] content
-- @
data ViewportPart
  = ViewportH ScrollBarPart -- ^ A part of the horizontal scrollbar.
  | ViewportV ScrollBarPart -- ^ A part of the vertical scrollbar.
  deriving (Eq, Ord, Show)

-- | The pixel width of a scrollbar strip used by 'viewport'.
-- Exported so callers that compose a viewport inside their own layout can
-- account for the strip in their geometry without hard-coding the value.
scrollRegionBarSize :: Double
scrollRegionBarSize = 16

-- | Configuration for 'viewport', set via 'contentSize'.
newtype ViewportConfig = ViewportConfig { viewportConfigContentSize :: Size }

defaultViewportConfig :: ViewportConfig
defaultViewportConfig = ViewportConfig { viewportConfigContentSize = Size 0 0 }

-- | Sets the virtual content size a 'viewport' scrolls over. Scrollbars
-- appear automatically on axes where this exceeds the viewport's own
-- bounds. Defaults to @'Size' 0 0@ (nothing to scroll).
contentSize :: Size -> Attr e ev msg ViewportConfig
contentSize sz = configAny $ \cfg -> cfg { viewportConfigContentSize = sz }

-- | A scrollable window onto a fixed-size virtual content area, set via
-- 'contentSize'. Scrollbars appear automatically on axes where the content
-- exceeds the viewport, and are drawn first so that a drag or click this
-- frame is reflected immediately. The content action then runs with
-- virtual bounds — the full content rectangle translated so the scrolled
-- portion aligns with the viewport — clipped to the visible area. Mouse
-- interaction works naturally because translated bounds are in window
-- coordinates; the clip region hides the rest.
--
viewport :: Ord e => (ViewportPart -> e) -> [Attr e Void msg ViewportConfig] -> UI e msg () -> UI e msg ()
viewport mkId attrs content = do
  outer <- getBounds
  let Size cw ch = viewportConfigContentSize (configure defaultViewportConfig attrs)
      ow      = rectWidth outer
      oh      = rectHeight outer
      -- Two-pass: check V with full height to determine reduced width, then
      -- H, then re-check V with reduced height.
      needsV1 = ch > oh
      vpW1    = if needsV1 then ow - scrollRegionBarSize else ow
      needsH  = cw > vpW1
      vpH     = if needsH  then oh - scrollRegionBarSize else oh
      needsV  = ch > vpH
      vpW     = if needsV  then ow - scrollRegionBarSize else ow
      hRatio  = if needsH then Just (max 0 (min 1 (vpW / cw))) else Nothing
      vRatio  = if needsV then Just (max 0 (min 1 (vpH / ch))) else Nothing
      vpRect  = outer { rectWidth = vpW, rectHeight = vpH }
      hBar    = outer { rectY = rectY outer + vpH, rectHeight = scrollRegionBarSize, rectWidth = vpW }
      vBar    = outer { rectX = rectX outer + vpW, rectWidth  = scrollRegionBarSize, rectHeight = vpH }
  forM_ hRatio $ \r -> withBounds hBar $ scrollBar (mkId . ViewportH) [orientation Horizontal, thumbRatio r]
  forM_ vRatio $ \r -> withBounds vBar $ scrollBar (mkId . ViewportV) [orientation Vertical, thumbRatio r]
  hPos <- maybe (pure 0) (const (getScrollState (mkId (ViewportH ScrollTrack)))) hRatio
  vPos <- maybe (pure 0) (const (getScrollState (mkId (ViewportV ScrollTrack)))) vRatio
  let offsetX    = hPos * max 0 (cw - vpW)
      offsetY    = vPos * max 0 (ch - vpH)
      virtBounds = outer
        { rectX      = rectX outer - offsetX
        , rectY      = rectY outer - offsetY
        , rectWidth  = cw
        , rectHeight = ch
        }
  withBounds vpRect $ clipToCurrent $ withBounds virtBounds content

-- | Renders only the items that fall within the current bounds of a
-- uniform-height list, given the list's current scroll position in pixels.
-- Not a 'control' — no element ID, no attrs, nothing with a sensible
-- default: every argument is essential data the caller must already have,
-- so unlike the rest of this module there's nothing to gain from making any
-- of them attributes.
--
-- Renders one extra item beyond what's fully visible to cover a partially
-- clipped final row. Does not draw a scrollbar or manage scroll state
-- itself — pair with 'scrollBar' and 'getScrollState'.
--
-- @rowHeight@ is clamped to a minimum of @1@ before use, so a caller passing
-- zero or a negative height can't turn the division below into an infinite
-- or wildly oversized render loop.
virtualContent
  :: Double                -- ^ current scroll position, in pixels
  -> Double                -- ^ height of one item, in pixels
  -> Int                   -- ^ total item count
  -> (Int -> UI e msg ())  -- ^ renders the item at the given index
  -> UI e msg ()
virtualContent scrollPos rowHeight0 itemCount renderItem = do
  vp <- getBounds
  let rowHeight = max 1 rowHeight0
      firstIdx  = floor (scrollPos / rowHeight) :: Int
      subOffset = scrollPos - fromIntegral firstIdx * rowHeight
      visibleN  = ceiling ((rectHeight vp + subOffset) / rowHeight) :: Int
  clipToCurrent $ forM_ [0 .. visibleN - 1] $ \j ->
    let i        = firstIdx + j
        itemRect = vp { rectY = rectY vp + fromIntegral j * rowHeight - subOffset, rectHeight = rowHeight }
    in when (i >= 0 && i < itemCount) $ withBounds itemRect (renderItem i)

-- Shared: items and panel -------------------------------------------------

-- | Lets 'items' work across every config with a plain data-item list --
-- currently the configs behind 'itemsLayout', 'compositeControl', and
-- 'selectionControl'.
class HasItemsConfig cfg a where
  setItems :: [a] -> cfg -> cfg

-- | Sets the raw data items, one per element, in order. Defaults to @[]@.
items :: HasItemsConfig cfg a => [a] -> Attr e ev msg cfg
items xs = configAny (setItems xs)

-- | Lets 'itemsPanel' work across every config with a box layout -- see
-- 'Blink.Layout.BoxConfig'. Combined with 'orientation' (to
-- choose 'Blink.Layout.vBox' vs 'Blink.Layout.hBox'), this is the whole of
-- how items are arranged; per-item sizing is a separate, per-item concern
-- -- see 'ItemTemplate'.
class HasItemsPanelConfig cfg where
  setItemsPanel :: BoxConfig -> cfg -> cfg

-- | Sets spacing\/margin\/alignment\/fill-cross for the item arrangement.
-- Defaults to 'Blink.Layout.defaultBoxConfig'.
itemsPanel :: HasItemsPanelConfig cfg => BoxConfig -> Attr e ev msg cfg
itemsPanel p = configAny (setItemsPanel p)

-- ItemsLayout ---------------------------------------------------------

-- | Renders one item of an 'itemsLayout'\/'compositeControl', given its
-- index and value, and the 'Layout' it should occupy -- e.g. a fixed
-- main-axis size for a uniform row height, or 'Fill' to share space equally
-- with the other items. 'itemsPanel'\/'orientation' still govern the
-- overall arrangement and the cross axis (stretched to 'Fill' when
-- @'Blink.Layout.boxFillCross' = 'True'@, the default).
type ItemTemplate e msg a = Int -> a -> (Layout, UI e msg ())

-- | Configuration for 'itemsLayout' and 'compositeControl', set via
-- 'items', 'itemTemplate', 'itemsPanel', and 'orientation'. Defaults to no
-- items, a blank template, and a vertical stack.
data CompositeControlConfig e msg a = CompositeControlConfig
  { compositeControlConfigItems       :: [a]
  , compositeControlConfigTemplate    :: ItemTemplate e msg a
  , compositeControlConfigOrientation :: Orientation
  , compositeControlConfigBoxConfig   :: BoxConfig
  }

defaultCompositeControlConfig :: CompositeControlConfig e msg a
defaultCompositeControlConfig = CompositeControlConfig
  { compositeControlConfigItems       = []
  , compositeControlConfigTemplate    = \_ _ -> (Layout Fill Fill TopLeft, pure ())
  , compositeControlConfigOrientation = Vertical
  , compositeControlConfigBoxConfig   = defaultBoxConfig
  }

instance HasItemsConfig (CompositeControlConfig e msg a) a where
  setItems xs cfg = cfg { compositeControlConfigItems = xs }

instance HasItemsPanelConfig (CompositeControlConfig e msg a) where
  setItemsPanel p cfg = cfg { compositeControlConfigBoxConfig = p }

instance HasOrientationConfig (CompositeControlConfig e msg a) where
  setOrientation o cfg = cfg { compositeControlConfigOrientation = o }

-- | Sets how each data item is rendered -- the DataTemplate. Defaults to
-- rendering nothing.
itemTemplate :: ItemTemplate e msg a -> Attr e ev msg (CompositeControlConfig e msg a)
itemTemplate f = configAny $ \cfg -> cfg { compositeControlConfigTemplate = f }

-- | Arranges 'items' via 'itemTemplate', stacked according to
-- 'orientation' and 'itemsPanel'. Shared by 'itemsLayout' and
-- 'compositeControl'.
renderCompositeItems :: CompositeControlConfig e msg a -> UI e msg ()
renderCompositeItems cfg = arrange (compositeControlConfigBoxConfig cfg)
  [ compositeControlConfigTemplate cfg idx item
  | (idx, item) <- zip [0 ..] (compositeControlConfigItems cfg)
  ]
  where
    arrange = case compositeControlConfigOrientation cfg of
      Horizontal -> hBox
      Vertical   -> vBox

-- | Renders each item of 'items' via 'itemTemplate', stacked according to
-- 'orientation' and 'itemsPanel'. A primitive, not a
-- control -- see the module header.
--
-- @
-- itemsLayout
--   [ items [Small, Medium, Large]
--   , itemTemplate $ \\_ sz -> (Layout Fill Fill TopLeft, drawText black AlignLeft (describe sz))
--   ]
-- @
itemsLayout :: [Attr e Void msg (CompositeControlConfig e msg a)] -> UI e msg ()
itemsLayout attrs = renderCompositeItems (configure defaultCompositeControlConfig attrs)

-- | Events reported by 'compositeControl': a lifecycle event via
-- @CompositeControlEvent@ (see 'ControlEvent'). A composite has no domain
-- events of its own beyond the generic ones -- item-specific behaviour
-- (like 'selectionControl''s 'SelectionEvent') is layered on top.
newtype CompositeEvent = CompositeControlEvent ControlEvent
  deriving (Eq, Show)

instance HasControlEvent CompositeEvent where
  liftControl = CompositeControlEvent
  matchControl (CompositeControlEvent ce) = Just ce

-- | The entry point for composite controls (radio groups, lists, trees):
-- an 'itemsLayout' with an element id, so it gets normal mouse-over,
-- style-driven chrome, and Tab\/Shift-Tab navigation as a single unit via
-- 'withinComposite' -- a child that takes focus inside stays scoped to this
-- composite's own Tab order until it's left. No bespoke focus code lives
-- here; see 'withinComposite' for how a composite's focus is claimed,
-- retained, and released.
compositeControl :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg (CompositeControlConfig e msg a)] -> UI e msg ()
compositeControl eid attrs = do
  applyMouseOver eid attrs
  renderChrome eid $ withinComposite eid $
    renderCompositeItems (configure defaultCompositeControlConfig attrs)
  applyFocus eid attrs

-- SelectionControl -------------------------------------------------------

-- | Whether an item is currently the selected one.
data SelectionState = Selected | Unselected
  deriving (Eq, Show)

-- | Which item, if any, is selected -- by value, by position, or none. Set
-- via 'selected' or 'selectedIndex'.
data SelectedItem a = None | Item a | ItemAtIndex Int
  deriving (Eq, Show)

-- | Renders one item of a 'selectionControl': its element id (used only
-- for click detection -- items never take focus), current 'SelectionState',
-- and value -- returning the 'Layout' it should occupy, same as
-- 'ItemTemplate'.
type SelectionItemTemplate e msg a = e -> SelectionState -> a -> (Layout, UI e msg ())

-- | Fired when a click lands on an item, carrying its index and value.
data SelectionEvent a = Activated Int a
  deriving (Eq, Show)

-- | Runs a reaction with the activated item's index and value on every
-- 'Activated'.
onSelect :: (Int -> a -> [Out e msg]) -> Attr e (SelectionEvent a) msg cfg
onSelect reaction = onEvent $ \ev -> case ev of
  Activated idx val -> reaction idx val

-- | Configuration for 'selectionControl', set via 'items', 'itemsPanel',
-- 'orientation', 'itemContainer', and
-- 'selected'\/'selectedIndex'. Defaults to no items, nothing selected, a
-- blank template, and a vertical stack.
data SelectionConfig e msg a = SelectionConfig
  { selectionConfigItems       :: [a]
  , selectionConfigSelection   :: SelectedItem a
  , selectionConfigTemplate    :: SelectionItemTemplate e msg a
  , selectionConfigOrientation :: Orientation
  , selectionConfigBoxConfig   :: BoxConfig
  }

defaultSelectionConfig :: SelectionConfig e msg a
defaultSelectionConfig = SelectionConfig
  { selectionConfigItems       = []
  , selectionConfigSelection   = None
  , selectionConfigTemplate    = \_ _ _ -> (Layout Fill Fill TopLeft, pure ())
  , selectionConfigOrientation = Vertical
  , selectionConfigBoxConfig   = defaultBoxConfig
  }

instance HasItemsConfig (SelectionConfig e msg a) a where
  setItems xs cfg = cfg { selectionConfigItems = xs }

instance HasItemsPanelConfig (SelectionConfig e msg a) where
  setItemsPanel p cfg = cfg { selectionConfigBoxConfig = p }

instance HasOrientationConfig (SelectionConfig e msg a) where
  setOrientation o cfg = cfg { selectionConfigOrientation = o }

-- | Sets how each item is rendered, given its element id, 'SelectionState',
-- and value -- see 'SelectionItemTemplate'. Defaults to rendering nothing.
itemContainer :: SelectionItemTemplate e msg a -> Attr e ev msg (SelectionConfig e msg a)
itemContainer f = configAny $ \cfg -> cfg { selectionConfigTemplate = f }

-- | Selects by value: an item is 'Selected' when it equals @v@. Defaults
-- to 'None'.
selected :: a -> Attr e ev msg (SelectionConfig e msg a)
selected v = configAny $ \cfg -> cfg { selectionConfigSelection = Item v }

-- | Selects by position: the item at index @i@ is 'Selected'. Defaults to
-- 'None'.
selectedIndex :: Int -> Attr e ev msg (SelectionConfig e msg a)
selectedIndex i = configAny $ \cfg -> cfg { selectionConfigSelection = ItemAtIndex i }

-- | Renders 'items' via 'itemContainer', each resolved against 'selected'
-- \/'selectedIndex' into a 'SelectionState', arranged by
-- 'orientation'\/'itemsPanel' (built on 'itemsLayout').
-- Detects a click on any item -- no focus, no keyboard navigation -- and
-- fires 'Activated' with that item's index and value via 'onSelect';
-- changing the selection is the caller's own responsibility, by feeding a
-- new 'selected'\/'selectedIndex' back in from its reaction. Draws no
-- chrome of its own -- see the module header.
--
-- @
-- data Element = SizeItem Int
--
-- selectionControl SizeItem
--   [ items [Small, Medium, Large]
--   , selected (currentSize model)
--   , onSelect (\\_ sz -> post (SetSize sz))
--   , itemContainer $ \\_ st sz ->
--       ( Layout Fill Fill TopLeft
--       , drawText black AlignLeft ((if st == Selected then "> " else "") \<\> describe sz)
--       )
--   ]
-- @
selectionControl
  :: (Ord e, Eq a)
  => (Int -> e)
  -> [Attr e (SelectionEvent a) msg (SelectionConfig e msg a)]
  -> UI e msg ()
selectionControl mkId attrs = do
  let cfg      = configure defaultSelectionConfig attrs
      itemList = selectionConfigItems cfg
      sel      = selectionConfigSelection cfg
      stateAt idx val = case sel of
        None          -> Unselected
        Item v        -> if v == val then Selected else Unselected
        ItemAtIndex i -> if i == idx then Selected else Unselected

  itemsLayout
    [ items itemList
    , orientation (selectionConfigOrientation cfg)
    , itemsPanel (selectionConfigBoxConfig cfg)
    , itemTemplate $ \idx val ->
        let eid               = mkId idx
            (layout, content) = selectionConfigTemplate cfg eid (stateAt idx val) val
            wrapped = do
              clicked <- isClickedOver eid
              when clicked $ fire attrs [Activated idx val]
              content
        in (layout, wrapped)
    ]

