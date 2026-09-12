{- |
Module: Blink.View

= The View monad

'View' is the core abstraction in Blink: a state-threading computation
parameterised over an /element type/ @e@ and a /message type/ @msg@.

@
newtype View e msg a = View { runView :: ViewContext e msg -> IO (a, ViewContext e msg) }
@

The computation runs in 'IO' only because 'TextMeasurer' (see /Text
measurement/ below) queries the backend's real font metrics; nothing else in
the view tree touches 'IO'.

Composing 'View' actions with '>>=', '>>' and 'mapM_' builds a view tree. Each
node in the tree reads from the shared 'ViewContext' (bounds, input, theme, focus
state) and may append draw commands to it or queue application state changes.

= Element identity

Every interactive control is identified by a value of type @e@, typically a
sum type with one constructor per control:

@
data MyElem = OkButton | CancelButton | NameInput
  deriving (Eq, Ord)
@

Element IDs are used to look up styles from the active 'Blink.Style.Theme', to track hover
and press state within a frame, and to route keyboard events to the focused
control.

= Messages

Application state is not part of the context at all: views are pure
functions of a model value supplied by the host, and controls report changes
by queuing a @msg@ value with 'emit' rather than mutating anything. The host
reads the queued messages back with 'getMessages' once the frame completes
and folds them into its own state via "Blink.Update".

= Effects and settling

A control queues two kinds of thing during a frame, both riding in the
same 'Effect' queue: a @msg@ via 'emit', for the application, and a
'UiEffect' via 'emitUi', for Blink's own presentation state, never seen by
the application. 'Effect' is opaque outside "Blink.View.Context" — a
handler builds its @['Effect' e msg]@ result with
'Blink.Controls.Control.post' or 'Blink.Controls.Control.postWith' (see
'Blink.Controls.Table.onColumnSortRequested' for an example), and can
queue both kinds from the same reaction: the message that tells the
application what happened, and an effect reacting to it on the
presentation side.

'emitUi' only appends a 'UiEffect' to the queue; it does not change the
running 'ViewContext'. 'nextFrameContext' (or, mid-frame,
'rerenderContext') applies every queued effect via @applyUiEffects@ before
the next render starts — this is "settling". A control reading its own
state back ('getScrollState', 'getFocus', 'getSelection') always sees the last settled value,
never a write some other control queued moments earlier in the same
frame. 'settleEffects' and 'settleAndClearEffects' expose the same
operation directly, for a caller that needs a queued effect applied
without advancing every other part of the frame the way a real render
does.

Queue an effect only as a direct reaction to something happening this
frame — a click, a key press, a drag — carrying that decision forward as
state other code can read back as settled and stable. A focus change
requested via 'requestFocus' is this kind of thing, and so is a
scrollbar's position while it is being dragged: both persist until
something later decides otherwise.

Not every write belongs on that queue. Something fully determined by this
frame's own inputs — the current item order, the current selection, the
current viewport — carries no decision worth deferring, because
recomputing it from scratch always gives the same answer.
'Blink.Controls.List.scrollRowIntoView' is this kind of thing: it corrects
the scroll position immediately with 'setScrollStateNow' rather than
queuing it, because keeping the selection visible is arithmetic on the
current frame, not a decision anything else needs to read back.

= Focus, scroll, hold, and selection

Some controls carry presentation state that is no business of the
application — which element holds keyboard focus, a scrollbar's position,
whether a repeat button is still being held down, a text input's cursor.
This state is baked directly into the 'ViewContext': focus lives in the
interaction state, and scroll positions (@elmScrollStates@, a @Map e
'ScrollState'@) and repeat-press state (@elmHoldStates@, a @Map e
'Blink.View.Hold.HoldState'@) live in the element state, both keyed by element ID,
populating lazily on first write and persisting across frames. Selection
(@elmSelection@, a @SelectionSlot@) holds just one 'Selection' at a time,
tagged with the element it belongs to, since only the focused control ever
has one; writing a new element's selection replaces whichever one was there
before. The application never sees any of this traffic.

Focus ('setFocus', 'clearFocus') changes immediately, exactly like
'registerMouseOver' and mouse capture, because sibling arbitration within a
single tree walk depends on it (see the
<https://github.com/BenjaminGale/blink/blob/main/docs/concepts/01-immediate-mode-api/05-focus.md concepts guide's section on focus>
for why). Scroll, hold, and selection have no such sibling-arbitration
requirement, so they queue a 'UiEffect' with 'emitUi' instead of mutating
immediately; a write made partway through a frame is not visible to a read
later in that same frame. 'nextFrameContext' applies the queued effects via
@applyUiEffects@ when building the next frame's context, so the change
takes effect starting then.

= The render loop

Each frame follows the same three steps:

>  1. buildCtx  ---->  2. runView  ---->  3. extract
>  (emptyViewContext /       (walk the           (getDrawCommands,
>   nextFrameContext)       view tree)            getMessages)

  1. Build a fresh 'ViewContext' with 'emptyViewContext' (first frame) or advance an
     existing one with 'nextFrameContext'.
  2. Run the view tree via 'runView'.
  3. Pass the resulting context to 'getDrawCommands' to obtain the renderer
     input, and to 'getMessages' to advance the application state.

The 'TextMeasurer' re-exported from "Blink.Rendering" is threaded through
'emptyViewContext' at step 1; see /Text measurement/ below for how controls use
it during step 2.

= Animation

Two frame kinds drive the loop: those triggered by a platform input event
(mouse, keyboard) and those triggered by an animation ticker running on a
fixed interval. 'AnimationState' — set by the backend, not application code —
records which kind the current frame is and how much wall-clock time has
passed.

A component that animates (a progress spinner, say) calls 'requiresAnimation'
unconditionally on every frame it is visible, to keep the ticker alive, and
wraps the code that advances its animation state in 'withAnimationFrame' so
that advance happens once per tick rather than once per input event too:

@
withAnimationFrame $ do
  dt <- getAnimDelta
  emit (AdvanceSpinner dt)
requiresAnimation
@

= Drawing

'Blink.View.Drawing.fillRect', 'Blink.View.Drawing.strokeRect', and
'Blink.View.Drawing.drawText' all operate on the /current bounds/ returned
by 'getBounds'. 'withBounds' temporarily replaces the current bounds for a
sub-tree — used internally by the layout system.
'Blink.View.Drawing.withClip' wraps a sub-tree in a clip region matching
the current bounds; drawing outside the region is discarded.

= Interaction

Interaction queries are scoped to an element ID. 'registerMouseOver' \/
'wasMouseOverLastFrame' \/ 'isAnyMouseOver' let any number of elements
independently register and query "over" this frame, with no shared slot to
contend over — this is what 'Blink.Controls.control' uses.

'isRegionHit' is the lower-level primitive this builds on: it checks whether
the mouse is within the /current bounds/, without reference to any element ID.

= Focus and keyboard navigation

Focus is tracked per scope, not as a single flat element: root owns one
'FocusState', and every composite (a list, a tree — anything with
sub-items) owns its own, persisted in @ftScopes@ — a @Map e
'FocusState'@, keyed by scope id the same way @elmScrollStates@ is keyed by
element id. 'withFocusScope' is where a composite swaps the ambient scope
for its own while its children render, and folds the result back.

  * 'isFocused' — a single-hop check against whichever scope is currently
    ambient. A leaf checking its own id gets a plain exact-match; a
    composite checking its own id gets CSS's @:focus-within@ for free,
    because 'withFocusScope' is what makes the composite's own id read as
    ambiently focused whenever a descendant is — no chain-walk needed here.
  * 'setFocus' \/ 'clearFocus' — set or clear the ambient scope's focused
    element directly.
  * 'consumeKey' — remove a key event from the frame's queue so that it is not
    handled by multiple controls in the same frame.

Tab and Shift-Tab navigation between controls is managed automatically by
'Blink.Controls.control'.

= Styles

'getStyleSet' returns all style variants for a 'Blink.Style.StyleKey'
(normal, hovered, pressed, focused, disabled); a control resolves the
active variant from its current interaction state and makes it available to
its own content via 'currentStyle'.

= Text measurement

'Blink.View.Drawing.drawText' renders whatever text it is given without needing to know its
pixel size. Controls that must — placing a cursor, computing where a click
landed, sizing a box to fit its label — go through the backend's
'TextMeasurer' instead, via 'charOffset', 'charAtOffset', and 'measureText'.
These wrap the raw 'TextMeasurer' functions so callers never touch
@ctxTextMeasure@ directly.

= Disabled state

'disableWhen' marks an entire sub-tree as disabled. Disabled controls render
normally but ignore all input. 'whenEnabled' is a guard that skips its body
when disabled.

= Putting it together

Higher-level controls in "Blink.Controls" are built entirely from the
primitives above, using the geometric hover model. A minimal button,
stripped of styling and focus handling, shows how the pieces interlock:

@
miniButton :: Ord e => e -> Text -> View e msg Bool
miniButton eid label = do
  isHit <- isRegionHit
  when isHit $ registerMouseOver eid
  fillRect (if isHit then RGBA 0.3 0.3 0.3 1 else RGBA 0.2 0.2 0.2 1)
  drawText (RGBA 1 1 1 1) AlignCenter label
  released <- isButtonReleased
  pure (isHit && released)
@

'registerMouseOver' records the hit so a later frame can look back at it via
'wasMouseOverLastFrame'; 'Blink.View.Drawing.fillRect' and
'Blink.View.Drawing.drawText' read the current bounds implicitly. See
'Blink.Controls.control' for the full version, which adds focus, tab
navigation, and style-driven chrome on top of exactly this shape.

= Module organisation

This module is a thin re-exporting shell over "Blink.View.Context" (the
monad, the render loop, and the state types 'ViewContext' embeds — focus,
scroll, selection, hold, animation, navigation) and the feature modules
built on it: "Blink.View.Mouse" (position, buttons, capture, hover,
occlusion), "Blink.View.Focus" (focus queries, claim\/clear, nested
scopes), "Blink.View.Scroll", "Blink.View.Extent", "Blink.View.Cursor",
"Blink.View.Selection", "Blink.View.Hold", "Blink.View.Animation", and
"Blink.View.Navigation" (each pairing its pure type with the monadic
accessors built on top of it). Import this module rather than any of
those directly.
-}
module Blink.View
  ( -- * The View monad
    View
  , runView
  , ViewContext
    -- * Re-export for convenience
    -- | From "Blink.Rendering"; re-exported since 'emptyViewContext' takes a
    -- 'TextMeasurer' and 'noOpTextMeasurer' is the usual choice outside a
    -- real backend (tests, headless rendering).
  , TextMeasurer (..)
  , noOpTextMeasurer
    -- * The render loop
  , emptyViewContext
  , nextFrameContext
  , rerenderContext
  , getDrawCommands
  , getMessages
  , hasPendingUiEffects
  , settleEffects
  , settleAndClearEffects
  , contextRequiresAnimation
    -- * Messages
  , Effect
  , UiEffect
  , emit
  , emitUi
    -- * Scroll state
    -- | 'ScrollState' and 'clampScrollPos' live in "Blink.View.Scroll";
    -- re-exported here since a 'ScrollState' is threaded through
    -- 'ViewContext'.
  , ScrollState
  , getScrollState
  , clampScrollPos
  , contextScrollState
  , requestScrollTo
  , requestScrollBy
  , postScrollBy
  , setScrollStateNow
    -- * Extent state
    -- | 'ExtentState' lives in "Blink.View.Extent"; re-exported here
    -- since it's threaded through 'ViewContext' the same way
    -- 'ScrollState' is.
  , ExtentState
  , getExtentState
  , contextExtentState
  , requestExtentBy
    -- * Cursor index state
    -- | 'CursorIndexState' lives in "Blink.View.Context"; internal to
    -- 'Blink.Controls.List.listBase', re-exported here the same way
    -- 'ExtentState' is.
  , CursorIndexState
  , getCursorIndex
  , contextCursorIndex
  , setCursorIndex
    -- * Repeat-press ("hold") state
  , resolveHoldRepeats
    -- * Selection
    -- | 'Selection' itself, and the pure helpers built on it
    -- ('Blink.View.Selection.selectionLow', 'Blink.View.Selection.cursor',
    -- etc.), live in "Blink.View.Selection"; re-exported here since a
    -- 'Selection' is threaded through 'ViewContext'.
  , Selection (..)
  , getSelection
  , contextSelection
  , requestSelectionAt
    -- * Bounds
  , getBounds
  , getWindowSize
  , withBounds
    -- * Drawing
    -- | Minimal primitives; see "Blink.View.Drawing" for
    -- 'Blink.View.Drawing.fillRect', 'Blink.View.Drawing.strokeRect',
    -- 'Blink.View.Drawing.drawText', 'Blink.View.Drawing.withClip',
    -- 'Blink.View.Drawing.withBackground', and
    -- 'Blink.View.Drawing.withBorder', built on top of these.
  , draw
  , getInteractionClip
  , withInteractionClip
    -- * Interaction
  , getInput
  , contextInput
  , getMousePos
  , getWheelDelta
  , isRegionHit
  , acquireCapture
  , registerMouseOver
  , wasMouseOverLastFrame
  , isAnyMouseOver
  , registerHitRect
  , isOccludedFor
  , isButtonDown
  , isButtonReleased
  , isDragging
  , isMouseFree
  , MouseCapture (..)
  , getCaptured
  , contextCaptured
  , contextButtonDown
  , contextButtonReleased
  , getMouse
  , contextMouse
    -- * Focus and keyboard navigation
  , FocusState (previousTabStop)
  , FocusClaim (..)
  , currentFocus
  , isNothingFocused
  , getFocus
  , isFocused
  , hasGainedFocus
  , hasLostFocus
  , setFocus
  , setFocusWhen
  , clearFocus
  , disclaimFocus
  , requestFocus
  , requestClearFocus
  , FreshClaim (..)
  , withFocusScope
  , consumeKey
  , withoutKeyEvents
  , getPreviousTabStop
  , setPreviousTabStop
  , contextFocus
  , contextFocusChain
  , contextPreviousTabStop
    -- | 'NavigationKeys' and 'defaultNavigationKeys' live in
    -- "Blink.View.Navigation"; re-exported here since a 'NavigationKeys'
    -- is threaded through 'ViewContext'.
  , NavigationKeys (..)
  , defaultNavigationKeys
  , getNavigationKeys
  , withNavigationKeys
  , getCurrentScope
    -- * Styles
  , getStyleSet
  , getMetrics
  , contextTheme
  , currentStyle
  , withStyle
  , currentMetrics
  , withMetrics
    -- * Text measurement
  , charOffset
  , charAtOffset
  , measureText
    -- * Disabled state
  , isDisabled
  , disableWhen
  , whenEnabled
    -- * Animation
    -- | 'AnimationState' and 'mkAnimationState' live in
    -- "Blink.View.Animation"; re-exported here since an 'AnimationState'
    -- is threaded through 'ViewContext'.
  , AnimationState (animDelta, animElapsed, animIsTick)
  , mkAnimationState
  , requiresAnimation
  , withAnimationFrame
  , getAnimDelta
  , getAnimElapsed
  , contextAnimation
  ) where

import Blink.Rendering (TextMeasurer (..), noOpTextMeasurer)
import Blink.Input (MouseCapture (..))
import Blink.View.Context
import Blink.View.Mouse
import Blink.View.Focus
import Blink.View.Scroll
import Blink.View.Extent
import Blink.View.Cursor
import Blink.View.Selection
import Blink.View.Hold
import Blink.View.Animation
import Blink.View.Navigation
