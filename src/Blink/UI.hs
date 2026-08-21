{- |
Module: Blink.UI

= The UI monad

'UI' is the core abstraction in Blink: a state-threading computation
parameterised over an /element type/ @e@ and a /message type/ @msg@.

@
newtype UI e msg a = UI { runUI :: UIContext e msg -> IO (a, UIContext e msg) }
@

The computation runs in 'IO' only because 'TextMeasurer' (see /Text
measurement/ below) queries the backend's real font metrics; nothing else in
the UI tree touches 'IO'.

Composing 'UI' actions with '>>=', '>>' and 'mapM_' builds a UI tree. Each
node in the tree reads from the shared 'UIContext' (bounds, input, theme, focus
state) and may append draw commands to it or queue application state changes.

= Element identity

Every interactive control is identified by a value of type @e@, typically a
sum type with one constructor per control:

@
data MyElem = OkButton | CancelButton | NameInput
  deriving (Eq, Ord)
@

Element IDs are used to look up styles from the active 'Theme', to track hover
and press state within a frame, and to route keyboard events to the focused
control.

= Messages

Application state is not part of the context at all: views are pure
functions of a model value supplied by the host, and controls report changes
by queuing a @msg@ value with 'emit' rather than mutating anything. The host
reads the queued messages back with 'getMessages' once the frame completes
and folds them into its own state via "Blink.Update".

= Focus, scroll, and selection

Some controls carry presentation state that is no business of the
application — which element holds keyboard focus, a scrollbar's position, a
text input's cursor. This state is baked directly into the 'UIContext':
focus lives in the interaction state, and scroll positions (@elmScrollStates@, a
@Map e 'ScrollState'@) and selections (@elmSelections@, a @Map e
['Selection']@) live in the element state. Both maps are keyed by element ID,
populate lazily on first write, and persist across frames; the application
never sees the traffic.

Focus ('setFocus', 'clearFocus') changes immediately, exactly like
'registerMouseOver' and mouse capture: a control's decision (take focus when
nothing else has it, hand off on Tab) is only correct if the next sibling in
the same tree walk can see it, the same way capture arbitration needs a
later hoverer to see that an earlier one already has the mouse. Scroll and
selection have no such sibling-arbitration requirement — each write targets
a specific element nobody else is contending for — so they queue a
'UiEffect' with 'emitUi' instead of mutating immediately; a write made
partway through a frame is not visible to a read later in that same frame.
'nextFrameContext' applies the queued effects via 'applyUiEffects' when
building the next frame's context, so the change takes effect starting then.

= The render loop

Each frame follows the same three steps:

>  1. buildCtx  ---->  2. runUI  ---->  3. extract
>  (emptyUIContext /       (walk the           (getDrawCommands,
>   nextFrameContext)       UI tree)            getMessages)

  1. Build a fresh 'UIContext' with 'emptyUIContext' (first frame) or advance an
     existing one with 'nextFrameContext'.
  2. Run the UI tree via 'runUI'.
  3. Pass the resulting context to 'getDrawCommands' to obtain the renderer
     input, and to 'getMessages' to advance the application state.

The 'TextMeasurer' re-exported from "Blink.Rendering" is threaded through
'emptyUIContext' at step 1; see /Text measurement/ below for how controls use
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

'fillRect', 'strokeRect', and 'drawText' all operate on the /current bounds/
returned by 'getBounds'. 'withBounds' temporarily replaces the current bounds
for a sub-tree — used internally by the layout system. 'clipToCurrent' wraps a
sub-tree in a clip region matching the current bounds; drawing outside the
region is discarded.

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

'getStyleSet' returns all style variants for an element (normal, hovered,
pressed, focused, disabled); see 'Blink.Controls.getStyle' for how a control
resolves the active variant from its current interaction state.

= Text measurement

'drawText' renders whatever text it is given without needing to know its
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
miniButton :: Ord e => e -> Text -> UI e msg Bool
miniButton eid label = do
  isHit <- isRegionHit
  when isHit $ registerMouseOver eid
  fillRect (if isHit then RGBA 0.3 0.3 0.3 1 else RGBA 0.2 0.2 0.2 1)
  drawText (RGBA 1 1 1 1) AlignCenter label
  released <- isButtonReleased
  pure (isHit && released)
@

'registerMouseOver' records the hit so a later frame can look back at it via
'wasMouseOverLastFrame'; 'fillRect' and 'drawText' read the current bounds
implicitly. See 'Blink.Controls.control' for the full version, which adds
focus, tab navigation, and style-driven chrome on top of exactly this shape.
-}
module Blink.UI
  ( -- * The UI monad
    UI
  , runUI
  , UIContext
    -- * Re-export for convenience
    -- | From "Blink.Rendering"; re-exported since 'emptyUIContext' takes a
    -- 'TextMeasurer' and 'noOpTextMeasurer' is the usual choice outside a
    -- real backend (tests, headless rendering).
  , TextMeasurer (..)
  , noOpTextMeasurer
    -- * The render loop
  , emptyUIContext
  , nextFrameContext
  , rerenderContext
  , getDrawCommands
  , getMessages
  , getUiEffects
  , applyUiEffects
  , contextRequiresAnimation
    -- * Messages
  , Out (..)
  , UiEffect (..)
  , emit
  , emitUi
    -- * Scroll state
  , ScrollState
  , getScrollState
  , clampScrollPos
  , contextScrollPosition
    -- * Selections
  , Selection (..)
  , getSelections
  , getSelection
  , contextSelections
  , contextSelection
  , selectionLow
  , selectionHigh
  , selectionHasExtent
  , cursor
  , collapseToLow
  , collapseToHigh
  , collapseToActive
  , extendActive
    -- * Bounds
  , getBounds
  , withBounds
    -- * Drawing
  , fillRect
  , strokeRect
  , drawText
  , clipToCurrent
  , withBackground
  , withBorder
    -- * Interaction
  , getInput
  , contextInput
  , getMousePos
  , isRegionHit
  , acquireCapture
  , registerMouseOver
  , wasMouseOverLastFrame
  , isAnyMouseOver
  , isButtonDown
  , isButtonReleased
  , isDragging
  , isMouseFree
  , MouseCapture (..)
  , getCapturedElement
  , contextCaptured
  , contextButtonDown
  , contextButtonReleased
    -- * Focus and keyboard navigation
  , FocusState (focusedElement, previousTabStop)
  , isNothingFocused
  , getFocus
  , isFocused
  , setFocus
  , setFocusWhen
  , clearFocus
  , withFocusScope
  , consumeKey
  , getPreviousTabStop
  , setPreviousTabStop
  , contextFocus
  , contextFocusChain
  , contextPrevTabStop
    -- * Styles
  , getStyleSet
  , contextTheme
    -- * Text measurement
  , charOffset
  , charAtOffset
  , measureText
    -- * Disabled state
  , isDisabled
  , disableWhen
  , whenEnabled
    -- * Animation
  , AnimationState (animDelta, animElapsed, animIsTick)
  , mkAnimationState
  , requiresAnimation
  , withAnimationFrame
  , getAnimDelta
  , getAnimElapsed
  , contextAnimation
  ) where

import Control.Monad (when, unless)
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Blink.Rendering (Colour (..), isVisible, TextAlign (..), DrawCommand (..), TextMeasurer (..), noOpTextMeasurer)
import Blink.Geometry (Point, Rectangle, Size, BorderEdges, containsPoint, intersectRect)
import Blink.Input (Key (..), KeyEvent (..), InputState (..))
import Blink.Style (StyleSet (..), Theme (..))

-- | Per-frame animation state threaded through the 'UIContext'. Set by the
-- backend at the start of each frame; read by 'withAnimationFrame' and
-- 'getAnimDelta'.
data AnimationState = AnimationState
  { animDelta   :: Float
    -- ^ Wall-clock seconds elapsed since the previous frame, clamped to
    -- 100 ms. Zero on the first frame.
  , animElapsed :: Float
    -- ^ Total wall-clock seconds elapsed since the application started,
    -- accumulated from 'animDelta' each frame.
  , animIsTick  :: Bool
    -- ^ 'True' when this frame was triggered by the animation ticker rather
    -- than a platform input event.
  }

-- | Constructs an 'AnimationState', clamping the delta to 100 ms so the
-- bound documented on 'animDelta' holds regardless of caller — the
-- constructor itself isn't exported, so this is the only way to build one.
mkAnimationState :: Float -> Float -> Bool -> AnimationState
mkAnimationState delta elapsed isTick = AnimationState
  { animDelta   = min 0.1 delta
  , animElapsed = elapsed
  , animIsTick  = isTick
  }

-- | Per-instance scroll position in @[0, 1]@.
newtype ScrollState = ScrollState { scrollPosition :: Double }
  deriving (Eq, Ord, Show)

-- | A contiguous selection or cursor within a linear sequence. The selected
-- range is @(min anchor active, max anchor active)@. When @anchor == active@
-- the selection is a cursor with no extent.
data Selection = Selection
  { selectionAnchor :: Int  -- ^ The fixed end.
  , selectionActive :: Int  -- ^ The moving end (cursor position).
  }
  deriving (Eq, Show)

-- | A cross-frame presentation effect: a scroll or selection change that
-- takes effect starting the next frame rather than immediately. Queued with
-- 'emitUi' and applied by 'applyUiEffects', which 'nextFrameContext' runs
-- automatically between frames. Focus is not represented here — see
-- 'setFocus' for why it changes immediately instead.
data UiEffect e
  = ScrollTo e Double
    -- ^ Sets the scroll position to an absolute value, clamped to @[0, 1]@
    -- by 'applyUiEffects' when the effect is applied. Every caller
    -- ('Blink.Controls.scrollBar', 'Blink.Controls.textInputControl')
    -- already passes a value in the @[0, 1]@ convention documented on
    -- 'ScrollState'; 'Blink.Controls.textInputControl' converts to and from
    -- pixels locally since its selection\/cursor math is naturally
    -- pixel-based.
  | ScrollBy e Double
    -- ^ Adjusts the scroll position by a delta, clamped to @[0, 1]@ — this
    -- constructor is only ever used in the normalised @[0, 1]@ convention.
    -- Composes with other 'ScrollBy' effects queued in the same frame for
    -- the same element rather than last-write-wins.
  | SetSelectionAt e Selection
  deriving (Eq, Show)

-- | One item from the frame's output queue: either an application message or
-- a 'UiEffect'. A single ordered queue holds both so that the relative
-- ordering between a message and an effect emitted in the same frame is
-- preserved.
data Out e msg
  = OutMsg msg
  | OutUi (UiEffect e)
  deriving (Eq, Show)

-- | Per-scope focus bookkeeping: which single child (if any) currently holds
-- focus within this scope, the last tab stop visited within it (for
-- Shift-Tab), and whether this scope's claim was reaffirmed during the
-- current frame's render pass. Root and every composite (a list, a tree —
-- anything with sub-items, see 'withFocusScope') each own one of these; a
-- composite's own is persisted in @ftScopes@ between frames, the same
-- way scroll and selection state persist per element.
data FocusState e = FocusState
  { focusedElement   :: Maybe e
    -- ^ The element this scope currently has focused, if any.
  , previousTabStop  :: Maybe e
    -- ^ The element visited just before the current one, scoped to this
    -- level, for Shift-Tab.
  , focusedThisFrame :: Bool
    -- ^ 'True' if this scope's claim was reaffirmed during this frame's
    -- render pass. Used to clear stale focus when whatever held it is no
    -- longer present in the UI.
  }

-- | The empty, never-focused 'FocusState' — root's initial value, and every
-- composite's the first time it renders.
emptyFocusState :: FocusState e
emptyFocusState = FocusState { focusedElement = Nothing, previousTabStop = Nothing, focusedThisFrame = False }

-- | 'True' when nothing is focused in the given scope. Pattern-matches
-- directly rather than requiring @Eq e@, so it's usable wherever a plain
-- 'Bool' guard is more convenient than matching by hand.
isNothingFocused :: Maybe e -> Bool
isNothingFocused Nothing  = True
isNothingFocused (Just _) = False

-- | Which element, if any, holds mouse capture during a drag. A control
-- acquires capture on press so that it keeps receiving drag input even once
-- the cursor leaves its bounds; see 'acquireCapture'.
data MouseCapture e
  = MouseNotCaptured
  | MouseCapturedBy e
  deriving (Eq, Show)

-- | The left mouse button's state for the current frame, together with
-- which element (if any) holds mouse capture. 'ButtonReleased' lasts for
-- exactly one frame — the frame the button goes from held to up — so that a
-- control can tell "the button just came up" (fire a click) apart from "the
-- button has been up for a while" ('ButtonUp'). The frame after a release,
-- state falls back to 'ButtonUp' even though the raw held/not-held reading
-- hasn't changed since the release frame.
--
-- Capture can only be held while the button is down, or through the single
-- frame it's released on — never while 'ButtonUp' — which is why it lives
-- inside 'ButtonDown' and 'ButtonReleased' rather than as a separate field:
-- a captured element while the button has been up for a while is not a
-- state that should be representable.
data ButtonState e
  = ButtonUp
    -- ^ Not held, and didn't just come up this frame. Never carries capture.
  | ButtonDown (MouseCapture e)
    -- ^ Held, whether this is the first frame of the press or a later one —
    -- nothing distinguishes those two for controls, which only ever care
    -- whether the button is currently held.
  | ButtonReleased (MouseCapture e)
    -- ^ Came up this frame, having been held the frame before. Still
    -- carries whatever captured the press, so a control can distinguish a
    -- drag-release from a plain click.
  deriving (Eq, Show)

-- | The capture carried by a 'ButtonState', or 'MouseNotCaptured' when the
-- button is up.
captureOf :: ButtonState e -> MouseCapture e
captureOf ButtonUp             = MouseNotCaptured
captureOf (ButtonDown cap)     = cap
captureOf (ButtonReleased cap) = cap

-- | Per-frame keyboard-focus targeting state: which element has focus, and
-- which was the most recent tab stop. Reset and carried forward by
-- 'nextFrameContext'. Unlike mouse\/capture state, focus also advances on a
-- re-render of the same frame (see 'rerenderContext'), since a scope that
-- goes unclaimed on a re-render should expire even though the button
-- reading hasn't changed.
data FocusTracker e = FocusTracker
  { ftAmbient :: FocusState e
    -- ^ The *currently ambient* scope's own focus state — root's, unless a
    -- 'withFocusScope' call further up the stack has swapped it for a
    -- composite's own.
  , ftScopes  :: Map.Map e (FocusState e)
    -- ^ Every composite's own persisted 'FocusState', flat, keyed directly
    -- by scope id regardless of nesting depth — the same shape as
    -- 'elmScrollStates'. See 'withFocusScope'.
  }

-- | The empty 'FocusTracker' — nothing focused anywhere, no composite scopes
-- recorded yet.
emptyFocusTracker :: FocusTracker e
emptyFocusTracker = FocusTracker { ftAmbient = emptyFocusState, ftScopes = Map.empty }

-- | Advances a 'FocusTracker' to the next frame by applying 'nextFocusFrame'
-- to the ambient scope and every persisted composite scope.
nextFocusTrackerFrame :: FocusTracker e -> FocusTracker e
nextFocusTrackerFrame ft = ft
  { ftAmbient = nextFocusFrame (ftAmbient ft)
  , ftScopes  = Map.map nextFocusFrame (ftScopes ft)
  }

-- | Cross-frame per-element presentation state. Persists unchanged across
-- frames; never exposed to the application.
data ElementState e = ElementState
  { elmScrollStates   :: Map.Map e ScrollState
  , elmSelections     :: Map.Map e [Selection]
  , elmMouseOverPrev  :: Set.Set e
    -- ^ Elements 'registerMouseOver' was called for during the previous
    -- frame. Snapshotted by 'nextFrameContext' from the completed frame's
    -- @outMouseOverThisFrame@; read via 'wasMouseOverLastFrame' to derive
    -- mouse-enter\/-exit by comparing against this frame's geometric hit
    -- test. Tracks every element hit this frame, so more than one control
    -- can be moused over at once.
  }

-- | Outputs accumulated during a single frame: draw commands, the queued
-- 'Out' events (messages and UI effects, in emit order), and the animation
-- continuation flag. Reset to empty at the start of each frame by
-- 'nextFrameContext'.
data FrameOutputs e msg = FrameOutputs
  { outDrawCommands       :: [DrawCommand]
  , outEvents             :: [Out e msg]
  , outRequiresAnimation  :: Bool
  , outMouseOverThisFrame :: Set.Set e
    -- ^ Elements 'registerMouseOver' has been called for so far this frame.
    -- Snapshotted into @elmMouseOverPrev@ by 'nextFrameContext' once the
    -- frame completes.
  }

-- | The frame context threaded through every 'UI' computation. Carries the
-- current bounds, input state, active theme, accumulated draw commands, focus
-- state, scroll and selection state, and queued messages. Construct with
-- 'emptyUIContext' or 'nextFrameContext'; extract results with
-- 'getDrawCommands' and 'getMessages'.
--
-- [@e@] Element identity type — identifies focusable\/hoverable controls.
-- [@msg@] Message type emitted by controls via 'emit'; the application never
-- lives in this context.
data UIContext e msg = UIContext
  { ctxBounds          :: Rectangle
  , ctxInput           :: InputState
  , ctxTheme           :: Theme e
  , ctxDisabled        :: Bool
  , ctxInteractionClip :: Maybe Rectangle
    -- ^ When set, 'isRegionHit' additionally requires the mouse to fall within
    -- this rectangle. Set by 'clipToCurrent' and restored on exit, so it
    -- tracks the innermost enclosing clip region.
  , ctxAnimation       :: AnimationState
    -- ^ Per-frame animation state: wall-clock delta and tick flag. Set by
    -- "Blink.App" at the start of each frame.
  , ctxTextMeasure     :: TextMeasurer
    -- ^ Text measurement service supplied at configure time. Controls call
    -- 'charOffset' and 'charAtOffset' rather than accessing this directly.
  , ctxFocus           :: FocusTracker e
    -- ^ Keyboard-focus targeting state. See 'FocusTracker'.
  , ctxButton          :: ButtonState e
    -- ^ The left mouse button's state this frame, and which element (if
    -- any) holds mouse capture. See 'ButtonState'. Unlike focus, this does
    -- not change on a re-render of the same frame — see 'rerenderContext'.
  , ctxElements        :: ElementState e
  , ctxOutputs         :: FrameOutputs e msg
  }

-- | The UI monad. A state-threading computation in 'IO' that reads from a
-- 'UIContext' and emits draw commands and messages as a side effect. Use the
-- 'Functor', 'Applicative', and 'Monad' instances to compose UI trees. See
-- 'Blink.Controls.control' for higher-level building blocks.
--
-- [@e@] Element identity type.
-- [@msg@] Message type emitted via 'emit'.
-- [@a@] Result type.
newtype UI e msg a = UI
  { runUI :: UIContext e msg -> IO (a, UIContext e msg)
    -- ^ Runs the computation against a context, producing a result and the
    -- updated context.
  }

instance Functor (UI e msg) where
  fmap f (UI g) = UI $ \ctx -> do
    (a, ctx') <- g ctx
    pure (f a, ctx')

instance Applicative (UI e msg) where
  pure a = UI $ \ctx -> pure (a, ctx)
  UI f <*> UI x = UI $ \ctx -> do
    (g, ctx')  <- f ctx
    (a, ctx'') <- x ctx'
    pure (g a, ctx'')

instance Monad (UI e msg) where
  return = pure
  UI x >>= f = UI $ \ctx -> do
    (a, ctx') <- x ctx
    runUI (f a) ctx'

emptyFrameOutputs :: FrameOutputs e msg
emptyFrameOutputs = FrameOutputs
  { outDrawCommands       = []
  , outEvents             = []
  , outRequiresAnimation  = False
  , outMouseOverThisFrame = Set.empty
  }

-- | Constructs the initial 'UIContext' for the first frame.
emptyUIContext :: Rectangle -> InputState -> Theme e -> TextMeasurer -> UIContext e msg
emptyUIContext bounds input thm measurer = UIContext
  { ctxBounds          = bounds
  , ctxInput           = input
  , ctxTheme           = thm
  , ctxDisabled        = False
  , ctxInteractionClip = Nothing
  , ctxAnimation       = mkAnimationState 0 0 False
  , ctxTextMeasure     = measurer
  , ctxFocus           = emptyFocusTracker
  , ctxButton          = nextButtonState False (inputLeftButtonDown input) MouseNotCaptured
  , ctxElements        = ElementState
      { elmScrollStates  = Map.empty
      , elmSelections    = Map.empty
      , elmMouseOverPrev = Set.empty
      }
  , ctxOutputs         = emptyFrameOutputs
  }

-- | Advances the context to the next frame, given the backend-supplied
-- inputs for the frame about to run: window bounds, raw input, active theme,
-- and animation state. First runs 'applyUiEffects' on the 'UiEffect's queued
-- during the frame that just completed, so focus, scroll, and selection
-- changes take effect starting this new frame. Then resets per-frame state
-- (draw commands, hover element, queued messages, and the focus-visited
-- flag) while preserving cross-frame state (focus element, scroll state,
-- selections, and tab-stop bookkeeping). Also snapshots the completed
-- frame's @outMouseOverThisFrame@ into @elmMouseOverPrev@, so
-- 'wasMouseOverLastFrame' reflects this frame once it, in turn, becomes
-- "last frame".
nextFrameContext :: Ord e => Rectangle -> InputState -> Theme e -> AnimationState -> UIContext e msg -> UIContext e msg
nextFrameContext bounds input thm anim ctx0 = ctx
  { ctxBounds      = bounds
  , ctxInput       = input
  , ctxTheme       = thm
  , ctxAnimation   = anim
  , ctxButton      = nextButtonState wasDown isDown (captureOf (ctxButton ctx))
  , ctxFocus       = nextFocusTrackerFrame (ctxFocus ctx)
  , ctxElements    = (ctxElements ctx)
      { elmMouseOverPrev = outMouseOverThisFrame (ctxOutputs ctx) }
  , ctxOutputs     = emptyFrameOutputs
  }
  where
    ctx     = applyUiEffects (getUiEffects ctx0) ctx0
    wasDown = inputLeftButtonDown (ctxInput ctx)
    isDown  = inputLeftButtonDown input

-- | Rebuilds the context to re-render the current frame rather than advance
-- to a new one: refreshes bounds\/theme\/animation, applies queued
-- 'UiEffect's, and resets per-frame outputs, like 'nextFrameContext'. Leaves
-- @ctxButton@ as-is rather than re-deriving it from input, since the caller
-- is re-rendering the same frame, not moving to the next one; deriving it
-- again would compare the frame's input against itself.
rerenderContext :: Ord e => Rectangle -> InputState -> Theme e -> AnimationState -> UIContext e msg -> UIContext e msg
rerenderContext bounds input thm anim ctx0 = ctx
  { ctxBounds      = bounds
  , ctxInput       = input
  , ctxTheme       = thm
  , ctxAnimation   = anim
  , ctxFocus       = nextFocusTrackerFrame (ctxFocus ctx)
  , ctxElements    = (ctxElements ctx)
      { elmMouseOverPrev = outMouseOverThisFrame (ctxOutputs ctx) }
  , ctxOutputs     = emptyFrameOutputs
  }
  where
    ctx = applyUiEffects (getUiEffects ctx0) ctx0

-- | Whether moving from @prevDown@ to @currDown@ leaves the button held,
-- just-released, or up, and whether @existingCapture@ carries forward:
-- capture survives while the button stays held or on its release frame, and
-- is dropped the moment the button was not down last frame — both when it
-- is fully up and on a fresh press — so a new press never inherits a stale
-- capture from a previous drag\/click cycle. Acquisition — setting capture
-- in the first place — happens in 'acquireCapture'. A frame with no
-- previous frame to compare against (the first frame) passes @prevDown =
-- False@, which always yields 'ButtonUp' or an uncaptured 'ButtonDown'.
nextButtonState :: Bool -> Bool -> MouseCapture e -> ButtonState e
nextButtonState prevDown currDown existingCapture
  | currDown  = ButtonDown carriedCapture
  | prevDown  = ButtonReleased carriedCapture
  | otherwise = ButtonUp
  where
    carriedCapture
      | prevDown  = existingCapture
      | otherwise = MouseNotCaptured

gets :: (UIContext e msg -> a) -> UI e msg a
gets f = UI $ \ctx -> pure (f ctx, ctx)

modify :: (UIContext e msg -> UIContext e msg) -> UI e msg ()
modify f = UI $ \ctx -> pure ((), f ctx)

-- | Modifies the currently ambient scope's own 'FocusState'.
modifyFocusState :: (FocusState e -> FocusState e) -> UI e msg ()
modifyFocusState f = modify $ \ctx -> ctx { ctxFocus = (ctxFocus ctx) { ftAmbient = f (ftAmbient (ctxFocus ctx)) } }

-- | Modifies the current frame's 'ButtonState'.
modifyButton :: (ButtonState e -> ButtonState e) -> UI e msg ()
modifyButton f = modify $ \ctx -> ctx { ctxButton = f (ctxButton ctx) }

modifyOut :: (FrameOutputs e msg -> FrameOutputs e msg) -> UI e msg ()
modifyOut f = modify $ \ctx -> ctx { ctxOutputs = f (ctxOutputs ctx) }

-- | The current scroll position for the given element, in @[0, 1]@. Returns
-- @0@ when no position has been recorded yet.
getScrollState :: Ord e => e -> UI e msg Double
getScrollState eid = gets (contextScrollPosition eid)

-- | The current scroll position for the given element, in @[0, 1]@, read
-- directly from a 'UIContext' outside the 'UI' monad — e.g. to assert on the
-- result of a completed frame. Returns @0@ when no position has been
-- recorded yet.
contextScrollPosition :: Ord e => e -> UIContext e msg -> Double
contextScrollPosition eid ctx =
  scrollPosition (Map.findWithDefault (ScrollState 0) eid (elmScrollStates (ctxElements ctx)))

-- | All selections for the given element. Returns @[]@ when none have been recorded.
getSelections :: Ord e => e -> UI e msg [Selection]
getSelections eid = gets (contextSelections eid)

-- | All selections for the given element, read directly from a 'UIContext'
-- outside the 'UI' monad. Returns @[]@ when none have been recorded.
contextSelections :: Ord e => e -> UIContext e msg -> [Selection]
contextSelections eid ctx = Map.findWithDefault [] eid (elmSelections (ctxElements ctx))

-- | The first selection for the given element, or 'Nothing'.
getSelection :: Ord e => e -> UI e msg (Maybe Selection)
getSelection eid = listToMaybe <$> getSelections eid

-- | The first selection for the given element, or 'Nothing', read directly
-- from a 'UIContext' outside the 'UI' monad.
contextSelection :: Ord e => e -> UIContext e msg -> Maybe Selection
contextSelection eid = listToMaybe . contextSelections eid

-- Internal: writes a scroll position directly into the context, bypassing
-- the deferred-effect queue. Clamps to @[0, 1]@ so this is the single point
-- that enforces the 'ScrollState' invariant regardless of which 'UiEffect'
-- reaches it. Used only by 'applyUiEffects'.
writeScrollState :: Ord e => e -> Double -> UIContext e msg -> UIContext e msg
writeScrollState eid v ctx = ctx { ctxElements = (ctxElements ctx)
  { elmScrollStates = Map.insert eid (ScrollState (clampScrollPos v)) (elmScrollStates (ctxElements ctx)) } }

-- Internal: writes a selection directly into the context, bypassing the
-- deferred-effect queue. Used only by 'applyUiEffects'.
writeSelection :: Ord e => e -> Selection -> UIContext e msg -> UIContext e msg
writeSelection eid sel ctx = ctx { ctxElements = (ctxElements ctx)
  { elmSelections = Map.insert eid [sel] (elmSelections (ctxElements ctx)) } }

-- | The lower bound of the selected range: @min selectionAnchor selectionActive@.
selectionLow :: Selection -> Int
selectionLow s = min (selectionAnchor s) (selectionActive s)

-- | The upper bound of the selected range: @max selectionAnchor selectionActive@.
selectionHigh :: Selection -> Int
selectionHigh s = max (selectionAnchor s) (selectionActive s)

-- | 'True' when the selection has non-zero extent (anchor ≠ active).
selectionHasExtent :: Selection -> Bool
selectionHasExtent s = selectionAnchor s /= selectionActive s

-- | A cursor with no selection extent. Equivalent to @'Selection' n n@.
cursor :: Int -> Selection
cursor n = Selection n n

-- | Collapse the selection to a cursor at the lower bound.
collapseToLow :: Selection -> Selection
collapseToLow = cursor . selectionLow

-- | Collapse the selection to a cursor at the upper bound.
collapseToHigh :: Selection -> Selection
collapseToHigh = cursor . selectionHigh

-- | Collapse the selection to a cursor at the active (moving) end.
collapseToActive :: Selection -> Selection
collapseToActive = cursor . selectionActive

-- | Apply a function to the active end, keeping the anchor fixed.
extendActive :: (Int -> Int) -> Selection -> Selection
extendActive f s = s { selectionActive = f (selectionActive s) }

-- | Clamp a scroll position to @[0, 1]@.
clampScrollPos :: Double -> Double
clampScrollPos = max 0 . min 1

-- | The current layout rectangle. Set by the layout system via 'withBounds'.
getBounds :: UI e msg Rectangle
getBounds = gets ctxBounds

-- | The current mouse cursor position in window coordinates.
getMousePos :: UI e msg Point
getMousePos = inputMousePosition <$> getInput

-- | The raw input state for the current frame.
getInput :: UI e msg InputState
getInput = gets contextInput

-- | The raw input state for the current frame, read directly from a
-- 'UIContext' outside the 'UI' monad — e.g. so a backend or test driver can
-- carry mouse\/button state forward into the next 'nextFrameContext' call.
contextInput :: UIContext e msg -> InputState
contextInput = ctxInput

-- | Removes all events for the given key from the current frame's key queue,
-- preventing other controls from handling the same keypress.
consumeKey :: Key -> UI e msg ()
consumeKey k = modify $ \ctx ->
  let input = ctxInput ctx
  in ctx { ctxInput = input { inputKeyEvents = filter (\e -> key e /= k) (inputKeyEvents input) } }

-- | The element that was the most recent tab stop before the current one,
-- scoped to the currently ambient scope (root, or a composite's own while
-- inside 'withFocusScope') — used by 'Blink.Controls.control' to implement
-- Shift-Tab navigation.
getPreviousTabStop :: UI e msg (Maybe e)
getPreviousTabStop = gets contextPrevTabStop

-- | The element that was the most recent tab stop before the current one,
-- read directly from a 'UIContext' outside the 'UI' monad.
contextPrevTabStop :: UIContext e msg -> Maybe e
contextPrevTabStop = previousTabStop . ftAmbient . ctxFocus

-- | Records the current element as the previous tab stop, scoped to the
-- currently ambient scope. Called automatically by 'Blink.Controls.control';
-- call manually when building custom focusable controls.
setPreviousTabStop :: e -> UI e msg ()
setPreviousTabStop eid = modifyFocusState $ \fs -> fs { previousTabStop = Just eid }

getTheme :: UI e msg (Theme e)
getTheme = gets contextTheme

-- | The active 'Theme', read directly from a 'UIContext' outside the 'UI'
-- monad — e.g. so a backend or test driver can carry it forward unchanged
-- into the next 'nextFrameContext' call.
contextTheme :: UIContext e msg -> Theme e
contextTheme = ctxTheme

-- | Returns all style variants for the given element. Falls back to the theme's
-- default style when no element-specific style is registered.
getStyleSet :: Ord e => e -> UI e msg StyleSet
getStyleSet eid = do
  t <- getTheme
  return $ Map.findWithDefault (themeDefaultStyle t) eid (themeElementStyles t)

-- | 'True' when the left button is currently held.
isButtonDown :: UI e msg Bool
isButtonDown = gets contextButtonDown

-- | 'True' when the left button is currently held, read directly from a
-- 'UIContext' outside the 'UI' monad.
contextButtonDown :: UIContext e msg -> Bool
contextButtonDown ctx = case ctxButton ctx of
  ButtonDown _ -> True
  _            -> False

-- | 'True' on the one frame the left button transitions from held to up.
isButtonReleased :: UI e msg Bool
isButtonReleased = gets contextButtonReleased

-- | 'True' on the one frame the left button transitions from held to up,
-- read directly from a 'UIContext' outside the 'UI' monad.
contextButtonReleased :: UIContext e msg -> Bool
contextButtonReleased ctx = case ctxButton ctx of
  ButtonReleased _ -> True
  _                -> False

-- | 'True' on every frame that the given element is being dragged — from the
-- initial press through to release.
isDragging :: Eq e => e -> UI e msg Bool
isDragging eid = (== MouseCapturedBy eid) <$> gets contextCaptured

-- | Which element currently holds mouse capture, if any. Exported for
-- control authors that need to inspect capture state directly, e.g. when
-- implementing focus-on-click without using 'Blink.Controls.control'.
getCapturedElement :: UI e msg (MouseCapture e)
getCapturedElement = gets contextCaptured

-- | Which element currently holds mouse capture, if any, read directly from
-- a 'UIContext' outside the 'UI' monad.
contextCaptured :: UIContext e msg -> MouseCapture e
contextCaptured = captureOf . ctxButton

-- | Acquires mouse capture for the element if the left button is currently
-- down and nothing is captured yet, making this the first point of capture
-- for that press — the control a drag holds onto once the cursor leaves the
-- element that started it.
acquireCapture :: e -> UI e msg ()
acquireCapture eid = modifyButton $ \btn -> case btn of
  ButtonDown MouseNotCaptured -> ButtonDown (MouseCapturedBy eid)
  _                           -> btn

-- | Records that the element was hit by the mouse this frame — typically
-- called after a geometric hit test such as 'isRegionHit' succeeds. Building
-- block for mouse-enter\/-exit: compare against 'wasMouseOverLastFrame' for
-- the same element to detect the transition. Any number of elements can each
-- call this in the same frame; all are remembered.
registerMouseOver :: Ord e => e -> UI e msg ()
registerMouseOver eid = modifyOut $ \out ->
  out { outMouseOverThisFrame = Set.insert eid (outMouseOverThisFrame out) }

-- | 'True' when 'registerMouseOver' was called for the element on the
-- previous frame. Compare against this frame's own hit test to derive
-- mouse-enter (@not wasOver && isOver@) and mouse-exit (@wasOver && not
-- isOver@).
wasMouseOverLastFrame :: Ord e => e -> UI e msg Bool
wasMouseOverLastFrame eid = gets $ \ctx -> Set.member eid (elmMouseOverPrev (ctxElements ctx))

-- | 'True' when 'registerMouseOver' has been called for any element so far
-- this frame. Reflects the whole frame's hover set only once every control
-- that might register one has run — call it after the rest of the view, the
-- same way the hover getter under the legacy single-owner hover model this
-- replaces was read at the end of a frame, for controls built with
-- geometric hover (many elements can be "over" at once, so unlike that
-- legacy getter there is no single element to name — only whether the set
-- is non-empty).
isAnyMouseOver :: UI e msg Bool
isAnyMouseOver = gets (not . Set.null . outMouseOverThisFrame . ctxOutputs)

-- | The currently ambient scope's focused element, if any — root's, unless
-- inside 'withFocusScope'.
getFocus :: UI e msg (Maybe e)
getFocus = gets contextFocus

-- | The currently ambient scope's focused element, read directly from a
-- 'UIContext' outside the 'UI' monad.
contextFocus :: UIContext e msg -> Maybe e
contextFocus = focusedElement . ftAmbient . ctxFocus

-- | The full root-to-leaf focus chain, read directly from a 'UIContext'
-- outside the 'UI' monad by following each scope's own focused element into
-- @ftScopes@ until it bottoms out. No production code needs this —
-- every real check is the single-hop 'contextFocus'\/'isFocused' at
-- whichever scope is ambient at the time — but it's useful for tests and
-- debugging tools that want to see the whole nested claim at once.
--
-- Guards against revisiting an id already on the chain:
-- 'Blink.Controls.focusOnClick' with 'Blink.Controls.FocusTarget' lets a
-- click redirect focus onto any id, including an
-- enclosing composite's own — 'Blink.Controls.radioGroup' does exactly this
-- so that clicking an item leaves the group itself focused, not the item —
-- which writes that id into its own scope entry in @ftScopes@. That's
-- harmless for the single-hop checks every real caller uses, but would
-- otherwise send this walk into an infinite loop.
contextFocusChain :: Ord e => UIContext e msg -> [e]
contextFocusChain ctx = go Set.empty (contextFocus ctx)
  where
    scopes = ftScopes (ctxFocus ctx)
    go _    Nothing  = []
    go seen (Just x)
      | x `Set.member` seen = []
      | otherwise            = x : go (Set.insert x seen) (focusedElement (Map.findWithDefault emptyFocusState x scopes))

-- | 'True' when the given element id is the currently ambient scope's
-- focused element. For a leaf, this is exactly "am I focused"; for a
-- composite checking its own id, single-hop equality already gives
-- CSS's @:focus-within@ for free — 'withFocusScope' is what makes a
-- composite's own id read as ambiently focused whenever a descendant is, by
-- construction, so no chain-walk is needed here.
isFocused :: Eq e => e -> UI e msg Bool
isFocused eid = (== Just eid) <$> getFocus

-- | Transfers keyboard focus to the given element, in the currently ambient
-- scope. Takes effect immediately — like 'registerMouseOver' and mouse
-- capture, not like the deferred scroll\/selection writes — because a
-- control's own focus decision (take it when nothing else has it, hand off
-- on Tab) is only correct if the next sibling in the same tree walk can see
-- it happened.
setFocus :: e -> UI e msg ()
setFocus eid = modifyFocusState $ \fs -> fs { focusedElement = Just eid, focusedThisFrame = True }

-- | Transfers keyboard focus to the given element when the condition is
-- 'True'.
setFocusWhen :: Bool -> e -> UI e msg ()
setFocusWhen b eid = when b (setFocus eid)

-- | Removes keyboard focus from the currently ambient scope. Immediate,
-- like 'setFocus'.
clearFocus :: UI e msg ()
clearFocus = modifyFocusState $ \fs -> fs { focusedElement = Nothing }

-- | Marks a sub-tree as belonging to a composite focus scope (a list, a
-- tree — anything with sub-items), addressed by its own globally-unique id.
-- Whether descendants get to see — and update — this scope's own persisted
-- state depends on whether the scope is /currently/ the live focus target:
--
--   * It is (ambient's focused element is already this id), or nothing is
--     focused anywhere so it's free to become the target on this pass:
--     descendants run against this scope's own persisted 'FocusState' —
--     looked up from @ftScopes@, defaulting to @emptyFocusState@ the
--     first time — so they can auto-claim or resume exactly as if they were
--     standalone. Whatever they end up with is folded back into
--     @ftScopes@ under this id, and the enclosing scope's own
--     focused element is (re)affirmed as pointing at this id — every frame
--     it claims, even when nothing inside ends up focused, the same way a
--     plain focused control reaffirms itself every frame it renders.
--   * It isn't: descendants run against a /blocking/ ambient value instead —
--     not this scope's own persisted state, and not necessarily the literal
--     real ambient either (see @blockFreshClaim@ below) — so nothing reads
--     as an invitation to auto-claim. If nothing inside claims explicitly
--     despite that, the real ambient is restored unchanged and nothing is
--     written back: this is what stops a stale remembered child from being
--     handed a copy of old state, recognising itself in it, and
--     reaffirming — which would silently steal focus back on a frame where
--     this scope was never actually the target. If something inside /does/
--     claim explicitly (an outright click, not an auto-claim) despite the
--     block, that claim is honoured and folded back in as if this scope had
--     been the live target all along.
--
-- @blockFreshClaim@ overrides the "nothing is focused, free to claim" half
-- of the first case for one frame, and changes what "blocking" value gets
-- used in the second. It exists for a caller (see
-- 'Blink.Controls.compositeControl') that gives the composite's own id an
-- ordinary focus claim of its own, ahead of this call: if that claim was
-- just given up via Tab this very frame, real ambient reads empty for an
-- instant reason that has nothing to do with "nothing was ever focused" —
-- feeding descendants that real, empty value would read as an invitation to
-- auto-claim immediately, undoing the Tab press that was meant to move
-- focus off the composite entirely. So in that one case, descendants are
-- instead given this scope's own id as the blocking value (nothing they
-- recognise as themselves), the same placeholder the old chain-based model
-- used for exactly this. Standalone use (no such outer claim of its own)
-- always passes 'False', so the blocking value is always the literal real
-- ambient there.
--
-- Composes for arbitrary nesting: a composite inside another's
-- 'withFocusScope' only ever swaps\/restores its own scope, and does the
-- same lookup\/render\/write-back around its own children.
--
-- = Invariant: a disabled composite never holds focus
--
-- A disabled control must never appear to hold keyboard focus, even
-- vacuously ("composite focused, no child chosen"). While disabled, this
-- function runs @action@ against the ambient context completely unmodified:
-- no substitution, no claim, no write-back — so it can neither claim focus
-- for the composite nor leave a stale claim behind, regardless of what
-- ambient says and regardless of whether the caller remembered to check
-- 'isDisabled' itself. This is enforced here, once, rather than left as a
-- convention every caller (present or future) has to uphold on its own —
-- see the integration coverage in "Blink.ControlsSpec" for the regression
-- this guards against.
withFocusScope :: Ord e => e -> Bool -> UI e msg a -> UI e msg a
withFocusScope scopeId blockFreshClaim (UI f) = UI $ \ctx ->
  if ctxDisabled ctx
    then f ctx
    else case scopeMode scopeId blockFreshClaim (contextFocus ctx) of
      Claim         -> runClaimed ctx
      Blocked blockValue -> runBlocked ctx blockValue
  where
    -- The claiming scope's descendants run against its own persisted
    -- 'FocusState' (or a fresh one), and whatever they end up with is
    -- folded back under this id, with the enclosing scope reaffirmed as
    -- pointing here.
    runClaimed ctx = do
      let enclosing = ftAmbient (ctxFocus ctx)
          child0    = Map.findWithDefault emptyFocusState scopeId (ftScopes (ctxFocus ctx))
      (a, ctx', after) <- runWithAmbient child0 ctx
      pure (a, foldBackAsClaim enclosing after ctx')

    -- The blocked scope's descendants run against a value nothing inside
    -- recognises as itself, so nothing reads as an invitation to
    -- auto-claim. If nothing claims anyway, the real ambient is restored
    -- untouched; if something claims explicitly despite the block, it's
    -- folded back exactly as 'runClaimed' would.
    runBlocked ctx blockValue = do
      let real = ftAmbient (ctxFocus ctx)
      (a, ctx', after) <- runWithAmbient (real { focusedElement = blockValue }) ctx
      if focusedElement after == blockValue
        then pure (a, ctx' { ctxFocus = (ctxFocus ctx') { ftAmbient = real } })
        else pure (a, foldBackAsClaim real after ctx')

    -- Swaps the ambient 'FocusState' for @ambient@ while @f@ runs, and
    -- reports what it became by the time @f@ finishes, alongside the
    -- resulting context.
    runWithAmbient ambient ctx = do
      (a, ctx') <- f (ctx { ctxFocus = (ctxFocus ctx) { ftAmbient = ambient } })
      pure (a, ctx', ftAmbient (ctxFocus ctx'))

    -- Records @after@ as this scope's own persisted state, and points
    -- @base@ (the value to restore around this scope) at this scope's id —
    -- the write-back shared by a claim and a blocked-but-claimed-anyway
    -- resolution alike.
    foldBackAsClaim base after ctx' = ctx'
      { ctxFocus = (ctxFocus ctx')
          { ftAmbient = base { focusedElement = Just scopeId, focusedThisFrame = True }
          , ftScopes  = Map.insert scopeId (after { focusedThisFrame = True }) (ftScopes (ctxFocus ctx'))
          } }

-- | Which of the two policies documented on 'withFocusScope' applies this
-- frame: 'Claim' if the scope is (or is free to become) the live focus
-- target, 'Blocked' with the ambient value descendants should see
-- otherwise.
data ScopeMode e = Claim | Blocked (Maybe e)

scopeMode :: Eq e => e -> Bool -> Maybe e -> ScopeMode e
scopeMode scopeId blockFreshClaim currentAmbient = case currentAmbient of
  Just cid | cid == scopeId      -> Claim
  Nothing  | not blockFreshClaim -> Claim
  Nothing                        -> Blocked (Just scopeId)
  real                           -> Blocked real

-- | Advances a 'FocusState' to the next frame: carries it forward if it was
-- reaffirmed this frame, otherwise clears it back to @emptyFocusState@.
-- Applied to the root scope and every entry in @ftScopes@ — a scope
-- that stops being reaffirmed (its composite removed from the tree, or its
-- specific focused child gone while the composite itself still renders)
-- expires independently, the same way root-level focus already did.
nextFocusFrame :: FocusState e -> FocusState e
nextFocusFrame fs
  | focusedThisFrame fs = fs { focusedThisFrame = False }
  | otherwise            = emptyFocusState

-- | Runs a sub-tree within a different bounding rectangle. The previous bounds
-- are restored when the sub-tree completes. Used by the layout system to
-- assign each child its allocated slot.
withBounds :: Rectangle -> UI e msg a -> UI e msg a
withBounds r (UI f) = UI $ \ctx -> do
  (a, ctx') <- f (ctx { ctxBounds = r })
  pure (a, ctx' { ctxBounds = ctxBounds ctx })

-- | 'True' when the current sub-tree has been marked disabled.
isDisabled :: UI e msg Bool
isDisabled = gets ctxDisabled

-- | Marks a sub-tree as disabled when the condition is 'True'. The flag is
-- restored to its previous value once the sub-tree completes.
disableWhen :: Bool -> UI e msg a -> UI e msg a
disableWhen True (UI f) = UI $ \ctx -> do
  (a, ctx') <- f (ctx { ctxDisabled = True })
  pure (a, ctx' { ctxDisabled = ctxDisabled ctx })
disableWhen False action = action

draw :: DrawCommand -> UI e msg ()
draw cmd = modifyOut $ \out -> out { outDrawCommands = cmd : outDrawCommands out }

-- | Builds a 'DrawCommand' from the current bounds and queues it.
drawAt :: (Rectangle -> DrawCommand) -> UI e msg ()
drawAt mkCmd = do
  r <- getBounds
  draw (mkCmd r)

-- | Fills the current bounds with a solid colour.
fillRect :: Colour -> UI e msg ()
fillRect colour = drawAt (\r -> FillRect r colour)

-- | Strokes the border of the current bounds with the given colour and per-side widths.
strokeRect :: Colour -> BorderEdges -> UI e msg ()
strokeRect colour edges = drawAt (\r -> StrokeBorder r colour edges)

-- | Renders text within the current bounds using the given colour and alignment.
drawText :: Colour -> TextAlign -> Text -> UI e msg ()
drawText colour align text = drawAt (\r -> DrawText r text colour align)

-- | Wraps a sub-tree in a clip region matching the current bounds. Draw
-- commands produced by the sub-tree that fall outside the region are discarded,
-- and mouse hit-testing is also restricted to the same region.
clipToCurrent :: UI e msg a -> UI e msg a
clipToCurrent (UI f) = UI $ \ctx -> do
  let r       = ctxBounds ctx
      newClip = maybe r (intersectRect r) (ctxInteractionClip ctx)
      ctx'    = ctx { ctxInteractionClip = Just newClip
                    , ctxOutputs = (ctxOutputs ctx)
                        { outDrawCommands = PushClip r : outDrawCommands (ctxOutputs ctx) } }
  (a, ctx'') <- f ctx'
  let ctx''' = ctx'' { ctxOutputs = (ctxOutputs ctx'')
                         { outDrawCommands = PopClip : outDrawCommands (ctxOutputs ctx'') }
                     , ctxInteractionClip = ctxInteractionClip ctx }
  pure (a, ctx''')

-- | Fills the current bounds with @colour@ then runs @content@ on top.
-- Skips the fill when @colour@ is fully transparent.
withBackground :: Colour -> UI e msg a -> UI e msg a
withBackground colour content = do
  when (isVisible colour) $ fillRect colour
  content

-- | Runs @content@, then strokes a border around the current bounds on top.
-- Drawing the border after content ensures it is always visible over children.
-- Skips the stroke when @colour@ is fully transparent, mirroring
-- 'withBackground' — a caller that reserves border space in every state via
-- @styleBorderColour@ but only wants it to actually render in some of them
-- (e.g. a resting-state border that becomes visible on focus) relies on this.
withBorder :: Colour -> BorderEdges -> UI e msg a -> UI e msg a
withBorder colour edges content = do
  result <- content
  when (isVisible colour) $ strokeRect colour edges
  pure result

-- | Queues a message to be delivered to the application once the frame
-- completes. Messages are delivered in emit order by 'getMessages'.
emit :: msg -> UI e msg ()
emit msg = modifyOut $ \out -> out { outEvents = OutMsg msg : outEvents out }

-- | Queues a 'UiEffect' — a focus, scroll, or selection change — to be
-- applied by 'applyUiEffects' between this frame and the next. 'setFocus',
-- 'clearFocus', and the scroll\/selection writes inside "Blink.Controls" are
-- built on this; reach for it directly only when writing a custom control.
emitUi :: UiEffect e -> UI e msg ()
emitUi eff = modifyOut $ \out -> out { outEvents = OutUi eff : outEvents out }

-- | Extracts the draw commands produced during the frame, in submission order.
getDrawCommands :: UIContext e msg -> [DrawCommand]
getDrawCommands = reverse . outDrawCommands . ctxOutputs

-- | Extracts the messages queued with 'emit' during the frame, in emit order.
-- 'UiEffect's queued with 'emitUi' (or 'setFocus', 'clearFocus', etc.) are
-- excluded; they are applied automatically by 'nextFrameContext' and never
-- reach the application.
getMessages :: UIContext e msg -> [msg]
getMessages ctx = [msg | OutMsg msg <- reverse (outEvents (ctxOutputs ctx))]

-- | Extracts the 'UiEffect's queued with 'emitUi' during the frame, in emit
-- order, interleaved order with messages discarded. Used by
-- 'nextFrameContext' to drive 'applyUiEffects'; not needed by ordinary
-- application or control code.
getUiEffects :: UIContext e msg -> [UiEffect e]
getUiEffects ctx = [eff | OutUi eff <- reverse (outEvents (ctxOutputs ctx))]

-- | 'True' when 'requiresAnimation' was called at least once during the
-- frame, read directly from a 'UIContext' outside the 'UI' monad. The
-- backend reads this after each frame to decide whether to keep its
-- animation ticker running.
contextRequiresAnimation :: UIContext e msg -> Bool
contextRequiresAnimation = outRequiresAnimation . ctxOutputs

-- | Applies the 'UiEffect's queued during a frame (via 'emitUi' or a
-- control's scroll\/selection writes) to the context handed to the next
-- frame. Run automatically by 'nextFrameContext' — no host or application
-- code needs to call this directly. Effects are folded in queue order:
-- 'ScrollTo' and 'SetSelectionAt' each overwrite what came before for the
-- same target, while 'ScrollBy' composes with a previous write to the same
-- target. Every scroll write passes through the same internal clamp, so the
-- result is always in @[0, 1]@ regardless of which constructor produced it:
--
-- @
-- applyUiEffects [ScrollBy eid 0.6, ScrollBy eid 0.6] ctx
--   -- scroll position 1.0 (0.6 + 0.6, clamped — not last-write-wins)
--
-- applyUiEffects [ScrollBy eid 0.6, ScrollTo eid 0.2] ctx
--   -- scroll position 0.2 (ScrollTo overwrites the pending ScrollBy)
--
-- applyUiEffects [ScrollTo eid 1.5] ctx
--   -- scroll position 1.0 (out-of-range input, clamped)
-- @
applyUiEffects :: Ord e => [UiEffect e] -> UIContext e msg -> UIContext e msg
applyUiEffects effects ctx0 = foldl' step ctx0 effects
  where
    step ctx (ScrollTo eid v)         = writeScrollState eid v ctx
    step ctx (ScrollBy eid dv)        = writeScrollState eid (currentScroll eid ctx + dv) ctx
    step ctx (SetSelectionAt eid sel) = writeSelection eid sel ctx

    currentScroll eid ctx =
      scrollPosition (Map.findWithDefault (ScrollState 0) eid (elmScrollStates (ctxElements ctx)))

-- | 'True' when the mouse cursor is within the current bounds and within the
-- active interaction clip region (set by 'clipToCurrent'). This is the
-- lower-level, element-agnostic primitive; for a specific control's hit area
-- (bounds inset by its margin), see 'Blink.Controls.isMouseOver'.
isRegionHit :: UI e msg Bool
isRegionHit = do
  r    <- getBounds
  p    <- getMousePos
  clip <- gets ctxInteractionClip
  return $ containsPoint p r && maybe True (containsPoint p) clip

-- | Skips its argument entirely when the current sub-tree is disabled.
whenEnabled :: UI e msg () -> UI e msg ()
whenEnabled ui = do
  disabled <- isDisabled
  unless disabled ui

-- | 'True' when no element currently holds mouse capture — i.e. no drag is
-- in progress. Use alongside 'isDragging' to decide whether a control should
-- respond to hover: @free || dragging@ allows hover when the mouse is
-- uncontested or when this element itself owns the capture.
isMouseFree :: UI e msg Bool
isMouseFree = isNotCaptured <$> gets contextCaptured
  where
    isNotCaptured MouseNotCaptured = True
    isNotCaptured (MouseCapturedBy _) = False

-- | Signals that animation should continue running. Call unconditionally on
-- every frame from any component that needs animation, including frames not
-- triggered by the ticker, so "Blink.App"'s ticker does not go quiet while
-- the component is visible.
requiresAnimation :: UI e msg ()
requiresAnimation = modifyOut $ \out -> out { outRequiresAnimation = True }

-- | Runs @action@ only on frames triggered by the animation ticker. On frames
-- triggered by mouse movement, keyboard input, or other platform events, this
-- is a no-op. Pair with 'requiresAnimation' so the ticker keeps firing.
withAnimationFrame :: UI e msg () -> UI e msg ()
withAnimationFrame action = do
  isTick <- gets (animIsTick . ctxAnimation)
  when isTick action

-- | Wall-clock seconds elapsed since the previous frame, clamped to 100 ms.
-- Zero on the first frame. Use inside 'withAnimationFrame' to advance
-- animation state by the correct amount regardless of ticker jitter.
getAnimDelta :: UI e msg Float
getAnimDelta = gets (animDelta . ctxAnimation)

-- | Total wall-clock seconds elapsed since the application started.
-- Derived by accumulating 'animDelta' each frame; use this to compute
-- animation phase without storing per-component state.
getAnimElapsed :: UI e msg Float
getAnimElapsed = gets (animElapsed . ctxAnimation)

-- | The frame's full 'AnimationState', read directly from a 'UIContext'
-- outside the 'UI' monad — e.g. so a backend can carry it forward into the
-- next 'nextFrameContext' call.
contextAnimation :: UIContext e msg -> AnimationState
contextAnimation = ctxAnimation

-- | Returns the x offset (pixels) of character index @n@ from the start of
-- @text@, using the backend's text measurer.
charOffset :: Text -> Int -> UI e msg Float
charOffset text n = UI $ \ctx -> do
  v <- tmCharOffset (ctxTextMeasure ctx) text n
  pure (v, ctx)

-- | Returns the character index closest to x offset @x@ in @text@, using the
-- backend's text measurer.
charAtOffset :: Text -> Float -> UI e msg Int
charAtOffset text x = UI $ \ctx -> do
  v <- tmCharAtOffset (ctxTextMeasure ctx) text x
  pure (v, ctx)

-- | Returns the pixel dimensions of @text@ as rendered by the current font.
measureText :: Text -> UI e msg Size
measureText text = UI $ \ctx -> do
  v <- tmTextSize (ctxTextMeasure ctx) text
  pure (v, ctx)
