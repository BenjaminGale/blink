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

Focus is tracked as a 'Focus' chain, not a single flat element: a plain
control holds a bare 'FocusSingle', but a composite (a list, a tree —
anything with sub-items) can be focused with no child chosen yet
('FocusComposite' with a 'FocusNothing' tail) or with a specific child
current ('FocusComposite' wrapping that child's own 'Focus'), nested to
arbitrary depth for composites of composites. 'withinComposite' is where
composites actually build and consume these chains.

  * 'isFocused' — non-exclusive, the way 'Blink.Controls.isMouseOver' already is for
    geometric nesting (CSS's @:focus-within@, generalised to also cover plain
    exact-match for free): 'True' for every id along the current chain, not
    just the terminal one. A leaf control checking its own id behaves exactly
    as it always has; an ancestor composite now correctly sees 'True' for its
    own id whenever focus is anywhere inside it.
  * 'setFocus' \/ 'clearFocus' — set or clear focus for a single element, as
    a bare 'FocusSingle'\/'FocusNothing'. 'withinComposite' is where a
    composite writes its own 'FocusComposite' shape instead.
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
  , setHot
  , registerMouseOver
  , wasMouseOverLastFrame
  , isAnyMouseOver
  , isButtonDown
  , isButtonReleased
  , isDragging
  , isMouseFree
  , getCapturedElement
  , contextCaptured
  , contextButtonDown
  , contextButtonReleased
    -- * Focus and keyboard navigation
  , Focus (..)
  , isNothingFocused
  , focusContains
  , getFocus
  , isFocused
  , setFocus
  , setFocusWhen
  , clearFocus
  , withinComposite
  , consumeKey
  , getPreviousTabStop
  , setPreviousTabStop
  , contextFocus
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
  , AnimationState (..)
  , requiresAnimation
  , withAnimationFrame
  , getAnimDelta
  , getAnimElapsed
  , contextAnimation
  ) where

import Control.Monad (when, unless)
import Data.List (foldl')
import Data.Maybe (isNothing, listToMaybe)
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

-- | The shape of the tree-wide focus value. A plain, non-composite-aware
-- control only ever produces\/consumes 'FocusNothing' or 'FocusSingle' — the
-- same two-state model this replaced. A composite (a list, a tree — anything
-- with sub-items) additionally uses 'FocusComposite' to combine its own id
-- with whatever focus its children currently hold, nested to arbitrary depth
-- for composites of composites; see 'withinComposite' for how composites
-- build and consume these chains, and 'isFocused' for how an id is matched
-- against one.
--
-- 'FocusSingle' eid and @'FocusComposite' eid 'FocusNothing'@ are
-- semantically identical for everything this type is used for — kept as
-- separate constructors for now as a stylistic distinction ("never has
-- children" vs. "has none right now").
data Focus e
  = FocusNothing
    -- ^ Nothing is focused anywhere in the tree.
  | FocusSingle e
    -- ^ A plain element holds focus, with no composite wrapping it.
  | FocusComposite e (Focus e)
    -- ^ A composite holds focus; the wrapped 'Focus' is whatever its
    -- children currently hold ('FocusNothing' if none has been chosen yet).
  deriving (Eq, Show)

-- | 'True' when no element anywhere is focused — the top-level
-- 'FocusNothing' case, not merely "no child chosen yet" (a composite that
-- has claimed itself with @'FocusComposite' compositeId 'FocusNothing'@ is
-- not "nothing focused"). Pattern-matches directly rather than requiring
-- @Eq e@, so it's usable wherever a plain 'Bool' guard is more convenient
-- than matching on 'Focus' by hand.
isNothingFocused :: Focus e -> Bool
isNothingFocused FocusNothing = True
isNothingFocused _            = False

-- | Tracks which element holds keyboard focus and whether it was visited
-- during the current frame's render pass.
data FocusState e = FocusState
  { focusedElement   :: Focus e
    -- ^ The current focus chain; 'FocusNothing' if nothing is focused.
  , focusedThisFrame :: Bool
    -- ^ 'True' if the focused element was encountered during this frame's render pass.
    -- Used to clear stale focus when a focused element is no longer present in the UI.
  }

-- | Per-frame interactive targeting state: which element the mouse is over,
-- which holds capture during a drag, which has keyboard focus, and which was
-- the most recent tab stop. Reset and carried forward by 'nextFrameContext'.
--
-- 'ixnButtonDown'\/'ixnButtonReleased' are derived by 'nextInteractionFrame'
-- from the previous and current raw button-down values. Four states arise
-- from the two-frame comparison:
--
-- @
-- prev  curr  ixnButtonDown  ixnButtonReleased  meaning
-- ----  ----  -------------  -----------------  -------
-- F     F     False          False              Up       — not held, no event
-- F     T     True           False              Pressed  — went down this frame
-- T     T     True           False              Down     — held, no event
-- T     F     False          True               Released — went up this frame
-- @
--
-- Controls read 'ixnButtonDown' for press state and 'ixnButtonReleased' for
-- click detection. Capture is held through Released so that drag-release can be
-- distinguished from a plain click.
data InteractionState e = InteractionState
  { ixnCaptured        :: Maybe e
    -- ^ The element holding mouse capture during a drag, if any. See 'nextCapture'.
  , ixnFocus           :: FocusState e
    -- ^ Which element holds keyboard focus, and whether it's still present this frame.
  , ixnPrevTabStop     :: Maybe e
    -- ^ The element visited just before the currently focused one, for Shift-Tab.
  , ixnCompositePrevTabStop :: Map.Map e e
    -- ^ Per-composite equivalent of 'ixnPrevTabStop', keyed by composite id:
    -- the last child registered as a tab stop while that composite last ran.
    -- Keeps Tab\/Shift-Tab wraparound within one composite's children scoped
    -- to that composite instead of reaching into the surrounding tree via
    -- the single app-wide 'ixnPrevTabStop' slot. See 'withinComposite'.
  , ixnButtonDown      :: Bool
    -- ^ 'True' when the left button is currently held (Pressed or Down state).
  , ixnButtonReleased  :: Bool
    -- ^ 'True' on the one frame the left button transitions from held to up.
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
  , ctxInteraction     :: InteractionState e
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

emptyInteractionState :: InteractionState e
emptyInteractionState = InteractionState
  { ixnCaptured             = Nothing
  , ixnFocus                = FocusState { focusedElement = FocusNothing, focusedThisFrame = False }
  , ixnPrevTabStop          = Nothing
  , ixnCompositePrevTabStop = Map.empty
  , ixnButtonDown           = False
  , ixnButtonReleased       = False
  }

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
  , ctxAnimation       = AnimationState { animDelta = 0, animElapsed = 0, animIsTick = False }
  , ctxTextMeasure     = measurer
  , ctxInteraction     = emptyInteractionState { ixnButtonDown = inputLeftButtonDown input }
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
  , ctxInteraction = nextInteractionFrame
      (inputLeftButtonDown (ctxInput ctx))
      (inputLeftButtonDown input)
      (ctxInteraction ctx)
  , ctxElements    = (ctxElements ctx)
      { elmMouseOverPrev = outMouseOverThisFrame (ctxOutputs ctx) }
  , ctxOutputs     = emptyFrameOutputs
  }
  where
    ctx = applyUiEffects (getUiEffects ctx0) ctx0

-- | Advances 'InteractionState' to the next frame: derives button transition
-- state from the previous and current raw down values, advances capture,
-- and carries focus forward only if it was visited this frame. The previous
-- tab stop is preserved for Shift-Tab navigation.
nextInteractionFrame :: Bool -> Bool -> InteractionState e -> InteractionState e
nextInteractionFrame prevDown currDown ixn = ixn
  { ixnButtonDown     = currDown
  , ixnButtonReleased = prevDown && not currDown
  , ixnCaptured       = nextCapture prevDown currDown (ixnCaptured ixn)
  , ixnFocus          = nextFocusFrame (ixnFocus ixn)
  }

gets :: (UIContext e msg -> a) -> UI e msg a
gets f = UI $ \ctx -> pure (f ctx, ctx)

modify :: (UIContext e msg -> UIContext e msg) -> UI e msg ()
modify f = UI $ \ctx -> pure ((), f ctx)

modifyIxn :: (InteractionState e -> InteractionState e) -> UI e msg ()
modifyIxn f = modify $ \ctx -> ctx { ctxInteraction = f (ctxInteraction ctx) }

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
-- used by 'Blink.Controls.control' to implement Shift-Tab navigation.
getPreviousTabStop :: UI e msg (Maybe e)
getPreviousTabStop = gets contextPrevTabStop

-- | The element that was the most recent tab stop before the current one,
-- read directly from a 'UIContext' outside the 'UI' monad.
contextPrevTabStop :: UIContext e msg -> Maybe e
contextPrevTabStop = ixnPrevTabStop . ctxInteraction

-- | Records the current element as the previous tab stop. Called automatically
-- by 'Blink.Controls.control'; call manually when building custom focusable controls.
setPreviousTabStop :: e -> UI e msg ()
setPreviousTabStop eid = modifyIxn $ \ixn -> ixn { ixnPrevTabStop = Just eid }

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

-- | 'True' when the left button is currently held (Pressed or Down state).
isButtonDown :: UI e msg Bool
isButtonDown = gets contextButtonDown

-- | 'True' when the left button is currently held, read directly from a
-- 'UIContext' outside the 'UI' monad.
contextButtonDown :: UIContext e msg -> Bool
contextButtonDown = ixnButtonDown . ctxInteraction

-- | 'True' on the one frame the left button transitions from held to up.
isButtonReleased :: UI e msg Bool
isButtonReleased = gets contextButtonReleased

-- | 'True' on the one frame the left button transitions from held to up,
-- read directly from a 'UIContext' outside the 'UI' monad.
contextButtonReleased :: UIContext e msg -> Bool
contextButtonReleased = ixnButtonReleased . ctxInteraction

-- | Derives the next frame's captured element from the button transition.
-- Capture is held while the button is down and through the release frame so
-- that a control's focus handling can distinguish a drag release from a
-- plain click. Cleared as soon as the button was not down last frame — both
-- when it is fully up and on a fresh press — so a new press never inherits a
-- stale capture from a previous drag\/click cycle.
-- Acquisition — setting capture in the first place — happens in 'setHot'.
nextCapture :: Bool -> Bool -> Maybe e -> Maybe e
nextCapture prevDown _currDown existing
  | prevDown  = existing
  | otherwise = Nothing

-- | 'True' on every frame that the given element is being dragged — from the
-- initial press through to release.
isDragging :: Eq e => e -> UI e msg Bool
isDragging eid = (== Just eid) <$> gets (ixnCaptured . ctxInteraction)

-- | The element that currently holds mouse capture, or 'Nothing' when no drag
-- is in progress. Exported for control authors that need to inspect capture
-- state directly, e.g. when implementing focus-on-click without using
-- 'Blink.Controls.control'.
getCapturedElement :: UI e msg (Maybe e)
getCapturedElement = gets contextCaptured

-- | The element that currently holds mouse capture, or 'Nothing', read
-- directly from a 'UIContext' outside the 'UI' monad.
contextCaptured :: UIContext e msg -> Maybe e
contextCaptured = ixnCaptured . ctxInteraction

-- | Acquires mouse capture for the element if the left button is currently
-- down and nothing is captured yet, making this the first point of capture
-- for that press — the "hot" control a drag holds onto once the cursor
-- leaves the element that started it.
setHot :: e -> UI e msg ()
setHot eid = modifyIxn $ \ixn ->
  if ixnButtonDown ixn && isNothing (ixnCaptured ixn)
  then ixn { ixnCaptured = Just eid }
  else ixn

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

-- | The current tree-wide focus chain; 'FocusNothing' if nothing is focused.
getFocus :: UI e msg (Focus e)
getFocus = gets contextFocus

-- | The current tree-wide focus chain, read directly from a 'UIContext'
-- outside the 'UI' monad.
contextFocus :: UIContext e msg -> Focus e
contextFocus = focusedElement . ixnFocus . ctxInteraction

-- | 'True' when the given element id appears anywhere along a 'Focus'
-- chain — not just as the terminal (innermost) element. The terminal
-- element is always part of its own chain, so a leaf control checking its
-- own id is unaffected by this; an ancestor composite additionally matches
-- for its own id whenever focus is anywhere inside it. Pure, so it can be
-- tested directly against hand-built 'Focus' values; 'isFocused' is this
-- applied to the current tree-wide focus.
focusContains :: Eq e => e -> Focus e -> Bool
focusContains _ FocusNothing            = False
focusContains x (FocusSingle e)         = x == e
focusContains x (FocusComposite e rest) = x == e || focusContains x rest

-- | 'True' when the given element id appears anywhere along the current
-- focus chain — see 'focusContains'.
isFocused :: Eq e => e -> UI e msg Bool
isFocused eid = focusContains eid <$> getFocus

-- | Transfers keyboard focus to the given element. If it already holds
-- focus, its existing focus (including any 'FocusComposite' structure) is
-- retained rather than replaced; otherwise it takes focus fresh, as a bare
-- 'FocusSingle'. Takes effect immediately — like 'registerMouseOver' and
-- mouse capture, not like the deferred scroll\/selection writes — because a
-- control's own focus decision (take it when nothing else has it, hand off
-- on Tab) is only correct if the next sibling in the same tree walk can see
-- it happened.
setFocus :: Eq e => e -> UI e msg ()
setFocus eid = modifyIxn $ \ixn ->
  let current = focusedElement (ixnFocus ixn)
      retained = case current of
        FocusSingle e'      | e' == eid -> current
        FocusComposite e' _ | e' == eid -> current
        _                                -> FocusSingle eid
  in ixn { ixnFocus = FocusState { focusedElement = retained, focusedThisFrame = True } }

-- | Transfers keyboard focus to the given element when the condition is
-- 'True'.
setFocusWhen :: Eq e => Bool -> e -> UI e msg ()
setFocusWhen b eid = when b (setFocus eid)

-- | Removes keyboard focus from all elements. Immediate, like 'setFocus'.
clearFocus :: UI e msg ()
clearFocus = modifyIxn $ \ixn -> ixn { ixnFocus = (ixnFocus ixn) { focusedElement = FocusNothing } }

-- | Marks a sub-tree as belonging to a composite element (a list, a tree —
-- anything with sub-items) for focus purposes, the way
-- 'Blink.Controls.isMouseOver' generalises to geometric nesting.
-- Non-exclusive: true for every id along the current chain, not just the
-- terminal one, so an ancestor composite
-- correctly sees itself as focused whenever focus is anywhere inside it —
-- see 'isFocused'.
--
-- Takes the focus value to expose as an explicit argument (@exposed@)
-- rather than reading the tree-wide ambient focus itself, so a caller that
-- has already settled this frame's focus can hand that value down (or
-- substitute a placeholder) instead of having it re-derived here. A
-- standalone caller with no such value of its own just passes its current
-- 'getFocus' straight through.
--
-- Asks one question of @exposed@: is this composite's own id at the front
-- of it, is nothing focused at all, or does something else entirely
-- already hold it?
--
--   * Own id at the front (@'FocusComposite' compositeId childFocus@):
--     strips the tag, exposing @childFocus@ to @inner@ unwrapped — children
--     compare against exactly the value they'd recognise standalone.
--   * Nothing focused (@'FocusNothing'@): passes it through unchanged —
--     children are free to auto-claim via their own ordinary logic, exactly
--     as they would standalone.
--   * Anything else — a bare @'FocusSingle' compositeId@ (this composite
--     itself is the exact focus target, no child chosen), an unrelated
--     element, or a different composite's own chain: @inner@ runs against
--     that value completely unmodified, and the result is left exactly as
--     it was: no wrap, no overwrite. (Passing 'FocusNothing' here instead
--     would tell every descendant "nothing is focused," letting one of
--     them auto-claim and steal focus that already legitimately belongs to
--     this composite itself, or to something outside it, purely because
--     this composite happened to render later in the same pass.)
--
-- In the first two cases, once @inner@ finishes, whatever focus resulted is
-- re-wrapped as @'FocusComposite' compositeId after@ — unconditionally,
-- even when @after@ is 'FocusNothing'. This re-affirms "composite focused,
-- no child chosen" every frame the composite renders, the same way a plain
-- focused control's own focus rules re-affirm its focus every frame it
-- renders — without this, that state would silently decay to 'FocusNothing'
-- one frame later, since nothing else reaffirms it.
--
-- A per-composite previous-tab-stop is substituted for the real one while
-- @inner@ runs, so Tab\/Shift-Tab wraps within this composite's own children.
--
-- Composes for arbitrary nesting: a composite inside another composite's
-- @withinComposite@ only ever strips\/re-wraps its own outermost tag, and
-- does the same expose\/render\/re-wrap step around its own children.
--
-- = Invariant: a disabled composite never holds focus
--
-- A disabled control must never appear to hold keyboard focus, even in the
-- vacuous "composite focused, no child chosen" shape ('FocusComposite'
-- compositeId 'FocusNothing'). While disabled, this function runs @inner@
-- against the ambient context completely unmodified: no substitution, no
-- claim, no re-wrap -- so it can neither claim focus for the composite nor
-- leave a stale claim behind, regardless of what @exposed@ says and
-- regardless of whether the caller remembered to check 'isDisabled' itself.
-- This is enforced here, once, rather than left as a convention every
-- caller (present or future) has to uphold on its own -- see the
-- integration coverage in "Blink.ControlsSpec" for the regression this
-- guards against.
withinComposite :: Ord e => e -> Focus e -> UI e msg a -> UI e msg a
withinComposite compositeId exposed (UI f) = UI $ \ctx ->
  if ctxDisabled ctx
    then f ctx
    else case exposed of
      FocusComposite cid childFocus | cid == compositeId -> claim ctx childFocus
      FocusNothing                                        -> claim ctx FocusNothing
      _                                                    -> passthrough ctx
  where
    -- Substitutes @exposed@ for the ambient focus while @inner@ runs, same
    -- as 'claim' does. If @exposed@ comes back untouched, the real
    -- pre-substitution focus is restored instead (so a placeholder that
    -- blocked a claim doesn't linger). Otherwise something inside @inner@ --
    -- necessarily one of this composite's own children, since nothing else
    -- runs meanwhile -- claimed focus for itself, so the result is wrapped
    -- as belonging to this composite, the same as 'claim' would.
    passthrough ctx = do
      let ixn0  = ctxInteraction ctx
          real  = focusedElement (ixnFocus ixn0)
          ctxIn = ctx { ctxInteraction = ixn0 { ixnFocus = (ixnFocus ixn0) { focusedElement = exposed } } }
      (a, ctx'') <- f ctxIn
      let ixn''  = ctxInteraction ctx''
          after  = focusedElement (ixnFocus ixn'')
          final
            | after == exposed = real
            | otherwise         = FocusComposite compositeId after
          ixn''' = ixn'' { ixnFocus = (ixnFocus ixn'') { focusedElement = final } }
      pure (a, ctx'' { ctxInteraction = ixn''' })

    claim ctx exposedFocus = do
      let ixn0       = ctxInteraction ctx
          realPrev   = ixnPrevTabStop ixn0
          scopedPrev = Map.lookup compositeId (ixnCompositePrevTabStop ixn0)
          ixnIn      = ixn0
            { ixnFocus       = (ixnFocus ixn0) { focusedElement = exposedFocus }
            , ixnPrevTabStop = scopedPrev
            }
      (a, ctx'') <- f (ctx { ctxInteraction = ixnIn })
      let ixn''      = ctxInteraction ctx''
          scoped     = case ixnPrevTabStop ixn'' of
            Just v  -> Map.insert compositeId v (ixnCompositePrevTabStop ixn'')
            Nothing -> Map.delete compositeId (ixnCompositePrevTabStop ixn'')
          flowedPrev = if ixnPrevTabStop ixn'' == scopedPrev then realPrev else ixnPrevTabStop ixn''
          after      = focusedElement (ixnFocus ixn'')
          ixn'''     = ixn''
            { ixnCompositePrevTabStop = scoped
            , ixnPrevTabStop          = flowedPrev
            , ixnFocus                = FocusState
                { focusedElement   = FocusComposite compositeId after
                , focusedThisFrame = True
                }
            }
      pure (a, ctx'' { ctxInteraction = ixn''' })

-- | Advances a 'FocusState' to the next frame: carries focus forward if it
-- was explicitly set this frame, otherwise clears it. Used by 'nextInteractionFrame'.
nextFocusFrame :: FocusState e -> FocusState e
nextFocusFrame fs = FocusState
  { focusedElement   = if focusedThisFrame fs
                       then focusedElement fs
                       else FocusNothing
  , focusedThisFrame = False
  }

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
isMouseFree = isNothing <$> gets (ixnCaptured . ctxInteraction)

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
