{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module: Blink.View.Context

Internal. The 'View' monad, the 'ViewContext' record, the render loop built
on it, and the state types threaded through it (focus, scroll, extent,
selection, hold, animation, navigation) — kept here, rather than in modules
of their own, because 'ViewContext' embeds them directly and needs their
definitions regardless of whether anything else does.

Feature modules ("Blink.View.Mouse", "Blink.View.Focus",
"Blink.View.Scroll", "Blink.View.Extent", "Blink.View.Selection", "Blink.View.Hold",
"Blink.View.Animation", "Blink.View.Navigation") import this module for
context access and build their own topic's public API on top — pure
helpers and monadic accessors alike. "Blink.View" re-exports the combined
public surface; import that instead of this module directly.
-}
module Blink.View.Context
  ( -- * The View monad
    View (..)
  , ViewContext (..)
  , ElementState (..)
  , SelectionSlot (..)
  , FrameOutputs (..)
  , gets
  , modify
  , withField
  , modifyOut
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
  , Effect (..)
  , UiEffect (..)
  , HasUiEffect (..)
  , emit
  , emitUi
  , queueUiEffects
    -- * Bounds
  , getBounds
  , getWindowSize
  , withBounds
    -- * Drawing
  , draw
  , getInteractionClip
  , withInteractionClip
    -- * Interaction
  , getInput
  , contextInput
  , consumeKey
  , withoutKeyEvents
    -- * Focus scope (raw)
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
    -- * Image measurement
  , measureImage
  , withMeasurers
    -- * Disabled state
  , isDisabled
  , disableWhen
  , whenEnabled
    -- * Focus (pure)
  , FocusClaim (..)
  , currentFocus
  , isGained
  , tryClaim
  , tickFocusClaim
  , LostFocus (..)
  , pendingLostFocus
  , tickLostFocus
  , FocusState (..)
  , emptyFocusState
  , isNothingFocused
  , nextFocusFrame
  , reaffirm
  , FocusTracker (..)
  , emptyFocusTracker
  , lookupScope
  , nextFocusTrackerFrame
  , FreshClaim (..)
  , ScopeMode (..)
  , scopeMode
    -- * Scroll (pure)
  , ScrollState (..)
  , clampScrollPos
  , writeScrollState
    -- * Extent (pure)
  , ExtentState (..)
    -- * Cursor index (pure)
  , CursorIndexState (..)
    -- * Selection (pure)
  , Selection (..)
  , selectionLow
  , selectionHigh
  , selectionHasExtent
  , cursor
  , collapseToLow
  , collapseToHigh
  , collapseToActive
  , extendActive
    -- * Hold (pure)
  , HoldState (..)
  , repeatsDueBy
    -- * Animation (pure)
  , AnimationState (..)
  , mkAnimationState
    -- * Navigation (pure)
  , NavigationKeys (..)
  , defaultNavigationKeys
  ) where

import Control.Monad (unless)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Blink.Rendering (DrawCommand, Measurers (..), noOpMeasurers, TextMeasurer (..), ImageMeasurer (..), ImagePath)
import Blink.Geometry (Rectangle, Size)
import Blink.Input
  ( Key (..), KeyEvent (..), Modifier (..), InputState (..)
  , Mouse (..), emptyMouse, advanceButton, advanceHover
  )
import Blink.Style (Style, StyleSet, Metrics, StyleKey (..), Theme (..), resolveStyle)

--------------------------------------------------------------------------------
-- Focus (pure)
--------------------------------------------------------------------------------

-- | Which element (if any) a scope currently has focused, and whether that
-- claim has been reaffirmed since it was set. A claim must be reaffirmed
-- every frame by whatever holds it actually rendering and re-claiming it
-- (see 'Blink.View.Focus.setFocus'); one frame of grace ('ClaimedLastFrame')
-- is given before an unreaffirmed claim is dropped, so a claim surviving
-- exactly one frame without a render in between (e.g. the frame a queued
-- 'Focus' effect is applied, before the new holder has rendered even once)
-- isn't mistaken for abandonment. 'GainedThisFrame'\/'GainedLastFrame' are
-- only ever entered by an explicit 'Focus' effect (see @setFocusChange@),
-- never by 'Blink.View.Focus.setFocus' reaffirming a claim -- that's what
-- keeps a self-claim from masquerading as a redirect.
data FocusClaim e
  = Unclaimed
  | ClaimedLastFrame e
    -- ^ Held as of the previous frame boundary but not yet reaffirmed this
    -- pass; dropped to 'Unclaimed' if still unreaffirmed at the next boundary.
  | ClaimedThisFrame e
    -- ^ Reaffirmed during the current frame (or just granted).
  | GainedLastFrame e
    -- ^ Landed via a 'Focus' effect as of the previous frame boundary; its
    -- final frame of visibility to 'Blink.View.Focus.hasGainedFocus', then
    -- it fades to 'ClaimedThisFrame' -- an ordinary held claim from here on.
  | GainedThisFrame e
    -- ^ Landed via a 'Focus' effect this frame boundary.
  deriving (Eq, Show)

-- | The element currently focused, regardless of reaffirmation status.
currentFocus :: FocusClaim e -> Maybe e
currentFocus Unclaimed             = Nothing
currentFocus (ClaimedLastFrame e)  = Just e
currentFocus (ClaimedThisFrame e)  = Just e
currentFocus (GainedLastFrame e)   = Just e
currentFocus (GainedThisFrame e)   = Just e

-- | 'True' when this element is the target of a 'Focus' effect that landed
-- within the last two frames.
isGained :: Eq e => e -> FocusClaim e -> Bool
isGained eid (GainedLastFrame e) = e == eid
isGained eid (GainedThisFrame e) = e == eid
isGained _   _                   = False

-- | Claims focus for @eid@ (see 'Blink.View.Focus.setFocus'): succeeds when
-- nothing currently holds it, or @eid@ is reaffirming itself; refused,
-- unchanged, when a different element already holds it this frame -- so an
-- element calling 'Blink.View.Focus.setFocus' for itself can never steal
-- focus out from under whoever legitimately has it, regardless of render
-- order. Reaffirming a still-fresh 'Gained*' claim for the same element
-- leaves it exactly as it was, so a control reaffirming itself every frame
-- doesn't cut short its own 'Blink.View.Focus.hasGainedFocus' visibility
-- window.
tryClaim :: Eq e => e -> FocusClaim e -> FocusClaim e
tryClaim eid claim
  | isGained eid claim = claim
  | otherwise = case currentFocus claim of
      Nothing                     -> ClaimedThisFrame eid
      Just holder | holder == eid -> ClaimedThisFrame eid
                  | otherwise     -> claim

-- | Advances a 'FocusClaim' to the next frame: a reaffirmed claim gets one
-- frame of grace before it must be reaffirmed again; a claim already on
-- grace that wasn't reaffirmed again is dropped. A 'Gained*' claim ages down
-- the same way, but past 'GainedLastFrame' it settles into an ordinary
-- 'ClaimedThisFrame' rather than being dropped -- it's still held, just no
-- longer freshly granted.
tickFocusClaim :: FocusClaim e -> FocusClaim e
tickFocusClaim (GainedThisFrame e)  = GainedLastFrame e
tickFocusClaim (GainedLastFrame e)  = ClaimedThisFrame e
tickFocusClaim (ClaimedThisFrame e) = ClaimedLastFrame e
tickFocusClaim (ClaimedLastFrame _) = Unclaimed
tickFocusClaim Unclaimed            = Unclaimed

-- | Whether a scope has a displaced element pending observation, and for
-- how much longer. A redirect is visible for exactly one full frame
-- regardless of when during that frame it happened, so every element gets a
-- chance to see it however render order falls; the two "pending"
-- constructors carry the same @Maybe e@ but tell 'nextFocusFrame' whether
-- this is its first or last frame of visibility.
data LostFocus e
  = NothingLost
  | LostLastFrame (Maybe e)
    -- ^ Was pending as of the previous frame boundary; this is its final
    -- frame of visibility, then it expires to 'NothingLost'.
  | LostThisFrame (Maybe e)
    -- ^ Just happened during the current frame.
  deriving (Eq, Show)

-- | The element a scope's elements may still observe as displaced, if any.
-- See 'Blink.View.Focus.hasLostFocus'.
pendingLostFocus :: LostFocus e -> Maybe (Maybe e)
pendingLostFocus NothingLost       = Nothing
pendingLostFocus (LostLastFrame f) = Just f
pendingLostFocus (LostThisFrame f) = Just f

-- | Advances a @LostFocus@ to the next frame: a loss just noticed gets one
-- more frame of visibility before it expires.
tickLostFocus :: LostFocus e -> LostFocus e
tickLostFocus (LostThisFrame f) = LostLastFrame f
tickLostFocus (LostLastFrame _) = NothingLost
tickLostFocus NothingLost       = NothingLost

-- | Per-scope focus bookkeeping: which single child (if any) currently holds
-- focus within this scope, the last tab stop visited within it (for
-- Shift-Tab), and any pending focus-change notice. Root and every composite
-- (a list, a tree — anything with sub-items, see
-- 'Blink.View.Focus.withFocusScope') each own one of these; a composite's
-- own is persisted in @ftScopes@ between frames, the same way scroll and
-- selection state persist per element.
data FocusState e = FocusState
  { focusClaim      :: FocusClaim e
    -- ^ The element this scope currently has focused, if any, and its
    -- reaffirmation status. See 'FocusClaim'.
  , previousTabStop :: Maybe e
    -- ^ The element visited just before the current one, scoped to this
    -- level, for Shift-Tab.
  , focusLost       :: LostFocus e
    -- ^ The element this scope's most recent redirect displaced, if still
    -- within its one-frame observation window. See @LostFocus@.
  }

-- | The empty, never-focused 'FocusState' — root's initial value, and every
-- composite's the first time it renders.
emptyFocusState :: FocusState e
emptyFocusState = FocusState
  { focusClaim      = Unclaimed
  , previousTabStop = Nothing
  , focusLost       = NothingLost
  }

-- | 'True' when nothing is focused in the given scope. Pattern-matches
-- directly rather than requiring @Eq e@, so it's usable wherever a plain
-- 'Bool' guard is more convenient than matching by hand.
isNothingFocused :: Maybe e -> Bool
isNothingFocused Nothing  = True
isNothingFocused (Just _) = False

-- | Advances a 'FocusState' to the next frame: ticks 'focusClaim' via
-- 'tickFocusClaim', so a claim not reaffirmed for a full frame expires to
-- 'Unclaimed'. Applied to the root scope and every entry in @ftScopes@ — a
-- scope that stops being reaffirmed (its composite removed from the tree, or
-- its specific focused child gone while the composite itself still renders)
-- expires independently, the same way root-level focus already did.
-- 'previousTabStop' is untouched here and simply persists, the same way
-- @elmScrollStates@ is never purged for elements that stop rendering.
--
-- Also advances 'focusLost' independently of that, via 'tickLostFocus': a
-- loss just noticed stays visible for exactly one more frame so every
-- element gets a chance to observe it regardless of render order, then
-- expires the frame after.
nextFocusFrame :: FocusState e -> FocusState e
nextFocusFrame fs = fs
  { focusClaim = tickFocusClaim (focusClaim fs)
  , focusLost  = tickLostFocus (focusLost fs)
  }

-- | Renews a claim that's aged into its one-frame grace period back to
-- freshly reaffirmed, without touching a claim that's already fresh or
-- already gone. Used when folding a scope's result back into its persisted
-- state (see 'Blink.View.Focus.withFocusScope'): being written back here
-- proves the scope was actually visited this frame, so its claim shouldn't
-- be allowed to decay any further towards 'tickFocusClaim'\'s expiry -- only
-- a scope that genuinely stops being visited (its composite removed from
-- the tree) should expire.
reaffirm :: FocusClaim e -> FocusClaim e
reaffirm Unclaimed             = Unclaimed
reaffirm (ClaimedLastFrame e)  = ClaimedThisFrame e
reaffirm (ClaimedThisFrame e)  = ClaimedThisFrame e
reaffirm (GainedLastFrame e)   = GainedLastFrame e
reaffirm (GainedThisFrame e)   = GainedThisFrame e

-- | Per-frame keyboard-focus targeting state: which element has focus, and
-- which was the most recent tab stop. Reset and carried forward by
-- 'nextFrameContext'. Unlike mouse\/capture state, focus also advances on a
-- re-render of the same frame (see 'rerenderContext'), since a scope that
-- goes unclaimed on a re-render should expire even though the button
-- reading hasn't changed.
data FocusTracker e = FocusTracker
  { ftAmbient :: FocusState e
    -- ^ The *currently ambient* scope's own focus state — root's, unless a
    -- 'Blink.View.Focus.withFocusScope' call further up the stack has
    -- swapped it for a composite's own.
  , ftScopes  :: Map.Map e (FocusState e)
    -- ^ Every composite's own persisted 'FocusState', flat, keyed directly
    -- by scope id regardless of nesting depth — the same shape as
    -- @elmScrollStates@. See 'Blink.View.Focus.withFocusScope'.
  }

-- | The empty 'FocusTracker' — nothing focused anywhere, no composite scopes
-- recorded yet.
emptyFocusTracker :: FocusTracker e
emptyFocusTracker = FocusTracker { ftAmbient = emptyFocusState, ftScopes = Map.empty }

-- | A composite scope's own persisted 'FocusState', or 'emptyFocusState' if
-- it hasn't rendered yet.
lookupScope :: Ord e => e -> FocusTracker e -> FocusState e
lookupScope scopeId ft = Map.findWithDefault emptyFocusState scopeId (ftScopes ft)

-- | Advances a 'FocusTracker' to the next frame by applying 'nextFocusFrame'
-- to the ambient scope and every persisted composite scope.
nextFocusTrackerFrame :: FocusTracker e -> FocusTracker e
nextFocusTrackerFrame ft = ft
  { ftAmbient = nextFocusFrame (ftAmbient ft)
  , ftScopes  = Map.map nextFocusFrame (ftScopes ft)
  }

-- | Whether a fresh (unclaimed) ambient may be read as an invitation for a
-- scope to auto-claim focus this frame — see 'Blink.View.Focus.withFocusScope'.
data FreshClaim = AllowFreshClaim | BlockFreshClaim
  deriving (Eq, Show)

-- | Which of the two policies documented on 'Blink.View.Focus.withFocusScope'
-- applies this frame: 'Claim' if the scope is (or is free to become) the
-- live focus target, 'Blocked' with the ambient value descendants should see
-- otherwise.
data ScopeMode e = Claim | Blocked (Maybe e)

-- | Resolves which 'ScopeMode' applies to a scope this frame, given its id,
-- its 'FreshClaim' policy, and the ambient scope's currently focused element.
scopeMode :: Eq e => e -> FreshClaim -> Maybe e -> ScopeMode e
scopeMode scopeId freshClaim currentAmbient = case currentAmbient of
  Just cid | cid == scopeId          -> Claim
  Nothing  | freshClaim == AllowFreshClaim -> Claim
  Nothing                            -> Blocked (Just scopeId)
  real                               -> Blocked real

--------------------------------------------------------------------------------
-- Scroll (pure)
--------------------------------------------------------------------------------

-- | Per-instance scroll position in @[0, 1]@.
newtype ScrollState = ScrollState { scrollPosition :: Double }
  deriving (Eq, Ord, Show)

-- | Clamp a scroll position to @[0, 1]@.
clampScrollPos :: Double -> Double
clampScrollPos = max 0 . min 1

-- | An accumulated offset for one element, in whatever unit the caller
-- gives it (e.g. pixels) -- unlike 'ScrollState', never clamped to
-- @[0, 1]@; a caller wanting bounds of its own (e.g. a minimum column
-- width) applies them itself when reading it back. Defaults to @0@ for
-- an element nothing has adjusted yet.
newtype ExtentState = ExtentState { extentValue :: Double }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Cursor index (pure)
--------------------------------------------------------------------------------

-- | The row index a list-like control's cursor last held, so
-- 'Blink.Controls.List.listBase' can notice it moving between frames.
-- Absent for an element nothing has recorded yet.
newtype CursorIndexState = CursorIndexState { cursorIndexValue :: Int }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Selection (pure)
--------------------------------------------------------------------------------

-- | A contiguous selection or cursor within a linear sequence. The selected
-- range is @(min anchor active, max anchor active)@. When @anchor == active@
-- the selection is a cursor with no extent.
data Selection = Selection
  { selectionAnchor :: Int  -- ^ The fixed end.
  , selectionActive :: Int  -- ^ The moving end (cursor position).
  }
  deriving (Eq, Show)

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

--------------------------------------------------------------------------------
-- Hold (pure)
--------------------------------------------------------------------------------

-- | Per-element bookkeeping for a control that keeps firing while held down
-- -- e.g. 'Blink.Controls.RepeatButton.repeatButton' -- rather than
-- once per press: the animation clock's elapsed time when the current
-- continuous press began, and how many repeats have fired during it so
-- far.
data HoldState = HoldState
  { holdStartedAt  :: Double
  , holdFiredCount :: Int
  } deriving (Eq, Ord, Show)

-- | How many repeats should have fired by the time @heldFor@ seconds have
-- elapsed since a press began, given @initialDelay@ (seconds held before
-- the first repeat) and @interval@ (seconds between each one thereafter):
-- none before @initialDelay@, then one at that instant and one more every
-- @interval@ after. A pure function of @heldFor@ alone -- a caller diffs
-- this against a stored 'holdFiredCount' rather than against last frame's
-- @heldFor@, so a long frame (a hitch, or several interval crossings at
-- once) still fires every repeat it stepped over rather than silently
-- dropping them.
repeatsDueBy :: Double -> Double -> Double -> Int
repeatsDueBy initialDelay interval heldFor
  | heldFor < initialDelay = 0
  | otherwise              = floor ((heldFor - initialDelay) / interval) + 1

--------------------------------------------------------------------------------
-- Animation (pure)
--------------------------------------------------------------------------------

-- | Per-frame animation state threaded through 'ViewContext'. Set by the
-- backend at the start of each frame; read by
-- 'Blink.View.Animation.withAnimationFrame' and
-- 'Blink.View.Animation.getAnimDelta'.
data AnimationState = AnimationState
  { animDelta   :: Float
    -- ^ Wall-clock seconds elapsed since the previous frame, clamped to
    -- @[0, 0.1]@ seconds. Zero on the first frame.
  , animElapsed :: Float
    -- ^ Total wall-clock seconds elapsed since the application started,
    -- accumulated from 'animDelta' each frame.
  , animIsTick  :: Bool
    -- ^ 'True' when this frame was triggered by the animation ticker rather
    -- than a platform input event.
  }

-- | Constructs an 'AnimationState', clamping the delta to @[0, 0.1]@ seconds
-- so the bound documented on 'animDelta' holds regardless of caller —
-- "Blink.View" doesn't re-export the constructor, so this is the only way
-- to build one from outside this module and its siblings.
mkAnimationState :: Float -> Float -> Bool -> AnimationState
mkAnimationState delta elapsed isTick = AnimationState
  { animDelta   = max 0 (min 0.1 delta)
  , animElapsed = elapsed
  , animIsTick  = isTick
  }

--------------------------------------------------------------------------------
-- Navigation (pure)
--------------------------------------------------------------------------------

-- | The specific key\/modifier combinations that currently mean "give up
-- focus here and let the next render claim it" ('navAdvance') or "return to
-- whichever tab stop was previous" ('navRetreat'). Every
-- 'Blink.Controls.Control.control' consults this instead of a
-- hardcoded Tab\/Shift-Tab, so a container can redefine it for its own
-- children by opening a new ambient set around them with
-- 'Blink.View.Navigation.withNavigationKeys'.
data NavigationKeys = NavigationKeys
  { navAdvance :: [(Key, [Modifier])]
  , navRetreat :: [(Key, [Modifier])]
  } deriving (Eq, Show)

-- | Plain Tab\/Shift-Tab — what every control uses unless some enclosing
-- container has redefined it. The root ambient default.
defaultNavigationKeys :: NavigationKeys
defaultNavigationKeys = NavigationKeys
  { navAdvance = [(KeyTab, [])]
  , navRetreat = [(KeyTab, [Shift])]
  }

--------------------------------------------------------------------------------
-- The View monad and ViewContext
--------------------------------------------------------------------------------

-- | A cross-frame presentation effect: a scroll, selection, or explicit
-- focus change that takes effect starting the next frame rather than
-- immediately. Queued with 'emitUi' and applied by @applyUiEffects@, which
-- 'nextFrameContext' runs automatically between frames.
--
-- Focus claimed\/cleared by an element acting on *itself* (auto-claim,
-- self-clear) is not represented here — see @Blink.View.Focus.setFocus@
-- for why that changes immediately instead. @Focus@\/@ClearFocus@ are
-- specifically for an explicit "make a different, named element focused (or
-- clear whoever is)" change, triggered from a place that only knows the
-- winner (or that there's no winner), not who's currently focused —
-- 'Blink.Controls.Label.label' redirecting a click onto its
-- 'Blink.Controls.Label.target', say. Whoever is displaced is looked up
-- when the effect is *applied* (real 'ViewContext' access, unlike the
-- reaction that queued it), and deferring lets every affected element
-- observe the change consistently regardless of render order — see
-- 'FocusClaim' and 'LostFocus'.
--
-- Constructors are exposed to sibling feature modules (e.g.
-- "Blink.View.Focus" constructs @Focus@\/@ClearFocus@) but not re-exported
-- from "Blink.View": produced only via 'Blink.View.Scroll.requestScrollTo',
-- 'Blink.View.Scroll.requestScrollBy', 'Blink.View.Scroll.postScrollBy',
-- 'Blink.View.Extent.requestExtentBy', 'Blink.View.Selection.requestSelectionAt',
-- @Blink.View.Focus.requestFocus@, or @Blink.View.Focus.requestClearFocus@ --
-- each of which, via 'HasUiEffect', also works from a
-- 'Blink.Update.Update' handler reacting to a message, not only from
-- view code reacting to an input event.
data UiEffect e
  = ScrollTo e Double
    -- ^ Sets the scroll position to an absolute value, clamped to @[0, 1]@
    -- when applied. Every caller ('Blink.Controls.ScrollBar.scrollBar',
    -- 'Blink.Controls.TextInput.textInput') already passes a value in
    -- the @[0, 1]@ convention documented on 'ScrollState';
    -- 'Blink.Controls.TextInput.textInput' converts to and from pixels
    -- locally since its selection\/cursor math is naturally pixel-based.
    -- See 'Blink.View.Scroll.requestScrollTo'.
  | ScrollBy e Double
    -- ^ Adjusts the scroll position by a delta, clamped to @[0, 1]@ — this
    -- constructor is only ever used in the normalised @[0, 1]@ convention.
    -- Composes with other @ScrollBy@ effects queued in the same frame for
    -- the same element rather than last-write-wins. See
    -- 'Blink.View.Scroll.requestScrollBy'\/'Blink.View.Scroll.postScrollBy'.
  | AdjustExtent e Double
    -- ^ Adjusts an element's 'ExtentState' by a delta, unclamped —
    -- composes with other @AdjustExtent@ effects queued in the same
    -- frame for the same element, the same way @ScrollBy@ does. See
    -- 'Blink.View.Extent.requestExtentBy'.
  | SetSelectionAt e Selection
    -- ^ See 'Blink.View.Selection.requestSelectionAt'.
  | SetHoldState e (Maybe HoldState)
    -- ^ Sets ('Just') or clears ('Nothing') an element's repeat-press
    -- state -- see 'HoldState' and 'Blink.View.Hold.resolveHoldRepeats',
    -- its only caller. Last-write-wins; unlike @ScrollTo@\/@ScrollBy@
    -- there's no absolute\/relative pair since a repeat-press has no
    -- meaningful "adjust by" -- a control either anchors a fresh press or
    -- clears one.
  | SetCursorIndex e (Maybe Int)
    -- ^ Sets ('Just') or clears ('Nothing') a list-like control's
    -- 'CursorIndexState' -- see 'Blink.Controls.List.listBase', its only
    -- caller. Last-write-wins, the same as @SetHoldState@.
  | Focus (Maybe e) e
    -- ^ Makes the given element focused within the given scope (@Nothing@ =
    -- root, @Just scopeId@ = the composite scope with that id — see
    -- @Blink.View.Focus.withFocusScope@), applied atomically at the next
    -- frame boundary. See @Blink.View.Focus.requestFocus@.
  | ClearFocus (Maybe e)
    -- ^ Clears whoever is focused within the given scope, with nothing new
    -- claiming it, applied atomically at the next frame boundary — the
    -- "clear" counterpart to @Focus@. See
    -- @Blink.View.Focus.requestClearFocus@.
  deriving (Eq, Show)

-- | One item from the frame's output queue: either an application message or
-- a 'UiEffect'. A single ordered queue holds both so that the relative
-- ordering between a message and an effect emitted in the same frame is
-- preserved.
data Effect e msg
  = EffectMsg msg
  | EffectUi (UiEffect e)
  deriving (Eq, Show)

-- | Cross-frame presentation state. Persists unchanged across frames; never
-- exposed to the application. Scroll position is tracked per element
-- (@elmScrollStates@), as is an unclamped accumulated extent
-- (@elmExtentStates@), repeat-press ("hold") state (@elmHoldStates@), and a
-- list-like control's last-known cursor row index (@elmCursorIndices@);
-- selection is exclusive across elements, tracked as a single
-- 'SelectionSlot' (@elmSelection@) rather than a map.
data ElementState e = ElementState
  { elmScrollStates   :: Map.Map e ScrollState
  , elmExtentStates   :: Map.Map e ExtentState
  , elmHoldStates     :: Map.Map e HoldState
  , elmCursorIndices  :: Map.Map e CursorIndexState
  , elmSelection      :: SelectionSlot e
  }

-- | At most one element holds a selection at a time; setting it on one
-- element clears any other. 'NoSelection' when none does.
data SelectionSlot e
  = NoSelection
  | SelectionAt e Selection

-- | Outputs accumulated during a single frame: draw commands, the queued
-- 'Effect' events (messages and 'UiEffect's, in emit order), and the animation
-- continuation flag. Reset to empty at the start of each frame by
-- 'nextFrameContext'.
data FrameOutputs e msg = FrameOutputs
  { outDrawCommands       :: [DrawCommand]
  , outEvents             :: [Effect e msg]
  , outRequiresAnimation  :: Bool
  }

-- | The frame context threaded through every 'View' computation. Carries the
-- current bounds, input state, active theme, accumulated draw commands, focus
-- state, scroll and selection state, and queued messages. Construct with
-- 'emptyViewContext' or 'nextFrameContext'; extract results with
-- 'getDrawCommands' and 'getMessages'.
--
-- Fields are visible to sibling feature modules ("Blink.View.Mouse",
-- "Blink.View.Focus", ...), which need direct access to implement their
-- own state transitions; "Blink.View" does not re-export them, so this
-- remains an implementation detail for consumers of the library.
--
-- [@e@] Element identity type — identifies focusable\/hoverable controls.
-- [@msg@] Message type emitted by controls via 'emit'; the application never
-- lives in this context.
data ViewContext e msg = ViewContext
  { ctxBounds          :: Rectangle
  , ctxWindowBounds    :: Rectangle
    -- ^ The backend-supplied window rectangle for this frame (origin always
    -- @0,0@), independent of any 'withBounds' narrowing applied while
    -- descending the tree. See 'getWindowSize'.
  , ctxInput           :: InputState
  , ctxTheme           :: Theme e
  , ctxDisabled        :: Bool
  , ctxInteractionClip :: Maybe Rectangle
    -- ^ When set, @Blink.View.Mouse.isRegionHit@ additionally requires the
    -- mouse to fall within this rectangle. Set by
    -- 'Blink.View.Drawing.withClip' and restored on exit, so it tracks the
    -- innermost enclosing clip region.
  , ctxAnimation       :: AnimationState
    -- ^ Per-frame animation state: wall-clock delta and tick flag. Set by
    -- "Blink.App" at the start of each frame.
  , ctxMeasurers       :: Measurers
    -- ^ Measurement services supplied at configure time. Controls call
    -- 'charOffset', 'charAtOffset', 'measureText', and 'measureImage'
    -- rather than accessing this directly.
  , ctxFocus           :: FocusTracker e
    -- ^ Keyboard-focus targeting state. See 'FocusTracker'.
  , ctxNavigationKeys  :: NavigationKeys
    -- ^ Which keys currently mean "advance"\/"retreat" focus. See
    -- 'NavigationKeys'; set via 'Blink.View.Navigation.withNavigationKeys'.
  , ctxCurrentScope    :: Maybe e
    -- ^ The scope id currently ambient -- 'Nothing' for root, @'Just'
    -- scopeId@ while inside that scope's own
    -- @Blink.View.Focus.withFocusScope@ call. Lets an effect queued from
    -- deep inside a scope (a Shift-Tab retreat, a click redirecting focus
    -- to a different element) address /that/ scope instead of always root.
    -- See 'getCurrentScope'.
  , ctxMouse           :: Mouse e
    -- ^ The left mouse button's state this frame (and which element, if
    -- any, holds mouse capture), plus per-element hover state. See
    -- 'Blink.Input.Mouse'. Unlike focus, the button reading does not change
    -- on a re-render of the same frame — see 'rerenderContext'.
  , ctxElements        :: ElementState e
  , ctxOutputs         :: FrameOutputs e msg
  , ctxStyle           :: Style
    -- ^ Set by 'withStyle', read back via 'currentStyle'.
  , ctxMetrics         :: Metrics
    -- ^ Set by 'withMetrics', read back via 'currentMetrics'.
  }

-- | The View monad. A state-threading computation in 'IO' that reads from a
-- 'ViewContext' and emits draw commands and messages as a side effect. Use the
-- 'Functor', 'Applicative', and 'Monad' instances to compose view trees. See
-- 'Blink.Controls.control' for higher-level building blocks.
--
-- [@e@] Element identity type.
-- [@msg@] Message type emitted via 'emit'.
-- [@a@] Result type.
newtype View e msg a = View
  { runView :: ViewContext e msg -> IO (a, ViewContext e msg)
    -- ^ Runs the computation against a context, producing a result and the
    -- updated context.
  }

instance Functor (View e msg) where
  fmap f (View g) = View $ \ctx -> do
    (a, ctx') <- g ctx
    pure (f a, ctx')

instance Applicative (View e msg) where
  pure a = View $ \ctx -> pure (a, ctx)
  View f <*> View x = View $ \ctx -> do
    (g, ctx')  <- f ctx
    (a, ctx'') <- x ctx'
    pure (g a, ctx'')

instance Monad (View e msg) where
  return = pure
  View x >>= f = View $ \ctx -> do
    (a, ctx') <- x ctx
    runView (f a) ctx'

emptyFrameOutputs :: FrameOutputs e msg
emptyFrameOutputs = FrameOutputs
  { outDrawCommands       = []
  , outEvents             = []
  , outRequiresAnimation  = False
  }

-- | Constructs the initial 'ViewContext' for the first frame, with
-- 'noOpMeasurers' -- the common case, since most tests and headless
-- rendering don't care about measurement. A real backend (or a test that
-- does care) overrides that via 'withMeasurers'.
emptyViewContext :: Rectangle -> InputState -> Theme e -> ViewContext e msg
emptyViewContext bounds input thm = ViewContext
  { ctxBounds          = bounds
  , ctxWindowBounds    = bounds
  , ctxInput           = input
  , ctxTheme           = thm
  , ctxDisabled        = False
  , ctxInteractionClip = Nothing
  , ctxAnimation       = mkAnimationState 0 0 False
  , ctxMeasurers       = noOpMeasurers
  , ctxFocus           = emptyFocusTracker
  , ctxNavigationKeys  = defaultNavigationKeys
  , ctxCurrentScope    = Nothing
  , ctxMouse           = advanceButton False (inputLeftButtonDown input) emptyMouse
  , ctxElements        = ElementState
      { elmScrollStates  = Map.empty
      , elmExtentStates  = Map.empty
      , elmHoldStates    = Map.empty
      , elmCursorIndices = Map.empty
      , elmSelection     = NoSelection
      }
  , ctxOutputs         = emptyFrameOutputs
  , ctxStyle           = resolveStyle (snd (themeDefaultStyle thm)) Set.empty
  , ctxMetrics         = fst (themeDefaultStyle thm)
  }

-- | Advances the context to the next frame, given the backend-supplied
-- inputs for the frame about to run: window bounds, raw input, active theme,
-- and animation state. First runs @applyUiEffects@ on the 'UiEffect's queued
-- during the frame that just completed, so focus, scroll, and selection
-- changes take effect starting this new frame. Then resets per-frame state
-- (draw commands, hover element, queued messages, and the focus-visited
-- flag) while preserving cross-frame state (focus element, scroll state,
-- selections, and tab-stop bookkeeping). Also advances the button state via
-- 'advanceButton', on top of what @finishFrame@ already rolled forward for
-- hover, so @Blink.View.Mouse.wasMouseOverLastFrame@ reflects this frame once
-- it, in turn, becomes "last frame".
nextFrameContext :: Ord e => Rectangle -> InputState -> Theme e -> AnimationState -> ViewContext e msg -> ViewContext e msg
nextFrameContext bounds input thm anim ctx0 =
  fctx { ctxMouse = advanceButton wasDown isDown (ctxMouse fctx) }
  where
    ctx     = settleEffects ctx0
    fctx    = finishFrame bounds input thm anim ctx
    wasDown = inputLeftButtonDown (ctxInput ctx)
    isDown  = inputLeftButtonDown input

-- | Rebuilds the context to re-render the current frame rather than advance
-- to a new one: refreshes bounds\/theme\/animation, applies queued
-- 'UiEffect's, and resets per-frame outputs, like 'nextFrameContext'. Leaves
-- the button reading as-is rather than re-deriving it from input, since the
-- caller is re-rendering the same frame, not moving to the next one;
-- deriving it again would compare the frame's input against itself.
rerenderContext :: Ord e => Rectangle -> InputState -> Theme e -> AnimationState -> ViewContext e msg -> ViewContext e msg
rerenderContext bounds input thm anim ctx0 = finishFrame bounds input thm anim ctx
  where
    ctx = settleEffects ctx0

-- | Given a context that already has any queued 'UiEffect's applied,
-- refreshes bounds\/theme\/animation, advances focus to the next frame,
-- rolls this completed frame's hover results forward via 'advanceHover', and
-- resets per-frame outputs. Leaves the button reading unchanged.
finishFrame :: Rectangle -> InputState -> Theme e -> AnimationState -> ViewContext e msg -> ViewContext e msg
finishFrame bounds input thm anim ctx = ctx
  { ctxBounds      = bounds
  , ctxWindowBounds = bounds
  , ctxInput       = input
  , ctxTheme       = thm
  , ctxAnimation   = anim
  , ctxFocus       = nextFocusTrackerFrame (ctxFocus ctx)
  , ctxMouse       = advanceHover (ctxMouse ctx)
  , ctxOutputs     = emptyFrameOutputs
  , ctxStyle       = resolveStyle (snd (themeDefaultStyle thm)) Set.empty
  , ctxMetrics     = fst (themeDefaultStyle thm)
  }

-- | Reads a value out of the current context without changing it. Building
-- block for every read-only accessor in "Blink.View" and its feature
-- modules.
gets :: (ViewContext e msg -> a) -> View e msg a
gets f = View $ \ctx -> pure (f ctx, ctx)

-- | Replaces the current context with @f@ applied to it. Building block for
-- every state-changing action in "Blink.View" and its feature modules.
modify :: (ViewContext e msg -> ViewContext e msg) -> View e msg ()
modify f = View $ \ctx -> pure ((), f ctx)

-- | Runs @action@ with a single context field temporarily overridden via
-- @set@, restoring the field to whatever @get@ read from the original
-- context once @action@ completes. Shared save\/run\/restore shape behind
-- 'withBounds', 'withStyle', 'withMetrics', 'Blink.View.Navigation.withNavigationKeys',
-- and 'disableWhen'.
withField :: (ViewContext e msg -> a) -> (a -> ViewContext e msg -> ViewContext e msg) -> a -> View e msg b -> View e msg b
withField get set v (View f) = View $ \ctx -> do
  (a, ctx') <- f (set v ctx)
  pure (a, set (get ctx) ctx')

-- | Modifies the current frame's output accumulator. Building block for
-- 'draw', 'emit', 'emitUi', and every feature module's own output-side
-- effects (e.g. 'Blink.View.Animation.requiresAnimation').
modifyOut :: (FrameOutputs e msg -> FrameOutputs e msg) -> View e msg ()
modifyOut f = modify $ \ctx -> ctx { ctxOutputs = f (ctxOutputs ctx) }

-- | The current layout rectangle. Set by the layout system via 'withBounds'.
getBounds :: View e msg Rectangle
getBounds = gets ctxBounds

-- | The backend-supplied window rectangle for this frame, at origin
-- @0,0@. Unlike 'getBounds', this is unaffected by 'withBounds' narrowing
-- as the tree is descended, so it reads the same everywhere in the tree for
-- a given frame.
getWindowSize :: View e msg Rectangle
getWindowSize = gets ctxWindowBounds

-- | Runs a sub-tree within a different bounding rectangle. The previous bounds
-- are restored when the sub-tree completes. Used by the layout system to
-- assign each child its allocated slot.
withBounds :: Rectangle -> View e msg a -> View e msg a
withBounds = withField ctxBounds (\v c -> c { ctxBounds = v })

-- | The raw input state for the current frame.
getInput :: View e msg InputState
getInput = gets contextInput

-- | The raw input state for the current frame, read directly from a
-- 'ViewContext' outside the 'View' monad — e.g. so a backend or test driver can
-- carry mouse\/button state forward into the next 'nextFrameContext' call.
contextInput :: ViewContext e msg -> InputState
contextInput = ctxInput

-- | Removes all events for the given key from the current frame's key queue,
-- preventing other controls from handling the same keypress.
consumeKey :: Key -> View e msg ()
consumeKey k = modify $ \ctx ->
  let input = ctxInput ctx
  in ctx { ctxInput = input { inputKeyEvents = filter (\e -> key e /= k) (inputKeyEvents input) } }

-- | Hides the given key\/modifier combinations from 'getInput' -- and so
-- from anything reading raw key events, e.g. 'Blink.Controls.Control.onKeyPressed'
-- -- for the duration of @action@, restoring the real input once it
-- completes. Unlike 'consumeKey', this doesn't affect what anyone else
-- sees: a control that itself observes some keys as reserved navigation
-- (e.g. Ctrl+Tab) can keep them out of its own raw reporting without
-- removing them from the frame's key queue, so whatever consumes them for
-- real afterward still can.
withoutKeyEvents :: [(Key, [Modifier])] -> View e msg a -> View e msg a
withoutKeyEvents keys (View f) = View $ \ctx ->
  let input  = ctxInput ctx
      hidden = filter (\e -> (key e, modifiers e) `elem` keys) (inputKeyEvents input)
      filtered = input { inputKeyEvents = filter (\e -> (key e, modifiers e) `notElem` keys) (inputKeyEvents input) }
  in do
    (a, ctx') <- f (ctx { ctxInput = filtered })
    let restoredInput = (ctxInput ctx') { inputKeyEvents = hidden ++ inputKeyEvents (ctxInput ctx') }
    pure (a, ctx' { ctxInput = restoredInput })

-- | The scope id currently ambient -- 'Nothing' for root, @'Just' scopeId@
-- while inside that scope's own @Blink.View.Focus.withFocusScope@ call. An
-- effect that needs to address the ambient scope specifically (e.g.
-- @Blink.View.Focus.requestFocus@ for a Shift-Tab retreat) should use this
-- rather than assuming root, so it still targets the right scope when
-- called from inside one.
getCurrentScope :: View e msg (Maybe e)
getCurrentScope = gets ctxCurrentScope

getTheme :: View e msg (Theme e)
getTheme = gets contextTheme

-- | The active 'Theme', read directly from a 'ViewContext' outside the 'View'
-- monad — e.g. so a backend or test driver can carry it forward unchanged
-- into the next 'nextFrameContext' call.
contextTheme :: ViewContext e msg -> Theme e
contextTheme = ctxTheme

-- | Returns the @('Metrics', 'StyleSet')@ pair registered for the given
-- 'StyleKey'. Falls back to the theme's default when nothing is
-- registered for it.
getStyleSet :: Ord e => StyleKey e -> View e msg (Metrics, StyleSet)
getStyleSet styleKey = do
  t <- getTheme
  return $ Map.findWithDefault (themeDefaultStyle t) styleKey (themeElementStyles t)

-- | Just the 'Metrics' half of 'getStyleSet' -- used where a size is
-- needed independently of interaction state (e.g. hit-testing before the
-- active 'Style' is known).
getMetrics :: Ord e => StyleKey e -> View e msg Metrics
getMetrics styleKey = fst <$> getStyleSet styleKey

-- | Runs a sub-tree with the given 'Style' as the one 'currentStyle' reads
-- back. The previous style is restored once the sub-tree completes.
withStyle :: Style -> View e msg a -> View e msg a
withStyle = withField ctxStyle (\v c -> c { ctxStyle = v })

-- | The 'Style' set by the nearest enclosing 'withStyle'.
currentStyle :: View e msg Style
currentStyle = gets ctxStyle

-- | Runs a sub-tree with the given 'Metrics' as the one 'currentMetrics'
-- reads back. The previous metrics are restored once the sub-tree
-- completes.
withMetrics :: Metrics -> View e msg a -> View e msg a
withMetrics = withField ctxMetrics (\v c -> c { ctxMetrics = v })

-- | The 'Metrics' set by the nearest enclosing 'withMetrics'.
currentMetrics :: View e msg Metrics
currentMetrics = gets ctxMetrics

-- | 'True' when the current sub-tree has been marked disabled.
isDisabled :: View e msg Bool
isDisabled = gets ctxDisabled

-- | Marks a sub-tree as disabled when the condition is 'True'. The flag is
-- restored to its previous value once the sub-tree completes.
disableWhen :: Bool -> View e msg a -> View e msg a
disableWhen True  = withField ctxDisabled (\v c -> c { ctxDisabled = v }) True
disableWhen False = id

-- | Skips its argument entirely when the current sub-tree is disabled.
whenEnabled :: View e msg () -> View e msg ()
whenEnabled ui = do
  disabled <- isDisabled
  unless disabled ui

-- | Queues a 'DrawCommand' against the current frame's output, in submission
-- order. Minimal drawing primitive -- see "Blink.View.Drawing" for the
-- higher-level operations ('Blink.View.Drawing.fillRect',
-- 'Blink.View.Drawing.strokeRect', 'Blink.View.Drawing.drawText',
-- 'Blink.View.Drawing.withClip', etc.) built on top of it and 'getBounds'.
draw :: DrawCommand -> View e msg ()
draw cmd = modifyOut $ \out -> out { outDrawCommands = cmd : outDrawCommands out }

-- | The active interaction clip region, if any -- see 'Blink.View.Drawing.withClip'.
getInteractionClip :: View e msg (Maybe Rectangle)
getInteractionClip = gets ctxInteractionClip

-- | Runs @action@ with the interaction clip region replaced, restoring the
-- previous region once @action@ completes. Building block for
-- 'Blink.View.Drawing.withClip'.
withInteractionClip :: Maybe Rectangle -> View e msg a -> View e msg a
withInteractionClip = withField ctxInteractionClip (\v c -> c { ctxInteractionClip = v })

-- | Queues a message to be delivered to the application once the frame
-- completes. Messages are delivered in emit order by 'getMessages'.
emit :: msg -> View e msg ()
emit msg = modifyOut $ \out -> out { outEvents = EffectMsg msg : outEvents out }

-- | Queues a 'UiEffect' — a focus, scroll, or selection change — to be
-- applied by @applyUiEffects@ between this frame and the next.
-- @Blink.View.Focus.setFocus@, @Blink.View.Focus.clearFocus@, and the
-- scroll\/selection writes inside "Blink.Controls" are built on this;
-- reach for it directly only when writing a custom control.
emitUi :: UiEffect e -> View e msg ()
emitUi eff = modifyOut $ \out -> out { outEvents = EffectUi eff : outEvents out }

-- | Contexts that can queue a 'UiEffect' to take effect from the next frame
-- onward: 'View' (via 'emitUi', queuing immediately as view code runs) and
-- 'Blink.Update.Update' (queuing to be applied once the frame's messages
-- have all been folded). Lets 'Blink.View.Scroll.requestScrollTo' and its
-- siblings work unchanged from either, rather than needing a separate copy
-- of each for 'Blink.Update.Update'.
class HasUiEffect e m | m -> e where
  queueEffect :: UiEffect e -> m ()

instance HasUiEffect e (View e msg) where
  queueEffect = emitUi

-- | Extracts the draw commands produced during the frame, in submission order.
getDrawCommands :: ViewContext e msg -> [DrawCommand]
getDrawCommands = reverse . outDrawCommands . ctxOutputs

-- | Extracts the messages queued with 'emit' during the frame, in emit order.
-- 'UiEffect's queued with 'emitUi' (or the focus\/scroll\/selection helpers
-- built on it) are excluded; they are applied automatically by
-- 'nextFrameContext' and never reach the application.
getMessages :: ViewContext e msg -> [msg]
getMessages ctx = [msg | EffectMsg msg <- reverse (outEvents (ctxOutputs ctx))]

-- | Queues extra 'UiEffect's as though 'emitUi' had queued them during the
-- frame that just completed, ordered after whatever the view itself already
-- queued. Lets the frame loop fold in 'UiEffect's a 'Blink.Update.Update'
-- handler requested (via its own 'HasUiEffect' instance) -- it has no
-- 'ViewContext' of its own to queue them through directly, since it only
-- runs after the frame's view pass has already produced one.
queueUiEffects :: [UiEffect e] -> ViewContext e msg -> ViewContext e msg
queueUiEffects effs ctx = ctx { ctxOutputs = foldl' queueOne (ctxOutputs ctx) effs }
  where queueOne out eff = out { outEvents = EffectUi eff : outEvents out }

-- Internal: the 'UiEffect's queued with 'emitUi' during the frame, in emit
-- order, messages discarded.
getUiEffects :: ViewContext e msg -> [UiEffect e]
getUiEffects ctx = [eff | EffectUi eff <- reverse (outEvents (ctxOutputs ctx))]

-- | 'True' when 'Blink.View.Animation.requiresAnimation' was called at
-- least once during the frame, read directly from a 'ViewContext' outside
-- the 'View' monad. The backend reads this after each frame to decide
-- whether to keep its animation ticker running.
contextRequiresAnimation :: ViewContext e msg -> Bool
contextRequiresAnimation = outRequiresAnimation . ctxOutputs

-- | Writes a scroll position directly into the context, bypassing the
-- deferred-effect queue -- visible to any 'Blink.View.Scroll.getScrollState'
-- read later in this same frame, unlike 'Blink.View.Scroll.requestScrollTo', which only
-- takes effect from the next frame onward. Clamps to @[0, 1]@ so this is
-- the single point that enforces the 'ScrollState' invariant regardless of
-- which caller reaches it, @applyUiEffects@ included. See
-- 'Blink.View.Scroll.setScrollStateNow', the monadic wrapper built on top
-- of it for a control correcting its own scroll position as a direct,
-- same-frame consequence of what it's about to render (e.g.
-- 'Blink.Controls.List.scrollRowIntoView'), as opposed to reacting to a
-- user gesture like a drag, which should stay on the deferred queue so a
-- frame's own read of "current scroll" stays stable throughout its
-- rendering.
writeScrollState :: Ord e => e -> Double -> ViewContext e msg -> ViewContext e msg
writeScrollState eid v ctx = ctx { ctxElements = (ctxElements ctx)
  { elmScrollStates = Map.insert eid (ScrollState (clampScrollPos v)) (elmScrollStates (ctxElements ctx)) } }

-- Internal: writes an extent value directly into the context, bypassing
-- the deferred-effect queue. Used only by @applyUiEffects@.
writeExtentState :: Ord e => e -> Double -> ViewContext e msg -> ViewContext e msg
writeExtentState eid v ctx = ctx { ctxElements = (ctxElements ctx)
  { elmExtentStates = Map.insert eid (ExtentState v) (elmExtentStates (ctxElements ctx)) } }

-- Internal: writes (or clears) an element's repeat-press state directly
-- into the context, bypassing the deferred-effect queue. Used only by
-- @applyUiEffects@.
writeHoldState :: Ord e => e -> Maybe HoldState -> ViewContext e msg -> ViewContext e msg
writeHoldState eid mhs ctx = ctx { ctxElements = (ctxElements ctx)
  { elmHoldStates = case mhs of
      Just hs -> Map.insert eid hs (elmHoldStates (ctxElements ctx))
      Nothing -> Map.delete eid (elmHoldStates (ctxElements ctx))
  } }

-- Internal: writes (or clears) a list-like control's cursor index directly
-- into the context, bypassing the deferred-effect queue. Used only by
-- @applyUiEffects@.
writeCursorIndexState :: Ord e => e -> Maybe Int -> ViewContext e msg -> ViewContext e msg
writeCursorIndexState eid mi ctx = ctx { ctxElements = (ctxElements ctx)
  { elmCursorIndices = case mi of
      Just i  -> Map.insert eid (CursorIndexState i) (elmCursorIndices (ctxElements ctx))
      Nothing -> Map.delete eid (elmCursorIndices (ctxElements ctx))
  } }

-- Internal: writes a selection directly into the context, bypassing the
-- deferred-effect queue, replacing whichever element held the selection
-- before. Used only by @applyUiEffects@.
writeSelection :: e -> Selection -> ViewContext e msg -> ViewContext e msg
writeSelection eid sel ctx = ctx { ctxElements = (ctxElements ctx)
  { elmSelection = SelectionAt eid sel } }

-- Internal: applies queued 'UiEffect's to a context, folded in queue order.
applyUiEffects :: Ord e => [UiEffect e] -> ViewContext e msg -> ViewContext e msg
applyUiEffects effects ctx0 = foldl' step ctx0 effects
  where
    step ctx (ScrollTo eid v)        = writeScrollState eid v ctx
    step ctx (ScrollBy eid dv)       = writeScrollState eid (currentScroll eid ctx + dv) ctx
    step ctx (AdjustExtent eid dv)   = writeExtentState eid (currentExtent eid ctx + dv) ctx
    step ctx (SetSelectionAt eid sel) = writeSelection eid sel ctx
    step ctx (SetHoldState eid mhs)  = writeHoldState eid mhs ctx
    step ctx (SetCursorIndex eid mi) = writeCursorIndexState eid mi ctx
    step ctx (Focus sid target)     = setFocusChange sid (Just target) ctx
    step ctx (ClearFocus sid)       = setFocusChange sid Nothing ctx

    currentScroll eid ctx =
      scrollPosition (Map.findWithDefault (ScrollState 0) eid (elmScrollStates (ctxElements ctx)))

    currentExtent eid ctx =
      extentValue (Map.findWithDefault (ExtentState 0) eid (elmExtentStates (ctxElements ctx)))

-- | Applies whatever effects are pending on @ctx@.
settleEffects :: Ord e => ViewContext e msg -> ViewContext e msg
settleEffects ctx = applyUiEffects (getUiEffects ctx) ctx

-- | 'settleEffects', plus drops the settled 'UiEffect's (only) from the
-- output queue -- unlike bare 'settleEffects', safe to treat as "this
-- frame is done" and use as the seed for a later frame, since a
-- non-idempotent effect (e.g. @ScrollBy@\/@AdjustExtent@) queued on the
-- settled frame can't then be re-applied a second time when that later
-- frame's own 'nextFrameContext'\/'rerenderContext' settles again. Draw
-- commands and messages already queued are left untouched (dropping them
-- here would falsify a caller reading them off the very context this
-- returns). Distinct from either of those: it leaves
-- bounds\/input\/theme\/focus\/hover exactly as they were, rather than
-- also advancing them the way a genuinely new (or re-rendered) frame
-- would.
settleAndClearEffects :: Ord e => ViewContext e msg -> ViewContext e msg
settleAndClearEffects ctx = ctx'
  { ctxOutputs = (ctxOutputs ctx') { outEvents = filter isMsg (outEvents (ctxOutputs ctx')) } }
  where
    ctx' = settleEffects ctx
    isMsg (EffectMsg _) = True
    isMsg (EffectUi _)  = False

-- | 'True' when any effect is pending on @ctx@.
hasPendingUiEffects :: ViewContext e msg -> Bool
hasPendingUiEffects = not . null . getUiEffects

-- | Applies a @Focus@\/@ClearFocus@ effect to whichever scope it targets —
-- root's 'ftAmbient' (@Nothing@) or a specific composite's entry in
-- @ftScopes@ (@Just scopeId@) — setting the new focus holder (if any, as a
-- fresh 'GainedThisFrame' claim) and recording whoever it displaced (looking
-- up the scope's previous holder to fill in 'focusLost') for one frame's
-- observation. Used only by @applyUiEffects@.
setFocusChange :: Ord e => Maybe e -> Maybe e -> ViewContext e msg -> ViewContext e msg
setFocusChange scopeId newFocus ctx = ctx { ctxFocus = updateScope (ctxFocus ctx) }
  where
    updateScope ft = case scopeId of
      Nothing  -> ft { ftAmbient = setIt (ftAmbient ft) }
      Just sid -> ft { ftScopes = Map.insert sid (setIt (lookupScope sid ft)) (ftScopes ft) }
    setIt fs = fs
      { focusClaim = maybe Unclaimed GainedThisFrame newFocus
      , focusLost  = LostThisFrame (currentFocus (focusClaim fs))
      }

-- | Returns the x offset (pixels) of character index @n@ from the start of
-- @text@, using the backend's text measurer.
charOffset :: Text -> Int -> View e msg Float
charOffset text n = View $ \ctx -> do
  v <- tmCharOffset (msrText (ctxMeasurers ctx)) text n
  pure (v, ctx)

-- | Returns the character index closest to x offset @x@ in @text@, using the
-- backend's text measurer.
charAtOffset :: Text -> Float -> View e msg Int
charAtOffset text x = View $ \ctx -> do
  v <- tmCharAtOffset (msrText (ctxMeasurers ctx)) text x
  pure (v, ctx)

-- | Returns the pixel dimensions of @text@ as rendered by the current font.
measureText :: Text -> View e msg Size
measureText text = View $ \ctx -> do
  v <- tmTextSize (msrText (ctxMeasurers ctx)) text
  pure (v, ctx)

-- | Returns the natural pixel dimensions of the image at @path@, using the
-- backend's image measurer.
measureImage :: ImagePath -> View e msg Size
measureImage path = View $ \ctx -> do
  v <- imNaturalSize (msrImage (ctxMeasurers ctx)) path
  pure (v, ctx)

-- | Overrides a context's measurement services -- a real backend supplies
-- its own via this, on top of 'emptyViewContext's default 'noOpMeasurers'.
withMeasurers :: Measurers -> ViewContext e msg -> ViewContext e msg
withMeasurers measurers ctx = ctx { ctxMeasurers = measurers }
