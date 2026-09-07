{- |
Module: Blink.Focus

Pure keyboard-focus state: which element (if any) holds focus within a
scope, how a claim ages across frames, and the bookkeeping a nested focus
scope (a list, a tree — anything with sub-items) needs to persist its own
focus independently of its enclosing scope.

No dependency on the 'Blink.View' monad -- 'Blink.View' holds a
'FocusTracker' in its context and exposes monadic accessors
('Blink.View.getFocus', 'Blink.View.setFocus', 'Blink.View.withFocusScope',
etc.) built on top of what's defined here, the same relationship
"Blink.Input" has with the mouse state 'Blink.View' threads through its own
context.
-}
module Blink.Focus
  ( -- * Focus claims
    FocusClaim (..)
  , currentFocus
  , isGained
  , tryClaim
  , tickFocusClaim
    -- * Displaced focus
  , LostFocus (..)
  , pendingLostFocus
  , tickLostFocus
    -- * Per-scope state
  , FocusState (..)
  , emptyFocusState
  , isNothingFocused
  , nextFocusFrame
  , reaffirm
    -- * Nested scopes
  , FocusTracker (..)
  , emptyFocusTracker
  , lookupScope
  , nextFocusTrackerFrame
  , FreshClaim (..)
  , ScopeMode (..)
  , scopeMode
  ) where

import qualified Data.Map.Strict as Map

-- | Which element (if any) a scope currently has focused, and whether that
-- claim has been reaffirmed since it was set. A claim must be reaffirmed
-- every frame by whatever holds it actually rendering and re-claiming it
-- (see 'Blink.View.setFocus'); one frame of grace ('ClaimedLastFrame') is
-- given before an unreaffirmed claim is dropped, so a claim surviving
-- exactly one frame without a render in between (e.g. the frame a queued
-- 'Blink.View.Focus' effect is applied, before the new holder has rendered
-- even once) isn't mistaken for abandonment.
-- 'GainedThisFrame'\/'GainedLastFrame' are only ever entered by an explicit
-- 'Blink.View.Focus' effect (see 'Blink.View.setFocusChange'), never by
-- 'Blink.View.setFocus' reaffirming a claim -- that's what keeps a
-- self-claim from masquerading as a redirect.
data FocusClaim e
  = Unclaimed
  | ClaimedLastFrame e
    -- ^ Held as of the previous frame boundary but not yet reaffirmed this
    -- pass; dropped to 'Unclaimed' if still unreaffirmed at the next boundary.
  | ClaimedThisFrame e
    -- ^ Reaffirmed during the current frame (or just granted).
  | GainedLastFrame e
    -- ^ Landed via a 'Blink.View.Focus' effect as of the previous frame
    -- boundary; its final frame of visibility to 'Blink.View.hasGainedFocus',
    -- then it fades to 'ClaimedThisFrame' -- an ordinary held claim from here
    -- on.
  | GainedThisFrame e
    -- ^ Landed via a 'Blink.View.Focus' effect this frame boundary.
  deriving (Eq, Show)

-- | The element currently focused, regardless of reaffirmation status.
currentFocus :: FocusClaim e -> Maybe e
currentFocus Unclaimed             = Nothing
currentFocus (ClaimedLastFrame e)  = Just e
currentFocus (ClaimedThisFrame e)  = Just e
currentFocus (GainedLastFrame e)   = Just e
currentFocus (GainedThisFrame e)   = Just e

-- | 'True' when this element is the target of a 'Blink.View.Focus' effect
-- that landed within the last two frames.
isGained :: Eq e => e -> FocusClaim e -> Bool
isGained eid (GainedLastFrame e) = e == eid
isGained eid (GainedThisFrame e) = e == eid
isGained _   _                   = False

-- | Claims focus for @eid@ (see 'Blink.View.setFocus'): succeeds when
-- nothing currently holds it, or @eid@ is reaffirming itself; refused,
-- unchanged, when a different element already holds it this frame -- so an
-- element calling 'Blink.View.setFocus' for itself can never steal focus out
-- from under whoever legitimately has it, regardless of render order.
-- Reaffirming a still-fresh 'Gained*' claim for the same element leaves it
-- exactly as it was, so a control reaffirming itself every frame doesn't cut
-- short its own 'Blink.View.hasGainedFocus' visibility window.
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
-- See 'Blink.View.hasLostFocus'.
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
-- (a list, a tree — anything with sub-items, see 'Blink.View.withFocusScope')
-- each own one of these; a composite's own is persisted in @ftScopes@
-- between frames, the same way scroll and selection state persist per
-- element.
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
-- state (see 'Blink.View.withFocusScope'): being written back here proves
-- the scope was actually visited this frame, so its claim shouldn't be
-- allowed to decay any further towards 'tickFocusClaim'\'s expiry -- only a
-- scope that genuinely stops being visited (its composite removed from the
-- tree) should expire.
reaffirm :: FocusClaim e -> FocusClaim e
reaffirm Unclaimed             = Unclaimed
reaffirm (ClaimedLastFrame e)  = ClaimedThisFrame e
reaffirm (ClaimedThisFrame e)  = ClaimedThisFrame e
reaffirm (GainedLastFrame e)   = GainedLastFrame e
reaffirm (GainedThisFrame e)   = GainedThisFrame e

-- | Per-frame keyboard-focus targeting state: which element has focus, and
-- which was the most recent tab stop. Reset and carried forward by
-- 'Blink.View.nextFrameContext'. Unlike mouse\/capture state, focus also
-- advances on a re-render of the same frame (see
-- 'Blink.View.rerenderContext'), since a scope that goes unclaimed on a
-- re-render should expire even though the button reading hasn't changed.
data FocusTracker e = FocusTracker
  { ftAmbient :: FocusState e
    -- ^ The *currently ambient* scope's own focus state — root's, unless a
    -- 'Blink.View.withFocusScope' call further up the stack has swapped it
    -- for a composite's own.
  , ftScopes  :: Map.Map e (FocusState e)
    -- ^ Every composite's own persisted 'FocusState', flat, keyed directly
    -- by scope id regardless of nesting depth — the same shape as
    -- @elmScrollStates@. See 'Blink.View.withFocusScope'.
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
-- scope to auto-claim focus this frame — see 'Blink.View.withFocusScope'.
data FreshClaim = AllowFreshClaim | BlockFreshClaim
  deriving (Eq, Show)

-- | Which of the two policies documented on 'Blink.View.withFocusScope'
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
