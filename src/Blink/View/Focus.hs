{- |
Module: Blink.View.Focus

Keyboard focus and nested focus scopes: single-hop focus queries
('isFocused', 'hasGainedFocus', 'hasLostFocus'), the claim/clear API
('setFocus', 'clearFocus', 'requestFocus', 'requestClearFocus'), and
'withFocusScope', which lets a composite (a list, a tree — anything with
sub-items) own its own nested 'FocusState'. See "Blink.View" for the module
overview, including the focus/keyboard-navigation narrative; import that
instead of this module directly.
-}
module Blink.View.Focus
  ( FocusState (previousTabStop)
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
  , getPreviousTabStop
  , setPreviousTabStop
  , contextFocus
  , contextFocusChain
  , contextPreviousTabStop
  ) where

import Control.Monad (join, when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Blink.View.Context

-- | Modifies the currently ambient scope's own 'FocusState'.
modifyFocusState :: (FocusState e -> FocusState e) -> View e msg ()
modifyFocusState f = modify $ \ctx -> ctx { ctxFocus = (ctxFocus ctx) { ftAmbient = f (ftAmbient (ctxFocus ctx)) } }

-- | The currently ambient scope's focused element, if any — root's, unless
-- inside 'withFocusScope'.
getFocus :: View e msg (Maybe e)
getFocus = gets contextFocus

-- | The currently ambient scope's focused element, read directly from a
-- 'ViewContext' outside the 'View' monad.
contextFocus :: ViewContext e msg -> Maybe e
contextFocus = currentFocus . focusClaim . ftAmbient . ctxFocus

-- | The full root-to-leaf focus chain, read directly from a 'ViewContext'
-- outside the 'View' monad by following each scope's own focused element into
-- @ftScopes@ until it bottoms out. No production code needs this —
-- every real check is the single-hop 'contextFocus'\/'isFocused' at
-- whichever scope is ambient at the time — but it's useful for tests and
-- debugging tools that want to see the whole nested claim at once.
--
-- Guards against revisiting an id already on the chain: a click can
-- redirect focus onto any id (as 'Blink.Controls.Label.label' does with its
-- own 'Blink.Controls.Label.target'), including an enclosing composite's
-- own — a composite could use this so that clicking an item leaves the
-- composite itself focused, not the item — which writes that id into its
-- own scope entry in @ftScopes@. That's harmless for the single-hop checks
-- every real caller uses, but would otherwise send this walk into an
-- infinite loop.
contextFocusChain :: Ord e => ViewContext e msg -> [e]
contextFocusChain ctx = go Set.empty (contextFocus ctx)
  where
    go _    Nothing  = []
    go seen (Just x)
      | x `Set.member` seen = []
      | otherwise            = x : go (Set.insert x seen) (currentFocus (focusClaim (lookupScope x (ctxFocus ctx))))

-- | 'True' when the given element id is the currently ambient scope's
-- focused element. For a leaf, this is exactly "am I focused"; for a
-- composite checking its own id, single-hop equality already gives
-- CSS's @:focus-within@ for free — 'withFocusScope' is what makes a
-- composite's own id read as ambiently focused whenever a descendant is, by
-- construction, so no chain-walk is needed here.
isFocused :: Eq e => e -> View e msg Bool
isFocused eid = (== Just eid) <$> getFocus

-- | 'True' when the currently ambient scope's most recent redirect (a
-- @Focus@ effect landing within the last two frames -- see 'FocusClaim')
-- granted this element focus. Single-hop, exactly like 'isFocused'; never
-- 'True' from 'setFocus' reaffirming a claim, only from an explicit @Focus@.
hasGainedFocus :: Eq e => e -> View e msg Bool
hasGainedFocus eid = gets (isGained eid . focusClaim . ftAmbient . ctxFocus)

-- | 'True' when the currently ambient scope's most recent redirect (a
-- @Focus@\/@ClearFocus@ effect, still within its one-frame observation
-- window -- see 'LostFocus') displaced this element.
hasLostFocus :: Eq e => e -> View e msg Bool
hasLostFocus eid = gets ((== Just (Just eid)) . pendingLostFocus . focusLost . ftAmbient . ctxFocus)

-- | Transfers keyboard focus to the given element, in the currently ambient
-- scope. Takes effect immediately — like 'Blink.View.Mouse.registerMouseOver'
-- and mouse capture, not like the deferred scroll\/selection writes —
-- because a control's own focus decision (take it when nothing else has it,
-- hand off on Tab) is only correct if the next sibling in the same tree walk
-- can see it happened. Refused (see 'tryClaim') if a different element
-- already holds it this frame, so it can never steal focus out from under
-- whoever legitimately has it.
setFocus :: Eq e => e -> View e msg ()
setFocus eid = modifyFocusState $ \fs -> fs { focusClaim = tryClaim eid (focusClaim fs) }

-- | Transfers keyboard focus to the given element when the condition is
-- 'True'.
setFocusWhen :: Eq e => Bool -> e -> View e msg ()
setFocusWhen b eid = when b (setFocus eid)

-- | Removes keyboard focus from the currently ambient scope. Immediate,
-- like 'setFocus'.
clearFocus :: View e msg ()
clearFocus = modifyFocusState $ \fs -> fs { focusClaim = Unclaimed }

-- | Rejects a @Focus@ grant that just landed on this element, restoring
-- whoever held focus before it -- as if the grant had never been made.
-- Call before anything else this frame reads focus state for the element.
disclaimFocus :: View e msg ()
disclaimFocus = modifyFocusState $ \fs -> fs
  { focusClaim = maybe Unclaimed ClaimedThisFrame (join (pendingLostFocus (focusLost fs)))
  , focusLost  = NothingLost
  }

-- | Queues a @Focus@ effect: makes the given element focused within the
-- given scope (@Nothing@ = root, @Just scopeId@ = a specific composite's
-- scope — see 'withFocusScope'), taking effect at the next frame boundary.
-- Unlike 'setFocus' (immediate, for a control's own auto-claim\/retain
-- decision), this is for an explicit "make a different, specific element
-- focused" change — a click redirecting focus to a different element, or
-- Tab handing off to a specific known element — triggered from a place
-- that only knows
-- the winner, not who's currently focused: whoever is displaced is looked
-- up when the effect is applied, not supplied here, and deferring lets
-- every affected element observe the change consistently regardless of
-- render order (see 'hasGainedFocus'\/'hasLostFocus').
requestFocus :: Maybe e -> e -> View e msg ()
requestFocus scopeId target = emitUi (Focus scopeId target)

-- | Queues a @ClearFocus@ effect: clears whoever is focused within the
-- given scope, with nothing new claiming it, taking effect at the next
-- frame boundary — the "clear" counterpart to 'requestFocus'.
requestClearFocus :: Maybe e -> View e msg ()
requestClearFocus scopeId = emitUi (ClearFocus scopeId)

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
-- 'BlockFreshClaim' overrides the "nothing is focused, free to claim" half
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
-- always passes 'AllowFreshClaim', so the blocking value is always the
-- literal real ambient there.
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
withFocusScope :: Ord e => e -> FreshClaim -> View e msg a -> View e msg a
withFocusScope scopeId freshClaim (View f) = View $ \ctx ->
  if ctxDisabled ctx
    then f ctx
    else case scopeMode scopeId freshClaim (contextFocus ctx) of
      Claim               -> runClaimed scopeId f ctx
      Blocked blockValue  -> runBlocked scopeId f ctx blockValue

-- | The claiming scope's descendants run against its own persisted
-- 'FocusState' (or a fresh one), and whatever they end up with is folded
-- back under this id, with the enclosing scope reaffirmed as pointing here.
-- See 'withFocusScope'.
runClaimed
  :: Ord e
  => e
  -> (ViewContext e msg -> IO (a, ViewContext e msg))
  -> ViewContext e msg
  -> IO (a, ViewContext e msg)
runClaimed scopeId f ctx = do
  let enclosing = ftAmbient (ctxFocus ctx)
      child0    = lookupScope scopeId (ctxFocus ctx)
  (a, ctx') <- runWithAmbient scopeId f child0 ctx
  pure (a, foldBackAsClaim scopeId enclosing (ftAmbient (ctxFocus ctx')) ctx')

-- | The blocked scope's descendants run against a value nothing inside
-- recognises as itself, so nothing reads as an invitation to auto-claim. If
-- something claims explicitly despite the block, it's folded back exactly
-- as 'runClaimed' would. If nothing claims anyway, the real ambient is
-- restored untouched, and this scope's own saved claim is reaffirmed (same
-- as 'runClaimed' reaffirms a live one) rather than left alone -- otherwise
-- it would only ever be protected from the next-frame expiry while actually
-- live, and expire the instant it's merely not the live target, even though
-- this scope is still being rendered every frame. Reaffirming here means it
-- only really expires once this scope stops being visited at all (its
-- composite removed from the tree). See 'withFocusScope'.
runBlocked
  :: Ord e
  => e
  -> (ViewContext e msg -> IO (a, ViewContext e msg))
  -> ViewContext e msg
  -> Maybe e
  -> IO (a, ViewContext e msg)
runBlocked scopeId f ctx blockValue = do
  let real      = ftAmbient (ctxFocus ctx)
      persisted = lookupScope scopeId (ctxFocus ctx)
  (a, ctx') <- runWithAmbient scopeId f (real { focusClaim = maybe Unclaimed ClaimedThisFrame blockValue }) ctx
  let after = ftAmbient (ctxFocus ctx')
  if currentFocus (focusClaim after) == blockValue
    then pure (a, ctx'
      { ctxFocus = (ctxFocus ctx')
          { ftAmbient = real
          , ftScopes  = Map.insert scopeId (persisted { focusClaim = reaffirm (focusClaim persisted) })
                          (ftScopes (ctxFocus ctx'))
          } })
    else pure (a, foldBackAsClaim scopeId real after ctx')

-- | Swaps the ambient 'FocusState' for @ambient@, and 'ctxCurrentScope' to
-- this scope's own id, while @f@ runs -- restoring the previous scope id
-- after, so nesting reports each level's own immediate scope, not just the
-- outermost one. See 'withFocusScope'.
runWithAmbient
  :: e
  -> (ViewContext e msg -> IO (a, ViewContext e msg))
  -> FocusState e
  -> ViewContext e msg
  -> IO (a, ViewContext e msg)
runWithAmbient scopeId f ambient ctx = do
  (a, ctx') <- f (ctx { ctxFocus = (ctxFocus ctx) { ftAmbient = ambient }, ctxCurrentScope = Just scopeId })
  pure (a, ctx' { ctxCurrentScope = ctxCurrentScope ctx })

-- | Records @after@ as this scope's own persisted state, and points @base@
-- (the value to restore around this scope) at this scope's id -- the
-- write-back shared by a claim and a blocked-but-claimed-anyway resolution
-- alike. See 'withFocusScope'.
foldBackAsClaim :: Ord e => e -> FocusState e -> FocusState e -> ViewContext e msg -> ViewContext e msg
foldBackAsClaim scopeId base after ctx' = ctx'
  { ctxFocus = (ctxFocus ctx')
      { ftAmbient = base { focusClaim = tryClaim scopeId (focusClaim base) }
      , ftScopes  = Map.insert scopeId (after { focusClaim = reaffirm (focusClaim after) }) (ftScopes (ctxFocus ctx'))
      } }

-- | The element that was the most recent tab stop before the current one,
-- scoped to the currently ambient scope (root, or a composite's own while
-- inside 'withFocusScope') — used by 'Blink.Controls.control' to implement
-- Shift-Tab navigation.
getPreviousTabStop :: View e msg (Maybe e)
getPreviousTabStop = gets contextPreviousTabStop

-- | The element that was the most recent tab stop before the current one,
-- read directly from a 'ViewContext' outside the 'View' monad.
contextPreviousTabStop :: ViewContext e msg -> Maybe e
contextPreviousTabStop = previousTabStop . ftAmbient . ctxFocus

-- | Records the current element as the previous tab stop, scoped to the
-- currently ambient scope. Called automatically by 'Blink.Controls.control';
-- call manually when building custom focusable controls.
setPreviousTabStop :: e -> View e msg ()
setPreviousTabStop eid = modifyFocusState $ \fs -> fs { previousTabStop = Just eid }
