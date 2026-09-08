{- |
Module: Blink.View.Hold

Per-element repeat-press ("hold") state: the pure 'HoldState' type and
cadence logic ('repeatsDueBy'), plus 'resolveHoldRepeats', the monadic
accessor built on top of it. See "Blink.View" for the module overview;
import that instead of this module directly.
-}
module Blink.View.Hold
  ( HoldState (..)
  , repeatsDueBy
  , resolveHoldRepeats
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as Map
import Blink.View.Context
import Blink.View.Animation (requiresAnimation, getAnimElapsed)

-- Internal: the given element's repeat-press state, or 'Nothing' while it
-- isn't currently being held\/repeating. Used only by 'resolveHoldRepeats'.
contextHoldState :: Ord e => e -> ViewContext e msg -> Maybe HoldState
contextHoldState eid ctx = Map.lookup eid (elmHoldStates (ctxElements ctx))

-- | Given whether an element is held, and its initial-delay\/interval
-- cadence, returns how many repeats are due this frame. Requires animation
-- while held.
resolveHoldRepeats :: Ord e => e -> Bool -> Double -> Double -> View e msg Int
resolveHoldRepeats eid held initialDelay interval
  | not held = do
      mHold <- gets (contextHoldState eid)
      when (isJust mHold) $ emitUi (SetHoldState eid Nothing)
      pure 0
  | otherwise = do
      requiresAnimation
      now <- realToFrac <$> getAnimElapsed
      mHold <- gets (contextHoldState eid)
      let HoldState startedAt fired = fromMaybe (HoldState now 0) mHold
          due    = repeatsDueBy initialDelay interval (now - startedAt)
          toFire = max 0 (due - fired)
      emitUi (SetHoldState eid (Just (HoldState startedAt due)))
      pure toFire
