{- |
Module: Blink.View.Hold

Per-element repeat-press ("hold") state, plus the pure cadence logic built
on top of it: how many repeats are due for a press of a given age.

No dependency on the 'Blink.View' monad -- 'Blink.View' holds a
'HoldState' per element in its context and exposes monadic accessors
('Blink.View.getHoldState', 'Blink.View.contextHoldState') built on top of
what's defined here, the same relationship "Blink.View.Scroll" has with
the scroll state 'Blink.View' threads through its own context.
-}
module Blink.View.Hold
  ( HoldState (..)
  , repeatsDueBy
  ) where

-- | Per-element bookkeeping for a control that keeps firing while held down
-- -- e.g. 'Blink.View.Controls.RepeatButton.repeatButton' -- rather than
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
