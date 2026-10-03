{- |
Module: Blink.View.Animation

Per-frame animation state: the pure 'AnimationState' type and smart
constructor 'mkAnimationState', plus the monadic accessors
('requiresAnimation', 'withAnimationFrame', 'getAnimDelta',
'getAnimElapsed') built on top of the wall-clock delta and elapsed time
threaded through 'Blink.View.Context.ViewContext'. See "Blink.View" for the
module overview, including the animation-loop narrative; import that
instead of this module directly.
-}
module Blink.View.Animation
  ( AnimationState (animDelta, animElapsed, animIsTick)
  , mkAnimationState
  , requiresAnimation
  , withAnimationFrame
  , getAnimDelta
  , getAnimElapsed
  , contextAnimation
  ) where

import Control.Monad (when)
import Blink.View.Context

-- | Signals that animation should continue running. Call unconditionally on
-- every frame from any component that needs animation, including frames not
-- triggered by the ticker, so "Blink.App"'s ticker does not go quiet while
-- the component is visible.
requiresAnimation :: View e msg ()
requiresAnimation = modifyOut $ \out -> out { outRequiresAnimation = True }

-- | Runs @action@ only on frames triggered by the animation ticker. On frames
-- triggered by mouse movement, keyboard input, or other platform events, this
-- is a no-op. Pair with 'requiresAnimation' so the ticker keeps firing.
withAnimationFrame :: View e msg () -> View e msg ()
withAnimationFrame action = do
  isTick <- gets (animIsTick . ctxAnimation)
  when isTick action

-- | Wall-clock seconds elapsed since the previous frame, clamped to
-- @[0, 0.1]@ seconds. Zero on the first frame. Use inside 'withAnimationFrame' to advance
-- animation state by the correct amount regardless of ticker jitter.
getAnimDelta :: View e msg Float
getAnimDelta = gets (animDelta . ctxAnimation)

-- | Total wall-clock seconds elapsed since the application started.
-- Derived by accumulating 'animDelta' each frame; use this to compute
-- animation phase without storing per-component state.
getAnimElapsed :: View e msg Float
getAnimElapsed = gets (animElapsed . ctxAnimation)

-- | The frame's full 'AnimationState', read directly from a 'ViewContext'
-- outside the 'View' monad — e.g. so a backend can carry it forward into the
-- next 'nextFrameContext' call.
contextAnimation :: ViewContext e msg -> AnimationState
contextAnimation = ctxAnimation
