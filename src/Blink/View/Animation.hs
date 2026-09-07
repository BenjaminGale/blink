{- |
Module: Blink.View.Animation

Per-frame animation state: the wall-clock delta and total elapsed time
since the application started, and whether the current frame was
triggered by the animation ticker rather than a platform input event.

No dependency on the 'Blink.View' monad -- 'Blink.View' holds an
'AnimationState' in its context and exposes monadic accessors
('Blink.View.requiresAnimation', 'Blink.View.withAnimationFrame',
'Blink.View.getAnimDelta', 'Blink.View.getAnimElapsed') built on top of
what's defined here, the same relationship "Blink.View.Focus" has with the
focus state 'Blink.View' threads through its own context.
-}
module Blink.View.Animation
  ( AnimationState (animDelta, animElapsed, animIsTick)
  , mkAnimationState
  ) where

-- | Per-frame animation state threaded through 'Blink.View.ViewContext'.
-- Set by the backend at the start of each frame; read by
-- 'Blink.View.withAnimationFrame' and 'Blink.View.getAnimDelta'.
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
-- so the bound documented on 'animDelta' holds regardless of caller — the
-- constructor itself isn't exported, so this is the only way to build one.
mkAnimationState :: Float -> Float -> Bool -> AnimationState
mkAnimationState delta elapsed isTick = AnimationState
  { animDelta   = max 0 (min 0.1 delta)
  , animElapsed = elapsed
  , animIsTick  = isTick
  }
