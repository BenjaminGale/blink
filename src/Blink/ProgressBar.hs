{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A progress indicator: a filled bar for a known 'Progress' value, or a
-- continuously animating band while 'Indeterminate'.
module Blink.ProgressBar
  ( ProgressBarConfig
  , ProgressValue (..)
  , progressBar
  , progressBarStyleKey
  , progress
  , bandSpeed
  , isEnabled
  , style
  , StyleKey (..)
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost
  ) where

import Blink.Control
  ( ControlConfig, FocusOnClick (NoFocus), HasControlConfig (..)
  , control, defaultControlConfig, isEnabled, isFocusable, style
  )
import Blink.Element
  ( Attr, ElementEvent
  , configAny, configure
  , onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed, onFocusGained, onFocusLost
  )
import Blink.Geometry (Rectangle (..))
import Blink.Style (Style (..), StyleKey (..))
import Blink.UI

-- | The value passed to 'progressBar' via 'progress'.
data ProgressValue
  = Progress Double
    -- ^ A determinate value in @[0, 1]@, clamped and rendered as a filled bar.
  | Indeterminate
    -- ^ Unknown progress: a band animates continuously across the bar.
  deriving (Eq, Show)

-- | Configuration for 'progressBar', set via 'progress' and 'bandSpeed'.
-- Defaults to @'Progress' 0@ and a band speed of @0.5@.
data ProgressBarConfig e = ProgressBarConfig
  { progressBarConfigControl   :: ControlConfig e
  , progressBarConfigValue     :: ProgressValue
  , progressBarConfigBandSpeed :: Double
  }

-- | The 'StyleKey' 'progressBar' resolves its style from unless overridden
-- via 'style'.
progressBarStyleKey :: StyleKey e
progressBarStyleKey = Class "progressBar"

defaultProgressBarConfig :: ProgressBarConfig e
defaultProgressBarConfig = ProgressBarConfig
  { progressBarConfigControl   = defaultControlConfig progressBarStyleKey
  , progressBarConfigValue     = Progress 0
  , progressBarConfigBandSpeed = 0.5
  }

instance HasControlConfig e (ProgressBarConfig e) where
  controlConfig    = progressBarConfigControl
  setControlConfig cc cfg = cfg { progressBarConfigControl = cc }

-- | Sets the bar to 'Progress' (determinate) or 'Indeterminate'. Defaults
-- to @'Progress' 0@.
progress :: ProgressValue -> Attr e ev msg (ProgressBarConfig e)
progress p = configAny $ \cfg -> cfg { progressBarConfigValue = p }

-- | How fast the band sweeps across an 'Indeterminate' bar, in bar-widths
-- per second. Defaults to 0.5.
bandSpeed :: Double -> Attr e ev msg (ProgressBarConfig e)
bandSpeed v = configAny $ \cfg -> cfg { progressBarConfigBandSpeed = v }

-- | A progress indicator, set via 'progress' to 'Progress' for a
-- determinate bar or 'Indeterminate' for a continuously animating band. A
-- full 'control' like any other -- it reports hover\/click\/focus events
-- the same way every other control does, even though a caller has no real
-- reason to react to them. Never a tab stop, though: fixed behaviour, not
-- a default, so passing @isFocusable@ in @attrs@ has no effect on it.
progressBar :: Ord e => e -> [Attr e ElementEvent msg (ProgressBarConfig e)] -> UI e msg ()
progressBar eid attrs = control eid NoFocus cfg attrs $ do
  s <- currentStyle
  r <- getBounds
  case progressBarConfigValue cfg of
    Progress value -> do
      let clamped   = max 0 (min 1 value)
          fillRect' = r { rectWidth = rectWidth r * clamped }
      withBounds fillRect' $ fillRect (styleTextColour s)
    Indeterminate -> do
      requiresAnimation
      elapsed <- getAnimElapsed
      let speed = progressBarConfigBandSpeed cfg
          t     = realToFrac elapsed * speed
          phase = t - fromIntegral (floor t :: Int)
          bandW = rectWidth r * 0.3
          left  = rectX r - bandW + (rectWidth r + bandW) * phase
      withBounds (r { rectX = left, rectWidth = bandW }) $ fillRect (styleTextColour s)
  where
    cfg = configure defaultProgressBarConfig (attrs ++ [isFocusable False])
