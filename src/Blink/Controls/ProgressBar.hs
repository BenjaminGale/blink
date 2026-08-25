{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A progress indicator: a filled bar for a known 'Progress' value, or a
-- continuously animating band while 'Indeterminate'. A leaf, built
-- directly on 'controlBase' -- nothing derives from it, and it displays no
-- label, so it has no 'Blink.Controls.Labelled.LabelledConfig' either.
module Blink.Controls.ProgressBar
  ( ProgressBarConfig (..)
  , ProgressValue (..)
  , defaultProgressBarConfig
  , progressBarStyleKey
  , progressBar
  , progress
  , bandSpeed
  ) where

import Blink.Controls.Control
import Blink.Geometry (Rectangle (..))
import Blink.Style (Style (..))
import Blink.UI

-- | The value passed to 'progressBar' via 'progress'.
data ProgressValue
  = Progress Double
    -- ^ A determinate value in @[0, 1]@, clamped and rendered as a filled bar.
  | Indeterminate
    -- ^ Unknown progress: a band animates continuously across the bar.
  deriving (Eq, Show)

-- | Every capability 'progressBar' resolves: the wrapped 'ControlConfig',
-- its value, and its band speed while indeterminate.
data ProgressBarConfig e msg = ProgressBarConfig
  { pbControl   :: ControlConfig e msg
  , pbValue     :: ProgressValue
  , pbBandSpeed :: Double
  }

-- | The 'StyleKey' 'progressBar' resolves its style from unless overridden
-- via 'style'.
progressBarStyleKey :: StyleKey e
progressBarStyleKey = Class "progressBar"

-- | 'defaultControlConfig' (styled via 'progressBarStyleKey'), @'Progress' 0@,
-- and a band speed of 0.5.
defaultProgressBarConfig :: ProgressBarConfig e msg
defaultProgressBarConfig = ProgressBarConfig
  { pbControl   = defaultControlConfig { ccStyleKey = progressBarStyleKey }
  , pbValue     = Progress 0
  , pbBandSpeed = 0.5
  }

instance HasElementConfig e msg (ProgressBarConfig e msg) where
  overElement attr = Attr (\pc -> pc { pbControl = runAttr (overElement attr) (pbControl pc) })

instance HasControlConfig e msg (ProgressBarConfig e msg) where
  overControl attr = Attr (\pc -> pc { pbControl = runAttr attr (pbControl pc) })

-- | Sets the bar to 'Progress' (determinate) or 'Indeterminate'. Defaults
-- to @'Progress' 0@.
progress :: ProgressValue -> Attr (ProgressBarConfig e msg)
progress v = Attr (\pc -> pc { pbValue = v })

-- | How fast the band sweeps across an 'Indeterminate' bar, in bar-widths
-- per second. Defaults to 0.5.
bandSpeed :: Double -> Attr (ProgressBarConfig e msg)
bandSpeed v = Attr (\pc -> pc { pbBandSpeed = v })

-- | A progress indicator, set via 'progress' to 'Progress' for a
-- determinate bar or 'Indeterminate' for a continuously animating band. A
-- full control like any other -- it reports hover\/click\/focus events the
-- same way every other control does, even though a caller has no real
-- reason to react to them. Never a tab stop, though: fixed behaviour, not
-- a default -- 'progressBar' always overrides 'isFocusable' to 'False'
-- itself, so it wins regardless of what a caller passes.
progressBar :: Ord e => e -> [Attr (ProgressBarConfig e msg)] -> UI e msg ()
progressBar eid attrs = do
  let cfg  = resolve defaultProgressBarConfig attrs
      ctrl = (pbControl cfg)
        { ccIsFocusable  = False
        , ccFocusOnClick = NoFocus
        , ccContent      = body cfg
        }
  () <$ controlBase eid ctrl
  where
    body cfg = do
      s <- currentStyle
      r <- getBounds
      case pbValue cfg of
        Progress value -> do
          let clamped   = max 0 (min 1 value)
              fillRect' = r { rectWidth = rectWidth r * clamped }
          withBounds fillRect' $ fillRect (styleTextColour s)
        Indeterminate -> do
          requiresAnimation
          elapsed <- getAnimElapsed
          let speed = pbBandSpeed cfg
              t     = realToFrac elapsed * speed
              phase = t - fromIntegral (floor t :: Int)
              bandW = rectWidth r * 0.3
              left  = rectX r - bandW + (rectWidth r + bandW) * phase
          withBounds (r { rectX = left, rectWidth = bandW }) $ fillRect (styleTextColour s)
