{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A progress indicator: a filled bar for a known 'Progress' value, or a
-- continuously animating band while 'Indeterminate'. A leaf, built
-- directly on 'controlBase' -- nothing derives from it, and it displays no
-- label, so it has no 'Blink.Controls.Label.LabelledConfig' either.
module Blink.Controls.ProgressBar
  ( ProgressBarConfig (..)
  , ProgressValue (..)
  , defaultProgressBarConfig
  , progressBarStyleKey
  , progressBar
  , progress
  , bandSpeed
  ) where

import Control.Monad (void)

import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft), Rectangle (..))
import Blink.Layout.Constraints (HasLayoutConfig (..), Layout (..), fill)
import Blink.Style (Style (..))
import Blink.UI
import Blink.UI.Element (Element (..), noIntrinsicSize)

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
  , pbLayout    :: Layout
  }

-- | The 'StyleKey' 'progressBar' resolves its style from unless overridden
-- via 'style'.
progressBarStyleKey :: StyleKey e
progressBarStyleKey = Class "progressBar"

-- | 'defaultControlConfig' (styled via 'progressBarStyleKey'), @'Progress' 0@,
-- a band speed of 0.5, and @Layout fill fill TopLeft@ (see 'progressBar').
defaultProgressBarConfig :: ProgressBarConfig e msg
defaultProgressBarConfig = ProgressBarConfig
  { pbControl   = defaultControlConfig { ccStyleKey = progressBarStyleKey }
  , pbValue     = Progress 0
  , pbBandSpeed = 0.5
  , pbLayout    = Layout fill fill TopLeft
  }

instance HasElementConfig e msg (ProgressBarConfig e msg) where
  overElement attr = Attribute (\pc -> pc { pbControl = runAttribute (overElement attr) (pbControl pc) })

instance HasControlConfig e msg (ProgressBarConfig e msg) where
  overControl attr = Attribute (\pc -> pc { pbControl = runAttribute attr (pbControl pc) })

instance HasLayoutConfig (ProgressBarConfig e msg) where
  overLayout attr = Attribute (\pc -> pc { pbLayout = runAttribute attr (pbLayout pc) })

-- | Sets the bar to 'Progress' (determinate) or 'Indeterminate'. Defaults
-- to @'Progress' 0@.
progress :: ProgressValue -> Attribute (ProgressBarConfig e msg)
progress v = Attribute (\pc -> pc { pbValue = v })

-- | How fast the band sweeps across an 'Indeterminate' bar, in bar-widths
-- per second. Defaults to 0.5.
bandSpeed :: Double -> Attribute (ProgressBarConfig e msg)
bandSpeed v = Attribute (\pc -> pc { pbBandSpeed = v })

-- | A progress indicator, set via 'progress' to 'Progress' for a
-- determinate bar or 'Indeterminate' for a continuously animating band. A
-- full control like any other -- it reports hover\/click\/focus events the
-- same way every other control does, even though a caller has no real
-- reason to react to them. Never a tab stop, though: fixed behaviour, not
-- a default -- 'progressBar' always overrides 'isFocusable' to 'False'
-- itself, so it wins regardless of what a caller passes. Has no content of
-- its own to size to, so it defaults to filling the space it's given on
-- both axes, same as every control did before controls reported their own
-- 'Blink.Layout.Layout'. Override with 'Blink.Layout.Constraints.width'\/'Blink.Layout.Constraints.height'\/'Blink.Layout.Constraints.align'.
progressBar :: Ord e => e -> [Attribute (ProgressBarConfig e msg)] -> Element e msg
progressBar eid attrs = Element
  { elLayout  = pbLayout cfg
  , elMeasure = measureChrome (ccStyleKey ctrl) (Element (pbLayout cfg) noIntrinsicSize (pure ()))
  , elRun     = void (controlBase eid ctrl)
  }
  where
    cfg  = resolve defaultProgressBarConfig attrs
    ctrl = (pbControl cfg)
      { ccIsFocusable  = False
      , ccFocusOnClick = NoFocus
      , ccContent      = body
      }
    body = do
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
