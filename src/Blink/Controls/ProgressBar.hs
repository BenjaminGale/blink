{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A progress indicator: a filled bar for a known 'Progress' value, or a
-- continuously animating band while 'Indeterminate'. A leaf, built
-- directly on 'control' -- nothing derives from it, and it displays no
-- label, so it has no 'Blink.Controls.Label.LabelledConfig' either.
module Blink.Controls.ProgressBar
  ( ProgressBarConfig (..)
  , ProgressValue (..)
  , defaultProgressBarConfig
  , progressBarStyleKey
  , progressBar
  , progress
  , bandSpeed
  , bandWidth
  ) where

import Control.Monad (void, when)

import Blink.Controls.Control
import Blink.Controls.ProgressBar.Style (progressBarStyleKey)
import Blink.Geometry (Alignment (TopLeft), Rectangle (..))
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Style (Style (..))
import Blink.View
import Blink.View.Drawing (fillRect)
import Blink.Element (Element (..), HasLayoutConfig (..), noIntrinsicSize)

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
  , pbBandWidth :: Double
  , pbLayout    :: Layout
  }

-- | 'defaultControlConfig' (styled via 'progressBarStyleKey'), @'Progress' 0@,
-- a band speed of 0.75 sweeps\/s (one lap every ~1.3s), a band width of
-- 120px, and @Layout fill fill TopLeft@ (see 'progressBar').
defaultProgressBarConfig :: ProgressBarConfig e msg
defaultProgressBarConfig = ProgressBarConfig
  { pbControl   = defaultControlConfig { ccStyleKey = progressBarStyleKey }
  , pbValue     = Progress 0
  , pbBandSpeed = 0.75
  , pbBandWidth = 120
  , pbLayout    = Layout fill fill TopLeft
  }

instance HasControlConfig e msg (ProgressBarConfig e msg) where
  overControl attr = Attribute (\pc -> pc { pbControl = runAttribute attr (pbControl pc) })

instance HasLayoutConfig (ProgressBarConfig e msg) where
  overLayout attr = Attribute (\pc -> pc { pbLayout = runAttribute attr (pbLayout pc) })

-- | Sets the bar to 'Progress' (determinate) or 'Indeterminate'. Defaults
-- to @'Progress' 0@.
progress :: ProgressValue -> Attribute (ProgressBarConfig e msg)
progress v = Attribute (\pc -> pc { pbValue = v })

-- | How many full sweeps the band makes across an 'Indeterminate' bar per
-- second -- the same fixed-duration-lap convention most toolkits use for
-- an indeterminate/busy indicator (e.g. Material's ~2s linear
-- indeterminate cycle). A lap always takes the same time regardless of
-- the bar's width; the band's raw pixel speed is what adapts, covering
-- more distance per lap in a wider bar. Defaults to 0.75 (one lap every
-- ~1.3s).
bandSpeed :: Double -> Attribute (ProgressBarConfig e msg)
bandSpeed v = Attribute (\pc -> pc { pbBandSpeed = v })

-- | Width, in pixels, of the moving band on an 'Indeterminate' bar. Fixed
-- rather than proportional to the bar's own width, so the band doesn't
-- visibly resize as the bar (or its containing window) is resized.
-- Defaults to 120. Clamped to the bar's width if narrower.
bandWidth :: Double -> Attribute (ProgressBarConfig e msg)
bandWidth v = Attribute (\pc -> pc { pbBandWidth = v })

-- | A progress indicator, set via 'progress' to 'Progress' for a
-- determinate bar or 'Indeterminate' for a continuously animating band.
-- Takes no id by default -- pass 'Blink.Controls.Control.elementId' to
-- give one instance a stable identity and react to its hover\/click\/focus
-- events. Never a tab stop, though: fixed behaviour, not a default -- 'progressBar'
-- always overrides 'focusPolicy' to 'NotFocusable' itself, so it wins
-- regardless of what a caller passes. Has no content of its own to size to, so it
-- defaults to filling the space it's given on both axes, same as every
-- control did before controls reported their own 'Blink.Layout.Constraints.Layout'.
-- Override with 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
progressBar :: Ord e => [Attribute (ProgressBarConfig e msg)] -> Element e msg
progressBar attrs = Element
  { elLayout  = pbLayout cfg
  , elMeasure = measureChrome (ccStyleKey ctrl) (Element (pbLayout cfg) noIntrinsicSize (pure ()))
  , elRun     = void (control ctrl)
  }
  where
    cfg  = resolve defaultProgressBarConfig attrs
    ctrl = (pbControl cfg)
      { ccFocusPolicy  = NotFocusable
      , ccContent      = body
      }
    body ci = do
      s <- currentStyle
      r <- getBounds
      case pbValue cfg of
        Progress value -> do
          let clamped   = max 0 (min 1 value)
              fillRect' = r { rectWidth = rectWidth r * clamped }
          withBounds fillRect' $ fillRect (styleTextColour s)
        Indeterminate -> do
          when (not (ciDisabled ci)) requiresAnimation
          elapsed <- getAnimElapsed
          let bandW  = min (pbBandWidth cfg) (rectWidth r)
              cycles = realToFrac elapsed * pbBandSpeed cfg
              phase  = cycles - fromIntegral (floor cycles :: Int)
              left   = rectX r - bandW + (rectWidth r + bandW) * phase
          withBounds (r { rectX = left, rectWidth = bandW }) $ fillRect (styleTextColour s)
