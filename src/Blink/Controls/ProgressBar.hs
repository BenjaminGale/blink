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
  , progressBarTrackStyleKey
  , progressBarFillStyleKey
  , progressBar
  , progress
  , bandSpeed
  , bandWidth
    -- * Style
  , defaultStyleEntries
  ) where

import Control.Monad (when)
import qualified Data.Set as Set

import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft), Rectangle (..), clampFraction)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.View
import Blink.Element (Element (..), HasLayoutConfig (..), noIntrinsicSize)
import Blink.Style
import Blink.Controls.Style (plainFillStyle, plainStyle, trackMetrics, valueFillStyle, zeroMetrics)

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
  overControl = nested pbControl (\pc x -> pc { pbControl = x })

instance HasLayoutConfig (ProgressBarConfig e msg) where
  overLayout = nested pbLayout (\pc x -> pc { pbLayout = x })

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
-- Display-only: it takes no id, raises no events, and is never focusable.
-- Has no content of its own to size to, so it
-- defaults to filling the space it's given on both axes, same as every
-- control did before controls reported their own 'Blink.Layout.Constraints.Layout'.
-- Override with 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
progressBar :: Ord e => [Attribute (ProgressBarConfig e msg)] -> Element e msg
progressBar attrs = controlElement (pbLayout cfg) (Element (pbLayout cfg) noIntrinsicSize (pure ())) ctrl
  where
    cfg  = resolve defaultProgressBarConfig attrs
    ctrl = (pbControl cfg)
      { ccFocusPolicy  = NotFocusable
      , ccContent      = body
      }
    body ci = do
      r <- getBounds
      let partState = Set.singleton (commonState (ciDisabled ci) False False)
      drawPart progressBarTrackStyleKey partState
      case pbValue cfg of
        Progress value -> do
          let fillRect' = r { rectWidth = rectWidth r * clampFraction value }
          withBounds fillRect' (drawPart progressBarFillStyleKey partState)
        Indeterminate -> when (not (ciDisabled ci)) $ do
          requiresAnimation
          elapsed <- getAnimElapsed
          let bandW  = min (pbBandWidth cfg) (rectWidth r)
              cycles = realToFrac elapsed * pbBandSpeed cfg
              phase  = cycles - fromIntegral (floor cycles :: Int)
              left   = rectX r - bandW + (rectWidth r + bandW) * phase
          withBounds (r { rectX = left, rectWidth = bandW }) (drawPart progressBarFillStyleKey partState)

-- * Style

-- | The 'StyleKey' 'Blink.Controls.ProgressBar.progressBar' resolves
-- its style from unless overridden via 'Blink.Controls.Control.style'.
progressBarStyleKey :: StyleKey e
progressBarStyleKey = Class "progressBar"

-- | The 'StyleKey' the track, the full length the fill runs along,
-- resolves its style from.
progressBarTrackStyleKey :: StyleKey e
progressBarTrackStyleKey = Class "progressBarTrack"

-- | The 'StyleKey' the fill (or, while 'Indeterminate', the moving band)
-- resolves its style from.
progressBarFillStyleKey :: StyleKey e
progressBarFillStyleKey = Class "progressBarFill"

-- | This control's entries in 'Blink.Style.Defaults.defaultTheme': its
-- own chrome and each of its parts.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (progressBarStyleKey,      (trackMetrics, plainStyle p))
  , (progressBarTrackStyleKey, (zeroMetrics, plainFillStyle p (paletteSurface p)))
  , (progressBarFillStyleKey,  (zeroMetrics, valueFillStyle p (paletteAccent p)))
  ]
