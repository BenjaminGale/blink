{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Slider.Style

'Blink.Controls.Slider.slider''s registration in
'Blink.Style.Defaults.defaultTheme' -- the track look defined in
"Blink.Controls.Style", shared with a scrollbar's own track.
-}
module Blink.Controls.Slider.Style
  ( sliderStyleKey
  , defaultStyleEntries
  ) where

import Blink.Style
import Blink.Controls.Style (progressBarMetrics, sliderStyle)

-- | The 'StyleKey' 'Blink.Controls.Slider.slider' resolves its style
-- from unless overridden via 'Blink.Controls.Control.style'.
sliderStyleKey :: StyleKey e
sliderStyleKey = Class "slider"

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (sliderStyleKey, (progressBarMetrics, sliderStyle p)) ]
