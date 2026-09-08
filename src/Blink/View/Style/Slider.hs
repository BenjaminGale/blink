{- |
Module: Blink.View.Style.Slider

'Blink.View.Controls.Slider.slider''s registration in
'Blink.View.Style.Defaults.defaultTheme' -- the track look defined in
"Blink.View.Style.Control", shared with a scrollbar's own track.
-}
module Blink.View.Style.Slider
  ( defaultStyleEntries
  ) where

import Blink.View.Controls.Slider (sliderStyleKey)
import Blink.View.Style
import Blink.View.Style.Control (progressBarMetrics, sliderStyle)

-- | This control's one entry in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (sliderStyleKey, (progressBarMetrics, sliderStyle p)) ]
