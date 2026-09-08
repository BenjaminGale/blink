{- |
Module: Blink.View.Style.ScrollBar

'Blink.View.Controls.ScrollBar.scrollBar''s registration in
'Blink.View.Style.Defaults.defaultTheme' -- a composite of three shapes
defined in "Blink.View.Style.Control", none of them owned by the
scrollbar itself: its outer container reuses the plain wrapper look, its
buttons reuse the bordered-box look, and its track reuses the slider's
track look.
-}
module Blink.View.Style.ScrollBar
  ( defaultStyleEntries
  ) where

import Blink.View.Controls.ScrollBar (scrollBarButtonStyleKey, scrollBarStyleKey, scrollBarTrackStyleKey)
import Blink.View.Rendering (TextAlign (..))
import Blink.View.Style
import Blink.View.Style.Control (buttonStyle, controlMetrics, progressBarMetrics, sliderStyle, toggleGroupMetrics, toggleGroupStyle)

-- | This control's entries in 'Blink.View.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (scrollBarStyleKey,       (toggleGroupMetrics, toggleGroupStyle p))
  , (scrollBarButtonStyleKey, (controlMetrics,     buttonStyle AlignCenter p))
  , (scrollBarTrackStyleKey,  (progressBarMetrics, sliderStyle p))
  ]
