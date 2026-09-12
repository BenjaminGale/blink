{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.ScrollBar.Style

'Blink.Controls.ScrollBar.scrollBar''s registration in
'Blink.Style.Defaults.defaultTheme' -- a composite of three shapes
defined in "Blink.Controls.Style", none of them owned by the
scrollbar itself: its outer container reuses the plain wrapper look, its
arrow buttons reuse the same plain-icon look
'Blink.Controls.Tree.Style.treeChevronStyle' uses (no background\/border
of their own -- just the icon, recolouring on hover), and its track
reuses the slider's track look.
-}
module Blink.Controls.ScrollBar.Style
  ( scrollBarStyleKey
  , scrollBarButtonStyleKey
  , scrollBarTrackStyleKey
  , defaultStyleEntries
  ) where

import Blink.Style
import Blink.Controls.Style (iconStyle, progressBarMetrics, sliderStyle, toggleGroupMetrics, toggleGroupStyle)

-- | 'StyleKey's 'Blink.Controls.ScrollBar.scrollBar' resolves its own
-- chrome, its arrow buttons, and its track from unless overridden via
-- 'Blink.Controls.Control.style'.
scrollBarStyleKey, scrollBarButtonStyleKey, scrollBarTrackStyleKey :: StyleKey e
scrollBarStyleKey       = Class "scrollBar"
scrollBarButtonStyleKey = Class "scrollBarButton"
scrollBarTrackStyleKey  = Class "scrollBarTrack"

-- | This control's entries in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (scrollBarStyleKey,       (toggleGroupMetrics, toggleGroupStyle p))
  , (scrollBarButtonStyleKey, (toggleGroupMetrics, iconStyle p))
  , (scrollBarTrackStyleKey,  (progressBarMetrics, sliderStyle p))
  ]
