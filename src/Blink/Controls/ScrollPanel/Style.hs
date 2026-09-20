{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.ScrollPanel.Style

'Blink.Controls.ScrollPanel.scrollPanel''s registration in
'Blink.Style.Defaults.defaultTheme' -- 'Blink.Controls.Style.toggleGroupStyle',
the same transparent, borderless wrapper look 'Blink.Controls.ScrollBar.scrollBar'
uses for its own outer container: a scroll panel exists to make existing
content scrollable, not to impose a visual boundary of its own, and,
being 'Blink.Controls.Control.NotFocusable', never needs a focus ring
either.
-}
module Blink.Controls.ScrollPanel.Style
  ( scrollPanelStyleKey
  , defaultStyleEntries
  ) where

import Blink.Style
import Blink.Controls.Style (toggleGroupMetrics, toggleGroupStyle)

-- | The 'StyleKey' 'Blink.Controls.ScrollPanel.scrollPanel' resolves its
-- own chrome from unless overridden via 'Blink.Controls.Control.style'.
scrollPanelStyleKey :: StyleKey e
scrollPanelStyleKey = Class "scrollPanel"

-- | This control's entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (scrollPanelStyleKey, (toggleGroupMetrics, toggleGroupStyle p)) ]
