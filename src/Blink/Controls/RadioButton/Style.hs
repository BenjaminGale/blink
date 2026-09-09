{- |
Module: Blink.Controls.RadioButton.Style

'Blink.Controls.RadioButton.radioButton''s registration in
'Blink.Style.Defaults.defaultTheme' -- the flat-row look defined
in "Blink.Controls.Style", since a radio button doesn't have a
shape of its own.
-}
{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.RadioButton.Style
  ( radioButtonStyleKey
  , defaultStyleEntries
  ) where

import Blink.Style
import Blink.Controls.Style (flatRowMetrics, flatRowStyle)

-- | The 'StyleKey' 'Blink.Controls.RadioButton.radioButton' resolves
-- its style from unless overridden via 'Blink.Controls.Control.style'.
radioButtonStyleKey :: StyleKey e
radioButtonStyleKey = Class "radioButton"

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (radioButtonStyleKey, (flatRowMetrics, flatRowStyle p)) ]
