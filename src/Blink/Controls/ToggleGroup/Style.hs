{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.ToggleGroup.Style

'Blink.Controls.ToggleGroup.toggleButtonGroup' and
'Blink.Controls.ToggleGroup.radioButtonGroup''s registration in
'Blink.Style.Defaults.defaultTheme' -- the plain wrapper look
defined in "Blink.Controls.Style", since neither has a shape of its
own; the items inside resolve their own look from
"Blink.Controls.Button.Style"\/"Blink.Controls.RadioButton.Style".
-}
module Blink.Controls.ToggleGroup.Style
  ( toggleButtonGroupStyleKey
  , radioButtonGroupStyleKey
  , defaultStyleEntries
  ) where

import Blink.Style
import Blink.Controls.Style (toggleGroupMetrics, toggleGroupStyle)

-- | The 'StyleKey' 'Blink.Controls.ToggleGroup.toggleButtonGroup'
-- resolves its own chrome from unless overridden via
-- 'Blink.Controls.Control.style'.
toggleButtonGroupStyleKey :: StyleKey e
toggleButtonGroupStyleKey = Class "toggleButtonGroup"

-- | The 'StyleKey' 'Blink.Controls.ToggleGroup.radioButtonGroup'
-- resolves its own chrome from unless overridden via
-- 'Blink.Controls.Control.style'.
radioButtonGroupStyleKey :: StyleKey e
radioButtonGroupStyleKey = Class "radioButtonGroup"

-- | This control pair's entries in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (toggleButtonGroupStyleKey, (toggleGroupMetrics, toggleGroupStyle p))
  , (radioButtonGroupStyleKey,  (toggleGroupMetrics, toggleGroupStyle p))
  ]
