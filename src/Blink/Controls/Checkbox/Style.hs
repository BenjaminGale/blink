{- |
Module: Blink.Controls.Checkbox.Style

'Blink.Controls.Checkbox.checkbox''s registration in
'Blink.Style.Defaults.defaultTheme' -- the flat-row look defined
in "Blink.Controls.Style", since a checkbox doesn't have a shape of
its own.
-}
{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.Checkbox.Style
  ( checkboxStyleKey
  , defaultStyleEntries
  ) where

import Blink.Style
import Blink.Controls.Style (flatRowMetrics, flatRowStyle)

-- | The 'StyleKey' 'Blink.Controls.Checkbox.checkbox' resolves its
-- style from unless overridden via 'Blink.Controls.Control.style'.
checkboxStyleKey :: StyleKey e
checkboxStyleKey = Class "checkbox"

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (checkboxStyleKey, (flatRowMetrics, flatRowStyle p)) ]
