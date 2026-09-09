{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.Button.Style

'Blink.Controls.Button.button''s registration in
'Blink.Style.Defaults.defaultTheme' -- the bordered-box look
defined in "Blink.Controls.Style", since a button doesn't have a
shape of its own.
-}
module Blink.Controls.Button.Style
  ( buttonStyleKey
  , defaultStyleEntries
  ) where

import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (buttonStyle, controlMetrics)

-- | The 'StyleKey' 'Blink.Controls.Button.button' resolves its style
-- from unless overridden via 'Blink.Controls.Control.style'.
buttonStyleKey :: StyleKey e
buttonStyleKey = Class "button"

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (buttonStyleKey, (controlMetrics, buttonStyle AlignCenter p)) ]
