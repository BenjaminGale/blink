{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.ToggleButton.Style

'Blink.Controls.ToggleButton.toggleButton''s registration in
'Blink.Style.Defaults.defaultTheme' -- the bordered-box look
defined in "Blink.Controls.Style", since a toggle button doesn't
have a shape of its own, plus the 'toggleChecked'\/'toggleUnchecked'
pseudo-states 'Blink.Controls.ToggleButton.toggleBase' puts in
'Blink.Controls.Control.ccActiveStates' while selected\/unselected.
-}
module Blink.Controls.ToggleButton.Style
  ( toggleButtonStyleKey
  , toggleStyleGroup
  , toggleChecked
  , toggleUnchecked
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Blink.Rendering (Colour, TextAlign (..))
import Blink.Style
import Blink.Controls.Style (buttonStyle, controlMetrics)

-- | The 'StyleKey' 'Blink.Controls.ToggleButton.toggleButton' resolves
-- its style from unless overridden via 'Blink.Controls.Control.style'.
toggleButtonStyleKey :: StyleKey e
toggleButtonStyleKey = Class "toggleButton"

-- | The 'VisualState' group name shared by 'toggleChecked'\/
-- 'toggleUnchecked' -- see "Blink.Style"'s module header for why a
-- control defines its own pseudo-states as opaque exported constants
-- rather than letting callers build 'Custom' values themselves.
toggleStyleGroup :: Text
toggleStyleGroup = "Toggle"

-- | The pseudo-state 'Blink.Controls.ToggleButton.toggleBase' puts in
-- 'Blink.Controls.Control.ccActiveStates' while the control is
-- selected (see 'Blink.Controls.ToggleButton.isSelected') -- a theme
-- registers an override for this on its own 'StyleSet' (keyed to whatever
-- 'StyleKey' the control actually resolves to, e.g. 'toggleButtonStyleKey')
-- to give it a distinct "selected" look, composed with whatever
-- common\/focus state is also active.
toggleChecked :: VisualState
toggleChecked = Custom toggleStyleGroup "Checked"

-- | The pseudo-state 'Blink.Controls.ToggleButton.toggleBase' puts in
-- 'Blink.Controls.Control.ccActiveStates' while the control is
-- unselected. Themes typically register no override for this -- the plain
-- base look already reads as "unchecked".
toggleUnchecked :: VisualState
toggleUnchecked = Custom toggleStyleGroup "Unchecked"

-- | 'buttonStyle' with an added bold accent fill while 'toggleChecked' --
-- the same fill 'buttonStyle' already uses for 'CommonPressed'.
checkedStyle :: Colour -> Colour -> StyleSet -> StyleSet
checkedStyle accent onAccent s = s
  { styleOverrides = Map.insert toggleChecked
      (\st -> st { styleBackground = accent, styleTextColour = onAccent, styleBorderColour = Just accent })
      (styleOverrides s)
  }

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ ( toggleButtonStyleKey
    , (controlMetrics, checkedStyle (paletteAccent p) (paletteTextOnAccent p) (buttonStyle AlignCenter p))
    )
  ]
