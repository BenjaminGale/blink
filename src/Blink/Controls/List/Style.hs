{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Controls.List.Style

'Blink.Controls.List.list''s registration in
'Blink.Style.Defaults.defaultTheme', plus the pseudo-states
'Blink.Controls.List.list' puts in a row's own
'Blink.Controls.Control.ccActiveStates' while selected\/cursor.

Selected and cursor are independent facts about a row (a
'Blink.Controls.List.MultiSelection' row can be either, both, or
neither), so unlike 'Blink.Controls.ToggleButton.toggleChecked'\/
'Blink.Controls.ToggleButton.toggleUnchecked' -- one group, mutually
exclusive -- these are /two/ groups, each contributing exactly one
member every frame, the same way 'Blink.Style.CommonNormal'\/etc. and
'Blink.Style.FocusFocused'\/'Blink.Style.FocusUnfocused' already compose
independently of each other.
-}
module Blink.Controls.List.Style
  ( listStyleKey
  , listItemStyleKey
  , listSelected
  , listUnselected
  , listCursor
  , listNoCursor
  , defaultStyleEntries
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Blink.Style
import Blink.Controls.Style (flatRowMetrics, flatRowStyle, toggleGroupMetrics, toggleGroupStyle)

-- | The 'StyleKey' 'Blink.Controls.List.list' resolves its own chrome
-- from unless overridden via 'Blink.Controls.Control.style'.
listStyleKey :: StyleKey e
listStyleKey = Class "list"

-- | The 'StyleKey' each row resolves from unless overridden via a
-- differently-styled 'Blink.Controls.List.renderItem'.
listItemStyleKey :: StyleKey e
listItemStyleKey = Class "list-item"

-- | The pseudo-state group for whether a row is selected.
listSelectionGroup :: Text
listSelectionGroup = "ListSelection"

-- | Present in a row's 'Blink.Controls.Control.ccActiveStates' whenever
-- 'Blink.Controls.List.isSelected' is 'True' for that row.
listSelected :: VisualState
listSelected = Custom listSelectionGroup "Selected"

-- | Present whenever 'Blink.Controls.List.isSelected' is 'False'. Themes
-- typically register no override -- the plain row look already reads as
-- "unselected".
listUnselected :: VisualState
listUnselected = Custom listSelectionGroup "Unselected"

-- | The pseudo-state group for whether a row holds the keyboard cursor.
listCursorGroup :: Text
listCursorGroup = "ListCursor"

-- | Present whenever 'Blink.Controls.List.isCursor' is 'True' for that
-- row.
listCursor :: VisualState
listCursor = Custom listCursorGroup "Cursor"

-- | Present whenever 'Blink.Controls.List.isCursor' is 'False'.
listNoCursor :: VisualState
listNoCursor = Custom listCursorGroup "NoCursor"

-- | 'flatRowStyle' with a bold accent fill while 'listSelected', and a
-- focus-ring-coloured border while 'listCursor' -- so the cursor reads
-- distinctly from selection even on a row that's both (or neither, in a
-- 'Blink.Controls.List.MultiSelection').
listItemStyle :: Palette -> StyleSet
listItemStyle p = (flatRowStyle p)
  { styleOverrides = Map.union
      (Map.fromList
        [ ( listSelected
          , \s -> s { styleBackground = paletteAccent p, styleTextColour = paletteTextOnAccent p }
          )
        , ( listCursor
          , \s -> s { styleBorderColour = Just (paletteFocusRing p) }
          )
        ])
      (styleOverrides (flatRowStyle p))
  }

-- | This control's entries in 'Blink.Style.Defaults.defaultTheme': a
-- plain wrapper look for the list itself ('toggleGroupStyle' -- any
-- chrome belongs on the rows, not doubled up on the container), and
-- @listItemStyle@ for its rows.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (listStyleKey,     (toggleGroupMetrics, toggleGroupStyle p))
  , (listItemStyleKey, (flatRowMetrics, listItemStyle p))
  ]
