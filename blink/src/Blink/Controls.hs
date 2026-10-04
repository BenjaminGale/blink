-- | The public forms of every ready-made widget: each one's constructor,
-- its own attribute functions, and the types those need. Each widget's
-- own module ("Blink.Controls.Button", "Blink.Controls.Checkbox", ...)
-- documents the attribute functions used to configure it, and
-- "Blink.Controls.Control"'s own module header describes how it's built
-- underneath.
--
-- An attribute several widgets share ('value', 'step', 'orientation',
-- 'items', 'itemAttrs', 'selection', 'onSelectionChanged', 'content') is
-- one name, defined in "Blink.Element", that works on every widget that
-- has it. 'isEnabled' and 'style' work on every widget.
--
-- Each widget's config type is exported without its constructor, so it
-- can appear in type signatures (e.g. a helper returning
-- @['Blink.Element.Attribute' ('ButtonConfig' e msg)]@).
--
-- The style classes and visual states at the end are the keys a theme
-- uses for each widget and its parts (see "Blink.Style").
module Blink.Controls
  ( -- * Attributes every widget accepts
    isEnabled
  , style
    -- * Attributes shared across widgets
  , value
  , step
  , orientation
  , items
  , itemAttrs
  , selection
  , onSelectionChanged
  , content
    -- * Button
  , ButtonConfig
  , button
  , onActivated
  , activation
  , ButtonActivation (..)
    -- * ToggleButton
  , ToggleConfig
  , toggleButton
  , isSelected
  , onSelectedChanged
    -- * Checkbox
  , checkbox
    -- * RadioButton
  , radioButton
    -- * RepeatButton
  , RepeatButtonConfig
  , repeatButton
  , initialDelay
  , repeatInterval
    -- * Label
  , LabelConfig
  , label
  , text
  , mnemonic
  , target
    -- * List
  , ListConfig
  , ItemState (..)
  , list
  , requiredList
  , renderItem
  , rowHeight
  , onItemActivated
  , scrollListTo
  , scrollListToItem
    -- ** Selection models
  , SelectionModel (..)
  , EmptySelection (..)
  , Direction (..)
  , selectedItems
  , cursorItem
  , SingleSelection
  , unselected
  , selectItem
  , selectAt
  , selectFirst
  , singleSelection
  , singleItems
  , RequiredSelection
  , requireItem
  , requireAt
  , requireFirst
  , requiredItems
  , MultiSelection
  , multiSelection
  , multiSelected
  , multiSelectedAt
  , multiItems
  , RangeSelection
  , End (..)
  , noRange
  , rangeAt
  , rangeFrom
  , rangeAtPositions
  , rangeItems
  , rangeEnd
    -- * Tree
  , TreeConfig
  , TreeItemState (..)
  , tree
  , forest
  , expanded
  , renderNode
  , onExpansionChanged
  , flattenVisible
    -- * Table
  , TableConfig
  , table
  , columns
  , sortedBy
  , onColumnSortRequested
  , SortDirection (..)
  , ColumnConfig
  , ColumnWidth (..)
  , column
  , header
  , cell
  , cellWidth
  , sortable
    -- * TreeTable
  , TreeTableConfig
  , treeTable
    -- * ProgressBar
  , ProgressBarConfig
  , ProgressValue (..)
  , progressBar
  , progress
  , bandSpeed
  , bandWidth
    -- * Slider
  , SliderConfig
  , slider
  , onValueChanged
    -- * Divider
  , DividerConfig
  , divider
  , thickness
    -- * Image
  , ImageConfig
  , image
  , source
  , fitWidth
  , fitHeight
  , preserveRatio
    -- * TextInput
  , TextInputConfig
  , textInput
  , placeholder
  , inputFilter
  , displayFilter
  , onInput
  , onSubmit
    -- * ScrollBar
  , ScrollBarConfig
  , scrollBar
  , visibleFraction
  , maxScrollOffset
    -- * ScrollPanel
  , ScrollPanelConfig
  , scrollPanel
  , scrollPanelTo
    -- * ToggleGroup
  , ToggleGroupConfig
  , toggleButtonGroup
  , radioButtonGroup
  , itemSpacing
  , allowDeselect
    -- * MenuButton
  , MenuButtonConfig
  , menuButton
  , isOpen
  , onOpenChanged
    -- * MenuBar
  , MenuBarConfig
  , menuBar
  , menus
  , labelAttrs
  , menuItems
  , submenuItems
  , openMenu
  , onOpenMenuChanged
    -- * Style classes
  , buttonStyleKey
  , checkboxStyleKey
  , radioButtonStyleKey
  , toggleButtonStyleKey
  , toggleButtonGroupStyleKey
  , radioButtonGroupStyleKey
  , labelStyleKey
  , iconStyleKey
  , imageStyleKey
  , dividerStyleKey
  , dividerLineStyleKey
  , listStyleKey
  , listItemStyleKey
  , treeChevronStyleKey
  , tableHeaderStyleKey
  , tableColumnDividerStyleKey
  , tableColumnDividerLineStyleKey
  , progressBarStyleKey
  , progressBarTrackStyleKey
  , progressBarFillStyleKey
  , sliderStyleKey
  , sliderTrackStyleKey
  , sliderFillStyleKey
  , sliderThumbStyleKey
  , textInputStyleKey
  , textInputSelectionStyleKey
  , scrollBarStyleKey
  , scrollBarButtonStyleKey
  , scrollBarTrackStyleKey
  , scrollBarThumbStyleKey
  , scrollPanelStyleKey
  , menuItemStyleKey
  , menuButtonListStyleKey
  , menuBarStyleKey
  , menuBarLabelStyleKey
  , menuBarListStyleKey
    -- * Control-specific visual states
  , toggleChecked
  , toggleUnchecked
  , listSelected
  , listUnselected
  , listCursor
  , listNoCursor
  , menuItemSubmenuOpen
  ) where

import Blink.Element (content, itemAttrs, items, onSelectionChanged, orientation, selection, step, value)
import Blink.Controls.Control (isEnabled, style)
import Blink.Controls.Button (ButtonActivation (..), ButtonConfig, activation, button, onActivated, buttonStyleKey)
import Blink.Controls.Checkbox (checkbox, checkboxStyleKey)
import Blink.Controls.Divider (DividerConfig, divider, thickness, dividerStyleKey, dividerLineStyleKey)
import Blink.Controls.Image (ImageConfig, image, source, fitWidth, fitHeight, preserveRatio, imageStyleKey)
import Blink.Controls.Label (LabelConfig, label, mnemonic, target, text, labelStyleKey)
import Blink.Controls.List
  ( Direction (..), EmptySelection (..), End (..), ItemState (..), ListConfig, MultiSelection
  , RangeSelection, RequiredSelection, SelectionModel (..), SingleSelection
  , cursorItem, list, multiItems, multiSelected, multiSelectedAt, multiSelection, noRange, onItemActivated
  , rangeAt, rangeAtPositions, rangeEnd, rangeFrom, rangeItems, renderItem, requireAt, requireFirst, requireItem
  , requiredItems, requiredList, rowHeight, scrollListTo, scrollListToItem, selectAt, selectFirst, selectItem, selectedItems, singleItems
  , singleSelection, unselected
  , listStyleKey, listItemStyleKey, listSelected, listUnselected, listCursor, listNoCursor
  )
import Blink.Controls.MenuBar (MenuBarConfig, labelAttrs, menuBar, menuItems, menus, onOpenMenuChanged, openMenu, submenuItems, menuBarStyleKey, menuBarLabelStyleKey, menuBarListStyleKey)
import Blink.Controls.MenuButton (MenuButtonConfig, isOpen, menuButton, onOpenChanged, menuButtonListStyleKey)
import Blink.Controls.ProgressBar (ProgressBarConfig, ProgressValue (..), bandSpeed, bandWidth, progress, progressBar, progressBarStyleKey, progressBarTrackStyleKey, progressBarFillStyleKey)
import Blink.Controls.RadioButton (radioButton, radioButtonStyleKey)
import Blink.Controls.RepeatButton (RepeatButtonConfig, initialDelay, repeatButton, repeatInterval)
import Blink.Controls.ScrollBar (ScrollBarConfig, maxScrollOffset, scrollBar, visibleFraction, scrollBarStyleKey, scrollBarButtonStyleKey, scrollBarTrackStyleKey, scrollBarThumbStyleKey)
import Blink.Controls.ScrollPanel (ScrollPanelConfig, scrollPanel, scrollPanelTo, scrollPanelStyleKey)
import Blink.Controls.Slider (SliderConfig, onValueChanged, slider, sliderStyleKey, sliderTrackStyleKey, sliderFillStyleKey, sliderThumbStyleKey)
import Blink.Controls.TextInput (TextInputConfig, displayFilter, inputFilter, onInput, onSubmit, placeholder, textInput, textInputStyleKey, textInputSelectionStyleKey)
import Blink.Controls.ToggleButton (ToggleConfig, isSelected, onSelectedChanged, toggleButton, toggleButtonStyleKey, toggleChecked, toggleUnchecked)
import Blink.Controls.ToggleGroup (ToggleGroupConfig, allowDeselect, itemSpacing, radioButtonGroup, toggleButtonGroup, toggleButtonGroupStyleKey, radioButtonGroupStyleKey)
import Blink.Controls.Table
  ( ColumnConfig, ColumnWidth (..), SortDirection (..), TableConfig
  , cell, cellWidth, column, columns, header, onColumnSortRequested, sortable, sortedBy, table
  , tableHeaderStyleKey, tableColumnDividerStyleKey, tableColumnDividerLineStyleKey
  )
import Blink.Controls.Tree (TreeConfig, TreeItemState (..), expanded, flattenVisible, forest, onExpansionChanged, renderNode, tree, treeChevronStyleKey)
import Blink.Controls.TreeTable (TreeTableConfig, treeTable)
import Blink.Controls.Style (iconStyleKey)
import Blink.Controls.Menu (menuItemStyleKey, menuItemSubmenuOpen)
