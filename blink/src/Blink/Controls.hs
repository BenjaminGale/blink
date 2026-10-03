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
  , scrollViewportTo
  , visibleFraction
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
  ) where

import Blink.Element (content, itemAttrs, items, onSelectionChanged, orientation, selection, step, value)
import Blink.Controls.Control (isEnabled, style)
import Blink.Controls.Button (ButtonActivation (..), ButtonConfig, activation, button, onActivated)
import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Divider (DividerConfig, divider, thickness)
import Blink.Controls.Image (ImageConfig, image, source, fitWidth, fitHeight, preserveRatio)
import Blink.Controls.Label (LabelConfig, label, mnemonic, target, text)
import Blink.Controls.List
  ( Direction (..), EmptySelection (..), End (..), ItemState (..), ListConfig, MultiSelection
  , RangeSelection, RequiredSelection, SelectionModel (..), SingleSelection
  , cursorItem, list, multiItems, multiSelected, multiSelectedAt, multiSelection, noRange, onItemActivated
  , rangeAt, rangeAtPositions, rangeEnd, rangeFrom, rangeItems, renderItem, requireAt, requireFirst, requireItem
  , requiredItems, requiredList, rowHeight, scrollListTo, selectAt, selectFirst, selectItem, selectedItems, singleItems
  , singleSelection, unselected
  )
import Blink.Controls.MenuBar (MenuBarConfig, labelAttrs, menuBar, menuItems, menus, onOpenMenuChanged, openMenu, submenuItems)
import Blink.Controls.MenuButton (MenuButtonConfig, isOpen, menuButton, onOpenChanged)
import Blink.Controls.ProgressBar (ProgressBarConfig, ProgressValue (..), bandSpeed, bandWidth, progress, progressBar)
import Blink.Controls.RadioButton (radioButton)
import Blink.Controls.RepeatButton (RepeatButtonConfig, initialDelay, repeatButton, repeatInterval)
import Blink.Controls.ScrollBar (ScrollBarConfig, scrollBar, scrollViewportTo, visibleFraction)
import Blink.Controls.ScrollPanel (ScrollPanelConfig, scrollPanel, scrollPanelTo)
import Blink.Controls.Slider (SliderConfig, onValueChanged, slider)
import Blink.Controls.TextInput (TextInputConfig, displayFilter, inputFilter, onInput, onSubmit, placeholder, textInput)
import Blink.Controls.ToggleButton (ToggleConfig, isSelected, onSelectedChanged, toggleButton)
import Blink.Controls.ToggleGroup (ToggleGroupConfig, allowDeselect, itemSpacing, radioButtonGroup, toggleButtonGroup)
import Blink.Controls.Table
  ( ColumnConfig, ColumnWidth (..), SortDirection (..), TableConfig
  , cell, cellWidth, column, columns, header, onColumnSortRequested, sortable, sortedBy, table
  )
import Blink.Controls.Tree (TreeConfig, TreeItemState (..), expanded, flattenVisible, forest, onExpansionChanged, renderNode, tree)
import Blink.Controls.TreeTable (TreeTableConfig, treeTable)
