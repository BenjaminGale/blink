-- | The public forms of every ready-made widget: each one's constructor
-- plus its own attribute functions. Each widget's own module
-- ("Blink.Controls.Button", "Blink.Controls.Checkbox", ...) documents the
-- attribute functions used to configure it, and "Blink.Controls.Control"'s
-- own module header describes how it's built underneath.
--
-- An attribute several widgets share ('value', 'step', 'orientation',
-- 'items', 'itemAttrs', 'selection', 'onSelectionChanged', 'content') is
-- one name, defined in "Blink.Element", that works on every widget that
-- has it.
module Blink.Controls
  ( -- * Attributes shared across widgets
    value
  , step
  , orientation
  , items
  , itemAttrs
  , selection
  , onSelectionChanged
  , content
    -- * Button
  , button
  , onActivated
  , activation
    -- * ToggleButton
  , toggleButton
  , isSelected
  , onSelectedChanged
    -- * Checkbox
  , checkbox
    -- * RadioButton
  , radioButton
    -- * RepeatButton
  , repeatButton
  , initialDelay
  , repeatInterval
    -- * Label
  , label
  , text
  , mnemonic
  , target
    -- * List
  , list
  , renderItem
  , rowHeight
  , onItemActivated
    -- * Tree
  , tree
  , forest
  , expanded
  , renderNode
  , onExpansionChanged
    -- * Table
  , table
  , columns
    -- * ProgressBar
  , progressBar
  , progress
  , bandSpeed
  , bandWidth
    -- * Slider
  , slider
  , onValueChanged
    -- * Divider
  , divider
  , thickness
    -- * Image
  , image
  , source
  , fitWidth
  , fitHeight
  , preserveRatio
    -- * TextInput
  , textInput
  , placeholder
  , inputFilter
  , displayFilter
  , onInput
  , onSubmit
    -- * ScrollBar
  , scrollBar
  , visibleFraction
    -- * ScrollPanel
  , scrollPanel
    -- * ToggleGroup
  , toggleButtonGroup
  , radioButtonGroup
  , itemSpacing
  , allowDeselect
    -- * MenuButton
  , menuButton
  , isOpen
  , onOpenChanged
    -- * MenuBar
  , menuBar
  , menus
  , labelAttrs
  , menuItems
  , openMenu
  , onOpenMenuChanged
  ) where

import Blink.Element (content, itemAttrs, items, onSelectionChanged, orientation, selection, step, value)
import Blink.Controls.Button (activation, button, onActivated)
import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Divider (divider, thickness)
import Blink.Controls.Image (image, source, fitWidth, fitHeight, preserveRatio)
import Blink.Controls.Label (label, mnemonic, target, text)
import Blink.Controls.List (list, onItemActivated, renderItem, rowHeight)
import Blink.Controls.MenuBar (labelAttrs, menuBar, menuItems, menus, onOpenMenuChanged, openMenu)
import Blink.Controls.MenuButton (isOpen, menuButton, onOpenChanged)
import Blink.Controls.ProgressBar (bandSpeed, bandWidth, progress, progressBar)
import Blink.Controls.RadioButton (radioButton)
import Blink.Controls.RepeatButton (initialDelay, repeatButton, repeatInterval)
import Blink.Controls.ScrollBar (scrollBar, visibleFraction)
import Blink.Controls.ScrollPanel (scrollPanel)
import Blink.Controls.Slider (onValueChanged, slider)
import Blink.Controls.TextInput (displayFilter, inputFilter, onInput, onSubmit, placeholder, textInput)
import Blink.Controls.ToggleButton (isSelected, onSelectedChanged, toggleButton)
import Blink.Controls.ToggleGroup (allowDeselect, itemSpacing, radioButtonGroup, toggleButtonGroup)
import Blink.Controls.Table (columns, table)
import Blink.Controls.Tree (expanded, forest, onExpansionChanged, renderNode, tree)
