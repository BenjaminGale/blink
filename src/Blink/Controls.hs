-- | The public forms of every ready-made widget: each one's constructor
-- plus its own attribute functions. Each widget's own module
-- ("Blink.Controls.Button", "Blink.Controls.Checkbox", ...) documents the
-- attribute functions used to configure it, and "Blink.Controls.Control"'s
-- own module header describes how it's built underneath.
--
-- 'Blink.Controls.Slider.value'\/'Blink.Controls.Slider.step'
-- clash with 'Blink.Controls.TextInput.value' and 'Blink.Controls.ScrollBar.step'
-- respectively, so those two stay out
-- of this umbrella -- import "Blink.Controls.Slider" qualified for
-- them, the way the sample app's @UI@ module does.
--
-- 'Blink.Controls.List.onSelectionChanged' clashes with
-- 'Blink.Controls.ToggleGroup.onSelectionChanged' (the latter stays in
-- this umbrella, being here first) -- import "Blink.Controls.List"
-- qualified for it.
--
-- 'Blink.Controls.MenuButton.items' clashes with
-- 'Blink.Controls.ToggleGroup.items' (the latter stays in this umbrella,
-- being here first) -- import "Blink.Controls.MenuButton" qualified for it.
--
-- 'Blink.Controls.MenuBar.itemAttrs' clashes with
-- 'Blink.Controls.MenuButton.itemAttrs' (the latter stays in this
-- umbrella, being here first) -- import "Blink.Controls.MenuBar" qualified
-- for it.
module Blink.Controls
  ( -- * Button
    button
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
  , selection
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
  , thumbColourFor
    -- * Divider
  , divider
  , orientation
  , thickness
    -- * Image
  , image
  , source
  , fitWidth
  , fitHeight
  , preserveRatio
    -- * TextInput
  , textInput
  , value
  , placeholder
  , inputFilter
  , displayFilter
  , onInput
  , onSubmit
    -- * ScrollBar
  , scrollBar
  , scrollBarOrientation
  , visibleFraction
    -- * ToggleGroup
  , toggleButtonGroup
  , radioButtonGroup
  , items
  , toggleAttributes
  , groupOrientation
  , itemSpacing
  , selectedItem
  , allowDeselect
  , onSelectionChanged
    -- * MenuButton
    -- | 'Blink.Controls.MenuButton.items' clashes with 'items' above (this
    -- module's own, from "Blink.Controls.ToggleGroup") and stays out of
    -- this umbrella -- import "Blink.Controls.MenuButton" qualified for it.
  , menuButton
  , itemAttrs
  , isOpen
  , onOpenChanged
    -- * MenuBar
    -- | 'Blink.Controls.MenuBar.itemAttrs' clashes with 'itemAttrs' above
    -- (from "Blink.Controls.MenuButton") and stays out of this umbrella --
    -- import "Blink.Controls.MenuBar" qualified for it.
  , menuBar
  , menus
  , labelAttrs
  , menuItems
  , openMenu
  , onOpenMenuChanged
  ) where

import Blink.Controls.Button (activation, button, onActivated)
import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Divider (divider, orientation, thickness)
import Blink.Controls.Image (image, source, fitWidth, fitHeight, preserveRatio)
import Blink.Controls.Label (label, mnemonic, target, text)
import Blink.Controls.List (list, onItemActivated, renderItem, rowHeight, selection)
import Blink.Controls.MenuBar (labelAttrs, menuBar, menuItems, menus, onOpenMenuChanged, openMenu)
import Blink.Controls.MenuButton (isOpen, itemAttrs, menuButton, onOpenChanged)
import Blink.Controls.ProgressBar (bandSpeed, bandWidth, progress, progressBar)
import Blink.Controls.RadioButton (radioButton)
import Blink.Controls.RepeatButton (initialDelay, repeatButton, repeatInterval)
import Blink.Controls.ScrollBar (scrollBar, scrollBarOrientation, visibleFraction)
import Blink.Controls.Slider (onValueChanged, slider, thumbColourFor)
import Blink.Controls.TextInput (displayFilter, inputFilter, onInput, onSubmit, placeholder, textInput, value)
import Blink.Controls.ToggleButton (isSelected, onSelectedChanged, toggleButton)
import Blink.Controls.ToggleGroup
  ( allowDeselect, groupOrientation, itemSpacing, items, onSelectionChanged, radioButtonGroup
  , selectedItem, toggleAttributes, toggleButtonGroup
  )
import Blink.Controls.Table (columns, table)
import Blink.Controls.Tree (expanded, forest, onExpansionChanged, renderNode, tree)
