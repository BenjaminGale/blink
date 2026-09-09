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
  , target
    -- * List
  , list
  , selection
  , renderItem
  , rowHeight
  , onItemActivated
    -- * ProgressBar
  , progressBar
  , progress
  , bandSpeed
    -- * Slider
  , slider
  , onValueChanged
  , thumbColourFor
    -- * Divider
  , divider
  , orientation
  , thickness
    -- * TextInput
  , textInput
  , value
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
  ) where

import Blink.Controls.Button (activation, button, onActivated)
import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Divider (divider, orientation, thickness)
import Blink.Controls.Label (label, target, text)
import Blink.Controls.List (list, onItemActivated, renderItem, rowHeight, selection)
import Blink.Controls.ProgressBar (bandSpeed, progress, progressBar)
import Blink.Controls.RadioButton (radioButton)
import Blink.Controls.RepeatButton (initialDelay, repeatButton, repeatInterval)
import Blink.Controls.ScrollBar (scrollBar, scrollBarOrientation, visibleFraction)
import Blink.Controls.Slider (onValueChanged, slider, thumbColourFor)
import Blink.Controls.TextInput (displayFilter, inputFilter, onInput, onSubmit, textInput, value)
import Blink.Controls.ToggleButton (isSelected, onSelectedChanged, toggleButton)
import Blink.Controls.ToggleGroup
  ( allowDeselect, groupOrientation, itemSpacing, items, onSelectionChanged, radioButtonGroup
  , selectedItem, toggleAttributes, toggleButtonGroup
  )
