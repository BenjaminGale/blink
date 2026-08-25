-- | Every application-facing attribute function in one place, so a caller
-- building a view doesn't need to know (or care) which layer a given
-- attribute is actually defined against -- 'style' and 'onClicked' work the
-- same way whether the widget underneath is a plain 'Blink.Controls.Control.controlBase'
-- or a full 'Blink.Controls.Button.buttonBase'.
--
-- Not re-exported here: 'Blink.Controls.TextInput.value', which sets a text
-- field's edited value, not a caption -- a different type from this
-- module's own 'text'.
module Blink.Controls.Attributes
  ( -- * Element events
    onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost

    -- * Control
  , isFocusable
  , isEnabled
  , style
  , StyleKey (..)

    -- * Caption
  , text

    -- * Button / toggle
  , onActivated
  , isSelected
  , onSelectedChanged

    -- * Label
  , target

    -- * ProgressBar
  , progress
  , bandSpeed
  , ProgressValue (..)

    -- * TextInput
  , inputFilter
  , displayFilter
  , onInput
  , onSubmit
  ) where

import Blink.Controls.Button (isSelected, onActivated, onSelectedChanged)
import Blink.Controls.Control
  ( StyleKey (..)
  , isEnabled, isFocusable, style
  , onClicked, onFocusGained, onFocusLost, onKeyPressed, onMouseDown, onMouseEntered, onMouseExited, onMouseUp
  )
import Blink.Controls.Label (target)
import Blink.Controls.Labelled (text)
import Blink.Controls.ProgressBar (ProgressValue (..), bandSpeed, progress)
import Blink.Controls.TextInput (displayFilter, inputFilter, onInput, onSubmit)
