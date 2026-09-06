-- | The public forms of every ready-made widget. Each widget's own module
-- ("Blink.View.Controls.Button", "Blink.View.Controls.Checkbox", ...) documents the
-- attribute functions used to configure it, and "Blink.View.Controls.Control"'s
-- own module header describes how it's built underneath.
module Blink.View.Controls
  ( button
  , toggleButton
  , checkbox
  , radioButton
  , repeatButton
  , label
  , progressBar
  , slider
  , divider
  , textInput
  ) where

import Blink.View.Controls.Button (button)
import Blink.View.Controls.Checkbox (checkbox)
import Blink.View.Controls.Divider (divider)
import Blink.View.Controls.Label (label)
import Blink.View.Controls.ProgressBar (progressBar)
import Blink.View.Controls.RadioButton (radioButton)
import Blink.View.Controls.RepeatButton (repeatButton)
import Blink.View.Controls.Slider (slider)
import Blink.View.Controls.TextInput (textInput)
import Blink.View.Controls.Toggle (toggleButton)
