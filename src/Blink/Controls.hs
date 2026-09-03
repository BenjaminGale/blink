-- | The public forms of every ready-made widget. Each widget's own module
-- ("Blink.Controls.Button", "Blink.Controls.Checkbox", ...) documents the
-- attribute functions used to configure it, and the module headers of
-- "Blink.Controls.Element" and "Blink.Controls.Control" describe how the
-- layers underneath fit together.
module Blink.Controls
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

import Blink.Controls.Button (button)
import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Divider (divider)
import Blink.Controls.Label (label)
import Blink.Controls.ProgressBar (progressBar)
import Blink.Controls.RadioButton (radioButton)
import Blink.Controls.RepeatButton (repeatButton)
import Blink.Controls.Slider (slider)
import Blink.Controls.TextInput (textInput)
import Blink.Controls.Toggle (toggleButton)
