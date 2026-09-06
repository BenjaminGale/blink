-- | The public forms of every ready-made widget. Each widget's own module
-- ("Blink.UI.Controls.Button", "Blink.UI.Controls.Checkbox", ...) documents the
-- attribute functions used to configure it, and the module headers of
-- "Blink.UI.Controls.Element" and "Blink.UI.Controls.Control" describe how the
-- layers underneath fit together.
module Blink.UI.Controls
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

import Blink.UI.Controls.Button (button)
import Blink.UI.Controls.Checkbox (checkbox)
import Blink.UI.Controls.Divider (divider)
import Blink.UI.Controls.Label (label)
import Blink.UI.Controls.ProgressBar (progressBar)
import Blink.UI.Controls.RadioButton (radioButton)
import Blink.UI.Controls.RepeatButton (repeatButton)
import Blink.UI.Controls.Slider (slider)
import Blink.UI.Controls.TextInput (textInput)
import Blink.UI.Controls.Toggle (toggleButton)
