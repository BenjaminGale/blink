-- | The public forms of every ready-made widget. See "Blink.Controls.Attrs"
-- for the attribute functions used to configure them, and the module
-- headers of "Blink.Controls.Core" and "Blink.Controls.Button" for how the
-- layers underneath fit together.
module Blink.Controls
  ( button
  , toggleButton
  , checkbox
  , radioButton
  , label
  , progressBar
  , textInput
  ) where

import Blink.Controls.Button (button, toggleButton)
import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Label (label)
import Blink.Controls.ProgressBar (progressBar)
import Blink.Controls.RadioButton (radioButton)
import Blink.Controls.TextInput (textInput)
