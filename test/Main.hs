module Main (main) where

import Test.Hspec
import qualified Blink.AppSpec as App
import qualified Blink.UI.Controls.ButtonSpec as Button
import qualified Blink.UI.Controls.CheckboxSpec as Checkbox
import qualified Blink.UI.Controls.ControlSpec as Control
import qualified Blink.UI.Controls.DividerSpec as Divider
import qualified Blink.UI.Controls.ElementSpec as Element
import qualified Blink.UI.Controls.LabelSpec as Label
import qualified Blink.UI.Controls.ProgressBarSpec as ProgressBar
import qualified Blink.UI.Controls.RadioButtonSpec as RadioButton
import qualified Blink.UI.Controls.RepeatButtonSpec as RepeatButton
import qualified Blink.UI.Controls.SliderSpec as Slider
import qualified Blink.UI.Controls.TextInputSpec as TextInput
import qualified Blink.UI.Controls.ToggleSpec as Toggle
import qualified Blink.GeometrySpec as Geometry
import qualified Blink.InputSpec as Input
import qualified Blink.InteractionSpec as Interaction
import qualified Blink.UI.LayoutSpec as Layout
import qualified Blink.Style.DefaultsSpec as StyleDefaults
import qualified Blink.UISpec as UI
import qualified Blink.UpdateSpec as Update

main :: IO ()
main = hspec $ do
  App.spec
  UI.spec
  Update.spec
  Geometry.spec
  Layout.spec
  Input.spec
  Element.spec
  Control.spec
  Label.spec
  Button.spec
  Toggle.spec
  Checkbox.spec
  ProgressBar.spec
  RadioButton.spec
  RepeatButton.spec
  Slider.spec
  Divider.spec
  TextInput.spec
  Interaction.spec
  StyleDefaults.spec
