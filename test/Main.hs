module Main (main) where

import Test.Hspec
import qualified Blink.AppSpec as App
import qualified Blink.View.Controls.ButtonSpec as Button
import qualified Blink.View.Controls.CheckboxSpec as Checkbox
import qualified Blink.View.Controls.ControlSpec as Control
import qualified Blink.View.Controls.DividerSpec as Divider
import qualified Blink.View.Controls.FocusScopeSpec as FocusScope
import qualified Blink.View.Controls.LabelSpec as Label
import qualified Blink.View.Controls.ProgressBarSpec as ProgressBar
import qualified Blink.View.Controls.RadioButtonSpec as RadioButton
import qualified Blink.View.Controls.RepeatButtonSpec as RepeatButton
import qualified Blink.View.Controls.ScrollBarSpec as ScrollBar
import qualified Blink.View.Controls.SliderSpec as Slider
import qualified Blink.View.Controls.TextInputSpec as TextInput
import qualified Blink.View.Controls.ToggleButtonSpec as ToggleButton
import qualified Blink.View.Controls.ToggleGroupSpec as ToggleGroup
import qualified Blink.GeometrySpec as Geometry
import qualified Blink.InputSpec as Input
import qualified Blink.InteractionSpec as Interaction
import qualified Blink.View.LayoutSpec as Layout
import qualified Blink.View.Style.DefaultsSpec as StyleDefaults
import qualified Blink.ViewSpec as View
import qualified Blink.UpdateSpec as Update

main :: IO ()
main = hspec $ do
  App.spec
  View.spec
  Update.spec
  Geometry.spec
  Layout.spec
  Input.spec
  Control.spec
  FocusScope.spec
  Label.spec
  Button.spec
  ToggleButton.spec
  ToggleGroup.spec
  Checkbox.spec
  ProgressBar.spec
  RadioButton.spec
  RepeatButton.spec
  ScrollBar.spec
  Slider.spec
  Divider.spec
  TextInput.spec
  Interaction.spec
  StyleDefaults.spec
