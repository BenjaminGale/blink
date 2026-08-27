module Main (main) where

import Test.Hspec
import qualified Blink.AppSpec as App
import qualified Blink.Controls.ButtonSpec as Button
import qualified Blink.Controls.CheckboxSpec as Checkbox
import qualified Blink.Controls.ControlSpec as Control
import qualified Blink.Controls.ElementSpec as Element
import qualified Blink.Controls.LabelSpec as Label
import qualified Blink.Controls.ProgressBarSpec as ProgressBar
import qualified Blink.Controls.RadioButtonSpec as RadioButton
import qualified Blink.Controls.TextInputSpec as TextInput
import qualified Blink.Controls.ToggleSpec as Toggle
import qualified Blink.GeometrySpec as Geometry
import qualified Blink.InputSpec as Input
import qualified Blink.InteractionSpec as Interaction
import qualified Blink.LayoutSpec as Layout
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
  TextInput.spec
  Interaction.spec
  StyleDefaults.spec
