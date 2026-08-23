module Main (main) where

import Test.Hspec
import qualified Blink.AppSpec as App
import qualified Blink.ButtonSpec as Button
import qualified Blink.CheckboxSpec as Checkbox
import qualified Blink.ControlSpec as Control
import qualified Blink.ControlsSpec as Controls
import qualified Blink.ElementSpec as Element
import qualified Blink.GeometrySpec as Geometry
import qualified Blink.InteractionSpec as Interaction
import qualified Blink.LabelSpec as Label
import qualified Blink.LabelledControlSpec as LabelledControl
import qualified Blink.LayoutSpec as Layout
import qualified Blink.MouseSpec as Mouse
import qualified Blink.ProgressBarSpec as ProgressBar
import qualified Blink.RadioButtonSpec as RadioButton
import qualified Blink.TextInputSpec as TextInput
import qualified Blink.UISpec as UI
import qualified Blink.UpdateSpec as Update

main :: IO ()
main = hspec $ do
  App.spec
  UI.spec
  Update.spec
  Geometry.spec
  Layout.spec
  Mouse.spec
  Element.spec
  Control.spec
  Label.spec
  LabelledControl.spec
  Button.spec
  Checkbox.spec
  ProgressBar.spec
  RadioButton.spec
  TextInput.spec
  Controls.spec
  Interaction.spec
