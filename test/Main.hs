module Main (main) where

import Test.Hspec
import qualified Blink.AppSpec as App
import qualified Blink.Controls.ButtonSpec as Button
import qualified Blink.Controls.CheckboxSpec as Checkbox
import qualified Blink.Controls.ControlSpec as Control
import qualified Blink.Controls.DividerSpec as Divider
import qualified Blink.Controls.FocusScopeSpec as FocusScope
import qualified Blink.Controls.LabelSpec as Label
import qualified Blink.Controls.ListSpec as List
import qualified Blink.Controls.ProgressBarSpec as ProgressBar
import qualified Blink.Controls.RadioButtonSpec as RadioButton
import qualified Blink.Controls.RepeatButtonSpec as RepeatButton
import qualified Blink.Controls.ScrollBarSpec as ScrollBar
import qualified Blink.Controls.SliderSpec as Slider
import qualified Blink.Controls.TextInputSpec as TextInput
import qualified Blink.Controls.ToggleButtonSpec as ToggleButton
import qualified Blink.Controls.ToggleGroupSpec as ToggleGroup
import qualified Blink.Controls.TableSpec as Table
import qualified Blink.Controls.TreeSpec as Tree
import qualified Blink.GeometrySpec as Geometry
import qualified Blink.InputSpec as Input
import qualified Blink.InteractionSpec as Interaction
import qualified Blink.LayoutSpec as Layout
import qualified Blink.Style.DefaultsSpec as StyleDefaults
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
  List.spec
  Tree.spec
  Table.spec
  ProgressBar.spec
  RadioButton.spec
  RepeatButton.spec
  ScrollBar.spec
  Slider.spec
  Divider.spec
  TextInput.spec
  Interaction.spec
  StyleDefaults.spec
