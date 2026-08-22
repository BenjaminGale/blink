module Main (main) where

import Test.Hspec
import qualified Blink.AppSpec as App
import qualified Blink.ControlsSpec as Controls
import qualified Blink.GeometrySpec as Geometry
import qualified Blink.InteractionSpec as Interaction
import qualified Blink.LayoutSpec as Layout
import qualified Blink.MouseSpec as Mouse
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
  Controls.spec
  Interaction.spec
