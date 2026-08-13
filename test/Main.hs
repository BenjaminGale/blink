module Main (main) where

import Test.Hspec
import qualified Blink.AppSpec as App
import qualified Blink.ControlsSpec as Controls
import qualified Blink.GeometrySpec as Geometry
import qualified Blink.ItemsControlSpec as ItemsControl
import qualified Blink.LayoutSpec as Layout
import qualified Blink.UISpec as UI
import qualified Blink.UpdateSpec as Update

main :: IO ()
main = hspec $ do
  App.spec
  UI.spec
  Update.spec
  Geometry.spec
  Layout.spec
  Controls.spec
  ItemsControl.spec
