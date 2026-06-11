module Main (main) where

import Test.Hspec
import qualified Blink.AppSpec as App
import qualified Blink.ControlsSpec as Controls
import qualified Blink.GeometrySpec as Geometry
import qualified Blink.LayoutSpec as Layout
import qualified Blink.UISpec as UI

main :: IO ()
main = hspec $ do
  App.spec
  Controls.spec
  Geometry.spec
  Layout.spec
  UI.spec
