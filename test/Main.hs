module Main (main) where

import Test.Hspec
import qualified Blink.ControlsSpec as Controls
import qualified Blink.LayoutSpec as Layout

main :: IO ()
main = hspec $ do
  Controls.spec
  Layout.spec
