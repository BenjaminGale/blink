module Main (main) where

import Test.Hspec
import qualified Blink.ControlsSpec as Controls

main :: IO ()
main = hspec $ do
  Controls.spec
