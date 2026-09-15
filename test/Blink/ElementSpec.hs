module Blink.ElementSpec (spec) where

import Test.Hspec

import Blink.Element (Element (..), measureElement)
import Blink.Geometry (Alignment (TopLeft), Rectangle (..), Size (..))
import Blink.Layout.Constraints (Layout (..), exactly, fill, fitContent)
import Blink.View.Fixtures (run0)

available :: Rectangle
available = Rectangle 0 0 100 50

-- | Reports a size no caller should ever see chosen -- proves each test
-- below reads its result from 'elLayout', not from 'elMeasure', except
-- where the whole point is that it does (the 'fitContent' case).
wrongIfUsed :: Size
wrongIfUsed = Size 999 999

exactlySizedElement :: Element e msg
exactlySizedElement = Element (Layout (exactly 20) (exactly 10) TopLeft) (const (pure wrongIfUsed)) (pure ())

fillSizedElement :: Element e msg
fillSizedElement = Element (Layout fill fill TopLeft) (const (pure wrongIfUsed)) (pure ())

naturalSize :: Size
naturalSize = Size 42 17

fitContentElement :: Element e msg
fitContentElement = Element (Layout fitContent fitContent TopLeft) (const (pure naturalSize)) (pure ())

spec :: Spec
spec = describe "Blink.Element.measureElement" $ do
  it "reports an exactly-sized element's fixed size regardless of available space" $ do
    (size, _) <- run0 (measureElement available exactlySizedElement)
    size `shouldBe` Size 20 10

  it "reports a fill-sized element's size as the available space" $ do
    (size, _) <- run0 (measureElement available fillSizedElement)
    size `shouldBe` Size 100 50

  it "reports a fitContent-sized element's own natural size from elMeasure, ignoring available space" $ do
    (size, _) <- run0 (measureElement available fitContentElement)
    size `shouldBe` naturalSize
