module Blink.PopupSpec (spec) where

import Test.Hspec

import Blink.Element (Element, elementWithLayout)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..))
import Blink.Layout.Constraints (Layout (..), exactly)
import Blink.Popup (at, content, popup)
import Blink.Rendering (Colour (..), DrawCommand (..))
import Blink.View
import Blink.View.Drawing (fillRect)
import Blink.View.Fixtures

fillRectElement :: Colour -> Element e msg
fillRectElement colour = elementWithLayout (Layout (exactly 10) (exactly 10) TopLeft) (fillRect colour)

spec :: Spec
spec = describe "Blink.Popup" $ do
  describe "popup" $ do
    it "does not draw its content inline" $ do
      (_, ctx) <- runTwoElem (popup ElemA [content (fillRectElement (RGBA 1 0 0 1))])
      getDrawCommands ctx `shouldBe` []

    it "queues one pending popup keyed by the given id" $ do
      (_, ctx) <- runTwoElem (popup ElemA [content (fillRectElement (RGBA 1 0 0 1))])
      map popupId (getPendingPopups ctx) `shouldBe` [ElemA]

    it "anchors to the calling control's own bounds by default" $ do
      let anchorRect = Rectangle 5 5 20 20
      (_, ctx) <- runTwoElem (withBounds anchorRect (popup ElemA [content (fillRectElement (RGBA 1 0 0 1))]))
      map popupAnchor (getPendingPopups ctx) `shouldBe` [anchorRect]

    it "anchors to an explicit point when given 'at'" $ do
      (_, ctx) <- runTwoElem (popup ElemA [content (fillRectElement (RGBA 1 0 0 1)), at (Point 10 20)])
      map popupAnchor (getPendingPopups ctx) `shouldBe` [Rectangle 10 20 0 0]

    it "runs and draws its content once its own queued action is executed" $ do
      (_, ctx) <- runTwoElem (popup ElemA [content (fillRectElement (RGBA 1 0 0 1))])
      case getPendingPopups ctx of
        [queued] -> do
          (_, ctx') <- runView (popupRun queued) ctx
          getDrawCommands ctx' `shouldBe` [FillRect testBounds (RGBA 1 0 0 1)]
        other -> expectationFailure ("expected exactly one pending popup, got " ++ show (length other))
