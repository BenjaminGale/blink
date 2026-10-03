module Blink.PopupSpec (spec) where

import Test.Hspec

import Blink.Element (Element, elementWithLayout)
import Blink.Geometry
  (Alignment (TopLeft), Edge (..), Point (..), Rectangle (..), Side (..), Size (..), placePopup)
import Blink.Layout.Constraints (Layout (..), exactly)
import Blink.Popup (at, content, offset, placement, popup)
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

    it "defaults popupPlacement to (SideBottom, Start) and popupOffset to 0" $ do
      (_, ctx) <- runTwoElem (popup ElemA [content (fillRectElement (RGBA 1 0 0 1))])
      case getPendingPopups ctx of
        [queued] -> do
          popupPlacement queued `shouldBe` (SideBottom, Start)
          popupOffset queued `shouldBe` 0
        other -> expectationFailure ("expected exactly one pending popup, got " ++ show (length other))

    it "sets popupPlacement/popupOffset from the placement/offset attributes" $ do
      (_, ctx) <- runTwoElem
        (popup ElemA [content (fillRectElement (RGBA 1 0 0 1)), placement SideTop Middle, offset 12])
      case getPendingPopups ctx of
        [queued] -> do
          popupPlacement queued `shouldBe` (SideTop, Middle)
          popupOffset queued `shouldBe` 12
        other -> expectationFailure ("expected exactly one pending popup, got " ++ show (length other))

    it "runs and draws its content once its own queued action is executed" $ do
      (_, ctx) <- runTwoElem (popup ElemA [content (fillRectElement (RGBA 1 0 0 1))])
      case getPendingPopups ctx of
        [queued] -> do
          (_, ctx') <- runView (popupRun queued) ctx
          getDrawCommands ctx' `shouldBe` [FillRect testBounds (RGBA 1 0 0 1)]
        other -> expectationFailure ("expected exactly one pending popup, got " ++ show (length other))

  describe "placePopup" $ do
    let anchor = Rectangle 40 40 20 20
        window = testBounds -- Rectangle 0 0 100 100
        size   = Size 30 10

    describe "every (Side, Edge) combination, comfortably inside the window" $ do
      it "SideBottom Start sits below the anchor, flush with its left edge" $
        placePopup anchor window size (SideBottom, Start) 0 `shouldBe` Rectangle 40 60 30 10

      it "SideBottom Middle sits below the anchor, centred on it" $
        placePopup anchor window size (SideBottom, Middle) 0 `shouldBe` Rectangle 35 60 30 10

      it "SideBottom End sits below the anchor, flush with its right edge" $
        placePopup anchor window size (SideBottom, End) 0 `shouldBe` Rectangle 30 60 30 10

      it "SideTop Start sits above the anchor, flush with its left edge" $
        placePopup anchor window size (SideTop, Start) 0 `shouldBe` Rectangle 40 30 30 10

      it "SideTop Middle sits above the anchor, centred on it" $
        placePopup anchor window size (SideTop, Middle) 0 `shouldBe` Rectangle 35 30 30 10

      it "SideTop End sits above the anchor, flush with its right edge" $
        placePopup anchor window size (SideTop, End) 0 `shouldBe` Rectangle 30 30 30 10

      it "SideRight Start sits right of the anchor, flush with its top edge" $
        placePopup anchor window size (SideRight, Start) 0 `shouldBe` Rectangle 60 40 30 10

      it "SideRight Middle sits right of the anchor, centred on it" $
        placePopup anchor window size (SideRight, Middle) 0 `shouldBe` Rectangle 60 45 30 10

      it "SideRight End sits right of the anchor, flush with its bottom edge" $
        placePopup anchor window size (SideRight, End) 0 `shouldBe` Rectangle 60 50 30 10

      it "SideLeft Start sits left of the anchor, flush with its top edge" $
        placePopup anchor window size (SideLeft, Start) 0 `shouldBe` Rectangle 10 40 30 10

      it "SideLeft Middle sits left of the anchor, centred on it" $
        placePopup anchor window size (SideLeft, Middle) 0 `shouldBe` Rectangle 10 45 30 10

      it "SideLeft End sits left of the anchor, flush with its bottom edge" $
        placePopup anchor window size (SideLeft, End) 0 `shouldBe` Rectangle 10 50 30 10

    describe "flipping to the opposite Side when the preferred one would overflow the window" $ do
      let flipSize = Size 30 20

      it "flips SideBottom to SideTop when there's no room below" $ do
        let anchorNearBottom = Rectangle 40 85 20 10
        placePopup anchorNearBottom window flipSize (SideBottom, Start) 0 `shouldBe` Rectangle 40 65 30 20

      it "flips SideTop to SideBottom when there's no room above" $ do
        let anchorNearTop = Rectangle 40 5 20 10
        placePopup anchorNearTop window flipSize (SideTop, Start) 0 `shouldBe` Rectangle 40 15 30 20

      it "flips SideRight to SideLeft when there's no room to the right" $ do
        let anchorNearRight = Rectangle 85 40 10 20
        placePopup anchorNearRight window flipSize (SideRight, Start) 0 `shouldBe` Rectangle 55 40 30 20

      it "flips SideLeft to SideRight when there's no room to the left" $ do
        let anchorNearLeft = Rectangle 5 40 10 20
        placePopup anchorNearLeft window flipSize (SideLeft, Start) 0 `shouldBe` Rectangle 15 40 30 20
