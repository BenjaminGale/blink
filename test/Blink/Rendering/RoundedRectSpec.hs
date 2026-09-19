module Blink.Rendering.RoundedRectSpec (spec) where

import Data.List (sortOn)
import Test.Hspec

import Blink.Geometry (Rectangle (..), CornerRadii (..), EdgeVisibility (..), allEdgesVisible, uniformRadii)
import Blink.Rendering.RoundedRect (ringRects)

r :: Rectangle
r = Rectangle 0 0 20 20

-- | Only the top-left corner rounded -- isolates that corner's own arc
-- rows from the other three corners' (empty) and the straight edges,
-- which stay inset away from the rounded corner and so never land
-- inside its @radius x radius@ box.
topLeftOnly :: CornerRadii
topLeftOnly = (uniformRadii 0) { radiusTopLeft = 5 }

-- | The rects 'ringRects' produced for the top-left corner's own
-- @radius x radius@ box, ordered top to bottom.
topLeftArc :: [Rectangle]
topLeftArc = sortOn rectY [ rect | rect <- ringRects r topLeftOnly 5 allEdgesVisible, rectX rect < 5, rectY rect < 5 ]

spec :: Spec
spec = describe "Blink.Rendering.RoundedRect" $ do
  describe "ringRects" $ do
    it "produces the four full straight edges and no corner rects when every radius is 0" $
      ringRects r (uniformRadii 0) 2 allEdgesVisible `shouldMatchList`
        [ Rectangle 0 0 20 2   -- top
        , Rectangle 0 18 20 2  -- bottom
        , Rectangle 0 0 2 20   -- left
        , Rectangle 18 0 2 20  -- right
        ]

    it "omits a hidden edge, squared off, when its radius is 0" $
      ringRects r (uniformRadii 0) 2 (allEdgesVisible { edgeBottomVisible = False }) `shouldMatchList`
        [ Rectangle 0 0 20 2
        , Rectangle 0 0 2 20
        , Rectangle 18 0 2 20
        ]

    describe "a rounded corner (radius 5, thickness 5 -- fully solid, no inner hole)" $ do
      it "produces the exact row 1 pixel below the tip" $
        topLeftArc `shouldContain` [Rectangle 2 1 3 1]

      it "produces the exact row 2 pixels below the tip" $
        topLeftArc `shouldContain` [Rectangle 1 2 4 1]

      it "widens row by row, moving away from the tip toward the straight edge" $
        let widths = map rectWidth topLeftArc
        in zipWith (<) widths (tail widths) `shouldSatisfy` and
