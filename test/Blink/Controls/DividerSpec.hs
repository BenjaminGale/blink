{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.DividerSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.ControlBehaviour (styleAttributeSpec)
import Blink.Controls.Divider (DividerConfig, divider, dividerLineStyleKey, orientation, thickness)
import Blink.Controls.Fixtures (hitRectFor, mkTestTheme, noInput, plainStyle, plainStyleSet, standardMetrics, testColour)
import Blink.Geometry (Alignment (Center), Orientation (..), Rectangle (..), Size (..))
import Blink.Layout.Constraints (exactly)
import Blink.Rendering (Colour (..), DrawCommand (..))
import Blink.Style (StyleSet (..), Theme (..))
import Blink.View
import Blink.Element (align, height, measureElement, runElement, width)

data TestElement = Bar deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testStyleSet :: StyleSet
testStyleSet = plainStyleSet (plainStyle testColour)

-- | The line part in @colour@, everything else in 'testColour'.
themeWithLine :: Colour -> Theme TestElement
themeWithLine colour = (mkTestTheme standardMetrics testStyleSet)
  { themeElementStyles = Map.singleton dividerLineStyleKey (standardMetrics, plainStyleSet (plainStyle colour)) }

testTheme :: Theme TestElement
testTheme = themeWithLine testColour

-- | The line part transparent, so the "draws nothing" test below can
-- confirm the line itself goes undrawn -- the chrome background 'control'
-- always draws is unaffected.
noLineTheme :: Theme TestElement
noLineTheme = themeWithLine (RGBA 0 0 0 0)

-- | A horizontal divider's own resolved bounds at 'testBounds' with the
-- default thickness (1) and 'testMetrics': fills the offered width, and is
-- just tall enough for its thickness plus chrome (1 + 2*10 margin + 2*5
-- padding == 31), pinned to the top since 'defaultDividerConfig' aligns
-- 'TopLeft'.
horizontalOuterRect :: Rectangle
horizontalOuterRect = Rectangle 0 0 100 31

-- | The margin-inset hit area for 'horizontalOuterRect'.
hitRect :: Rectangle
hitRect = hitRectFor horizontalOuterRect

-- | Content rect for 'horizontalOuterRect': inset by margin (10) then
-- padding (5) -- the thin strip the line itself is drawn into.
contentRect :: Rectangle
contentRect = Rectangle 15 15 70 1

type Attribute' = Attribute (DividerConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme

run :: [Attribute'] -> IO (ViewContext TestElement String)
run attrs = snd <$> runView (runElement (divider attrs)) seedCtx

runWith :: Theme TestElement -> [Attribute'] -> IO (ViewContext TestElement String)
runWith theme attrs = snd <$> runView (runElement (divider attrs)) (emptyViewContext testBounds noInput theme)

spec :: Spec
spec = describe "Blink.Controls.Divider" $ do
  styleAttributeSpec testBounds seedCtx hitRect (runElement . divider)

  describe "no id" $ do
    it "still draws the line (in its resting style), just like it does with an id" $ do
      ctx <- run []
      getDrawCommands ctx `shouldContain` [FillRect contentRect testColour]

  describe "rendering" $ do
    it "fills the content area in the line part's colour" $ do
      ctx <- run []
      getDrawCommands ctx `shouldContain` [FillRect contentRect testColour]

    it "draws nothing when the line part is transparent, leaving the chrome background alone" $ do
      ctx <- runWith noLineTheme []
      getDrawCommands ctx `shouldNotContain` [FillRect contentRect testColour]

  describe "orientation" $ do
    it "gives the same size whichever order height and orientation come in" $ do
      let measure attrs = fst <$> runView (measureElement testBounds (divider attrs)) seedCtx
      heightFirst      <- measure [height (exactly 40), orientation Vertical]
      orientationFirst <- measure [orientation Vertical, height (exactly 40)]
      -- 31 is the vertical default width: the 1px line plus 2*10 margin and 2*5 padding.
      heightFirst `shouldBe` Size 31 40
      orientationFirst `shouldBe` Size 31 40

    it "runs horizontally by default, filling the offered width" $ do
      ctx <- run []
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 15 70 1) testColour]

    it "runs vertically when set, filling the offered height instead" $ do
      ctx <- run [orientation Vertical]
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 15 1 70) testColour]

  describe "thickness" $ do
    it "defaults to 1" $ do
      ctx <- run []
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 15 70 1) testColour]

    it "widens the drawn line's cross-axis size when set" $ do
      ctx <- run [thickness 4]
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 15 70 4) testColour]

  describe "layout" $ do
    it "shrinks to a fixed length when its main axis is overridden with width" $ do
      ctx <- run [width (exactly 40)]
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 15 10 1) testColour]

    it "positions the whole (already thin) control within extra offered space via align" $ do
      ctx <- run [align Center]
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 49.5 70 1) testColour]
