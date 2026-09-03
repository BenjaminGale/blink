{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.DividerSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control (Attribute, elementId)
import Blink.Controls.ControlBehaviour (ControlBehaviourConfig (..), controlBehaviourSpec)
import Blink.Controls.Divider (DividerConfig, divider, orientation, thickness)
import Blink.Controls.ElementBehaviour (tagged)
import Blink.Controls.FixedFocusBehaviour (fixedNotFocusableSpec)
import Blink.Geometry (Alignment (Center), Orientation (..), Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (align, exactly, width)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.UI
import Blink.UI.Element (runElement)

data TestElement = Bar deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testColour :: Colour
testColour = RGBA 0 0 0 1

-- | Set (unlike most other controls' test styles) since the drawn line
-- itself -- not some secondary decoration -- is what a divider's border
-- colour means; see 'noLineTheme' for the "nothing set" case.
testStyle :: Style
testStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignCenter
  , styleBorderColour = Just testColour
  }

testMetrics :: Metrics
testMetrics = Metrics
  { metricsMargin      = uniform 10
  , metricsPadding     = uniform 5
  , metricsBorderEdges = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet { styleBase = testStyle, styleOverrides = Map.empty }

testTheme :: Theme TestElement
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = (testMetrics, testStyleSet) }

-- | Same as 'testTheme' but with no border colour set, so the "draws
-- nothing" tests below can confirm the line itself goes undrawn -- the
-- chrome background\/border 'controlBase' always draws is unaffected.
noLineTheme :: Theme TestElement
noLineTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  = (testMetrics, testStyleSet { styleBase = testStyle { styleBorderColour = Nothing } })
  }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

-- | A horizontal divider's own resolved bounds at 'testBounds' with the
-- default thickness (1) and 'testMetrics': fills the offered width, and is
-- just tall enough for its thickness plus chrome (1 + 2*10 margin + 2*5
-- padding == 31), pinned to the top since 'defaultDividerConfig' aligns
-- 'TopLeft'.
horizontalOuterRect :: Rectangle
horizontalOuterRect = Rectangle 0 0 100 31

-- | The margin-inset hit area for 'horizontalOuterRect'.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) horizontalOuterRect

-- | Content rect for 'horizontalOuterRect': inset by margin (10) then
-- padding (5) -- the thin strip the line itself is drawn into.
contentRect :: Rectangle
contentRect = Rectangle 15 15 70 1

type Attribute' = Attribute (DividerConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

run :: [Attribute'] -> IO (UIContext TestElement String)
run attrs = snd <$> runUI (runElement (divider attrs)) seedCtx

runWith :: Theme TestElement -> [Attribute'] -> IO (UIContext TestElement String)
runWith theme attrs = snd <$> runUI (runElement (divider attrs)) (emptyUIContext testBounds noInput theme noOpTextMeasurer)

-- | 'divider' with 'elementId' 'Bar' set -- for the shared behaviour
-- contracts below, which need a real identity to track hover\/click\/focus
-- against.
renderWithId :: [Attribute'] -> UI TestElement String ()
renderWithId attrs = runElement (divider (elementId Bar : attrs))

spec :: Spec
spec = describe "Blink.Controls.Divider" $ do
  controlBehaviourSpec (ControlBehaviourConfig { cbcAutoClaims = False, cbcClickFocuses = False })
    testBounds seedCtx Bar (Point 5 5) hitRect (Point 200 200) renderWithId

  fixedNotFocusableSpec testBounds seedCtx renderWithId

  describe "no id" $ do
    -- No 'elementId' at all -- 'run' never adds one, unlike 'renderWithId'.
    it "raises no events at all, even with every handler attached and the cursor pressed and released over it" $ do
      result <- runInteractions testBounds seedCtx (runElement (divider tagged)) []
                  [MouseDown (Point 50 15), MouseUp (Point 50 15)]
      resultMessages result `shouldBe` []

    it "still draws the line (in its resting style), just like it does with an id" $ do
      ctx <- run []
      getDrawCommands ctx `shouldContain` [FillRect contentRect testColour]

  describe "rendering" $ do
    it "fills the content area in the style's border colour by default" $ do
      ctx <- run []
      getDrawCommands ctx `shouldContain` [FillRect contentRect testColour]

    it "draws nothing when no border colour is set, leaving the chrome background alone" $ do
      ctx <- runWith noLineTheme []
      getDrawCommands ctx `shouldNotContain` [FillRect contentRect testColour]

  describe "orientation" $ do
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
