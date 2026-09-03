{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.SliderSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Element (Attribute)
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..), Key (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Controls.Slider (SliderConfig, onValueChanged, slider, step, value)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.UI
import Blink.UI.Element (runElement)

data TestElement = Handle deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignCenter
  , styleBorderColour = Nothing
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

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

-- | Content rect for 'testBounds': inset by margin (10) then padding (5).
contentRect :: Rectangle
contentRect = Rectangle 15 15 70 70

-- | The centre of 'contentRect', where a click maps to a value of 0.5.
midPoint :: Point
midPoint = Point 50 50

filledAt :: Double -> DrawCommand
filledAt w = FillRect (contentRect { rectWidth = w }) testColour

thumbAt :: Double -> DrawCommand
thumbAt x = FillRect (contentRect { rectX = x, rectWidth = 8 }) testColour

type Attribute' = Attribute (SliderConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

run :: [Attribute'] -> IO (UIContext TestElement String)
run attrs = snd <$> runUI (runElement (slider Handle attrs)) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Slider" $ do
  controlBehaviourSpec defaultControlBehaviourConfig testBounds seedCtx Handle (Point 5 5) hitRect (Point 200 200) (runElement . slider Handle)

  describe "rendering" $ do
    it "fills the correct proportion and centres the thumb at 0.5" $ do
      ctx <- run [value 0.5]
      getDrawCommands ctx `shouldContain` [filledAt 35]
      getDrawCommands ctx `shouldContain` [thumbAt 46]

    it "fills nothing and pins the thumb to the start at 0.0" $ do
      ctx <- run [value 0.0]
      getDrawCommands ctx `shouldContain` [filledAt 0]
      getDrawCommands ctx `shouldContain` [thumbAt 15]

    it "fills the full track and pins the thumb to the end at 1.0" $ do
      ctx <- run [value 1.0]
      getDrawCommands ctx `shouldContain` [filledAt 70]
      getDrawCommands ctx `shouldContain` [thumbAt 77]

    it "clamps values above 1.0 to the end" $ do
      ctx <- run [value 1.5]
      getDrawCommands ctx `shouldContain` [filledAt 70]
      getDrawCommands ctx `shouldContain` [thumbAt 77]

    it "clamps values below 0.0 to the start" $ do
      ctx <- run [value (-0.5)]
      getDrawCommands ctx `shouldContain` [filledAt 0]
      getDrawCommands ctx `shouldContain` [thumbAt 15]

  describe "dragging" $ do
    it "reports the value at the clicked position on mouse down" $ do
      let attrs = [value 0, onValueChanged (\v -> [OutMsg (show v)])]
      result <- runInteractions testBounds seedCtx (runElement (slider Handle attrs)) [] [MouseDown midPoint]
      resultMessages result `shouldBe` ["0.5"]

    it "keeps reporting the value as the drag continues past the track" $ do
      let attrs = [value 0, onValueChanged (\v -> [OutMsg (show v)])]
      result <- runInteractions testBounds seedCtx (runElement (slider Handle attrs)) []
                  [MouseDown midPoint, DragTo (Point 200 50)]
      resultMessages result `shouldBe` ["0.5", "1.0"]

    it "does not report a value while disabled" $ do
      let attrs = [value 0, onValueChanged (\v -> [OutMsg (show v)])]
      result <- runInteractions testBounds seedCtx (disableWhen True (runElement (slider Handle attrs))) [] [MouseDown midPoint]
      resultMessages result `shouldBe` []

  describe "keyboard" $ do
    let focused attrs = runInteractions testBounds seedCtx (runElement (slider Handle attrs)) [Wait 1]

    it "increases the value by the step on Right while focused" $ do
      let attrs = [value 0.5, onValueChanged (\v -> [OutMsg (show v)])]
      result <- focused attrs [PressKey KeyRight []]
      resultMessages result `shouldBe` ["0.6"]

    it "decreases the value by the step on Left while focused" $ do
      let attrs = [value 0.5, onValueChanged (\v -> [OutMsg (show v)])]
      result <- focused attrs [PressKey KeyLeft []]
      resultMessages result `shouldBe` ["0.4"]

    it "increases the value by the step on Up while focused" $ do
      let attrs = [value 0.5, onValueChanged (\v -> [OutMsg (show v)])]
      result <- focused attrs [PressKey KeyUp []]
      resultMessages result `shouldBe` ["0.6"]

    it "decreases the value by the step on Down while focused" $ do
      let attrs = [value 0.5, onValueChanged (\v -> [OutMsg (show v)])]
      result <- focused attrs [PressKey KeyDown []]
      resultMessages result `shouldBe` ["0.4"]

    it "respects a custom step" $ do
      let attrs = [value 0.5, step 0.25, onValueChanged (\v -> [OutMsg (show v)])]
      result <- focused attrs [PressKey KeyRight []]
      resultMessages result `shouldBe` ["0.75"]

    it "clamps to the minimum instead of going below it" $ do
      let attrs = [value 0.05, onValueChanged (\v -> [OutMsg (show v)])]
      result <- focused attrs [PressKey KeyLeft []]
      resultMessages result `shouldBe` ["0.0"]

    it "does not fire again once already at the minimum" $ do
      let attrs = [value 0.0, onValueChanged (\v -> [OutMsg (show v)])]
      result <- focused attrs [PressKey KeyLeft []]
      resultMessages result `shouldBe` []

    it "does not fire again once already at the maximum" $ do
      let attrs = [value 1.0, onValueChanged (\v -> [OutMsg (show v)])]
      result <- focused attrs [PressKey KeyRight []]
      resultMessages result `shouldBe` []
