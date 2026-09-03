{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.SliderSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control (isFocusable)
import Blink.Controls.Element (Attribute)
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform, uniformBorder)
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

-- | Not black in any channel, so the hover\/drag shading tests below (which
-- darken it) produce a colour distinguishable from 'testColour' itself.
testColour :: Colour
testColour = RGBA 0.4 0.4 0.4 1

-- | Scales @c@'s RGB by @factor@, replicating (not importing)
-- 'Blink.Controls.Slider.slider's own hover\/drag thumb-shading formula,
-- so the expected colour is computed the same way the control computes it
-- rather than as a separately-transcribed literal.
scaleColour :: Double -> Colour -> Colour
scaleColour factor (RGBA r g b a) = RGBA (r * factor) (g * factor) (b * factor) a

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

-- | The horizontal margin 'Blink.Controls.Slider.contentInset' leaves
-- between 'contentRect' and the track on each side.
trackInset :: Double
trackInset = 6

-- | The track's own rect: 'contentRect' narrowed by 'trackInset' on the
-- left and right.
trackRect :: Rectangle
trackRect = contentRect { rectX = rectX contentRect + trackInset, rectWidth = rectWidth contentRect - 2 * trackInset }

-- | The centre of 'trackRect' (and, since the inset is symmetric, also of
-- 'contentRect'), where a click maps to a value of 0.5.
midPoint :: Point
midPoint = Point 50 50

-- | The thin filled track, vertically centred in 'contentRect' (4px tall,
-- inset by (70 - 4) \/ 2 == 33px from its top).
filledAt :: Double -> DrawCommand
filledAt w = FillRect (trackRect { rectY = rectY contentRect + 33, rectWidth = w, rectHeight = 4 }) testColour

-- | The 14px square thumb, vertically centred in 'contentRect' (inset by
-- (70 - 14) \/ 2 == 28px from its top), at the given left edge and colour.
thumbColouredAt :: Colour -> Double -> DrawCommand
thumbColouredAt c x = FillRect (contentRect { rectX = x, rectY = rectY contentRect + 28, rectWidth = 14, rectHeight = 14 }) c

-- | The thumb in the plain (not hovered, not dragging) colour.
thumbAt :: Double -> DrawCommand
thumbAt = thumbColouredAt testColour

-- | The full-width groove, at the same vertical position as the filled
-- track 'filledAt' draws over.
groove :: Colour -> DrawCommand
groove c = FillRect (trackRect { rectY = rectY contentRect + 33, rectHeight = 4 }) c

grooveColour :: Colour
grooveColour = RGBA 0.5 0.5 0.5 1

-- | A style identical to 'testStyle' but with a border colour set, so the
-- groove tests below can confirm it's drawn from that colour.
withGrooveColour :: Theme TestElement
withGrooveColour = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  = (testMetrics, StyleSet (testStyle { styleBorderColour = Just grooveColour }) Map.empty)
  }

-- | The focus ring around the whole control.
ringAt :: DrawCommand
ringAt = StrokeBorder contentRect testColour (uniformBorder 1)

type Attribute' = Attribute (SliderConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

run :: [Attribute'] -> IO (UIContext TestElement String)
run attrs = snd <$> runUI (runElement (slider Handle attrs)) seedCtx

runWithGroove :: [Attribute'] -> IO (UIContext TestElement String)
runWithGroove attrs = snd <$> runUI (runElement (slider Handle attrs)) (emptyUIContext testBounds noInput withGrooveColour noOpTextMeasurer)

spec :: Spec
spec = describe "Blink.Controls.Slider" $ do
  controlBehaviourSpec defaultControlBehaviourConfig testBounds seedCtx Handle (Point 5 5) hitRect (Point 200 200) (runElement . slider Handle)

  describe "rendering" $ do
    it "fills the correct proportion and centres the thumb at 0.5" $ do
      ctx <- run [value 0.5]
      getDrawCommands ctx `shouldContain` [filledAt 29]
      getDrawCommands ctx `shouldContain` [thumbAt 43]

    it "fills nothing and pins the thumb to the start at 0.0" $ do
      ctx <- run [value 0.0]
      getDrawCommands ctx `shouldContain` [filledAt 0]
      getDrawCommands ctx `shouldContain` [thumbAt 21]

    it "fills the full track and pins the thumb to the end at 1.0" $ do
      ctx <- run [value 1.0]
      getDrawCommands ctx `shouldContain` [filledAt 58]
      getDrawCommands ctx `shouldContain` [thumbAt 65]

    it "clamps values above 1.0 to the end" $ do
      ctx <- run [value 1.5]
      getDrawCommands ctx `shouldContain` [filledAt 58]
      getDrawCommands ctx `shouldContain` [thumbAt 65]

    it "clamps values below 0.0 to the start" $ do
      ctx <- run [value (-0.5)]
      getDrawCommands ctx `shouldContain` [filledAt 0]
      getDrawCommands ctx `shouldContain` [thumbAt 21]

  describe "groove" $ do
    it "draws the full-width groove in the style's border colour when one is set" $ do
      ctx <- runWithGroove [value 0.3]
      getDrawCommands ctx `shouldContain` [groove grooveColour]

    it "draws no full-width groove when no border colour is set" $ do
      ctx <- run [value 0.3]
      getDrawCommands ctx `shouldNotContain` [groove testColour]

  describe "focus ring" $ do
    it "draws a focus ring around the whole control while focused" $ do
      ctx <- run [value 0.5]
      getDrawCommands ctx `shouldContain` [ringAt]

    it "draws no focus ring while not focused" $ do
      ctx <- run [isFocusable False, value 0.5]
      getDrawCommands ctx `shouldNotContain` [ringAt]

  describe "hover/drag thumb colour" $ do
    it "draws the thumb in the plain colour when neither hovered nor dragging" $ do
      ctx <- run [value 0.5]
      getDrawCommands ctx `shouldContain` [thumbAt 43]
      getDrawCommands ctx `shouldContain` [filledAt 29]

    it "darkens only the thumb, not the filled track, on hover" $ do
      result <- runInteractions testBounds seedCtx (runElement (slider Handle [value 0.5])) [MoveTo midPoint] []
      let draws = resultDraws result
      draws `shouldContain` [thumbColouredAt (scaleColour 0.85 testColour) 43]
      draws `shouldContain` [filledAt 29]

    it "darkens the thumb further while dragging than while merely hovering" $ do
      result <- runInteractions testBounds seedCtx (runElement (slider Handle [value 0])) [] [MouseDown midPoint]
      let draws = resultDraws result
      draws `shouldContain` [thumbColouredAt (scaleColour 0.7 testColour) 21]
      draws `shouldContain` [filledAt 0]

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
