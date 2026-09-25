{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.SliderSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control (Attribute, onFocusGained, onFocusLost, post, postWith)
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Controls.Fixtures (contentRectFor, focusHeldBy, hitRectFor, mkTestTheme, noInput, plainStyle, plainStyleSet, standardMetrics)
import Blink.Geometry (Point (..), Rectangle (..))
import Blink.Input (Key (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Controls.Slider
  (SliderConfig, onValueChanged, slider, sliderStyleKey, sliderThumbStyleKey, sliderTrackStyleKey, step, value)
import Blink.Rendering (Colour (..), DrawCommand (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..), VisualState (..), soloBorder)
import Blink.View
import Blink.Element (runElement)

data TestElement = Handle | Other | FocusHolder deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

-- | Not black in any channel, so the hover\/drag shading tests below (which
-- darken it) produce a colour distinguishable from 'testColour' itself.
testColour :: Colour
testColour = RGBA 0.4 0.4 0.4 1

testStyle :: Style
testStyle = plainStyle testColour

testStyleSet :: StyleSet
testStyleSet = plainStyleSet testStyle

-- | Every part in 'testColour', except the thumb, which darkens on hover
-- and further while dragged.
testTheme :: Theme TestElement
testTheme = (mkTestTheme standardMetrics testStyleSet)
  { themeElementStyles = Map.singleton sliderThumbStyleKey (standardMetrics, thumbStyleSet) }

thumbStyleSet :: StyleSet
thumbStyleSet = StyleSet testStyle (Map.fromList
  [ (CommonMouseOver, \st -> st { styleBackground = hoverThumbColour })
  , (CommonPressed,   \st -> st { styleBackground = dragThumbColour })
  ])

hitRect :: Rectangle
hitRect = hitRectFor testBounds

contentRect :: Rectangle
contentRect = contentRectFor testBounds

-- | The horizontal margin 'Blink.Controls.Slider.contentInset' leaves
-- between 'contentRect' and the track on each side.
trackInset :: Double
trackInset = 5

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

-- | The thumb's hover and drag colours in 'testTheme'.
hoverThumbColour, dragThumbColour :: Colour
hoverThumbColour = RGBA (0.4 * 0.85) (0.4 * 0.85) (0.4 * 0.85) 1
dragThumbColour  = RGBA (0.4 * 0.7) (0.4 * 0.7) (0.4 * 0.7) 1

-- | The full-width groove, at the same vertical position as the filled
-- track 'filledAt' draws over.
groove :: Colour -> DrawCommand
groove = FillRect grooveRect

grooveRect :: Rectangle
grooveRect = trackRect { rectY = rectY contentRect + 33, rectHeight = 4 }

grooveColour :: Colour
grooveColour = RGBA 0.5 0.5 0.5 1

-- | 'testTheme' with the track part in @colour@.
withGrooveColour :: Colour -> Theme TestElement
withGrooveColour colour = testTheme
  { themeElementStyles = Map.insert sliderTrackStyleKey (standardMetrics, plainStyleSet (testStyle { styleBackground = colour }))
      (themeElementStyles testTheme)
  }

-- | 'testTheme' with the slider's own style given a 1px border that's
-- 'testColour' only while focused.
withFocusBorder :: Theme TestElement
withFocusBorder = testTheme
  { themeElementStyles = Map.insert sliderStyleKey (standardMetrics, focusBorderStyleSet) (themeElementStyles testTheme) }
  where
    focusBorderStyleSet = StyleSet (testStyle { styleBorder = soloBorder transparentColour 1 })
      (Map.singleton FocusFocused (\st -> st { styleBorder = soloBorder testColour 1 }))
    transparentColour = RGBA 0 0 0 0

-- | The focus ring, on the control's border edge.
ringAt :: DrawCommand
ringAt = StrokeBorder hitRect (soloBorder testColour 1)

type Attribute' = Attribute (SliderConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme

run :: [Attribute'] -> IO (ViewContext TestElement String)
run attrs = snd <$> runView (runElement (slider Handle attrs)) seedCtx

runWithGroove :: Colour -> [Attribute'] -> IO (ViewContext TestElement String)
runWithGroove colour attrs = snd <$> runView (runElement (slider Handle attrs)) (emptyViewContext testBounds noInput (withGrooveColour colour))

runWithFocusBorder :: View TestElement String () -> IO (ViewContext TestElement String)
runWithFocusBorder v = snd <$> runView v (emptyViewContext testBounds noInput withFocusBorder)

spec :: Spec
spec = describe "Blink.Controls.Slider" $ do
  controlBehaviourSpec defaultControlBehaviourConfig testBounds seedCtx Handle FocusHolder (Point 5 5) hitRect (Point 200 200) (runElement . slider Handle)

  describe "rendering" $ do
    it "fills the correct proportion and centres the thumb at 0.5" $ do
      ctx <- run [value 0.5]
      getDrawCommands ctx `shouldContain` [filledAt 30]
      getDrawCommands ctx `shouldContain` [thumbAt 43]

    it "fills nothing and pins the thumb to the start at 0.0" $ do
      ctx <- run [value 0.0]
      getDrawCommands ctx `shouldContain` [filledAt 0]
      getDrawCommands ctx `shouldContain` [thumbAt 20]

    it "fills the full track and pins the thumb to the end at 1.0" $ do
      ctx <- run [value 1.0]
      getDrawCommands ctx `shouldContain` [filledAt 60]
      getDrawCommands ctx `shouldContain` [thumbAt 66]

    it "clamps values above 1.0 to the end" $ do
      ctx <- run [value 1.5]
      getDrawCommands ctx `shouldContain` [filledAt 60]
      getDrawCommands ctx `shouldContain` [thumbAt 66]

    it "clamps values below 0.0 to the start" $ do
      ctx <- run [value (-0.5)]
      getDrawCommands ctx `shouldContain` [filledAt 0]
      getDrawCommands ctx `shouldContain` [thumbAt 20]

  describe "groove" $ do
    it "draws the full-width groove in the track part's colour" $ do
      ctx <- runWithGroove grooveColour [value 0.3]
      getDrawCommands ctx `shouldContain` [groove grooveColour]

    it "draws no groove when the track part is transparent" $ do
      ctx <- runWithGroove (RGBA 0 0 0 0) [value 0.3]
      [ d | d@(FillRect r _) <- getDrawCommands ctx, r == grooveRect ] `shouldBe` []

  describe "focus ring" $ do
    it "draws a focus ring around the whole control while focused" $ do
      ctx <- runWithFocusBorder (runElement (slider Handle [value 0.5]))
      getDrawCommands ctx `shouldContain` [ringAt]

    it "draws no focus ring while not focused" $ do
      ctx <- runWithFocusBorder (focusHeldBy FocusHolder >> runElement (slider Handle [value 0.5]))
      getDrawCommands ctx `shouldNotContain` [ringAt]

  describe "hover/drag thumb colour" $ do
    it "draws the thumb in the plain colour when neither hovered nor dragging" $ do
      ctx <- run [value 0.5]
      getDrawCommands ctx `shouldContain` [thumbAt 43]
      getDrawCommands ctx `shouldContain` [filledAt 30]

    it "darkens only the thumb, not the filled track, on hover" $ do
      result <- runInteractions testBounds seedCtx (runElement (slider Handle [value 0.5])) [MoveTo midPoint] []
      let draws = resultDraws result
      draws `shouldContain` [thumbColouredAt hoverThumbColour 43]
      draws `shouldContain` [filledAt 30]

    it "darkens the thumb further while dragging than while merely hovering" $ do
      result <- runInteractions testBounds seedCtx (runElement (slider Handle [value 0])) [] [MouseDown midPoint]
      let draws = resultDraws result
      draws `shouldContain` [thumbColouredAt dragThumbColour 20]
      draws `shouldContain` [filledAt 0]

  describe "dragging" $ do
    it "reports the value at the clicked position on mouse down" $ do
      let attrs = [value 0, onValueChanged (postWith (\v -> (show v)))]
      result <- runInteractions testBounds seedCtx (runElement (slider Handle attrs)) [] [MouseDown midPoint]
      resultMessages result `shouldBe` ["0.5"]

    it "keeps reporting the value as the drag continues past the track" $ do
      let attrs = [value 0, onValueChanged (postWith (\v -> (show v)))]
      result <- runInteractions testBounds seedCtx (runElement (slider Handle attrs)) []
                  [MouseDown midPoint, DragTo (Point 200 50)]
      resultMessages result `shouldBe` ["0.5", "1.0"]

    it "does not report a value while disabled" $ do
      let attrs = [value 0, onValueChanged (postWith (\v -> (show v)))]
      result <- runInteractions testBounds seedCtx (disableWhen True (runElement (slider Handle attrs))) [] [MouseDown midPoint]
      resultMessages result `shouldBe` []

  describe "focus" $ do
    -- 'Other' sits to the left, at 'dummyRect', and auto-claims focus the
    -- moment nothing else holds it -- so the slider under test, at
    -- 'sliderRect', never auto-claims itself and starts each of these
    -- unfocused, the same starting point 'Blink.Controls.ControlSpec's
    -- own click-to-focus tests use.
    let dummyRect  = Rectangle 0 0 40 100
        sliderRect = Rectangle 40 0 60 100
        grabPoint  = Point 70 50
        farAway    = Point 500 500
        renderTwo attrs = do
          withBounds dummyRect  (runElement (slider Other []))
          withBounds sliderRect (runElement (slider Handle attrs))

    it "takes focus as soon as the drag starts, before any movement or release" $ do
      let attrs = [onFocusGained (post ("gained" :: String))]
      result <- runInteractions testBounds seedCtx (renderTwo attrs) [] [MouseDown grabPoint, Wait 1]
      resultMessages result `shouldBe` ["gained"]

    it "keeps focus once taken, even if the drag ends with a release outside its bounds" $ do
      let attrs = [onFocusLost (post ("lost" :: String))]
      result <- runInteractions testBounds seedCtx (renderTwo attrs)
                  [MouseDown grabPoint, Wait 1]
                  [DragTo farAway, MouseUp farAway, Wait 1]
      resultMessages result `shouldBe` []

  describe "keyboard" $ do
    let focused attrs = runInteractions testBounds seedCtx (runElement (slider Handle attrs)) [Wait 1]

    it "increases the value by the step on Right while focused" $ do
      let attrs = [value 0.5, onValueChanged (postWith (\v -> (show v)))]
      result <- focused attrs [PressKey KeyRight []]
      resultMessages result `shouldBe` ["0.6"]

    it "decreases the value by the step on Left while focused" $ do
      let attrs = [value 0.5, onValueChanged (postWith (\v -> (show v)))]
      result <- focused attrs [PressKey KeyLeft []]
      resultMessages result `shouldBe` ["0.4"]

    it "increases the value by the step on Up while focused" $ do
      let attrs = [value 0.5, onValueChanged (postWith (\v -> (show v)))]
      result <- focused attrs [PressKey KeyUp []]
      resultMessages result `shouldBe` ["0.6"]

    it "decreases the value by the step on Down while focused" $ do
      let attrs = [value 0.5, onValueChanged (postWith (\v -> (show v)))]
      result <- focused attrs [PressKey KeyDown []]
      resultMessages result `shouldBe` ["0.4"]

    it "respects a custom step" $ do
      let attrs = [value 0.5, step 0.25, onValueChanged (postWith (\v -> (show v)))]
      result <- focused attrs [PressKey KeyRight []]
      resultMessages result `shouldBe` ["0.75"]

    it "clamps to the minimum instead of going below it" $ do
      let attrs = [value 0.05, onValueChanged (postWith (\v -> (show v)))]
      result <- focused attrs [PressKey KeyLeft []]
      resultMessages result `shouldBe` ["0.0"]

    it "does not fire again once already at the minimum" $ do
      let attrs = [value 0.0, onValueChanged (postWith (\v -> (show v)))]
      result <- focused attrs [PressKey KeyLeft []]
      resultMessages result `shouldBe` []

    it "does not fire again once already at the maximum" $ do
      let attrs = [value 1.0, onValueChanged (postWith (\v -> (show v)))]
      result <- focused attrs [PressKey KeyRight []]
      resultMessages result `shouldBe` []
