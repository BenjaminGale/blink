{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.LabelSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Element (Attribute)
import Blink.Controls.ControlBehaviour (ControlBehaviourConfig (..), controlBehaviourSpec)
import Blink.Controls.FixedFocusBehaviour (fixedNotFocusableSpec)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Controls.Label (LabelConfig, label, target, text)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.UI
import Blink.UI.Element (elLayout, runElement)

data TestElement = Caption | Target deriving (Eq, Ord, Show)

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

onCaption :: Point
onCaption = Point 50 50

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin the test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

type Attribute' = Attribute (LabelConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

-- | The behaviour contracts below are about interaction, not sizing --
-- they're written against a label that fills its given bounds entirely, as
-- every control did before controls reported their own 'Layout'. 'label'
-- now defaults to sizing its height to its own content, so these tests ask
-- for the old full-size behaviour explicitly, the same way any other
-- caller would.
fullSize :: [Attribute'] -> UI TestElement String ()
fullSize attrs = runElement (label Caption attrs) { elLayout = Layout fill fill TopLeft }

start :: [Attribute'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (fullSize attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Label" $ do
  controlBehaviourSpec (ControlBehaviourConfig { cbcAutoClaims = False, cbcClickFocuses = False })
    testBounds seedCtx Caption (Point 5 5) hitRect (Point 200 200) fullSize

  fixedNotFocusableSpec testBounds seedCtx fullSize

  it "draws its text in the resolved style" $ do
    ctx <- start [text "Hello"]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "Hello" testColour AlignCenter]

  it "never claims focus, even with nothing else focused" $ do
    result <- runInteractions testBounds seedCtx (fullSize []) [] []
    contextFocus (resultContext result) `shouldBe` Nothing

  it "does not take focus when clicked by default" $ do
    result <- runInteractions testBounds seedCtx (fullSize []) [] [ClickAt onCaption, Wait 1]
    contextFocus (resultContext result) `shouldBe` Nothing

  it "redirects a click's focus onto the element named by target" $ do
    let attrs = [target Target]
    result <- runInteractions testBounds seedCtx (fullSize attrs) [] [ClickAt onCaption, Wait 1]
    contextFocus (resultContext result) `shouldBe` Just Target
