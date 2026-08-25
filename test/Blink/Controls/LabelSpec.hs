{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.LabelSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Element (Attr)
import Blink.Controls.ControlBehaviour (ControlBehaviourConfig (..), controlBehaviourSpec)
import Blink.Controls.FixedFocusBehaviour (fixedNotFocusableSpec)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Controls.Label (LabelConfig, label, target, text)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

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
  , styleMargin       = uniform 10
  , stylePadding      = uniform 5
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet
  { styleSetNormal   = testStyle
  , styleSetHovered  = testStyle
  , styleSetPressed  = testStyle
  , styleSetFocused  = testStyle
  , styleSetDisabled = testStyle
  }

testTheme :: Theme TestElement
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = testStyleSet }

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

type Attr' = Attr (LabelConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

start :: [Attr'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (label Caption attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Label" $ do
  controlBehaviourSpec (ControlBehaviourConfig { cbcAutoClaims = False, cbcClickFocuses = False })
    testBounds seedCtx Caption (Point 5 5) hitRect (Point 200 200) (label Caption)

  fixedNotFocusableSpec testBounds seedCtx (label Caption)

  it "draws its text in the resolved style" $ do
    ctx <- start [text "Hello"]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "Hello" testColour AlignCenter]

  it "never claims focus, even with nothing else focused" $ do
    result <- runInteractions testBounds seedCtx (label Caption []) [] []
    contextFocus (resultContext result) `shouldBe` Nothing

  it "does not take focus when clicked by default" $ do
    result <- runInteractions testBounds seedCtx (label Caption []) [] [ClickAt onCaption, Wait 1]
    contextFocus (resultContext result) `shouldBe` Nothing

  it "redirects a click's focus onto the element named by target" $ do
    let attrs = [target Target]
    result <- runInteractions testBounds seedCtx (label Caption attrs) [] [ClickAt onCaption, Wait 1]
    contextFocus (resultContext result) `shouldBe` Just Target
