{-# LANGUAGE OverloadedStrings #-}
module Blink.ButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Attributes (Attr, FocusOnClick (..), focusOnClick, text)
import Blink.Button (ButtonConfig, ToggleButtonConfig, ToggleEvent, button, isSelected, onSelectedChanged, toggleButton)
import Blink.Element (ElementEvent, onClicked)
import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = Ok | Other deriving (Eq, Ord, Show)

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

pressedColour :: Colour
pressedColour = RGBA 1 1 1 1

toggleTestTheme :: Theme TestElement
toggleTestTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  = testStyleSet { styleSetPressed = testStyle { styleTextColour = pressedColour } }
  }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

down, releasedAt :: Point -> InputState
down p       = noInput { inputMousePosition = p, inputLeftButtonDown = True }
releasedAt p = noInput { inputMousePosition = p, inputLeftButtonDown = False }

onButton :: Point
onButton = Point 50 50

type Attr' = Attr TestElement ElementEvent String (ButtonConfig TestElement)

start :: [Attr'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (button Ok attrs) (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)

step :: [Attr'] -> InputState -> UIContext TestElement String -> IO (UIContext TestElement String)
step attrs input ctx = snd <$> runUI (button Ok attrs) (nextFrameContext testBounds input testTheme (contextAnimation ctx) ctx)

spec :: Spec
spec = describe "Blink.Button" $ do
  it "draws its text in the resolved style" $ do
    ctx <- start [text "OK"]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "OK" testColour AlignCenter]

  it "claims focus automatically, like any ordinary control" $ do
    ctx <- start []
    contextFocus ctx `shouldBe` Just Ok

  it "fires onClick when clicked" $ do
    let attrs = [onClicked (const [OutMsg ("clicked" :: String)])]
    ctx0 <- start attrs
    ctx1 <- step attrs (down onButton) ctx0
    ctx2 <- step attrs (releasedAt onButton) ctx1
    getMessages ctx2 `shouldBe` ["clicked"]

  it "fires onClick when Enter is pressed while focused" $ do
    let attrs = [onClicked (const [OutMsg ("clicked" :: String)])]
    ctx0 <- start attrs
    contextFocus ctx0 `shouldBe` Just Ok
    ctx1 <- step attrs (noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) ctx0
    getMessages ctx1 `shouldBe` ["clicked"]

  it "does not activate via Enter once disabled, even while still holding focus from before" $ do
    let attrs = [onClicked (const [OutMsg ("clicked" :: String)])]
    ctx0 <- start attrs
    contextFocus ctx0 `shouldBe` Just Ok
    ctx1 <- snd <$> runUI (disableWhen True (button Ok attrs))
                          (nextFrameContext testBounds (noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) testTheme (contextAnimation ctx0) ctx0)
    getMessages ctx1 `shouldBe` []

  it "always takes focus on itself when clicked, even if focusOnClick is set directly via Blink.Attributes" $ do
    let attrs = [focusOnClick (FocusTarget Other)]
    ctx0 <- start attrs
    ctx1 <- step attrs (down onButton) ctx0
    ctx2 <- step attrs (releasedAt onButton) ctx1
    ctx3 <- step attrs noInput ctx2
    contextFocus ctx3 `shouldBe` Just Ok

  describe "toggleButton" $ do
    it "draws in its normal style while not selected" $ do
      ctx <- startToggle []
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" testColour AlignCenter]

    it "draws in its pressed style while selected, even without being physically pressed" $ do
      ctx <- startToggle [isSelected True]
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" pressedColour AlignCenter]

    it "fires onSelectedChanged with the flipped value when clicked" $ do
      let attrs = [isSelected False, onSelectedChanged (\b -> [OutMsg (show b)])]
      ctx0 <- startToggle attrs
      ctx1 <- stepToggle attrs (down onButton) ctx0
      ctx2 <- stepToggle attrs (releasedAt onButton) ctx1
      getMessages ctx2 `shouldBe` [show True]

    it "fires onSelectedChanged with False when clicked while selected" $ do
      let attrs = [isSelected True, onSelectedChanged (\b -> [OutMsg (show b)])]
      ctx0 <- startToggle attrs
      ctx1 <- stepToggle attrs (down onButton) ctx0
      ctx2 <- stepToggle attrs (releasedAt onButton) ctx1
      getMessages ctx2 `shouldBe` [show False]

    it "fires onSelectedChanged when activated via Enter while focused" $ do
      let attrs = [isSelected False, onSelectedChanged (\b -> [OutMsg (show b)])]
      ctx0 <- startToggle attrs
      ctx1 <- stepToggle attrs (noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) ctx0
      getMessages ctx1 `shouldBe` [show True]

type ToggleAttr' = Attr TestElement ToggleEvent String (ToggleButtonConfig TestElement)

startToggle :: [ToggleAttr'] -> IO (UIContext TestElement String)
startToggle attrs = snd <$> runUI (toggleButton Ok attrs) (emptyUIContext testBounds noInput toggleTestTheme noOpTextMeasurer)

stepToggle :: [ToggleAttr'] -> InputState -> UIContext TestElement String -> IO (UIContext TestElement String)
stepToggle attrs input ctx = snd <$> runUI (toggleButton Ok attrs) (nextFrameContext testBounds input toggleTestTheme (contextAnimation ctx) ctx)
