{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Blink.ControlSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Attributes
  ( Attr, HasControlConfig (..), ControlConfig (..), FocusOnClick (..), defaultControlConfig
  )
import Blink.Control (control)
import Blink.Element (ElementEvent, onClicked, onFocusGained, onFocusLost, onMouseEntered)
import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform, insetRect)
import Blink.Input (InputState (..), Key (..), KeyEvent (..), Modifier (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = ElemA | ElemB deriving (Eq, Ord, Show)

-- | The shared config type every test control uses -- just a
-- 'ControlConfig' wrapper, since 'control' needs a resolved @cfg@ to read
-- 'ccTabStop'\/'ccFocusOnClick' off.
newtype TestConfig e = TestConfig { testControlConfig :: ControlConfig e }

defaultTestConfig :: TestConfig e
defaultTestConfig = TestConfig defaultControlConfig

instance HasControlConfig e (TestConfig e) where
  controlConfig    = testControlConfig
  setControlConfig cc c = c { testControlConfig = cc }

rectA, rectB :: Rectangle
rectA = Rectangle 0 0 50 100
rectB = Rectangle 50 0 50 100

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

down :: Point -> InputState
down p = noInput { inputMousePosition = p, inputLeftButtonDown = True }

releasedAt :: Point -> InputState
releasedAt p = noInput { inputMousePosition = p, inputLeftButtonDown = False }

hoverAt :: Point -> InputState
hoverAt p = noInput { inputMousePosition = p }

onA, onB :: Point
onA = Point 10 50
onB = Point 60 50

advance :: Ord e => InputState -> UIContext e msg -> UIContext e msg
advance input ctx = nextFrameContext testBounds input (contextTheme ctx) (contextAnimation ctx) ctx

type Attr' = Attr TestElement ElementEvent String (TestConfig TestElement)

-- | Renders 'ElemA' at 'rectA' and 'ElemB' at 'rectB' with the given
-- per-element config and attrs, for one frame against @ctx@.
runBoth
  :: TestConfig TestElement -> [Attr'] -> TestConfig TestElement -> [Attr']
  -> UIContext TestElement String -> IO (UIContext TestElement String)
runBoth cfgA attrsA cfgB attrsB ctx = snd <$> runUI render ctx
  where
    render = do
      withBounds rectA (control ElemA cfgA attrsA (pure ()))
      withBounds rectB (control ElemB cfgB attrsB (pure ()))

startBoth :: TestConfig TestElement -> [Attr'] -> TestConfig TestElement -> [Attr'] -> IO (UIContext TestElement String)
startBoth cfgA attrsA cfgB attrsB =
  runBoth cfgA attrsA cfgB attrsB (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)

spec :: Spec
spec = describe "Blink.Control" $ do
  describe "auto-claim" $ do
    it "claims focus when nothing is focused" $ do
      ctx <- startBoth defaultTestConfig [] defaultTestConfig []
      contextFocus ctx `shouldBe` Just ElemA

    it "only the first of several simultaneously-eligible controls claims it" $ do
      ctx <- startBoth defaultTestConfig [] defaultTestConfig []
      contextFocus ctx `shouldNotBe` Just ElemB

    it "a control with tabStop off does not auto-claim" $ do
      let cfgA = TestConfig defaultControlConfig { ccTabStop = False }
      ctx <- startBoth cfgA [] defaultTestConfig []
      contextFocus ctx `shouldBe` Just ElemB

    it "a disabled control does not auto-claim" $ do
      ctx <- snd <$> runUI (disableWhen True (control ElemA defaultTestConfig [] (pure ())))
                           (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      contextFocus ctx `shouldBe` Nothing

  describe "self-retain" $
    it "keeps holding focus across frames once claimed" $ do
      ctx0 <- startBoth defaultTestConfig [] defaultTestConfig []
      ctx1 <- runBoth defaultTestConfig [] defaultTestConfig [] (advance noInput ctx0)
      ctx2 <- runBoth defaultTestConfig [] defaultTestConfig [] (advance noInput ctx1)
      contextFocus ctx2 `shouldBe` Just ElemA

  describe "click-to-focus" $ do
    it "takes effect one frame after the click, firing FocusLost/FocusGained for the right elements" $ do
      let attrsA = [onFocusLost   (const [OutMsg ("A lost"   :: String)])]
          attrsB = [onFocusGained (const [OutMsg ("B gained" :: String)])]
      ctx0 <- startBoth defaultTestConfig attrsA defaultTestConfig attrsB
      contextFocus ctx0 `shouldBe` Just ElemA
      ctx1 <- runBoth defaultTestConfig attrsA defaultTestConfig attrsB (advance (down onB) ctx0)
      ctx2 <- runBoth defaultTestConfig attrsA defaultTestConfig attrsB (advance (releasedAt onB) ctx1)
      contextFocus ctx2 `shouldBe` Just ElemA -- not yet -- deferred
      ctx3 <- runBoth defaultTestConfig attrsA defaultTestConfig attrsB (advance noInput ctx2)
      contextFocus ctx3 `shouldBe` Just ElemB
      getMessages ctx3 `shouldBe` ["A lost", "B gained"]

    it "FocusTarget redirects focus to the named element instead of the clicker" $ do
      let cfgA = TestConfig defaultControlConfig { ccTabStop = False, ccFocusOnClick = FocusTarget ElemB }
          cfgB = TestConfig defaultControlConfig { ccTabStop = False }
      ctx0 <- startBoth cfgA [] cfgB []
      contextFocus ctx0 `shouldBe` Nothing -- neither eligible to auto-claim
      ctx1 <- runBoth cfgA [] cfgB [] (advance (down onA) ctx0)
      ctx2 <- runBoth cfgA [] cfgB [] (advance (releasedAt onA) ctx1)
      ctx3 <- runBoth cfgA [] cfgB [] (advance noInput ctx2)
      contextFocus ctx3 `shouldBe` Just ElemB

    it "NoFocus leaves focus unchanged when clicked" $ do
      let cfgA = TestConfig defaultControlConfig { ccTabStop = False, ccFocusOnClick = NoFocus }
      ctx0 <- runBoth cfgA [] cfgA [] (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx1 <- runBoth cfgA [] cfgA [] (advance (down onA) ctx0)
      ctx2 <- runBoth cfgA [] cfgA [] (advance (releasedAt onA) ctx1)
      ctx3 <- runBoth cfgA [] cfgA [] (advance noInput ctx2)
      contextFocus ctx3 `shouldBe` Nothing

  describe "keyboard navigation" $ do
    it "Tab gives up focus immediately, letting the next control auto-claim in the same frame" $ do
      ctx0 <- startBoth defaultTestConfig [] defaultTestConfig []
      contextFocus ctx0 `shouldBe` Just ElemA
      ctx1 <- runBoth defaultTestConfig [] defaultTestConfig []
                     (advance (noInput { inputKeyEvents = [KeyEvent KeyTab []] }) ctx0)
      contextFocus ctx1 `shouldBe` Just ElemB

    it "Shift-Tab hands focus to the previous tab stop one frame later" $ do
      ctx0 <- startBoth defaultTestConfig [] defaultTestConfig []
      contextFocus ctx0 `shouldBe` Just ElemA
      ctx1 <- runBoth defaultTestConfig [] defaultTestConfig []
                     (advance (noInput { inputKeyEvents = [KeyEvent KeyTab [Shift]] }) ctx0)
      contextFocus ctx1 `shouldBe` Just ElemA -- not yet -- deferred
      ctx2 <- runBoth defaultTestConfig [] defaultTestConfig [] (advance noInput ctx1)
      contextFocus ctx2 `shouldBe` Just ElemB

  describe "chrome" $
    it "draws background via styledElement, inset by margin" $ do
      ctx <- snd <$> runUI (control ElemA defaultTestConfig [] (pure ()))
                           (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      getDrawCommands ctx `shouldContain` [FillRect (insetRect (uniform 10) testBounds) testColour]

  describe "hit region" $ do
    -- rectA is (0,0)-(50,100); margin 10 insets it to (10,10)-(40,90). These
    -- confirm element's own raw events -- not just styling -- respect that
    -- inset, since 'control' narrows the bounds element sees before calling
    -- it, rather than each independently recomputing "am I hit".
    it "does not fire element events for a press/release inside the margin strip" $ do
      let attrs = [onClicked (const [OutMsg ("clicked" :: String)])]
          marginPoint = Point 5 5
      ctx0 <- startBoth defaultTestConfig attrs defaultTestConfig []
      ctx1 <- runBoth defaultTestConfig attrs defaultTestConfig [] (advance (down marginPoint) ctx0)
      ctx2 <- runBoth defaultTestConfig attrs defaultTestConfig [] (advance (releasedAt marginPoint) ctx1)
      getMessages ctx2 `shouldBe` []

    it "fires element events for a press/release inside the margin-inset hit area" $ do
      let attrs = [onClicked (const [OutMsg ("clicked" :: String)])]
      ctx0 <- startBoth defaultTestConfig attrs defaultTestConfig []
      ctx1 <- runBoth defaultTestConfig attrs defaultTestConfig [] (advance (down onA) ctx0)
      ctx2 <- runBoth defaultTestConfig attrs defaultTestConfig [] (advance (releasedAt onA) ctx1)
      getMessages ctx2 `shouldBe` ["clicked"]

    it "does not fire MouseEntered while merely hovering the margin strip" $ do
      let attrs = [onMouseEntered (const [OutMsg ("entered" :: String)])]
          marginPoint = Point 5 5
      ctx0 <- startBoth defaultTestConfig attrs defaultTestConfig []
      ctx1 <- runBoth defaultTestConfig attrs defaultTestConfig [] (advance (hoverAt marginPoint) ctx0)
      getMessages ctx1 `shouldBe` []

    it "fires MouseEntered when hovering inside the margin-inset hit area" $ do
      let attrs = [onMouseEntered (const [OutMsg ("entered" :: String)])]
      ctx0 <- startBoth defaultTestConfig attrs defaultTestConfig []
      ctx1 <- runBoth defaultTestConfig attrs defaultTestConfig [] (advance (hoverAt onA) ctx0)
      getMessages ctx1 `shouldBe` ["entered"]
