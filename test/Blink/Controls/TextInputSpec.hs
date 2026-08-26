{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.TextInputSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Char (isDigit)
import Test.Hspec

import Blink.Controls.Element (Attribute)
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Geometry (Point (..), Rectangle (..), Size (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..), Key (..), Modifier (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.Controls.TextInput
  (TextInputConfig, displayFilter, inputFilter, onInput, onSubmit, value, textInput)
import Blink.UI

data TestElement = Field | Other deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignLeft
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

-- | Every character measures 0 wide, so any click lands on position 0 --
-- fine for tests that don't care about exact click-to-offset placement.
noOpMeasurer :: TextMeasurer
noOpMeasurer = noOpTextMeasurer

-- | Every character is a fixed 20px wide, for tests that need real
-- character-offset math (click placement, scrolling).
fixedCharWidth :: TextMeasurer
fixedCharWidth = TextMeasurer
  { tmCharOffset   = \_ n -> pure (fromIntegral n * 20)
  , tmCharAtOffset = \_ x -> pure (round (x / 20))
  , tmTextSize     = \t -> pure (Size (fromIntegral (T.length t) * 20) 20)
  }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

focusPt :: Point
focusPt = Point 50 50

-- | Content rect for 'testBounds': inset by margin (10) then padding (5).
contentRect :: Rectangle
contentRect = Rectangle 15 15 70 70

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin the test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

cursorRectAt :: Double -> DrawCommand
cursorRectAt x = FillRect (Rectangle x 15 1 70) testColour

type Attribute' = Attribute (TextInputConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpMeasurer

-- | An un-rendered starting context using the given measurer -- needed for
-- exact character-offset assertions, since a context that has already had
-- 'textInput' run against it once would have auto-claimed focus and
-- auto-scrolled to keep its default end-of-text cursor visible before the
-- click ever happens, which is a real (and separately tested) behaviour but
-- would shift the pixel math these tests are checking.
seedWith :: TextMeasurer -> UIContext TestElement String
seedWith = emptyUIContext testBounds noInput testTheme

-- | 'Field' run alongside a second, already-focused element, so 'Field'
-- itself never gains focus.
unfocused :: [Attribute'] -> UI TestElement String ()
unfocused attrs = setFocus Other >> textInput Field attrs

spec :: Spec
spec = describe "Blink.Controls.TextInput" $ do
  controlBehaviourSpec defaultControlBehaviourConfig testBounds seedCtx Field (Point 5 5) hitRect (Point 200 200) (textInput Field)

  describe "rendering" $ do
    it "displays the value without a cursor when unfocused" $ do
      result <- runInteractions testBounds seedCtx (unfocused [value "hello"]) [] []
      resultDraws result `shouldContain` [DrawText contentRect "hello" testColour AlignLeft]
      resultDraws result `shouldNotContain` [cursorRectAt 15]

    it "displays the value with a cursor when focused" $ do
      result <- runInteractions testBounds seedCtx (textInput Field [value "hello"]) [] [ClickAt focusPt]
      resultDraws result `shouldContain` [DrawText contentRect "hello" testColour AlignLeft]
      resultDraws result `shouldContain` [cursorRectAt 15]

  describe "text editing" $ do
    it "appends typed characters to the value and fires onInput" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (textInput Field attrs) [] [TypeText "!"]
      resultMessages result `shouldBe` ["hello!"]

    it "removes the last character on backspace" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (textInput Field attrs) [] [PressKey KeyBackspace []]
      resultMessages result `shouldBe` ["hell"]

    it "does not fire onInput when there is no input" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (textInput Field attrs) [] []
      resultMessages result `shouldBe` []

    it "does not process input when a different element is focused" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (unfocused attrs) [] [PressKey KeyBackspace []]
      resultMessages result `shouldBe` []

  describe "disabled" $
    it "does not process input when disabled" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      focused <- runInteractions testBounds seedCtx (textInput Field attrs) [] [ClickAt focusPt]
      result  <- runInteractions testBounds (resultContext focused) (disableWhen True (textInput Field attrs)) [] [TypeText "!"]
      resultMessages result `shouldBe` []

  describe "cursor placement" $ do
    -- Uses the no-op measurer (every offset maps to 0), so any click lands
    -- on position 0 -- these tests only care that a click sets a cursor at
    -- all, not exactly where; see the digit-width tests elsewhere for real
    -- offset math.
    it "sets the cursor to the clicked position on mouse press" $ do
      result <- runInteractions testBounds seedCtx (textInput Field [value "hello"]) [] [ClickAt focusPt]
      contextSelections Field (resultContext result) `shouldBe` [Selection 0 0]

    it "extends the active end on drag while keeping the anchor" $ do
      result <- runInteractions testBounds seedCtx (textInput Field [value "hello"]) []
                  [MouseDown (Point 15 50), DragTo (Point 55 50)]
      case contextSelections Field (resultContext result) of
        [Selection a _] -> a `shouldBe` 0
        other           -> expectationFailure ("expected [Selection 0 _], got: " <> show other)

  describe "arrow navigation" $ do
    let seeded a v = do
          focused <- runInteractions testBounds seedCtx (textInput Field [value "hello"]) [] [ClickAt focusPt]
          settled <- runInteractions testBounds (resultContext focused) (emitUi (SetSelectionAt Field (Selection a v))) [] []
          pure (resultContext settled)

    it "moves the cursor left with Left" $ do
      base   <- seeded 3 3
      result <- runInteractions testBounds base (textInput Field [value "hello"]) [] [PressKey KeyLeft []]
      contextSelections Field (resultContext result) `shouldBe` [Selection 2 2]

    it "moves the cursor right with Right" $ do
      base   <- seeded 2 2
      result <- runInteractions testBounds base (textInput Field [value "hello"]) [] [PressKey KeyRight []]
      contextSelections Field (resultContext result) `shouldBe` [Selection 3 3]

    it "collapses an existing selection to its low end on plain Left" $ do
      base   <- seeded 1 3
      result <- runInteractions testBounds base (textInput Field [value "hello"]) [] [PressKey KeyLeft []]
      contextSelections Field (resultContext result) `shouldBe` [Selection 1 1]

    it "collapses an existing selection to its high end on plain Right" $ do
      base   <- seeded 1 3
      result <- runInteractions testBounds base (textInput Field [value "hello"]) [] [PressKey KeyRight []]
      contextSelections Field (resultContext result) `shouldBe` [Selection 3 3]

    it "extends the selection left with Shift+Left" $ do
      base   <- seeded 3 3
      result <- runInteractions testBounds base (textInput Field [value "hello"]) [] [PressKey KeyLeft [Shift]]
      contextSelections Field (resultContext result) `shouldBe` [Selection 3 2]

    it "extends the selection right with Shift+Right" $ do
      base   <- seeded 3 3
      result <- runInteractions testBounds base (textInput Field [value "hello"]) [] [PressKey KeyRight [Shift]]
      contextSelections Field (resultContext result) `shouldBe` [Selection 3 4]

    it "does not move the cursor past the beginning" $ do
      base   <- seeded 0 0
      result <- runInteractions testBounds base (textInput Field [value "hello"]) [] [PressKey KeyLeft []]
      contextSelections Field (resultContext result) `shouldBe` [Selection 0 0]

    it "does not move the cursor past the end" $ do
      base   <- seeded 5 5
      result <- runInteractions testBounds base (textInput Field [value "hello"]) [] [PressKey KeyRight []]
      contextSelections Field (resultContext result) `shouldBe` [Selection 5 5]

  describe "selection editing" $ do
    let seeded a v attrs = do
          focused <- runInteractions testBounds seedCtx (textInput Field attrs) [] [ClickAt focusPt]
          settled <- runInteractions testBounds (resultContext focused) (emitUi (SetSelectionAt Field (Selection a v))) [] []
          pure (resultContext settled)

    it "deletes the selected range on backspace" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      base   <- seeded 1 3 attrs
      result <- runInteractions testBounds base (textInput Field attrs) [] [PressKey KeyBackspace []]
      resultMessages result `shouldBe` ["hlo"]

    it "replaces the selected range with typed text" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      base   <- seeded 1 3 attrs
      result <- runInteractions testBounds base (textInput Field attrs) [] [TypeText "X"]
      resultMessages result `shouldBe` ["hXlo"]

    it "collapses the cursor to the insertion point after replacing a selection" $ do
      let attrs = [value "hello"]
      base   <- seeded 1 3 attrs
      result <- runInteractions testBounds base (textInput Field attrs) [] [TypeText "XY"]
      contextSelections Field (resultContext result) `shouldBe` [Selection 3 3]

  describe "inputFilter" $ do
    it "inserts only the characters the filter accepts" $ do
      let attrs = [value "12", inputFilter (T.filter isDigit), onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (textInput Field attrs) [] [TypeText "a3b"]
      resultMessages result `shouldBe` ["123"]

    it "does not fire onInput when every typed character is rejected" $ do
      let attrs = [value "12", inputFilter (T.filter isDigit), onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (textInput Field attrs) [] [TypeText "!"]
      resultMessages result `shouldBe` []

    it "still allows backspace regardless of the filter" $ do
      let attrs = [value "12", inputFilter (T.filter isDigit), onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (textInput Field attrs) [] [PressKey KeyBackspace []]
      resultMessages result `shouldBe` ["1"]

  describe "displayFilter" $ do
    it "displays a filtered value instead of the real one" $ do
      let attrs = [value "hunter2", displayFilter (T.map (const '*'))]
      result <- runInteractions testBounds seedCtx (textInput Field attrs) [] [ClickAt focusPt]
      resultDraws result `shouldContain` [DrawText contentRect "*******" testColour AlignLeft]
      resultDraws result `shouldNotContain` [DrawText contentRect "hunter2" testColour AlignLeft]

    it "still edits and reports the real (unfiltered) value" $ do
      let attrs = [value "hunter2", displayFilter (T.map (const '*')), onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (textInput Field attrs) [] [TypeText "!"]
      resultMessages result `shouldBe` ["hunter2!"]

    it "places the cursor using offsets measured against the masked text, not the real value" $ do
      let attrs = [value "hunter2", displayFilter (T.map (const '*'))]
      result <- runInteractions testBounds (seedWith fixedCharWidth) (textInput Field attrs) [] [ClickAt (Point 35 50)]
      contextSelections Field (resultContext result) `shouldBe` [Selection 1 1]

  describe "onSubmit" $ do
    it "fires when Enter is pressed while focused" $ do
      let attrs = [value "hello", onSubmit (const [OutMsg ("submitted" :: String)])]
      result <- runInteractions testBounds seedCtx (textInput Field attrs) [] [PressKey KeyReturn []]
      resultMessages result `shouldBe` ["submitted"]

    it "does not fire when a different element is focused" $ do
      let attrs = [value "hello", onSubmit (const [OutMsg ("submitted" :: String)])]
      result <- runInteractions testBounds seedCtx (unfocused attrs) [] [PressKey KeyReturn []]
      resultMessages result `shouldBe` []
