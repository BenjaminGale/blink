{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.TextInputSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Char (isDigit)
import Test.Hspec

import Blink.Controls.Element (Attribute)
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..), Key (..), Modifier (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.Controls.TextInput
  (TextInputConfig, displayFilter, inputFilter, onInput, onSubmit, value, textInput)
import Blink.UI
import Blink.UI.Element (elLayout, runElement)

data TestElement = Field | Other | Second deriving (Eq, Ord, Show)

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

-- | The behaviour \/ rendering contracts below are about interaction, not
-- sizing -- they're written against a field that fills its given bounds
-- entirely, as every control did before controls reported their own
-- 'Layout'. 'textInput' now defaults to sizing its height to one line of
-- text, so every call site here asks for the old full-size behaviour
-- explicitly, the same way any other caller would.
fullSizeTextInput :: TestElement -> [Attribute'] -> UI TestElement String ()
fullSizeTextInput eid attrs = runElement (textInput eid attrs) { elLayout = Layout fill fill TopLeft }

-- | 'Field' run alongside a second, already-focused element, so 'Field'
-- itself never gains focus.
unfocused :: [Attribute'] -> UI TestElement String ()
unfocused attrs = setFocus Other >> fullSizeTextInput Field attrs

spec :: Spec
spec = describe "Blink.Controls.TextInput" $ do
  controlBehaviourSpec defaultControlBehaviourConfig testBounds seedCtx Field (Point 5 5) hitRect (Point 200 200) (fullSizeTextInput Field)

  describe "rendering" $ do
    it "displays the value without a cursor when unfocused" $ do
      result <- runInteractions testBounds seedCtx (unfocused [value "hello"]) [] []
      resultDraws result `shouldContain` [DrawText contentRect "hello" testColour AlignLeft]
      resultDraws result `shouldNotContain` [cursorRectAt 15]

    it "displays the value with a cursor when focused" $ do
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field [value "hello"]) [] [ClickAt focusPt]
      resultDraws result `shouldContain` [DrawText contentRect "hello" testColour AlignLeft]
      resultDraws result `shouldContain` [cursorRectAt 15]

  describe "text editing" $ do
    -- The field selects its whole value on this first, implicit auto-claimed
    -- focus (see "focus and selection" below), so these collapse that
    -- selection to the end with a plain Right first -- the same as a real
    -- user would before typing or backspacing -- to exercise plain
    -- (non-selection) editing.
    it "appends typed characters to the value and fires onInput" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [PressKey KeyRight [], TypeText "!"]
      resultMessages result `shouldBe` ["hello!"]

    it "removes the last character on backspace" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [PressKey KeyRight [], PressKey KeyBackspace []]
      resultMessages result `shouldBe` ["hell"]

    it "does not fire onInput when there is no input" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] []
      resultMessages result `shouldBe` []

    it "does not process input when a different element is focused" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (unfocused attrs) [] [PressKey KeyBackspace []]
      resultMessages result `shouldBe` []

  describe "disabled" $
    it "does not process input when disabled" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      focused <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [ClickAt focusPt]
      result  <- runInteractions testBounds (resultContext focused) (disableWhen True (fullSizeTextInput Field attrs)) [] [TypeText "!"]
      resultMessages result `shouldBe` []

  describe "cursor placement" $ do
    -- Uses the no-op measurer (every offset maps to 0), so any click lands
    -- on position 0 -- these tests only care that a click sets a cursor at
    -- all, not exactly where; see the digit-width tests elsewhere for real
    -- offset math.
    it "sets the cursor to the clicked position on mouse press" $ do
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field [value "hello"]) [] [ClickAt focusPt]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 0 0)

    it "extends the active end on drag while keeping the anchor" $ do
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field [value "hello"]) []
                  [MouseDown (Point 15 50), DragTo (Point 55 50)]
      case contextSelection Field (resultContext result) of
        Just (Selection a _) -> a `shouldBe` 0
        other                -> expectationFailure ("expected Just (Selection 0 _), got: " <> show other)

  describe "focus and selection" $ do
    it "selects the entire value when it first claims focus with nothing else focused" $ do
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field [value "hello"]) [] []
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 0 5)

    it "selects the entire value when Tab moves focus onto it from another element" $ do
      let action = fullSizeTextInput Second [value "world"] >> fullSizeTextInput Field [value "hello"]
      result <- runInteractions testBounds seedCtx action [Wait 1] [Tab]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 0 5)

    it "clears a stale selection and places the cursor at the click position when a click returns focus to it" $ do
      -- Focus 'Field' and give it a real, non-empty selection away from
      -- position 0, then let 'Other' take focus without 'Field' ever
      -- rendering again -- clicking back into 'Field' should discard that
      -- stale selection and place the cursor at the new click, not extend
      -- from the old anchor.
      focused <- runInteractions testBounds (seedWith fixedCharWidth) (fullSizeTextInput Field [value "hello"]) []
                   [ClickAt (Point 88 50), PressKey KeyLeft [Shift]]
      contextSelection Field (resultContext focused) `shouldBe` Just (Selection 4 3)
      away    <- runInteractions testBounds (resultContext focused) (setFocus Other) [] []
      result  <- runInteractions testBounds (resultContext away) (fullSizeTextInput Field [value "hello"]) [] [ClickAt (Point 15 50)]
      case contextSelection Field (resultContext result) of
        Just (Selection a act) -> do
          a `shouldBe` act    -- a fresh cursor, not a range
          a `shouldNotBe` 4   -- moved by the click, not left at the old anchor
        other -> expectationFailure ("expected a single cursor selection, got: " <> show other)

  describe "arrow navigation" $ do
    -- Renders the field alongside the seeding 'emitUi' so 'Field' reconfirms
    -- its focus this frame -- unrendered, its focus would expire (see
    -- 'controlBase'), and a freshly claimed focus selects the whole value.
    let seeded a v = do
          focused <- runInteractions testBounds seedCtx (fullSizeTextInput Field [value "hello"]) [] [ClickAt focusPt]
          settled <- runInteractions testBounds (resultContext focused)
                       (fullSizeTextInput Field [value "hello"] >> emitUi (SetSelectionAt Field (Selection a v))) [] []
          pure (resultContext settled)

    it "moves the cursor left with Left" $ do
      base   <- seeded 3 3
      result <- runInteractions testBounds base (fullSizeTextInput Field [value "hello"]) [] [PressKey KeyLeft []]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 2 2)

    it "moves the cursor right with Right" $ do
      base   <- seeded 2 2
      result <- runInteractions testBounds base (fullSizeTextInput Field [value "hello"]) [] [PressKey KeyRight []]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 3 3)

    it "collapses an existing selection to its low end on plain Left" $ do
      base   <- seeded 1 3
      result <- runInteractions testBounds base (fullSizeTextInput Field [value "hello"]) [] [PressKey KeyLeft []]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 1 1)

    it "collapses an existing selection to its high end on plain Right" $ do
      base   <- seeded 1 3
      result <- runInteractions testBounds base (fullSizeTextInput Field [value "hello"]) [] [PressKey KeyRight []]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 3 3)

    it "extends the selection left with Shift+Left" $ do
      base   <- seeded 3 3
      result <- runInteractions testBounds base (fullSizeTextInput Field [value "hello"]) [] [PressKey KeyLeft [Shift]]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 3 2)

    it "extends the selection right with Shift+Right" $ do
      base   <- seeded 3 3
      result <- runInteractions testBounds base (fullSizeTextInput Field [value "hello"]) [] [PressKey KeyRight [Shift]]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 3 4)

    it "does not move the cursor past the beginning" $ do
      base   <- seeded 0 0
      result <- runInteractions testBounds base (fullSizeTextInput Field [value "hello"]) [] [PressKey KeyLeft []]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 0 0)

    it "does not move the cursor past the end" $ do
      base   <- seeded 5 5
      result <- runInteractions testBounds base (fullSizeTextInput Field [value "hello"]) [] [PressKey KeyRight []]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 5 5)

  describe "selection editing" $ do
    -- See the "arrow navigation" 'seeded' above for why the field itself is
    -- rendered alongside the seeding 'emitUi'.
    let seeded a v attrs = do
          focused <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [ClickAt focusPt]
          settled <- runInteractions testBounds (resultContext focused)
                       (fullSizeTextInput Field attrs >> emitUi (SetSelectionAt Field (Selection a v))) [] []
          pure (resultContext settled)

    it "deletes the selected range on backspace" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      base   <- seeded 1 3 attrs
      result <- runInteractions testBounds base (fullSizeTextInput Field attrs) [] [PressKey KeyBackspace []]
      resultMessages result `shouldBe` ["hlo"]

    it "replaces the selected range with typed text" $ do
      let attrs = [value "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      base   <- seeded 1 3 attrs
      result <- runInteractions testBounds base (fullSizeTextInput Field attrs) [] [TypeText "X"]
      resultMessages result `shouldBe` ["hXlo"]

    it "collapses the cursor to the insertion point after replacing a selection" $ do
      let attrs = [value "hello"]
      base   <- seeded 1 3 attrs
      result <- runInteractions testBounds base (fullSizeTextInput Field attrs) [] [TypeText "XY"]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 3 3)

  describe "inputFilter" $ do
    it "inserts only the characters the filter accepts" $ do
      let attrs = [value "12", inputFilter (T.filter isDigit), onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [PressKey KeyRight [], TypeText "a3b"]
      resultMessages result `shouldBe` ["123"]

    it "does not fire onInput when every typed character is rejected" $ do
      let attrs = [value "12", inputFilter (T.filter isDigit), onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [PressKey KeyRight [], TypeText "!"]
      resultMessages result `shouldBe` []

    it "still allows backspace regardless of the filter" $ do
      let attrs = [value "12", inputFilter (T.filter isDigit), onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [PressKey KeyRight [], PressKey KeyBackspace []]
      resultMessages result `shouldBe` ["1"]

  describe "displayFilter" $ do
    it "displays a filtered value instead of the real one" $ do
      let attrs = [value "hunter2", displayFilter (T.map (const '*'))]
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [ClickAt focusPt]
      resultDraws result `shouldContain` [DrawText contentRect "*******" testColour AlignLeft]
      resultDraws result `shouldNotContain` [DrawText contentRect "hunter2" testColour AlignLeft]

    it "still edits and reports the real (unfiltered) value" $ do
      let attrs = [value "hunter2", displayFilter (T.map (const '*')), onInput (\t -> [OutMsg (T.unpack t)])]
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [PressKey KeyRight [], TypeText "!"]
      resultMessages result `shouldBe` ["hunter2!"]

    it "places the cursor using offsets measured against the masked text, not the real value" $ do
      let attrs = [value "hunter2", displayFilter (T.map (const '*'))]
      result <- runInteractions testBounds (seedWith fixedCharWidth) (fullSizeTextInput Field attrs) [] [ClickAt (Point 35 50)]
      contextSelection Field (resultContext result) `shouldBe` Just (Selection 1 1)

  describe "onSubmit" $ do
    it "fires when Enter is pressed while focused" $ do
      let attrs = [value "hello", onSubmit (const [OutMsg ("submitted" :: String)])]
      result <- runInteractions testBounds seedCtx (fullSizeTextInput Field attrs) [] [PressKey KeyReturn []]
      resultMessages result `shouldBe` ["submitted"]

    it "does not fire when a different element is focused" $ do
      let attrs = [value "hello", onSubmit (const [OutMsg ("submitted" :: String)])]
      result <- runInteractions testBounds seedCtx (unfocused attrs) [] [PressKey KeyReturn []]
      resultMessages result `shouldBe` []
