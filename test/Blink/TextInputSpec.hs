{-# LANGUAGE OverloadedStrings #-}
module Blink.TextInputSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Char (isDigit)
import Test.Hspec

import Blink.Attributes (Attr, text)
import Blink.ControlBehaviour (controlBehaviourSpec)
import Blink.Geometry (Point (..), Rectangle (..), Size (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..), Key (..), KeyEvent (..), Modifier (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.TextInput
  (TextInputConfig, TextInputEvent, displayFilter, inputFilter, onInput, onSubmit, textInput)
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

down, releasedAt, heldAt :: Point -> InputState
down p       = noInput { inputMousePosition = p, inputLeftButtonDown = True }
releasedAt p = noInput { inputMousePosition = p, inputLeftButtonDown = False }
heldAt p     = noInput { inputMousePosition = p, inputLeftButtonDown = True }

typed :: T.Text -> InputState
typed t = noInput { inputTypedText = [t] }

pressed :: Key -> [Modifier] -> InputState
pressed k mods = noInput { inputKeyEvents = [KeyEvent k mods] }

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

type Attr' = Attr TestElement TextInputEvent String (TextInputConfig TestElement)

startWith :: TextMeasurer -> [Attr'] -> IO (UIContext TestElement String)
startWith measurer attrs = snd <$> runUI (textInput Field attrs) (emptyUIContext testBounds noInput testTheme measurer)

-- | An un-rendered starting context using the given measurer -- unlike
-- 'startWith', 'textInput' hasn't run against it yet, so the first real
-- frame (e.g. a click) is also its very first render. Needed for exact
-- character-offset assertions: 'startWith' would auto-claim focus and
-- auto-scroll to keep its default end-of-text cursor visible before the
-- click ever happens, which is a real (and separately tested) behaviour
-- but would shift the pixel math these tests are checking.
seedWith :: TextMeasurer -> UIContext TestElement String
seedWith = emptyUIContext testBounds noInput testTheme

-- | The text measurer is fixed for a whole run at 'emptyUIContext', not
-- re-supplied each frame -- 'nextFrameContext' carries it forward from
-- @ctx@ unchanged, so advancing a frame never needs it again.
step :: [Attr'] -> InputState -> UIContext TestElement String -> IO (UIContext TestElement String)
step attrs input ctx = snd <$> runUI (textInput Field attrs) (nextFrameContext testBounds input testTheme (contextAnimation ctx) ctx)

start :: [Attr'] -> IO (UIContext TestElement String)
start = startWith noOpMeasurer

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpMeasurer

-- | A selection\/scroll change 'textInput' makes this frame is queued as a
-- deferred effect, visible via 'contextSelections'\/'contextScrollPosition'
-- only from the frame after -- one more no-op frame settles it.
settle :: [Attr'] -> UIContext TestElement String -> IO (UIContext TestElement String)
settle attrs = step attrs noInput

-- | 'textInput' run alongside a second, already-focused element, so
-- 'Field' itself is never focused.
startUnfocused :: [Attr'] -> IO (UIContext TestElement String)
startUnfocused attrs = snd <$> runUI render (emptyUIContext testBounds noInput testTheme noOpMeasurer)
  where render = setFocus Other >> textInput Field attrs

stepUnfocused :: [Attr'] -> InputState -> UIContext TestElement String -> IO (UIContext TestElement String)
stepUnfocused attrs input ctx = snd <$> runUI (textInput Field attrs) (nextFrameContext testBounds input testTheme (contextAnimation ctx) ctx)

spec :: Spec
spec = describe "Blink.TextInput" $ do
  controlBehaviourSpec testBounds seedCtx Field (Point 5 5) hitRect (Point 200 200) (textInput Field)

  describe "rendering" $ do
    it "displays the value without a cursor when unfocused" $ do
      ctx <- startUnfocused [text "hello"]
      getDrawCommands ctx `shouldContain` [DrawText contentRect "hello" testColour AlignLeft]
      getDrawCommands ctx `shouldNotContain` [cursorRectAt 15]

    it "displays the value with a cursor when focused" $ do
      ctx0 <- start [text "hello"]
      ctx1 <- step [text "hello"] (down focusPt) ctx0
      ctx  <- step [text "hello"] (releasedAt focusPt) ctx1
      getDrawCommands ctx `shouldContain` [DrawText contentRect "hello" testColour AlignLeft]
      getDrawCommands ctx `shouldContain` [cursorRectAt 15]

  describe "text editing" $ do
    it "appends typed characters to the value and fires onInput" $ do
      let attrs = [text "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      ctx0 <- start attrs
      ctx  <- step attrs (typed "!") ctx0
      getMessages ctx `shouldBe` ["hello!"]

    it "removes the last character on backspace" $ do
      let attrs = [text "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      ctx0 <- start attrs
      ctx  <- step attrs (pressed KeyBackspace []) ctx0
      getMessages ctx `shouldBe` ["hell"]

    it "does not fire onInput when there is no input" $ do
      let attrs = [text "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      ctx0 <- start attrs
      ctx  <- step attrs noInput ctx0
      getMessages ctx `shouldBe` []

    it "does not process input when a different element is focused" $ do
      let attrs = [text "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      ctx0 <- startUnfocused attrs
      ctx  <- stepUnfocused attrs (pressed KeyBackspace []) ctx0
      getMessages ctx `shouldBe` []

  describe "disabled" $
    it "does not process input when disabled" $ do
      let attrs = [text "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      ctx0 <- start attrs
      ctx1 <- step attrs (down focusPt) ctx0
      ctx2 <- step attrs (releasedAt focusPt) ctx1
      ctx  <- snd <$> runUI (disableWhen True (textInput Field attrs)) (nextFrameContext testBounds (typed "!") testTheme (contextAnimation ctx2) ctx2)
      getMessages ctx `shouldBe` []

  describe "cursor placement" $ do
    -- Uses the no-op measurer (every offset maps to 0), so any click lands
    -- on position 0 -- these tests only care that a click sets a cursor at
    -- all, not exactly where; see the digit-width tests elsewhere for real
    -- offset math.
    it "sets the cursor to the clicked position on mouse press" $ do
      ctx0 <- start [text "hello"]
      ctx1 <- step [text "hello"] (down focusPt) ctx0
      ctx2 <- step [text "hello"] (releasedAt focusPt) ctx1
      ctx  <- settle [text "hello"] ctx2
      contextSelections Field ctx `shouldBe` [Selection 0 0]

    it "extends the active end on drag while keeping the anchor" $ do
      ctx0 <- start [text "hello"]
      ctx1 <- step [text "hello"] (down (Point 15 50)) ctx0
      ctx2 <- step [text "hello"] (heldAt (Point 55 50)) ctx1
      ctx  <- settle [text "hello"] ctx2
      case contextSelections Field ctx of
        [Selection a _] -> a `shouldBe` 0
        other           -> expectationFailure ("expected [Selection 0 _], got: " <> show other)

  describe "arrow navigation" $ do
    let seeded a v = do
          ctx0 <- start [text "hello"]
          ctx1 <- step [text "hello"] (down focusPt) ctx0
          ctx2 <- step [text "hello"] (releasedAt focusPt) ctx1
          ctx3 <- snd <$> runUI (emitUi (SetSelectionAt Field (Selection a v))) (nextFrameContext testBounds noInput testTheme (contextAnimation ctx2) ctx2)
          settle [text "hello"] ctx3

    it "moves the cursor left with Left" $ do
      base <- seeded 3 3
      ctx' <- step [text "hello"] (pressed KeyLeft []) base
      ctx  <- settle [text "hello"] ctx'
      contextSelections Field ctx `shouldBe` [Selection 2 2]

    it "moves the cursor right with Right" $ do
      base <- seeded 2 2
      ctx' <- step [text "hello"] (pressed KeyRight []) base
      ctx  <- settle [text "hello"] ctx'
      contextSelections Field ctx `shouldBe` [Selection 3 3]

    it "collapses an existing selection to its low end on plain Left" $ do
      base <- seeded 1 3
      ctx' <- step [text "hello"] (pressed KeyLeft []) base
      ctx  <- settle [text "hello"] ctx'
      contextSelections Field ctx `shouldBe` [Selection 1 1]

    it "collapses an existing selection to its high end on plain Right" $ do
      base <- seeded 1 3
      ctx' <- step [text "hello"] (pressed KeyRight []) base
      ctx  <- settle [text "hello"] ctx'
      contextSelections Field ctx `shouldBe` [Selection 3 3]

    it "extends the selection left with Shift+Left" $ do
      base <- seeded 3 3
      ctx' <- step [text "hello"] (pressed KeyLeft [Shift]) base
      ctx  <- settle [text "hello"] ctx'
      contextSelections Field ctx `shouldBe` [Selection 3 2]

    it "extends the selection right with Shift+Right" $ do
      base <- seeded 3 3
      ctx' <- step [text "hello"] (pressed KeyRight [Shift]) base
      ctx  <- settle [text "hello"] ctx'
      contextSelections Field ctx `shouldBe` [Selection 3 4]

    it "does not move the cursor past the beginning" $ do
      base <- seeded 0 0
      ctx' <- step [text "hello"] (pressed KeyLeft []) base
      ctx  <- settle [text "hello"] ctx'
      contextSelections Field ctx `shouldBe` [Selection 0 0]

    it "does not move the cursor past the end" $ do
      base <- seeded 5 5
      ctx' <- step [text "hello"] (pressed KeyRight []) base
      ctx  <- settle [text "hello"] ctx'
      contextSelections Field ctx `shouldBe` [Selection 5 5]

  describe "selection editing" $ do
    let seeded a v attrs = do
          ctx0 <- start attrs
          ctx1 <- step attrs (down focusPt) ctx0
          ctx2 <- step attrs (releasedAt focusPt) ctx1
          snd <$> runUI (emitUi (SetSelectionAt Field (Selection a v))) (nextFrameContext testBounds noInput testTheme (contextAnimation ctx2) ctx2)

    it "deletes the selected range on backspace" $ do
      let attrs = [text "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      base <- seeded 1 3 attrs
      ctx  <- step attrs (pressed KeyBackspace []) base
      getMessages ctx `shouldBe` ["hlo"]

    it "replaces the selected range with typed text" $ do
      let attrs = [text "hello", onInput (\t -> [OutMsg (T.unpack t)])]
      base <- seeded 1 3 attrs
      ctx  <- step attrs (typed "X") base
      getMessages ctx `shouldBe` ["hXlo"]

    it "collapses the cursor to the insertion point after replacing a selection" $ do
      let attrs = [text "hello"]
      base <- seeded 1 3 attrs
      ctx' <- step attrs (typed "XY") base
      ctx  <- settle attrs ctx'
      contextSelections Field ctx `shouldBe` [Selection 3 3]

  describe "inputFilter" $ do
    it "inserts only the characters the filter accepts" $ do
      let attrs = [text "12", inputFilter (T.filter isDigit), onInput (\t -> [OutMsg (T.unpack t)])]
      ctx0 <- start attrs
      ctx  <- step attrs (typed "a3b") ctx0
      getMessages ctx `shouldBe` ["123"]

    it "does not fire onInput when every typed character is rejected" $ do
      let attrs = [text "12", inputFilter (T.filter isDigit), onInput (\t -> [OutMsg (T.unpack t)])]
      ctx0 <- start attrs
      ctx  <- step attrs (typed "!") ctx0
      getMessages ctx `shouldBe` []

    it "still allows backspace regardless of the filter" $ do
      let attrs = [text "12", inputFilter (T.filter isDigit), onInput (\t -> [OutMsg (T.unpack t)])]
      ctx0 <- start attrs
      ctx  <- step attrs (pressed KeyBackspace []) ctx0
      getMessages ctx `shouldBe` ["1"]

  describe "displayFilter" $ do
    it "displays a filtered value instead of the real one" $ do
      let attrs = [text "hunter2", displayFilter (T.map (const '*'))]
      ctx0 <- start attrs
      ctx1 <- step attrs (down focusPt) ctx0
      ctx  <- step attrs (releasedAt focusPt) ctx1
      getDrawCommands ctx `shouldContain` [DrawText contentRect "*******" testColour AlignLeft]
      getDrawCommands ctx `shouldNotContain` [DrawText contentRect "hunter2" testColour AlignLeft]

    it "still edits and reports the real (unfiltered) value" $ do
      let attrs = [text "hunter2", displayFilter (T.map (const '*')), onInput (\t -> [OutMsg (T.unpack t)])]
      ctx0 <- start attrs
      ctx  <- step attrs (typed "!") ctx0
      getMessages ctx `shouldBe` ["hunter2!"]

    it "places the cursor using offsets measured against the masked text, not the real value" $ do
      let attrs = [text "hunter2", displayFilter (T.map (const '*'))]
      ctx1 <- step attrs (down (Point 35 50)) (seedWith fixedCharWidth)
      ctx2 <- step attrs (releasedAt (Point 35 50)) ctx1
      ctx  <- settle attrs ctx2
      contextSelections Field ctx `shouldBe` [Selection 1 1]

  describe "onSubmit" $ do
    it "fires when Enter is pressed while focused" $ do
      let attrs = [text "hello", onSubmit (const [OutMsg ("submitted" :: String)])]
      ctx0 <- start attrs
      ctx  <- step attrs (pressed KeyReturn []) ctx0
      getMessages ctx `shouldBe` ["submitted"]

    it "does not fire when a different element is focused" $ do
      let attrs = [text "hello", onSubmit (const [OutMsg ("submitted" :: String)])]
      ctx0 <- startUnfocused attrs
      ctx  <- stepUnfocused attrs (pressed KeyReturn []) ctx0
      getMessages ctx `shouldBe` []
