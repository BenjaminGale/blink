{-# LANGUAGE OverloadedStrings #-}
module Blink.ControlsSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Data.Text (Text)
import Blink.Controls (ScrollBarPart (..), ScrollState (..), SliderPart (..), StandardControls (..), button, checkbox, progressBar, radioGroup, scrollBar, slider, textInput)
import Blink.Geometry (Orientation (..), Point (..), Rectangle (..), insetRect, uniform)
import Blink.Input (ButtonState (..), Key (..), Modifier (..), KeyEvent (..), InputState (..))
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = TestControl | OtherControl
  deriving (Eq, Ord, Show)

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
  , styleBorderWidth  = 0
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
testTheme = Theme
  { themeElementStyles = Map.fromList [(TestControl, testStyleSet), (OtherControl, testStyleSet)]
  , themeDefaultStyle  = testStyleSet
  }

controlRect :: Rectangle
controlRect = Rectangle 0 0 100 100

bgRect :: Rectangle
bgRect = insetRect (uniform 10) controlRect

contentRect :: Rectangle
contentRect = insetRect (uniform 5) bgRect

mkCtx :: InputState -> UIContext TestElement () ()
mkCtx input = emptyUIContext controlRect input testTheme () ()

withFocus :: Maybe TestElement -> UIContext TestElement () s -> UIContext TestElement () s
withFocus e ctx = ctx { ctxFocusState = (ctxFocusState ctx) { focusedElement = e } }

getFocused :: UIContext TestElement () s -> Maybe TestElement
getFocused = focusedElement . ctxFocusState

-- The number of state modifiers queued during the frame.
dispatchCount :: UIContext e u s -> Int
dispatchCount = length . ctxDispatches

noInput :: InputState
noInput = InputState
  { inputMousePosition = Point 200 200
  , inputLeftButton    = ButtonUp
  , inputKeyEvents     = []
  , inputTypedText     = []
  }

mouseAt :: Point -> ButtonState -> [KeyEvent] -> InputState
mouseAt pos btn keys = InputState
  { inputMousePosition = pos
  , inputLeftButton    = btn
  , inputKeyEvents     = keys
  , inputTypedText     = []
  }

insidePoints :: [(String, Point)]
insidePoints =
  [ ("at the center",           Point 50 50)
  , ("at the top-left corner",  Point 10 10)
  , ("at the bottom-right corner", Point 90 90)
  ]

outsidePoints :: [(String, Point)]
outsidePoints =
  [ ("in the margin area",  Point 5 5)
  , ("outside the control", Point 200 200)
  ]

testBorderColour :: Colour
testBorderColour = RGBA 1 0 0 1

testStyleWithBorder :: Style
testStyleWithBorder = testStyle { styleBorderColour = Just testBorderColour, styleBorderWidth = 1 }

testStyleSetWithBorder :: StyleSet
testStyleSetWithBorder = StyleSet
  { styleSetNormal   = testStyleWithBorder
  , styleSetHovered  = testStyleWithBorder
  , styleSetPressed  = testStyleWithBorder
  , styleSetFocused  = testStyleWithBorder
  , styleSetDisabled = testStyleWithBorder
  }

testThemeWithBorder :: Theme TestElement
testThemeWithBorder = Theme
  { themeElementStyles = Map.fromList [(TestControl, testStyleSetWithBorder), (OtherControl, testStyleSetWithBorder)]
  , themeDefaultStyle  = testStyleSetWithBorder
  }

-- Zero-margin theme for checkbox: the box occupies a 20×20 slot at Rectangle 0 40 20 20
-- (MiddleLeft, Exactly 20×20 in a 100×100 rect). Without this, margin=10 collapses the
-- bgRect to zero and hover detection never fires.
zeroMarginStyle :: Style
zeroMarginStyle = testStyle { styleMargin = uniform 0, stylePadding = uniform 0 }

zeroMarginStyleSet :: StyleSet
zeroMarginStyleSet = StyleSet
  { styleSetNormal   = zeroMarginStyle
  , styleSetHovered  = zeroMarginStyle
  , styleSetPressed  = zeroMarginStyle
  , styleSetFocused  = zeroMarginStyle
  , styleSetDisabled = zeroMarginStyle
  }

checkboxTheme :: Theme TestElement
checkboxTheme = testTheme
  { themeElementStyles = Map.fromList [(TestControl, zeroMarginStyleSet), (OtherControl, testStyleSet)] }

transparentBgWithBorderStyle :: Style
transparentBgWithBorderStyle = testStyleWithBorder { styleBackground = RGBA 0 0 0 0 }

transparentBgWithBorderStyleSet :: StyleSet
transparentBgWithBorderStyleSet = StyleSet
  { styleSetNormal   = transparentBgWithBorderStyle
  , styleSetHovered  = transparentBgWithBorderStyle
  , styleSetPressed  = transparentBgWithBorderStyle
  , styleSetFocused  = transparentBgWithBorderStyle
  , styleSetDisabled = transparentBgWithBorderStyle
  }

transparentBgWithBorderTheme :: Theme TestElement
transparentBgWithBorderTheme = Theme
  { themeElementStyles = Map.fromList [(TestControl, transparentBgWithBorderStyleSet), (OtherControl, transparentBgWithBorderStyleSet)]
  , themeDefaultStyle  = transparentBgWithBorderStyleSet
  }

focusBorderStyleSet :: StyleSet
focusBorderStyleSet = testStyleSet { styleSetFocused = testStyleWithBorder }

focusBorderTheme :: Theme TestElement
focusBorderTheme = testTheme
  { themeElementStyles = Map.fromList [(TestControl, focusBorderStyleSet)] }

isStrokeRect :: DrawCommand -> Bool
isStrokeRect (StrokeRect {}) = True
isStrokeRect _               = False

type WidgetRunner = UIContext TestElement () () -> UIContext TestElement () ()

-- | Shared focus, tab, and hover tests for any widget whose primary interactive
--   element is TestControl. Pass a point inside the control's hittable area.
controlBehaviourSpec :: WidgetRunner -> Point -> Spec
controlBehaviourSpec run hitPoint = do
  describe "focus" $ do
    it "receives focus when nothing else is focused" $
      getFocused (run (mkCtx noInput))
        `shouldBe` Just TestControl

    it "does not take focus from another element" $
      getFocused (run (withFocus (Just OtherControl) (mkCtx noInput)))
        `shouldBe` Just OtherControl

    it "receives focus when clicked" $
      getFocused (run (withFocus (Just OtherControl) (mkCtx (mouseAt hitPoint ButtonReleased []))))
        `shouldBe` Just TestControl

    it "does not steal focus when the mouse is released on it after dragging from another element" $
      -- Simulate being mid-drag from OtherControl: capture is set to OtherControl on the release frame.
      let ctx = (mkCtx (mouseAt hitPoint ButtonReleased [])) { ctxCapturedElement = Just OtherControl }
      in getFocused (run ctx) `shouldBe` Nothing

  describe "tab navigation" $ do
    it "passes focus to the next control when Tab is pressed" $
      getFocused (run (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyTab []] })))
        `shouldBe` Nothing

    it "passes focus to the previous control when Shift+Tab is pressed" $
      getFocused (run (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyTab [Shift]] }) { ctxPreviousTabStop = Just OtherControl }))
        `shouldBe` Just OtherControl

  describe "hover detection" $ do
    it "is hovered when the mouse is inside" $
      ctxHoveredElement (run (mkCtx (mouseAt hitPoint ButtonUp [])))
        `shouldBe` Just TestControl

    it "is not hovered when the mouse is outside" $
      ctxHoveredElement (run (mkCtx (mouseAt (Point 200 200) ButtonUp [])))
        `shouldBe` Nothing

  describe "when disabled" $ do
    let disabledRun ctx = run (ctx { ctxDisabled = True })

    it "does not take auto-focus" $
      getFocused (disabledRun (mkCtx noInput))
        `shouldBe` Nothing

    it "does not steal focus when clicked" $
      getFocused (disabledRun (withFocus (Just OtherControl) (mkCtx (mouseAt hitPoint ButtonReleased []))))
        `shouldBe` Just OtherControl

    it "is not hovered when the mouse is inside" $
      ctxHoveredElement (disabledRun (mkCtx (mouseAt hitPoint ButtonUp [])))
        `shouldBe` Nothing

    it "is not recorded as the previous tab stop" $
      ctxPreviousTabStop (disabledRun (mkCtx noInput))
        `shouldBe` Nothing

-- | Background and border rendering tests. Only applicable to single controls
--   that fill controlRect directly (not composite widgets).
backgroundAndBorderSpec :: WidgetRunner -> Spec
backgroundAndBorderSpec run = do
  let runWithBorder ctx = run (ctx { ctxTheme = testThemeWithBorder })
  it "does not draw a background in the margin area" $
    ctxDrawCommands (run (mkCtx noInput))
      `shouldNotContain` [FillRect controlRect testColour]

  it "fills its background area" $
    ctxDrawCommands (run (mkCtx noInput))
      `shouldContain` [FillRect bgRect testColour]

  it "clips content to its padding area" $
    ctxDrawCommands (run (mkCtx noInput))
      `shouldContain` [PushClip contentRect]

  it "does not draw a border when borderColour is Nothing" $
    filter isStrokeRect (ctxDrawCommands (run (mkCtx noInput)))
      `shouldBe` []

  it "draws a border when borderColour is set" $
    ctxDrawCommands (runWithBorder (mkCtx noInput))
      `shouldContain` [StrokeRect bgRect testBorderColour 1]

  it "draws a border even when the background is transparent" $
    let runWithTransparentBgAndBorder ctx = run (ctx { ctxTheme = transparentBgWithBorderTheme })
    in ctxDrawCommands (runWithTransparentBgAndBorder (mkCtx noInput))
         `shouldContain` [StrokeRect bgRect testBorderColour 1]

runProgressBar :: Double -> WidgetRunner
runProgressBar value ctx = snd $ runUI (progressBar TestControl value) ctx

runButton :: WidgetRunner
runButton ctx = snd $ runUI (button TestControl "label") ctx

runTextInputControl :: WidgetRunner
runTextInputControl ctx = snd $ runUI (textInput TestControl "" (\_ s -> s)) ctx

-- Text editing tests use the entered text itself as the application state.
mkTextCtx :: Text -> InputState -> UIContext TestElement () Text
mkTextCtx value input = emptyUIContext controlRect input testTheme () value

runTextInput :: Text -> UIContext TestElement () Text -> UIContext TestElement () Text
runTextInput value ctx = snd $ runUI (textInput TestControl value (\t _ -> t)) ctx

-- Forces checkboxTheme so the 20×20 box slot is hittable regardless of mkCtx's theme.
runCheckboxControl :: WidgetRunner
runCheckboxControl ctx = snd $ runUI (checkbox TestControl "test label" False (\_ s -> s)) (ctx { ctxTheme = checkboxTheme })

-- Toggle tests record the dispatched value in a Maybe Bool application state.
runCheckbox :: Bool -> UIContext TestElement () (Maybe Bool) -> UIContext TestElement () (Maybe Bool)
runCheckbox checked ctx = snd $ runUI (checkbox TestControl "test label" checked (\v _ -> Just v)) ctx

mkCheckboxCtx :: InputState -> UIContext TestElement () (Maybe Bool)
mkCheckboxCtx input = emptyUIContext controlRect input checkboxTheme () Nothing

-- Center of the box bgRect (Rectangle 0 40 20 20) with zero-margin theme
boxPoint :: Point
boxPoint = Point 10 50

drawnTexts :: UIContext e u s -> [Text]
drawnTexts ctx = [t | DrawText _ t _ _ <- getDrawCommands ctx]

-- runSliderControl maps SliderTrack -> TestControl and SliderThumb -> OtherControl
-- so the control suite helpers work without modification.
runSliderControl :: WidgetRunner
runSliderControl ctx = snd $ runUI (slider tag Horizontal 0.5 (\_ s -> s)) ctx
  where
    tag SliderTrack = TestControl
    tag SliderThumb = OtherControl

-- slider setup: element type is SliderPart (mkId = id), app state IS the value.
-- Rect is 200×30; with zero margin/padding the thumb is 30×30, giving a travel
-- range of 170px. scrollPosFromMouse centres the thumb on the cursor, so:
--   value = clamp 0 1 ((mouseX - 15) / 170)
-- Key positions: mouseX=15 → 0.0, mouseX=100 → 0.5, mouseX=185 → 1.0.
sliderTheme :: Theme SliderPart
sliderTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = zeroMarginStyleSet }

sliderRect :: Rectangle
sliderRect = Rectangle 0 0 200 30

runSlider :: Orientation -> Double -> InputState -> UIContext SliderPart () Double
runSlider ori val input =
  snd $ runUI (slider id ori val (\v _ -> v))
    (emptyUIContext sliderRect input sliderTheme () val)

withSliderFocus :: Maybe SliderPart -> UIContext SliderPart () Double -> UIContext SliderPart () Double
withSliderFocus e ctx = ctx { ctxFocusState = (ctxFocusState ctx) { focusedElement = e } }

-- runRadioControl maps index 0 -> TestControl for the control suite helpers.
-- A single-item group is enough to exercise focus, tab, hover, and background.
runRadioControl :: WidgetRunner
runRadioControl ctx = snd $ runUI (radioGroup tag [("a" :: String, "Option")] "a" (\_ s -> s)) ctx
  where
    tag 0 = TestControl
    tag _ = OtherControl

-- radioGroup setup: element type is Int (mkId = id), app state IS the selection.
-- Three items of 30px each in a 100×90 rect (zero margin/padding), giving:
--   item 0: y 0–30  centre Point 50 15
--   item 1: y 30–60 centre Point 50 45
--   item 2: y 60–90 centre Point 50 75
radioGroupTheme :: Theme Int
radioGroupTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = zeroMarginStyleSet }

radioGroupRect :: Rectangle
radioGroupRect = Rectangle 0 0 100 90

radioItems :: [(String, Text)]
radioItems = [("a", "Alpha"), ("b", "Beta"), ("c", "Gamma")]

runRadioGroup :: String -> InputState -> UIContext Int () String
runRadioGroup sel input =
  snd $ runUI (radioGroup id radioItems sel (\v _ -> v))
    (emptyUIContext radioGroupRect input radioGroupTheme () sel)

withItemFocus :: Maybe Int -> UIContext Int () String -> UIContext Int () String
withItemFocus e ctx = ctx { ctxFocusState = (ctxFocusState ctx) { focusedElement = e } }

-- scrollBar setup: the element type is ScrollBarPart itself (mkId = id) and
-- the UI state is a StandardControls holding the position keyed by ScrollTrack.
scrollControls :: Double -> StandardControls ScrollBarPart
scrollControls pos = StandardControls (Map.singleton ScrollTrack (ScrollState pos)) Map.empty

scrollPos :: UIContext ScrollBarPart (StandardControls ScrollBarPart) () -> Double
scrollPos = scrollPosition . Map.findWithDefault (ScrollState 0) ScrollTrack . scScrollStates . ctxUIState

scrollTheme :: Theme ScrollBarPart
scrollTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle = zeroMarginStyleSet
  }

-- 20×200 vertical scrollbar with a 0.25 thumb ratio: buttons at y 0–20 and
-- 180–200, track at y 20–180.
scrollRect :: Rectangle
scrollRect = Rectangle 0 0 20 200

runScrollBar :: Double -> InputState -> UIContext ScrollBarPart (StandardControls ScrollBarPart) ()
runScrollBar pos input =
  snd $ runUI (scrollBar id Vertical 0.25) (emptyUIContext scrollRect input scrollTheme (scrollControls pos) ())

spec :: Spec
spec = do
  describe "textInput" $ do
    controlBehaviourSpec runTextInputControl (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runTextInputControl

    describe "rendering" $ do
      it "displays the value without a cursor when unfocused" $
        drawnTexts (runTextInput "hello" (withFocus (Just OtherControl) (mkTextCtx "hello" noInput)))
          `shouldContain` ["hello"]

      it "displays the value with a cursor when focused" $
        drawnTexts (runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput)))
          `shouldContain` ["hello|"]

    describe "text editing" $ do
      it "appends typed characters to the value" $
        applyDispatches (runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["!"] })))
          `shouldBe` "hello!"

      it "removes the last character on backspace" $
        applyDispatches (runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyBackspace []] })))
          `shouldBe` "hell"

      it "does not dispatch when backspace is pressed on an empty value" $
        dispatchCount (runTextInput "" (withFocus (Just TestControl) (mkTextCtx "" noInput { inputKeyEvents = [KeyEvent KeyBackspace []] })))
          `shouldBe` 0

      it "does not dispatch when there is no input" $
        dispatchCount (runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput)))
          `shouldBe` 0

      it "does not process input when unfocused" $
        dispatchCount (runTextInput "hello" (withFocus (Just OtherControl) (mkTextCtx "hello" noInput { inputTypedText = ["!"], inputKeyEvents = [KeyEvent KeyBackspace []] })))
          `shouldBe` 0

    describe "disabled" $ do
      it "does not process input when disabled" $
        dispatchCount (snd (runUI (disableWhen True (textInput TestControl "hello" (\t _ -> t))) (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["!"] }))))
          `shouldBe` 0

      it "does not show a cursor when focused and disabled" $
        drawnTexts (snd (runUI (disableWhen True (textInput TestControl "hello" (\t _ -> t))) (withFocus (Just TestControl) (mkTextCtx "hello" noInput))))
          `shouldNotContain` ["hello|"]

  describe "checkbox" $ do
    controlBehaviourSpec runCheckboxControl boxPoint

    describe "toggle behaviour" $ do
      it "dispatches True when the box is clicked while unchecked" $
        applyDispatches (runCheckbox False (mkCheckboxCtx (mouseAt boxPoint ButtonReleased [])))
          `shouldBe` Just True

      it "dispatches False when the box is clicked while checked" $
        applyDispatches (runCheckbox True (mkCheckboxCtx (mouseAt boxPoint ButtonReleased [])))
          `shouldBe` Just False

      it "dispatches toggle when Enter is pressed while focused" $
        applyDispatches (runCheckbox False (withFocus (Just TestControl) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] })))
          `shouldBe` Just True

      it "dispatches toggle when Space is pressed while focused" $
        applyDispatches (runCheckbox False (withFocus (Just TestControl) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeySpace []] })))
          `shouldBe` Just True

      it "does not dispatch when clicked outside the box" $
        applyDispatches (runCheckbox False (mkCheckboxCtx (mouseAt (Point 50 50) ButtonReleased [])))
          `shouldBe` Nothing

      it "does not dispatch when Enter is pressed while unfocused" $
        applyDispatches (runCheckbox False (withFocus (Just OtherControl) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] })))
          `shouldBe` Nothing

    describe "disabled" $ do
      it "does not dispatch when clicked while disabled" $
        applyDispatches (snd (runUI (disableWhen True (checkbox TestControl "test label" False (\v _ -> Just v))) (mkCheckboxCtx (mouseAt boxPoint ButtonReleased []))))
          `shouldBe` Nothing

      it "does not dispatch when Enter is pressed while disabled" $
        applyDispatches (snd (runUI (disableWhen True (checkbox TestControl "test label" False (\v _ -> Just v))) (withFocus (Just TestControl) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))))
          `shouldBe` Nothing

    describe "rendering" $ do
      it "draws the checkmark when checked" $
        drawnTexts (runCheckbox True (mkCheckboxCtx noInput))
          `shouldContain` ["✓"]

      it "does not draw the checkmark when unchecked" $
        drawnTexts (runCheckbox False (mkCheckboxCtx noInput))
          `shouldNotContain` ["✓"]

      it "draws the label text" $
        drawnTexts (runCheckbox False (mkCheckboxCtx noInput))
          `shouldContain` ["test label"]

    describe "focus ring" $ do
      it "draws a focus ring around the full control when focused" $
        ctxDrawCommands (runCheckbox False (withFocus (Just TestControl) (mkCheckboxCtx noInput) { ctxTheme = focusBorderTheme }))
          `shouldContain` [StrokeRect controlRect testBorderColour 1]

      it "does not draw a focus ring when unfocused" $
        ctxDrawCommands (runCheckbox False (withFocus (Just OtherControl) (mkCheckboxCtx noInput) { ctxTheme = focusBorderTheme }))
          `shouldNotContain` [StrokeRect controlRect testBorderColour 1]

  describe "scrollBar" $ do
    describe "button stepping" $ do
      it "steps forward by the thumb ratio when the increment button is clicked" $
        scrollPos (runScrollBar 0.5 (mouseAt (Point 10 190) ButtonReleased []))
          `shouldBe` 0.75

      it "steps back by the thumb ratio when the decrement button is clicked" $
        scrollPos (runScrollBar 0.5 (mouseAt (Point 10 10) ButtonReleased []))
          `shouldBe` 0.25

      it "clamps to 1 when stepping forward near the end" $
        scrollPos (runScrollBar 0.9 (mouseAt (Point 10 190) ButtonReleased []))
          `shouldBe` 1

      it "clamps to 0 when stepping back near the start" $
        scrollPos (runScrollBar 0.1 (mouseAt (Point 10 10) ButtonReleased []))
          `shouldBe` 0

    describe "track dragging" $ do
      it "centres the thumb on the cursor while the track is pressed" $
        scrollPos (runScrollBar 0 (mouseAt (Point 10 100) ButtonDown []))
          `shouldBe` 0.5

      it "continues tracking when the mouse moves off the track while the button is held" $
        let frame1 = runScrollBar 0 (mouseAt (Point 10 100) ButtonDown [])
            frame2 = snd $ runUI (scrollBar id Vertical 0.25)
                                 (nextFrameContext scrollRect (mouseAt (Point 200 40) ButtonDown []) frame1)
        in scrollPos frame2 `shouldBe` 0.0

      it "stops tracking when the button is released after dragging off the track" $
        let frame1 = runScrollBar 0 (mouseAt (Point 10 100) ButtonDown [])
            frame2 = snd $ runUI (scrollBar id Vertical 0.25)
                                 (nextFrameContext scrollRect (mouseAt (Point 200 40) ButtonUp []) frame1)
        in scrollPos frame2 `shouldBe` 0.5

    describe "without interaction" $ do
      it "leaves the position unchanged" $
        scrollPos (runScrollBar 0.5 noInput)
          `shouldBe` 0.5

  describe "progressBar" $ do
    describe "background and border" $ backgroundAndBorderSpec (runProgressBar 0.5)

    describe "rendering" $ do
      it "fills the correct proportion of the content area at 0.5" $
        ctxDrawCommands (runProgressBar 0.5 (mkCtx noInput))
          `shouldContain` [FillRect (Rectangle 15 15 35 70) testColour]

      it "fills the full content area at 1.0" $
        ctxDrawCommands (runProgressBar 1.0 (mkCtx noInput))
          `shouldContain` [FillRect contentRect testColour]

      it "fills zero width at 0.0" $
        ctxDrawCommands (runProgressBar 0.0 (mkCtx noInput))
          `shouldContain` [FillRect (Rectangle 15 15 0 70) testColour]

    describe "clamping" $ do
      it "clamps values above 1.0 to full width" $
        ctxDrawCommands (runProgressBar 1.5 (mkCtx noInput))
          `shouldContain` [FillRect contentRect testColour]

      it "clamps values below 0.0 to zero width" $
        ctxDrawCommands (runProgressBar (-0.5) (mkCtx noInput))
          `shouldContain` [FillRect (Rectangle 15 15 0 70) testColour]

  describe "button" $ do
    controlBehaviourSpec runButton (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runButton

    describe "rendering" $ do
      it "draws the label" $
        drawnTexts (runButton (mkCtx noInput))
          `shouldContain` ["label"]

    describe "click behaviour" $ do
      forM_ insidePoints $ \(desc, pt) ->
        it ("is clicked when the mouse is released " <> desc) $
          fst (runUI (button TestControl "label") (mkCtx (mouseAt pt ButtonReleased [])))
            `shouldBe` True

      forM_ outsidePoints $ \(desc, pt) ->
        it ("is not clicked when the mouse is released " <> desc) $
          fst (runUI (button TestControl "label") (mkCtx (mouseAt pt ButtonReleased [])))
            `shouldBe` False

      it "is clicked when Enter is pressed and the button has focus" $
        fst (runUI (button TestControl "label") (mkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
          `shouldBe` True

      it "is not clicked when Enter is pressed and the button does not have focus" $
        fst (runUI (button TestControl "label") (withFocus (Just OtherControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] })))
          `shouldBe` False

      it "is not clicked when Tab and Enter are pressed simultaneously" $
        fst (runUI (button TestControl "label")
          (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyTab [], KeyEvent KeyReturn []] })))
          `shouldBe` False

    describe "disabled" $ do
      it "is not activated by a click when disabled" $
        fst (runUI (disableWhen True (button TestControl "label")) (mkCtx (mouseAt (Point 50 50) ButtonReleased [])))
          `shouldBe` False

      it "is not activated by Enter when disabled" $
        fst (runUI (disableWhen True (button TestControl "label")) (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] })))
          `shouldBe` False

  describe "slider" $ do
    controlBehaviourSpec runSliderControl (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runSliderControl

    describe "drag interaction" $ do
      it "sets value to 0.5 when dragged to the midpoint" $
        applyDispatches (runSlider Horizontal 0 (mouseAt (Point 100 15) ButtonDown []))
          `shouldBe` 0.5

      it "sets value to 0 when dragged to the far left" $
        applyDispatches (runSlider Horizontal 0.5 (mouseAt (Point 15 15) ButtonDown []))
          `shouldBe` 0.0

      it "sets value to 1 when dragged to the far right" $
        applyDispatches (runSlider Horizontal 0.5 (mouseAt (Point 185 15) ButtonDown []))
          `shouldBe` 1.0

      it "continues tracking when the mouse moves outside the track while button held" $
        let frame1 = runSlider Horizontal 0 (mouseAt (Point 100 15) ButtonDown [])
            val1   = applyDispatches frame1
            frame2 = snd $ runUI (slider id Horizontal val1 (\v _ -> v))
                                 (nextFrameContext sliderRect (mouseAt (Point 300 15) ButtonDown []) frame1)
        in applyDispatches frame2 `shouldBe` 1.0

      it "stops tracking when the button is released" $
        let frame1 = runSlider Horizontal 0 (mouseAt (Point 100 15) ButtonDown [])
            val1   = applyDispatches frame1
            frame2 = snd $ runUI (slider id Horizontal val1 (\v _ -> v))
                                 (nextFrameContext sliderRect (mouseAt (Point 300 15) ButtonUp []) frame1)
        in dispatchCount frame2 `shouldBe` 0

    describe "keyboard nudging" $ do
      it "increases value by 0.05 when Right is pressed (Horizontal)" $
        applyDispatches (runSlider Horizontal 0.5 noInput { inputKeyEvents = [KeyEvent KeyRight []] })
          `shouldBe` 0.55

      it "decreases value by 0.05 when Left is pressed (Horizontal)" $
        applyDispatches (runSlider Horizontal 0.5 noInput { inputKeyEvents = [KeyEvent KeyLeft []] })
          `shouldBe` 0.45

      it "increases value by 0.05 when Down is pressed (Vertical)" $
        applyDispatches (runSlider Vertical 0.5 noInput { inputKeyEvents = [KeyEvent KeyDown []] })
          `shouldBe` 0.55

      it "decreases value by 0.05 when Up is pressed (Vertical)" $
        applyDispatches (runSlider Vertical 0.5 noInput { inputKeyEvents = [KeyEvent KeyUp []] })
          `shouldBe` 0.45

      it "clamps to 1 when nudging at the maximum" $
        applyDispatches (runSlider Horizontal 1.0 noInput { inputKeyEvents = [KeyEvent KeyRight []] })
          `shouldBe` 1.0

      it "clamps to 0 when nudging at the minimum" $
        applyDispatches (runSlider Horizontal 0.0 noInput { inputKeyEvents = [KeyEvent KeyLeft []] })
          `shouldBe` 0.0

      it "does not nudge when another element has focus" $
        applyDispatches (snd $ runUI (slider id Horizontal 0.5 (\v _ -> v))
          (withSliderFocus (Just SliderThumb) (emptyUIContext sliderRect noInput { inputKeyEvents = [KeyEvent KeyRight []] } sliderTheme () 0.5)))
          `shouldBe` 0.5

    describe "without interaction" $ do
      it "does not dispatch when there is no input" $
        dispatchCount (runSlider Horizontal 0.5 noInput)
          `shouldBe` 0

  describe "radioGroup" $ do
    controlBehaviourSpec runRadioControl (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runRadioControl

    describe "selection" $ do
      it "dispatches the value of a clicked item" $
        applyDispatches (runRadioGroup "a" (mouseAt (Point 50 45) ButtonReleased []))
          `shouldBe` "b"

      it "dispatches the correct value when the last item is clicked" $
        applyDispatches (runRadioGroup "a" (mouseAt (Point 50 75) ButtonReleased []))
          `shouldBe` "c"

      it "dispatches the value when Enter is pressed while an item is focused" $
        applyDispatches (snd $ runUI (radioGroup id radioItems "a" (\v _ -> v))
          (withItemFocus (Just 1) (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent KeyReturn []] } radioGroupTheme () "a")))
          `shouldBe` "b"

      it "dispatches the value when Space is pressed while an item is focused" $
        applyDispatches (snd $ runUI (radioGroup id radioItems "a" (\v _ -> v))
          (withItemFocus (Just 2) (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent KeySpace []] } radioGroupTheme () "a")))
          `shouldBe` "c"

      it "does not dispatch when no item is focused and a key is pressed" $
        dispatchCount (snd $ runUI (radioGroup id radioItems "a" (\v _ -> v))
          (withItemFocus (Just 99) (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent KeyReturn []] } radioGroupTheme () "a")))
          `shouldBe` 0

      it "does not dispatch when there is no interaction" $
        dispatchCount (runRadioGroup "b" noInput)
          `shouldBe` 0

    describe "rendering" $ do
      it "shows the selected mark on the selected item" $
        drawnTexts (runRadioGroup "b" noInput) `shouldContain` ["● Beta"]

      it "shows the unselected mark on other items" $ do
        drawnTexts (runRadioGroup "b" noInput) `shouldContain` ["○ Alpha"]
        drawnTexts (runRadioGroup "b" noInput) `shouldContain` ["○ Gamma"]

      it "displays all labels regardless of selection" $
        length (drawnTexts (runRadioGroup "a" noInput)) `shouldBe` 3
