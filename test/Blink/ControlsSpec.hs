{-# LANGUAGE OverloadedStrings #-}
module Blink.ControlsSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Data.Text (Text)
import Blink.Controls (ProgressValue (..), ScrollBarPart (..), ScrollRegionPart (..), SliderPart (..), button, checkbox, control, mouseToTrackPos, progressBar, radioGroup, scrollBar, scrollableRegion, scrollRegionBarSize, slider, textInput, thumbRect)
import Blink.Geometry (Orientation (..), Point (..), Rectangle (..), Size (..), insetRect, uniform)
import Blink.Input (Key (..), Modifier (..), KeyEvent (..), InputState (..))
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

mkCtx :: InputState -> UIContext TestElement ()
mkCtx input = emptyUIContext controlRect input testTheme () noOpTextMeasurer

withFocus :: Maybe TestElement -> UIContext TestElement s -> UIContext TestElement s
withFocus e ctx = ctx { ctxInteraction = (ctxInteraction ctx) { ixnFocus = (ixnFocus (ctxInteraction ctx)) { focusedElement = e } } }

getFocused :: UIContext TestElement s -> Maybe TestElement
getFocused = focusedElement . ixnFocus . ctxInteraction

-- The number of state modifiers queued during the frame.
dispatchCount :: UIContext e s -> Int
dispatchCount = length . outDispatches . ctxOutputs

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

mouseAt :: Point -> Bool -> [KeyEvent] -> InputState
mouseAt pos down keys = InputState
  { inputMousePosition  = pos
  , inputLeftButtonDown = down
  , inputKeyEvents      = keys
  , inputTypedText      = []
  }

-- | Sets 'ixnButtonReleased' so controls see a click this frame, without
-- requiring a prior down-frame in the test sequence.
withButtonReleased :: UIContext e s -> UIContext e s
withButtonReleased ctx = ctx
  { ctxInput       = (ctxInput ctx) { inputLeftButtonDown = False }
  , ctxInteraction = (ctxInteraction ctx) { ixnButtonDown = False, ixnButtonReleased = True }
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

type WidgetRunner = UIContext TestElement () -> IO (UIContext TestElement ())

-- | Shared focus, tab, and hover tests for any widget whose primary interactive
--   element is TestControl. Pass a point inside the control's hittable area.
controlBehaviourSpec :: WidgetRunner -> Point -> Spec
controlBehaviourSpec run hitPoint = do
  describe "focus" $ do
    it "receives focus when nothing else is focused" $ do
      ctx' <- run (mkCtx noInput)
      getFocused ctx' `shouldBe` Just TestControl

    it "does not take focus from another element" $ do
      ctx' <- run (withFocus (Just OtherControl) (mkCtx noInput))
      getFocused ctx' `shouldBe` Just OtherControl

    it "receives focus when clicked" $ do
      ctx' <- run (withFocus (Just OtherControl) (withButtonReleased (mkCtx (mouseAt hitPoint False []))))
      getFocused ctx' `shouldBe` Just TestControl

    it "does not steal focus when the mouse is released on it after dragging from another element" $ do
      -- Simulate being mid-drag from OtherControl: capture is set to OtherControl on the release frame.
      let base = withButtonReleased (mkCtx (mouseAt hitPoint False []))
          ctx  = base { ctxInteraction = (ctxInteraction base) { ixnCaptured = Just OtherControl } }
      ctx' <- run ctx
      getFocused ctx' `shouldBe` Nothing

  describe "tab navigation" $ do
    it "passes focus to the next control when Tab is pressed" $ do
      ctx' <- run (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyTab []] }))
      getFocused ctx' `shouldBe` Nothing

    it "passes focus to the previous control when Shift+Tab is pressed" $ do
      let base = withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyTab [Shift]] })
      ctx' <- run (base { ctxInteraction = (ctxInteraction base) { ixnPrevTabStop = Just OtherControl } })
      getFocused ctx' `shouldBe` Just OtherControl

  describe "hover detection" $ do
    it "is hovered when the mouse is inside" $ do
      ctx' <- run (mkCtx (mouseAt hitPoint False []))
      ixnHovered (ctxInteraction ctx') `shouldBe` Just TestControl

    it "is not hovered when the mouse is outside" $ do
      ctx' <- run (mkCtx (mouseAt (Point 200 200) False []))
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

  describe "when disabled" $ do
    let disabledRun ctx = run (ctx { ctxDisabled = True })

    it "does not take auto-focus" $ do
      ctx' <- disabledRun (mkCtx noInput)
      getFocused ctx' `shouldBe` Nothing

    it "does not steal focus when clicked" $ do
      ctx' <- disabledRun (withFocus (Just OtherControl) (withButtonReleased (mkCtx (mouseAt hitPoint False []))))
      getFocused ctx' `shouldBe` Just OtherControl

    it "is not hovered when the mouse is inside" $ do
      ctx' <- disabledRun (mkCtx (mouseAt hitPoint False []))
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

    it "is not recorded as the previous tab stop" $ do
      ctx' <- disabledRun (mkCtx noInput)
      ixnPrevTabStop (ctxInteraction ctx') `shouldBe` Nothing

-- | Background and border rendering tests. Only applicable to single controls
--   that fill controlRect directly (not composite widgets).
backgroundAndBorderSpec :: WidgetRunner -> Spec
backgroundAndBorderSpec run = do
  let runWithBorder ctx = run (ctx { ctxTheme = testThemeWithBorder })
  it "does not draw a background in the margin area" $ do
    ctx' <- run (mkCtx noInput)
    getDrawCommands ctx' `shouldNotContain` [FillRect controlRect testColour]

  it "fills its background area" $ do
    ctx' <- run (mkCtx noInput)
    getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

  it "clips content to its padding area" $ do
    ctx' <- run (mkCtx noInput)
    getDrawCommands ctx' `shouldContain` [PushClip contentRect]

  it "does not draw a border when borderColour is Nothing" $ do
    ctx' <- run (mkCtx noInput)
    filter isStrokeRect (getDrawCommands ctx') `shouldBe` []

  it "draws a border when borderColour is set" $ do
    ctx' <- runWithBorder (mkCtx noInput)
    getDrawCommands ctx' `shouldContain` [StrokeRect bgRect testBorderColour 1]

  it "draws a border even when the background is transparent" $ do
    ctx' <- run ((mkCtx noInput) { ctxTheme = transparentBgWithBorderTheme })
    getDrawCommands ctx' `shouldContain` [StrokeRect bgRect testBorderColour 1]

runProgressBar :: Double -> WidgetRunner
runProgressBar value ctx = fmap snd $ runUI (progressBar TestControl (Progress value)) ctx

runButton :: WidgetRunner
runButton ctx = fmap snd $ runUI (button TestControl "label") ctx

runTextInputControl :: WidgetRunner
runTextInputControl ctx = fmap snd $ runUI (textInput TestControl "" (\_ s -> s)) ctx

-- Text editing tests use the entered text itself as the application state.
mkTextCtx :: Text -> InputState -> UIContext TestElement Text
mkTextCtx value input = emptyUIContext controlRect input testTheme value noOpTextMeasurer

runTextInput :: Text -> UIContext TestElement Text -> IO (UIContext TestElement Text)
runTextInput value ctx = fmap snd $ runUI (textInput TestControl value (\t _ -> t)) ctx

-- Forces checkboxTheme so the 20×20 box slot is hittable regardless of mkCtx's theme.
runCheckboxControl :: WidgetRunner
runCheckboxControl ctx = fmap snd $ runUI (checkbox TestControl "test label" False (\_ s -> s)) (ctx { ctxTheme = checkboxTheme })

-- Toggle tests record the dispatched value in a Maybe Bool application state.
runCheckbox :: Bool -> UIContext TestElement (Maybe Bool) -> IO (UIContext TestElement (Maybe Bool))
runCheckbox checked ctx = fmap snd $ runUI (checkbox TestControl "test label" checked (\v _ -> Just v)) ctx

mkCheckboxCtx :: InputState -> UIContext TestElement (Maybe Bool)
mkCheckboxCtx input = emptyUIContext controlRect input checkboxTheme Nothing noOpTextMeasurer

-- Center of the box bgRect (Rectangle 0 40 20 20) with zero-margin theme
boxPoint :: Point
boxPoint = Point 10 50

drawnTexts :: UIContext e s -> [Text]
drawnTexts ctx = [t | DrawText _ t _ _ <- getDrawCommands ctx]

-- runSliderControl maps SliderTrack -> TestControl and SliderThumb -> OtherControl
-- so the control suite helpers work without modification.
runSliderControl :: WidgetRunner
runSliderControl ctx = fmap snd $ runUI (slider tag Horizontal 0.5 (\_ s -> s)) ctx
  where
    tag SliderTrack = TestControl
    tag SliderThumb = OtherControl

-- slider setup: element type is SliderPart (mkId = id), app state IS the value.
-- Rect is 200×30; with zero margin/padding the thumb is 30×30, giving a travel
-- range of 170px. mouseToTrackPos centres the thumb on the cursor, so:
--   value = clamp 0 1 ((mouseX - 15) / 170)
-- Key positions: mouseX=15 → 0.0, mouseX=100 → 0.5, mouseX=185 → 1.0.
sliderTheme :: Theme SliderPart
sliderTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = zeroMarginStyleSet }

sliderRect :: Rectangle
sliderRect = Rectangle 0 0 200 30

runSlider :: Orientation -> Double -> InputState -> IO (UIContext SliderPart Double)
runSlider ori val input =
  fmap snd $ runUI (slider id ori val (\v _ -> v))
    (emptyUIContext sliderRect input sliderTheme val noOpTextMeasurer)

withSliderFocus :: Maybe SliderPart -> UIContext SliderPart Double -> UIContext SliderPart Double
withSliderFocus e ctx = ctx { ctxInteraction = (ctxInteraction ctx) { ixnFocus = (ixnFocus (ctxInteraction ctx)) { focusedElement = e } } }

-- runRadioControl maps index 0 -> TestControl for the control suite helpers.
-- A single-item group is enough to exercise focus, tab, hover, and background.
runRadioControl :: WidgetRunner
runRadioControl ctx = fmap snd $ runUI (radioGroup tag [("a" :: String, "Option")] "a" (\_ s -> s)) ctx
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

mkRadioGroupCtx :: String -> InputState -> UIContext Int String
mkRadioGroupCtx sel input = emptyUIContext radioGroupRect input radioGroupTheme sel noOpTextMeasurer

runRadioGroup :: String -> UIContext Int String -> IO (UIContext Int String)
runRadioGroup sel = fmap snd . runUI (radioGroup id radioItems sel (\v _ -> v))

withItemFocus :: Maybe Int -> UIContext Int String -> UIContext Int String
withItemFocus e ctx = ctx { ctxInteraction = (ctxInteraction ctx) { ixnFocus = (ixnFocus (ctxInteraction ctx)) { focusedElement = e } } }

scrollPos :: UIContext ScrollBarPart () -> Double
scrollPos = scrollPosition . Map.findWithDefault (ScrollState 0) ScrollTrack . elmScrollStates . ctxElements

scrollTheme :: Theme ScrollBarPart
scrollTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle = zeroMarginStyleSet
  }

-- 20×200 vertical scrollbar with a 0.25 thumb ratio: buttons at y 0–20 and
-- 180–200, track at y 20–180.
scrollRect :: Rectangle
scrollRect = Rectangle 0 0 20 200

mkScrollBarCtx :: Double -> InputState -> UIContext ScrollBarPart ()
mkScrollBarCtx pos input =
  let base = emptyUIContext scrollRect input scrollTheme () noOpTextMeasurer
  in base { ctxElements = (ctxElements base) { elmScrollStates = Map.singleton ScrollTrack (ScrollState pos) } }

runScrollBar :: UIContext ScrollBarPart () -> IO (UIContext ScrollBarPart ())
runScrollBar = fmap snd . runUI (scrollBar id Vertical 0.25)

data ScrollRegionElem = SRPart ScrollRegionPart | SRChild
  deriving (Eq, Ord, Show)

srTheme :: Theme ScrollRegionElem
srTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = zeroMarginStyleSet }

-- outer: 200×100, virtual content: 400×100
-- viewport: 200×84 (H scrollbar takes 16px at bottom: y 84–100)
srOuterRect :: Rectangle
srOuterRect = Rectangle 0 0 200 100

runScrollableRegion :: Point -> IO (UIContext ScrollRegionElem ())
runScrollableRegion mousePos =
  let input = noInput { inputMousePosition = mousePos }
      ctx = emptyUIContext srOuterRect input srTheme () noOpTextMeasurer
  in fmap snd $ runUI (scrollableRegion SRPart (Size 400 100) (control SRChild (pure ()))) ctx


spec :: Spec
spec = describe "Controls" $ do
  describe "progressBar" $ do
    describe "background and border" $ backgroundAndBorderSpec (runProgressBar 0.5)

    describe "rendering" $ do
      it "fills the correct proportion of the content area at 0.5" $ do
        ctx' <- runProgressBar 0.5 (mkCtx noInput)
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 15 15 35 70) testColour]

      it "fills the full content area at 1.0" $ do
        ctx' <- runProgressBar 1.0 (mkCtx noInput)
        getDrawCommands ctx' `shouldContain` [FillRect contentRect testColour]

      it "fills zero width at 0.0" $ do
        ctx' <- runProgressBar 0.0 (mkCtx noInput)
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 15 15 0 70) testColour]

    describe "clamping" $ do
      it "clamps values above 1.0 to full width" $ do
        ctx' <- runProgressBar 1.5 (mkCtx noInput)
        getDrawCommands ctx' `shouldContain` [FillRect contentRect testColour]

      it "clamps values below 0.0 to zero width" $ do
        ctx' <- runProgressBar (-0.5) (mkCtx noInput)
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 15 15 0 70) testColour]

  describe "button" $ do
    controlBehaviourSpec runButton (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runButton

    describe "rendering" $ do
      it "draws the label" $ do
        ctx' <- runButton (mkCtx noInput)
        drawnTexts ctx' `shouldContain` ["label"]

    describe "click behaviour" $ do
      forM_ insidePoints $ \(desc, pt) ->
        it ("is clicked when the mouse is released " <> desc) $ do
          result <- fst <$> runUI (button TestControl "label") (withButtonReleased (mkCtx (mouseAt pt False [])))
          result `shouldBe` True

      forM_ outsidePoints $ \(desc, pt) ->
        it ("is not clicked when the mouse is released " <> desc) $ do
          result <- fst <$> runUI (button TestControl "label") (withButtonReleased (mkCtx (mouseAt pt False [])))
          result `shouldBe` False

      it "is clicked when Enter is pressed and the button has focus" $ do
        result <- fst <$> runUI (button TestControl "label") (mkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] })
        result `shouldBe` True

      it "is not clicked when Enter is pressed and the button does not have focus" $ do
        result <- fst <$> runUI (button TestControl "label") (withFocus (Just OtherControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        result `shouldBe` False

      it "is not clicked when Tab and Enter are pressed simultaneously" $ do
        result <- fst <$> runUI (button TestControl "label")
          (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyTab [], KeyEvent KeyReturn []] }))
        result `shouldBe` False

    describe "disabled" $ do
      it "is not activated by a click when disabled" $ do
        result <- fst <$> runUI (disableWhen True (button TestControl "label")) (withButtonReleased (mkCtx (mouseAt (Point 50 50) False [])))
        result `shouldBe` False

      it "is not activated by Enter when disabled" $ do
        result <- fst <$> runUI (disableWhen True (button TestControl "label")) (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        result `shouldBe` False

  describe "checkbox" $ do
    controlBehaviourSpec runCheckboxControl boxPoint

    describe "toggle behaviour" $ do
      it "dispatches True when the box is clicked while unchecked" $ do
        ctx' <- runCheckbox False (withButtonReleased (mkCheckboxCtx (mouseAt boxPoint False [])))
        applyDispatches ctx' `shouldBe` Just True

      it "dispatches False when the box is clicked while checked" $ do
        ctx' <- runCheckbox True (withButtonReleased (mkCheckboxCtx (mouseAt boxPoint False [])))
        applyDispatches ctx' `shouldBe` Just False

      it "dispatches toggle when Enter is pressed while focused" $ do
        ctx' <- runCheckbox False (withFocus (Just TestControl) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        applyDispatches ctx' `shouldBe` Just True

      it "dispatches toggle when Space is pressed while focused" $ do
        ctx' <- runCheckbox False (withFocus (Just TestControl) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeySpace []] }))
        applyDispatches ctx' `shouldBe` Just True

      it "does not dispatch when clicked outside the box" $ do
        ctx' <- runCheckbox False (withButtonReleased (mkCheckboxCtx (mouseAt (Point 50 50) False [])))
        applyDispatches ctx' `shouldBe` Nothing

      it "does not dispatch when Enter is pressed while unfocused" $ do
        ctx' <- runCheckbox False (withFocus (Just OtherControl) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        applyDispatches ctx' `shouldBe` Nothing

    describe "disabled" $ do
      it "does not dispatch when clicked while disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (checkbox TestControl "test label" False (\v _ -> Just v))) (withButtonReleased (mkCheckboxCtx (mouseAt boxPoint False [])))
        applyDispatches ctx' `shouldBe` Nothing

      it "does not dispatch when Enter is pressed while disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (checkbox TestControl "test label" False (\v _ -> Just v))) (withFocus (Just TestControl) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        applyDispatches ctx' `shouldBe` Nothing

    describe "rendering" $ do
      it "draws the checkmark when checked" $ do
        ctx' <- runCheckbox True (mkCheckboxCtx noInput)
        drawnTexts ctx' `shouldContain` ["✓"]

      it "does not draw the checkmark when unchecked" $ do
        ctx' <- runCheckbox False (mkCheckboxCtx noInput)
        drawnTexts ctx' `shouldNotContain` ["✓"]

      it "draws the label text" $ do
        ctx' <- runCheckbox False (mkCheckboxCtx noInput)
        drawnTexts ctx' `shouldContain` ["test label"]

    describe "focus ring" $ do
      it "draws a focus ring around the full control when focused" $ do
        ctx' <- runCheckbox False (withFocus (Just TestControl) (mkCheckboxCtx noInput) { ctxTheme = focusBorderTheme })
        getDrawCommands ctx' `shouldContain` [StrokeRect controlRect testBorderColour 1]

      it "does not draw a focus ring when unfocused" $ do
        ctx' <- runCheckbox False (withFocus (Just OtherControl) (mkCheckboxCtx noInput) { ctxTheme = focusBorderTheme })
        getDrawCommands ctx' `shouldNotContain` [StrokeRect controlRect testBorderColour 1]

  describe "textInput" $ do
    controlBehaviourSpec runTextInputControl (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runTextInputControl

    describe "rendering" $ do
      it "displays the value without a cursor when unfocused" $ do
        ctx' <- runTextInput "hello" (withFocus (Just OtherControl) (mkTextCtx "hello" noInput))
        drawnTexts ctx' `shouldContain` ["hello"]

      it "displays the value with a cursor when focused" $ do
        ctx' <- runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput))
        drawnTexts ctx' `shouldContain` ["hello"]
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 15 15 1 70) testColour]

    describe "text editing" $ do
      it "appends typed characters to the value" $ do
        ctx' <- runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["!"] }))
        applyDispatches ctx' `shouldBe` "hello!"

      it "removes the last character on backspace" $ do
        ctx' <- runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyBackspace []] }))
        applyDispatches ctx' `shouldBe` "hell"

      it "does not dispatch when backspace is pressed on an empty value" $ do
        ctx' <- runTextInput "" (withFocus (Just TestControl) (mkTextCtx "" noInput { inputKeyEvents = [KeyEvent KeyBackspace []] }))
        dispatchCount ctx' `shouldBe` 0

      it "does not dispatch when there is no input" $ do
        ctx' <- runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput))
        dispatchCount ctx' `shouldBe` 0

      it "does not process input when unfocused" $ do
        ctx' <- runTextInput "hello" (withFocus (Just OtherControl) (mkTextCtx "hello" noInput { inputTypedText = ["!"], inputKeyEvents = [KeyEvent KeyBackspace []] }))
        dispatchCount ctx' `shouldBe` 0

    describe "disabled" $ do
      it "does not process input when disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (textInput TestControl "hello" (\t _ -> t))) (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["!"] }))
        dispatchCount ctx' `shouldBe` 0

      it "does not show a cursor when focused and disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (textInput TestControl "hello" (\t _ -> t))) (withFocus (Just TestControl) (mkTextCtx "hello" noInput))
        getDrawCommands ctx' `shouldNotContain` [FillRect (Rectangle 15 15 1 70) testColour]

    describe "cursor placement" $ do
      it "sets the cursor to the clicked position on mouse press" $ do
        -- noOpTextMeasurer maps every offset to 0, so any click → position 0
        ctx' <- runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" (mouseAt (Point 50 50) True [])))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 0 0]

      it "extends the active end on drag while keeping anchor" $ do
        -- First frame: click starts drag; second frame: drag extends selection.
        frame1 <- runTextInput "hello" (withFocus (Just TestControl) (mkTextCtx "hello" (mouseAt (Point 50 50) True [])))
        frame2 <- fmap snd $ runUI (textInput TestControl "hello" (\t _ -> t))
                    (nextFrameContext controlRect (mouseAt (Point 70 50) True []) frame1)
        -- With noOpTextMeasurer both positions are 0, so selection is (0,0); the
        -- key check is that anchor was NOT reset on the second frame.
        case Map.lookup TestControl (elmSelections (ctxElements frame2)) of
          Just [Selection a _] -> a `shouldBe` 0
          other                -> expectationFailure $ "expected Just [Selection a _], got: " <> show other

    describe "arrow navigation" $ do
      let withSel a v ctx = ctx { ctxElements = (ctxElements ctx) { elmSelections = Map.singleton TestControl [Selection a v] } }

      it "moves cursor left with Left" $ do
        ctx' <- runTextInput "hello" (withSel 3 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyLeft []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 2 2]

      it "moves cursor right with Right" $ do
        ctx' <- runTextInput "hello" (withSel 2 2 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyRight []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 3]

      it "collapses selection to low end on plain Left" $ do
        ctx' <- runTextInput "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyLeft []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 1 1]

      it "collapses selection to high end on plain Right" $ do
        ctx' <- runTextInput "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyRight []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 3]

      it "extends selection left with Shift+Left" $ do
        ctx' <- runTextInput "hello" (withSel 3 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyLeft [Shift]] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 2]

      it "extends selection right with Shift+Right" $ do
        ctx' <- runTextInput "hello" (withSel 3 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyRight [Shift]] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 4]

      it "does not move cursor past the beginning" $ do
        ctx' <- runTextInput "hello" (withSel 0 0 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyLeft []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 0 0]

      it "does not move cursor past the end" $ do
        ctx' <- runTextInput "hello" (withSel 5 5 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyRight []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 5 5]

    describe "selection editing" $ do
      let withSel a v ctx = ctx { ctxElements = (ctxElements ctx) { elmSelections = Map.singleton TestControl [Selection a v] } }

      it "deletes the selected range on backspace" $ do
        ctx' <- runTextInput "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyBackspace []] })))
        applyDispatches ctx' `shouldBe` "hlo"

      it "replaces the selected range with typed text" $ do
        ctx' <- runTextInput "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["X"] })))
        applyDispatches ctx' `shouldBe` "hXlo"

      it "collapses cursor to insertion point after replacing selection" $ do
        ctx' <- runTextInput "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["XY"] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 3]

  describe "scrollBar" $ do
    describe "button stepping" $ do
      it "steps forward by the thumb ratio when the increment button is clicked" $ do
        ctx' <- runScrollBar (withButtonReleased (mkScrollBarCtx 0.5 (mouseAt (Point 10 190) False [])))
        scrollPos ctx' `shouldBe` 0.75

      it "steps back by the thumb ratio when the decrement button is clicked" $ do
        ctx' <- runScrollBar (withButtonReleased (mkScrollBarCtx 0.5 (mouseAt (Point 10 10) False [])))
        scrollPos ctx' `shouldBe` 0.25

      it "clamps to 1 when stepping forward near the end" $ do
        ctx' <- runScrollBar (withButtonReleased (mkScrollBarCtx 0.9 (mouseAt (Point 10 190) False [])))
        scrollPos ctx' `shouldBe` 1

      it "clamps to 0 when stepping back near the start" $ do
        ctx' <- runScrollBar (withButtonReleased (mkScrollBarCtx 0.1 (mouseAt (Point 10 10) False [])))
        scrollPos ctx' `shouldBe` 0

    describe "track dragging" $ do
      it "centres the thumb on the cursor while the track is pressed" $ do
        ctx' <- runScrollBar (mkScrollBarCtx 0 (mouseAt (Point 10 100) True []))
        scrollPos ctx' `shouldBe` 0.5

      it "continues tracking when the mouse moves off the track while the button is held" $ do
        frame1 <- runScrollBar (mkScrollBarCtx 0 (mouseAt (Point 10 100) True []))
        frame2 <- fmap snd $ runUI (scrollBar id Vertical 0.25)
                                   (nextFrameContext scrollRect (mouseAt (Point 200 40) True []) frame1)
        scrollPos frame2 `shouldBe` 0.0

      it "stops tracking when the button is released after dragging off the track" $ do
        frame1 <- runScrollBar (mkScrollBarCtx 0 (mouseAt (Point 10 100) True []))
        frame2 <- fmap snd $ runUI (scrollBar id Vertical 0.25)
                                   (nextFrameContext scrollRect (mouseAt (Point 200 40) False []) frame1)
        scrollPos frame2 `shouldBe` 0.5

    describe "without interaction" $ do
      it "leaves the position unchanged" $ do
        ctx' <- runScrollBar (mkScrollBarCtx 0.5 noInput)
        scrollPos ctx' `shouldBe` 0.5

  describe "slider" $ do
    controlBehaviourSpec runSliderControl (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runSliderControl

    describe "drag interaction" $ do
      it "sets value to 0.5 when dragged to the midpoint" $ do
        ctx' <- runSlider Horizontal 0 (mouseAt (Point 100 15) True [])
        applyDispatches ctx' `shouldBe` 0.5

      it "sets value to 0 when dragged to the far left" $ do
        ctx' <- runSlider Horizontal 0.5 (mouseAt (Point 15 15) True [])
        applyDispatches ctx' `shouldBe` 0.0

      it "sets value to 1 when dragged to the far right" $ do
        ctx' <- runSlider Horizontal 0.5 (mouseAt (Point 185 15) True [])
        applyDispatches ctx' `shouldBe` 1.0

      it "continues tracking when the mouse moves outside the track while button held" $ do
        frame1 <- runSlider Horizontal 0 (mouseAt (Point 100 15) True [])
        let val1 = applyDispatches frame1
        frame2 <- fmap snd $ runUI (slider id Horizontal val1 (\v _ -> v))
                                   (nextFrameContext sliderRect (mouseAt (Point 300 15) True []) frame1)
        applyDispatches frame2 `shouldBe` 1.0

      it "stops tracking when the button is released" $ do
        frame1 <- runSlider Horizontal 0 (mouseAt (Point 100 15) True [])
        let val1 = applyDispatches frame1
        frame2 <- fmap snd $ runUI (slider id Horizontal val1 (\v _ -> v))
                                   (nextFrameContext sliderRect (mouseAt (Point 300 15) False []) frame1)
        dispatchCount frame2 `shouldBe` 0

    describe "keyboard nudging" $ do
      it "increases value by 0.05 when Right is pressed (Horizontal)" $ do
        ctx' <- runSlider Horizontal 0.5 noInput { inputKeyEvents = [KeyEvent KeyRight []] }
        applyDispatches ctx' `shouldBe` 0.55

      it "decreases value by 0.05 when Left is pressed (Horizontal)" $ do
        ctx' <- runSlider Horizontal 0.5 noInput { inputKeyEvents = [KeyEvent KeyLeft []] }
        applyDispatches ctx' `shouldBe` 0.45

      it "increases value by 0.05 when Down is pressed (Vertical)" $ do
        ctx' <- runSlider Vertical 0.5 noInput { inputKeyEvents = [KeyEvent KeyDown []] }
        applyDispatches ctx' `shouldBe` 0.55

      it "decreases value by 0.05 when Up is pressed (Vertical)" $ do
        ctx' <- runSlider Vertical 0.5 noInput { inputKeyEvents = [KeyEvent KeyUp []] }
        applyDispatches ctx' `shouldBe` 0.45

      it "clamps to 1 when nudging at the maximum" $ do
        ctx' <- runSlider Horizontal 1.0 noInput { inputKeyEvents = [KeyEvent KeyRight []] }
        applyDispatches ctx' `shouldBe` 1.0

      it "clamps to 0 when nudging at the minimum" $ do
        ctx' <- runSlider Horizontal 0.0 noInput { inputKeyEvents = [KeyEvent KeyLeft []] }
        applyDispatches ctx' `shouldBe` 0.0

      it "does not nudge when another element has focus" $ do
        ctx' <- fmap snd $ runUI (slider id Horizontal 0.5 (\v _ -> v))
          (withSliderFocus (Just SliderThumb) (emptyUIContext sliderRect noInput { inputKeyEvents = [KeyEvent KeyRight []] } sliderTheme 0.5 noOpTextMeasurer))
        applyDispatches ctx' `shouldBe` 0.5

      it "does not nudge when disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (slider id Horizontal 0.5 (\v _ -> v)))
          (withSliderFocus (Just SliderTrack) (emptyUIContext sliderRect noInput { inputKeyEvents = [KeyEvent KeyRight []] } sliderTheme 0.5 noOpTextMeasurer))
        dispatchCount ctx' `shouldBe` 0

    describe "without interaction" $ do
      it "does not dispatch when there is no input" $ do
        ctx' <- runSlider Horizontal 0.5 noInput
        dispatchCount ctx' `shouldBe` 0

  describe "radioGroup" $ do
    controlBehaviourSpec runRadioControl (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runRadioControl

    describe "selection" $ do
      it "dispatches the value of a clicked item" $ do
        ctx' <- runRadioGroup "a" (withButtonReleased (mkRadioGroupCtx "a" (mouseAt (Point 50 45) False [])))
        applyDispatches ctx' `shouldBe` "b"

      it "dispatches the correct value when the last item is clicked" $ do
        ctx' <- runRadioGroup "a" (withButtonReleased (mkRadioGroupCtx "a" (mouseAt (Point 50 75) False [])))
        applyDispatches ctx' `shouldBe` "c"

      it "dispatches the value when Enter is pressed while an item is focused" $ do
        ctx' <- fmap snd $ runUI (radioGroup id radioItems "a" (\v _ -> v))
          (withItemFocus (Just 1) (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent KeyReturn []] } radioGroupTheme "a" noOpTextMeasurer))
        applyDispatches ctx' `shouldBe` "b"

      it "dispatches the value when Space is pressed while an item is focused" $ do
        ctx' <- fmap snd $ runUI (radioGroup id radioItems "a" (\v _ -> v))
          (withItemFocus (Just 2) (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent KeySpace []] } radioGroupTheme "a" noOpTextMeasurer))
        applyDispatches ctx' `shouldBe` "c"

      it "does not dispatch when no item is focused and a key is pressed" $ do
        ctx' <- fmap snd $ runUI (radioGroup id radioItems "a" (\v _ -> v))
          (withItemFocus (Just 99) (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent KeyReturn []] } radioGroupTheme "a" noOpTextMeasurer))
        dispatchCount ctx' `shouldBe` 0

      it "does not dispatch when there is no interaction" $ do
        ctx' <- runRadioGroup "b" (mkRadioGroupCtx "b" noInput)
        dispatchCount ctx' `shouldBe` 0

    describe "keyboard navigation" $ do
      let nav focusIdx k = do
            ctx' <- fmap snd $ runUI (radioGroup id radioItems "a" (\v _ -> v))
              (withItemFocus (Just focusIdx)
                (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent k []] } radioGroupTheme "a" noOpTextMeasurer))
            pure $ focusedElement (ixnFocus (ctxInteraction ctx'))

      it "moves focus to the next item when Down is pressed" $ do
        result <- nav 0 KeyDown
        result `shouldBe` Just 1

      it "moves focus to the previous item when Up is pressed" $ do
        result <- nav 1 KeyUp
        result `shouldBe` Just 0

      it "stays on the last item when Down is pressed at the end" $ do
        result <- nav 2 KeyDown
        result `shouldBe` Just 2

      it "stays on the first item when Up is pressed at the beginning" $ do
        result <- nav 0 KeyUp
        result `shouldBe` Just 0

      it "does not move focus when disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (radioGroup id radioItems "a" (\v _ -> v)))
          (withItemFocus (Just 0) (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent KeyDown []] } radioGroupTheme "a" noOpTextMeasurer))
        focusedElement (ixnFocus (ctxInteraction ctx')) `shouldBe` Just 0

    describe "rendering" $ do
      it "shows the selected mark on the selected item" $ do
        ctx' <- runRadioGroup "b" (mkRadioGroupCtx "b" noInput)
        drawnTexts ctx' `shouldContain` ["● Beta"]

      it "shows the unselected mark on other items" $ do
        ctx' <- runRadioGroup "b" (mkRadioGroupCtx "b" noInput)
        drawnTexts ctx' `shouldContain` ["○ Alpha"]
        drawnTexts ctx' `shouldContain` ["○ Gamma"]

      it "displays all labels regardless of selection" $ do
        ctx' <- runRadioGroup "a" (mkRadioGroupCtx "a" noInput)
        length (drawnTexts ctx') `shouldBe` 3

  describe "scrollableRegion" $ do
    describe "interaction clipping" $ do
      it "does not hover a child item when the mouse is over the horizontal scrollbar strip" $ do
        ctx' <- runScrollableRegion (Point 100 92)
        ixnHovered (ctxInteraction ctx') `shouldNotBe` Just SRChild

      it "hovers the child item when the mouse is within the viewport" $ do
        ctx' <- runScrollableRegion (Point 100 42)
        ixnHovered (ctxInteraction ctx') `shouldBe` Just SRChild

  -- Geometry: Rectangle 0 0 100 200 (vertical) / Rectangle 0 0 200 100 (horizontal)
  -- thumbH/thumbW = trackLen * ratio; range = trackLen - thumbH/W
  describe "thumbRect" $ do
    describe "Vertical" $ do
      let r = Rectangle 0 0 100 200
      it "places the thumb at the top when pos=0" $
        thumbRect Vertical 0 0.5 r `shouldBe` Rectangle 0 0 100 100
      it "places the thumb at the bottom when pos=1" $
        thumbRect Vertical 1 0.5 r `shouldBe` Rectangle 0 100 100 100
      it "centres the thumb at pos=0.5" $
        thumbRect Vertical 0.5 0.25 r `shouldBe` Rectangle 0 75 100 50
      it "thumb fills the track when ratio=1" $
        thumbRect Vertical 0 1 r `shouldBe` r
      it "produces a zero-height thumb when ratio=0" $
        rectHeight (thumbRect Vertical 0 0 r) `shouldBe` 0

    describe "Horizontal" $ do
      let r = Rectangle 0 0 200 100
      it "places the thumb at the left when pos=0" $
        thumbRect Horizontal 0 0.5 r `shouldBe` Rectangle 0 0 100 100
      it "places the thumb at the right when pos=1" $
        thumbRect Horizontal 1 0.5 r `shouldBe` Rectangle 100 0 100 100

  describe "mouseToTrackPos" $ do
    describe "Vertical" $ do
      let r = Rectangle 0 0 100 200; ratio = 0.5
      -- thumbH=100, range=100; pos = clamp 0 1 ((mouseY - thumbH/2) / range)
      it "returns 0 when the cursor is at the thumb-centre for pos=0" $
        mouseToTrackPos Vertical ratio r (Point 50 50) `shouldBe` 0
      it "returns 0.5 when the cursor is in the middle" $
        mouseToTrackPos Vertical ratio r (Point 50 100) `shouldBe` 0.5
      it "returns 1 when the cursor is at the thumb-centre for pos=1" $
        mouseToTrackPos Vertical ratio r (Point 50 150) `shouldBe` 1
      it "clamps to 0 when the cursor is above the track" $
        mouseToTrackPos Vertical ratio r (Point 50 0) `shouldBe` 0
      it "clamps to 1 when the cursor is below the track" $
        mouseToTrackPos Vertical ratio r (Point 50 200) `shouldBe` 1
      it "returns 0 when ratio=1 (no range)" $
        mouseToTrackPos Vertical 1 r (Point 50 100) `shouldBe` 0

    describe "Horizontal" $ do
      let r = Rectangle 0 0 200 100; ratio = 0.5
      -- thumbW=100, range=100; pos = clamp 0 1 ((mouseX - thumbW/2) / range)
      it "returns 0 when the cursor is at the thumb-centre for pos=0" $
        mouseToTrackPos Horizontal ratio r (Point 50 50) `shouldBe` 0
      it "returns 0.5 when the cursor is in the middle" $
        mouseToTrackPos Horizontal ratio r (Point 100 50) `shouldBe` 0.5
      it "returns 1 when the cursor is at the thumb-centre for pos=1" $
        mouseToTrackPos Horizontal ratio r (Point 150 50) `shouldBe` 1

  describe "thumbRect (square thumb)" $ do
    describe "Horizontal" $ do
      -- ratio=30/200=0.15, thumbW=30, range=170
      let r = Rectangle 0 0 200 30
      it "places the thumb at the left when pos=0" $
        thumbRect Horizontal 0 0.15 r `shouldBe` Rectangle 0 0 30 30
      it "places the thumb at the right when pos=1" $
        thumbRect Horizontal 1 0.15 r `shouldBe` Rectangle 170 0 30 30
      it "places the thumb in the middle at pos=0.5" $
        thumbRect Horizontal 0.5 0.15 r `shouldBe` Rectangle 85 0 30 30
      it "thumb fills a square track when ratio=1" $
        thumbRect Horizontal 0.5 1 (Rectangle 0 0 30 30) `shouldBe` Rectangle 0 0 30 30

    describe "Vertical" $ do
      -- ratio=30/200=0.15, thumbH=30, range=170
      let r = Rectangle 0 0 30 200
      it "places the thumb at the top when pos=0" $
        thumbRect Vertical 0 0.15 r `shouldBe` Rectangle 0 0 30 30
      it "places the thumb at the bottom when pos=1" $
        thumbRect Vertical 1 0.15 r `shouldBe` Rectangle 0 170 30 30
      it "places the thumb in the middle at pos=0.5" $
        thumbRect Vertical 0.5 0.15 r `shouldBe` Rectangle 0 85 30 30

  describe "scrollRegionBarSize" $
    it "is 16" $
      scrollRegionBarSize `shouldBe` 16

