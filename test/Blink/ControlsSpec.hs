{-# LANGUAGE OverloadedStrings #-}
module Blink.ControlsSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Data.Text (Text)
import Blink.Controls (ProgressValue (..), ScrollBarPart (..), ScrollRegionPart (..), SliderPart (..), activatable, button, checkbox, checkboxMark, control, focusRing, isControlHit, mouseToTrackPos, progressBar, radioGroup, rangeControl, scrollBar, scrollableRegion, scrollRegionBarSize, selector, slider, textInput, thumbRect, virtualContent)
import Blink.Geometry (Orientation (..), Point (..), Rectangle (..), Size (..), insetRect, noBorder, uniform, uniformBorder)
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
testStyleWithBorder = testStyle { styleBorderColour = Just testBorderColour, styleBorderEdges = uniformBorder 1 }

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
isStrokeRect (StrokeBorder {}) = True
isStrokeRect _                 = False

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

    it "retains focus on the previously focused element when a drag releases elsewhere" $ do
      -- OtherControl has focus; TestControl is being dragged (captured). On the
      -- drag-release frame the focused element must re-assert its own focus so it
      -- is not cleared by nextFocusFrame on the following frame.
      let base = withFocus (Just OtherControl) (withButtonReleased (mkCtx (mouseAt hitPoint False [])))
          ctx  = base { ctxInteraction = (ctxInteraction base) { ixnCaptured = Just OtherControl } }
      ctx' <- run ctx
      getFocused ctx' `shouldBe` Just OtherControl

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

    it "does not consume Tab or lose focus when disabled while focused" $ do
      ctx' <- disabledRun (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyTab []] }))
      getFocused ctx' `shouldBe` Just TestControl

    it "does not hand focus to the previous tab stop on Shift+Tab when disabled while focused" $ do
      let base = withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyTab [Shift]] })
      ctx' <- disabledRun (base { ctxInteraction = (ctxInteraction base) { ixnPrevTabStop = Just OtherControl } })
      getFocused ctx' `shouldBe` Just TestControl

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
    getDrawCommands ctx' `shouldContain` [StrokeBorder bgRect testBorderColour (uniformBorder 1)]

  it "draws a border even when the background is transparent" $ do
    ctx' <- run ((mkCtx noInput) { ctxTheme = transparentBgWithBorderTheme })
    getDrawCommands ctx' `shouldContain` [StrokeBorder bgRect testBorderColour (uniformBorder 1)]

runProgressBar :: Double -> WidgetRunner
runProgressBar value ctx = fmap snd $ runUI (progressBar TestControl (Progress value)) ctx

runButton :: WidgetRunner
runButton ctx = fmap snd $ runUI (button TestControl "label") ctx

runActivatable :: WidgetRunner
runActivatable ctx = fmap snd $ runUI (activatable TestControl (drawText testColour AlignCenter "x") [KeyReturn]) ctx

runTextInputControl :: WidgetRunner
runTextInputControl ctx = fmap snd $ runUI (textInput TestControl "" (\_ s -> s)) ctx

-- Text editing tests use the entered text itself as the application state.
mkTextCtx :: Text -> InputState -> UIContext TestElement Text
mkTextCtx value input = emptyUIContext controlRect input testTheme value noOpTextMeasurer

-- A measurer with a fixed 20px advance per character, for scroll tests where
-- noOpTextMeasurer's all-zero offsets can't produce a cursor position past
-- the viewport edge.
fixedCharWidth :: TextMeasurer
fixedCharWidth = TextMeasurer
  { tmCharOffset   = \_ n -> pure (fromIntegral n * 20)
  , tmCharAtOffset = \_ x -> pure (round (x / 20))
  , tmTextSize     = \_ -> pure (Size 0 0)
  }

mkTextCtxWith :: TextMeasurer -> Text -> InputState -> UIContext TestElement Text
mkTextCtxWith measurer value input = emptyUIContext controlRect input testTheme value measurer

runTextInput :: Text -> UIContext TestElement Text -> IO (UIContext TestElement Text)
runTextInput value ctx = fmap snd $ runUI (textInput TestControl value (\t _ -> t)) ctx

-- checkboxMark setup: zero margin/padding so the full controlRect is hittable.
-- App state is Maybe Bool so dispatches can be observed.
checkboxMarkTheme :: Theme TestElement
checkboxMarkTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = zeroMarginStyleSet }

mkCheckboxMarkCtx :: InputState -> UIContext TestElement (Maybe Bool)
mkCheckboxMarkCtx input = emptyUIContext controlRect input checkboxMarkTheme Nothing noOpTextMeasurer

runCheckboxMark :: Bool -> UIContext TestElement (Maybe Bool) -> IO (UIContext TestElement (Maybe Bool))
runCheckboxMark checked ctx = fmap snd $ runUI (checkboxMark TestControl checked (\v _ -> Just v)) ctx

runCheckboxMarkControl :: WidgetRunner
runCheckboxMarkControl ctx = fmap snd $ runUI (checkboxMark TestControl False (\_ s -> s)) (ctx { ctxTheme = checkboxMarkTheme })

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

runRangeControl :: WidgetRunner
runRangeControl ctx = fmap snd $ runUI (rangeControl TestControl OtherControl Vertical 0 1) ctx

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

  describe "activatable" $ do
    controlBehaviourSpec runActivatable (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runActivatable

    describe "draw action" $ do
      it "runs the supplied draw action" $ do
        ctx' <- runActivatable (mkCtx noInput)
        drawnTexts ctx' `shouldContain` ["x"]

    describe "activation" $ do
      forM_ insidePoints $ \(desc, pt) ->
        it ("activates when the mouse is released " <> desc) $ do
          result <- fst <$> runUI (activatable TestControl (pure ()) [KeyReturn]) (withButtonReleased (mkCtx (mouseAt pt False [])))
          result `shouldBe` True

      forM_ outsidePoints $ \(desc, pt) ->
        it ("does not activate when the mouse is released " <> desc) $ do
          result <- fst <$> runUI (activatable TestControl (pure ()) [KeyReturn]) (withButtonReleased (mkCtx (mouseAt pt False [])))
          result `shouldBe` False

      it "activates when a listed key is pressed while focused" $ do
        result <- fst <$> runUI (activatable TestControl (pure ()) [KeyReturn])
          (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        result `shouldBe` True

      it "does not activate when an unlisted key is pressed while focused" $ do
        result <- fst <$> runUI (activatable TestControl (pure ()) [KeyReturn])
          (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeySpace []] }))
        result `shouldBe` False

      it "does not activate when a listed key is pressed without focus" $ do
        result <- fst <$> runUI (activatable TestControl (pure ()) [KeyReturn])
          (withFocus (Just OtherControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        result `shouldBe` False

    describe "disabled" $ do
      it "does not activate on click when disabled" $ do
        result <- fst <$> runUI (disableWhen True (activatable TestControl (pure ()) [KeyReturn]))
          (withButtonReleased (mkCtx (mouseAt (Point 50 50) False [])))
        result `shouldBe` False

      it "does not activate on a listed key when disabled" $ do
        result <- fst <$> runUI (disableWhen True (activatable TestControl (pure ()) [KeyReturn]))
          (withFocus (Just TestControl) (mkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        result `shouldBe` False

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

  describe "checkboxMark" $ do
    controlBehaviourSpec runCheckboxMarkControl (Point 50 50)

    describe "rendering" $ do
      it "draws the checkmark when checked" $ do
        ctx' <- runCheckboxMark True (mkCheckboxMarkCtx noInput)
        drawnTexts ctx' `shouldContain` ["✓"]

      it "does not draw the checkmark when unchecked" $ do
        ctx' <- runCheckboxMark False (mkCheckboxMarkCtx noInput)
        drawnTexts ctx' `shouldNotContain` ["✓"]

    describe "toggle behaviour" $ do
      it "dispatches True when clicked while unchecked" $ do
        ctx' <- runCheckboxMark False (withButtonReleased (mkCheckboxMarkCtx (mouseAt (Point 50 50) False [])))
        applyDispatches ctx' `shouldBe` Just True

      it "dispatches False when clicked while checked" $ do
        ctx' <- runCheckboxMark True (withButtonReleased (mkCheckboxMarkCtx (mouseAt (Point 50 50) False [])))
        applyDispatches ctx' `shouldBe` Just False

      it "dispatches toggle when Enter is pressed while focused" $ do
        ctx' <- runCheckboxMark False (withFocus (Just TestControl) (mkCheckboxMarkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        applyDispatches ctx' `shouldBe` Just True

      it "dispatches toggle when Space is pressed while focused" $ do
        ctx' <- runCheckboxMark False (withFocus (Just TestControl) (mkCheckboxMarkCtx noInput { inputKeyEvents = [KeyEvent KeySpace []] }))
        applyDispatches ctx' `shouldBe` Just True

      it "does not dispatch when there is no interaction" $ do
        ctx' <- runCheckboxMark False (mkCheckboxMarkCtx noInput)
        applyDispatches ctx' `shouldBe` Nothing

    describe "disabled" $ do
      it "does not dispatch when clicked while disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (checkboxMark TestControl False (\v _ -> Just v)))
          (withButtonReleased (mkCheckboxMarkCtx (mouseAt (Point 50 50) False [])))
        applyDispatches ctx' `shouldBe` Nothing

      it "does not dispatch when Enter is pressed while disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (checkboxMark TestControl False (\v _ -> Just v)))
          (withFocus (Just TestControl) (mkCheckboxMarkCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        applyDispatches ctx' `shouldBe` Nothing

  describe "focusRing" $ do
    it "draws the focused style's border around the current bounds when focused" $ do
      ctx' <- fmap snd $ runUI (focusRing TestControl)
        ((withFocus (Just TestControl) (mkCtx noInput)) { ctxTheme = focusBorderTheme })
      getDrawCommands ctx' `shouldContain` [StrokeBorder controlRect testBorderColour (uniformBorder 1)]

    it "draws nothing when unfocused" $ do
      ctx' <- fmap snd $ runUI (focusRing TestControl)
        ((withFocus (Just OtherControl) (mkCtx noInput)) { ctxTheme = focusBorderTheme })
      getDrawCommands ctx' `shouldBe` []

    it "draws nothing when focused but the style has no border colour" $ do
      ctx' <- fmap snd $ runUI (focusRing TestControl) (withFocus (Just TestControl) (mkCtx noInput))
      getDrawCommands ctx' `shouldBe` []

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
        getDrawCommands ctx' `shouldContain` [StrokeBorder controlRect testBorderColour (uniformBorder 1)]

      it "does not draw a focus ring when unfocused" $ do
        ctx' <- runCheckbox False (withFocus (Just OtherControl) (mkCheckboxCtx noInput) { ctxTheme = focusBorderTheme })
        getDrawCommands ctx' `shouldNotContain` [StrokeBorder controlRect testBorderColour (uniformBorder 1)]

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

    describe "focus persistence" $ do
      let withSel a v ctx = ctx { ctxElements = (ctxElements ctx) { elmSelections = Map.singleton TestControl [Selection a v] } }

      it "leaves the selection unchanged on a frame where the control is not focused" $ do
        ctx' <- runTextInput "hello" (withSel 1 3 (withFocus (Just OtherControl) (mkTextCtx "hello" noInput)))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 1 3]

      it "restores the previous selection when focus returns without a click" $ do
        -- No withFocus: focus starts at Nothing, so TestControl auto-focuses
        -- this frame via the same path as gaining focus by Tab, not by click.
        ctx' <- runTextInput "hello" (withSel 2 4 (mkTextCtx "hello" noInput))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 2 4]

    describe "scrolling" $ do
      let withSel a v ctx = ctx { ctxElements = (ctxElements ctx) { elmSelections = Map.singleton TestControl [Selection a v] } }
          withScrollX x ctx = ctx { ctxElements = (ctxElements ctx) { elmScrollStates = Map.singleton TestControl (ScrollState x) } }
          textScrollX ctx = scrollPosition (Map.findWithDefault (ScrollState 0) TestControl (elmScrollStates (ctxElements ctx)))

      it "scrolls right to keep the cursor visible when it moves past the right edge" $ do
        -- Content width is 70px; fixedCharWidth puts the cursor at 100px
        -- (index 5 * 20px), past the visible window.
        let base = withSel 5 5 (withFocus (Just TestControl) (mkTextCtxWith fixedCharWidth "hello" noInput))
        ctx' <- runTextInput "hello" base
        textScrollX ctx' `shouldBe` 31

      it "scrolls left to keep the cursor visible when it moves before the left edge" $ do
        let base = withScrollX 50 (withSel 0 0 (withFocus (Just TestControl) (mkTextCtxWith fixedCharWidth "hello" noInput)))
        ctx' <- runTextInput "hello" base
        textScrollX ctx' `shouldBe` 0

  describe "rangeControl" $ do
    controlBehaviourSpec runRangeControl (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runRangeControl

    describe "drag interaction" $ do
      it "returns the drag position via mouseToTrackPos while dragging" $ do
        result <- fst <$> runUI (rangeControl TestControl OtherControl Vertical 0 0.5)
          (mkCtx (mouseAt (Point 50 50) True []))
        result `shouldBe` Just 0.5

      it "returns Nothing when the button is not held" $ do
        result <- fst <$> runUI (rangeControl TestControl OtherControl Vertical 0 0.5)
          (mkCtx (mouseAt (Point 50 50) False []))
        result `shouldBe` Nothing

      it "returns Nothing when not dragging even if the button is held elsewhere" $ do
        result <- fst <$> runUI (rangeControl TestControl OtherControl Vertical 0 0.5)
          (mkCtx (mouseAt (Point 200 200) True []))
        result `shouldBe` Nothing

    describe "thumb placement" $ do
      it "renders the thumb's chrome within the track's content rectangle when the thumb fills the track" $ do
        drawCtx <- fmap snd $ runUI (rangeControl TestControl OtherControl Vertical 0 1) (mkCtx noInput)
        -- With pos=0, ratio=1 the thumb fills the track's content rectangle
        -- exactly, so the thumb's own margin-inset background lands here.
        getDrawCommands drawCtx `shouldContain` [FillRect (insetRect (uniform 10) contentRect) testColour]

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

      -- Regression: releasing the mouse while it is still over the track (as
      -- opposed to having dragged off it) must not dispatch a further change.
      -- The arrow-key nudge check used to treat any click release as
      -- satisfying both the decrement and increment keys at once, queuing a
      -- spurious +step jump on release.
      it "does not dispatch on the release frame when the mouse is still over the track" $ do
        frame1 <- runSlider Horizontal 0 (mouseAt (Point 100 15) True [])
        let val1 = applyDispatches frame1
        frame2 <- fmap snd $ runUI (slider id Horizontal val1 (\v _ -> v))
                                   (nextFrameContext sliderRect (mouseAt (Point 100 15) False []) frame1)
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

  describe "selector" $ do
    let renderItem :: Int -> Bool -> (String, Text) -> UI Int String ()
        renderItem _eid isSelected (_val, lbl) =
          drawText testColour AlignLeft ((if isSelected then "SEL:" else "UNSEL:") <> lbl)

        runSelector :: String -> UIContext Int String -> IO (UIContext Int String)
        runSelector sel = fmap snd . runUI (selector id radioItems sel (\v _ -> v) renderItem)

    describe "selection" $ do
      it "dispatches the value of a clicked item" $ do
        ctx' <- runSelector "a" (withButtonReleased (mkRadioGroupCtx "a" (mouseAt (Point 50 45) False [])))
        applyDispatches ctx' `shouldBe` "b"

      it "dispatches the value when Enter is pressed while an item is focused" $ do
        ctx' <- fmap snd $ runUI (selector id radioItems "a" (\v _ -> v) renderItem)
          (withItemFocus (Just 1) (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent KeyReturn []] } radioGroupTheme "a" noOpTextMeasurer))
        applyDispatches ctx' `shouldBe` "b"

      it "does not dispatch when clicked while disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (selector id radioItems "a" (\v _ -> v) renderItem))
          (withButtonReleased (mkRadioGroupCtx "a" (mouseAt (Point 50 45) False [])))
        dispatchCount ctx' `shouldBe` 0

      it "does not dispatch when there is no interaction" $ do
        ctx' <- runSelector "b" (mkRadioGroupCtx "b" noInput)
        dispatchCount ctx' `shouldBe` 0

    describe "keyboard navigation" $ do
      let nav focusIdx k = do
            ctx' <- fmap snd $ runUI (selector id radioItems "a" (\v _ -> v) renderItem)
              (withItemFocus (Just focusIdx)
                (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent k []] } radioGroupTheme "a" noOpTextMeasurer))
            pure $ focusedElement (ixnFocus (ctxInteraction ctx'))

      it "moves focus to the next item when Down is pressed" $ do
        result <- nav 0 KeyDown
        result `shouldBe` Just 1

      it "moves focus to the previous item when Up is pressed" $ do
        result <- nav 1 KeyUp
        result `shouldBe` Just 0

      it "does not move focus when disabled" $ do
        ctx' <- fmap snd $ runUI (disableWhen True (selector id radioItems "a" (\v _ -> v) renderItem))
          (withItemFocus (Just 0) (emptyUIContext radioGroupRect noInput { inputKeyEvents = [KeyEvent KeyDown []] } radioGroupTheme "a" noOpTextMeasurer))
        focusedElement (ixnFocus (ctxInteraction ctx')) `shouldBe` Just 0

    describe "rendering" $ do
      it "passes isSelected=True for the selected item" $ do
        ctx' <- runSelector "b" (mkRadioGroupCtx "b" noInput)
        drawnTexts ctx' `shouldContain` ["SEL:Beta"]

      it "passes isSelected=False for other items" $ do
        ctx' <- runSelector "b" (mkRadioGroupCtx "b" noInput)
        drawnTexts ctx' `shouldContain` ["UNSEL:Alpha"]
        drawnTexts ctx' `shouldContain` ["UNSEL:Gamma"]

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

      it "handles arrow keys on the frame focus is gained by click" $ do
        -- Click on item 0 (centre Point 50 15) and press Down in the same frame
        -- with no prior focus. The newly focused item should handle the key.
        ctx' <- fmap snd $ runUI (radioGroup id radioItems "a" (\v _ -> v))
          (withButtonReleased (emptyUIContext radioGroupRect
            noInput { inputMousePosition = Point 50 15, inputKeyEvents = [KeyEvent KeyDown []] }
            radioGroupTheme "a" noOpTextMeasurer))
        focusedElement (ixnFocus (ctxInteraction ctx')) `shouldBe` Just 1

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

  describe "virtualContent" $ do
    -- Viewport is controlRect: 100×100. Each item marks itself with a
    -- FillRect whose colour encodes its index, so a test can assert both
    -- which indices were rendered and at what rectangle.
    let marker :: Int -> Rectangle -> DrawCommand
        marker i r = FillRect r (RGBA (fromIntegral i) 0 0 1)
        runVirtualContent pos itemH count =
          fmap snd $ runUI (virtualContent pos itemH count (\i -> fillRect (RGBA (fromIntegral i) 0 0 1))) (mkCtx noInput)

    it "renders items starting from the top when unscrolled" $ do
      -- itemHeight 20, viewport 100 tall -> exactly 5 full items fit.
      ctx' <- runVirtualContent 0 20 10
      getDrawCommands ctx' `shouldContain` [marker 0 (Rectangle 0 0 100 20)]
      getDrawCommands ctx' `shouldContain` [marker 4 (Rectangle 0 80 100 20)]
      getDrawCommands ctx' `shouldNotContain` [marker 5 (Rectangle 0 100 100 20)]

    it "clips the first item by the fractional scroll offset" $ do
      -- scrollPos 25 with itemHeight 20 -> first visible item is index 1,
      -- pushed up 5px above the viewport top.
      ctx' <- runVirtualContent 25 20 10
      getDrawCommands ctx' `shouldContain` [marker 1 (Rectangle 0 (-5) 100 20)]
      getDrawCommands ctx' `shouldNotContain` [marker 0 (Rectangle 0 0 100 20)]

    it "renders one extra item to cover the clipped final row" $ do
      ctx' <- runVirtualContent 25 20 10
      -- 6 items are needed to cover a 100px viewport once offset by 5px.
      getDrawCommands ctx' `shouldContain` [marker 6 (Rectangle 0 95 100 20)]

    it "does not render past the last item" $ do
      ctx' <- runVirtualContent 0 20 3
      getDrawCommands ctx' `shouldContain` [marker 2 (Rectangle 0 40 100 20)]
      getDrawCommands ctx' `shouldNotContain` [marker 3 (Rectangle 0 60 100 20)]

    it "renders nothing when there are no items" $ do
      ctx' <- runVirtualContent 0 20 0
      [c | c@(FillRect _ _) <- getDrawCommands ctx'] `shouldBe` []

    it "clips content to the current bounds" $ do
      ctx' <- runVirtualContent 0 20 10
      getDrawCommands ctx' `shouldContain` [PushClip controlRect]

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

  -- isControlHit uses testTheme (margin = uniform 10), so:
  --   bgRect = Rectangle 10 10 80 80 within controlRect = Rectangle 0 0 100 100
  describe "isControlHit" $ do
    it "returns True when the mouse is inside the background rect" $ do
      result <- fst <$> runUI (isControlHit TestControl) (mkCtx (mouseAt (Point 50 50) False []))
      result `shouldBe` True

    it "returns False when the mouse is in the margin area" $ do
      result <- fst <$> runUI (isControlHit TestControl) (mkCtx (mouseAt (Point 5 5) False []))
      result `shouldBe` False

    it "returns False when the mouse is outside the bounds" $ do
      result <- fst <$> runUI (isControlHit TestControl) (mkCtx (mouseAt (Point 200 200) False []))
      result `shouldBe` False

