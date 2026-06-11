{-# LANGUAGE OverloadedStrings #-}
module Blink.ControlsSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Data.Text (Text)
import Blink.Controls (ScrollBarPart (..), ScrollState (..), StandardControls (..), button, checkbox, progressBar, scrollBar, textInput)
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
