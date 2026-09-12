{-# LANGUAGE OverloadedStrings #-}
module Blink.AppSpec (spec) where

import Control.Monad (void, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..), uniform)
import Blink.Input (Key (..), KeyEvent (..), InputState (..))
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), emptyTheme, noBorder)
import Blink.View
import Blink.View.Drawing (fillRect, drawText)
import Blink.Element (Element, elLayout, elementWithLayout)
import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Control (control, defaultControlConfig, elementId, onFocusGained, onFocusLost, post, postWith, resolve)
import Blink.Controls.ToggleButton (isSelected, onSelectedChanged)
import qualified Blink.Controls.Slider as Slider
import qualified Blink.Controls.TextInput as TextInput
import Blink.Update (modify)

-- | Every test app below fills the whole test bounds; only the body of the
-- wrapped action varies per app.
fullView :: View e msg a -> Element e msg
fullView = elementWithLayout (Layout fill fill TopLeft) . void

-- Test infrastructure

mkInput :: Bool -> Bool -> FrameInput
mkInput quit animTick = FrameInput
  { mousePosition   = Point 0 0
  , mouseButtonDown = False
  , keyEvents       = []
  , typedText       = []
  , windowSize      = Size 100 100
  , quitRequested   = quit
  , isAnimationTick = animTick
  }

normalInput :: FrameInput
normalInput = mkInput False False

nullMeasurer :: TextMeasurer
nullMeasurer = noOpTextMeasurer

testStyle :: Style
testStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignLeft
  , styleBorderColour = Nothing
  }

testMetrics :: Metrics
testMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet { styleBase = testStyle, styleOverrides = mempty }

resultState :: FrameResult s -> s
resultState (Continue _ s) = s
resultState (Quit _ s)     = s

resultDraws :: FrameResult s -> [DrawCommand]
resultDraws (Continue ds _) = ds
resultDraws (Quit ds _)     = ds

drawnTexts :: FrameResult s -> [Text]
drawnTexts r = [t | DrawText _ t _ _ <- resultDraws r]

isContinue :: FrameResult s -> Bool
isContinue (Continue _ _) = True
isContinue _              = False

isQuit :: FrameResult s -> Bool
isQuit (Quit _ _) = True
isQuit _          = False

-- Test apps

counterApp :: App () (Int -> Int) Int
counterApp = App
  { startUp        = pure 0

  , theme          = const (emptyTheme (testMetrics, testStyleSet))
  , view           = \_ -> fullView (emit (+1))
  , update         = modify
  }

-- Emits a FillRect covering the full window bounds each frame.
drawingApp :: Colour -> App () () ()
drawingApp c = App
  { startUp        = pure ()

  , theme          = const (emptyTheme (testMetrics, testStyleSet))
  , view           = \_ -> fullView (fillRect c)
  , update         = \_ -> pure ()
  }

-- Dispatches (+1) and also draws the current app state as text.
-- The drawn value differs between continuous (pre-dispatch) and
-- event-driven (post-dispatch) modes.
stateDrawApp :: App () (Int -> Int) Int
stateDrawApp = App
  { startUp        = pure 0

  , theme          = const (emptyTheme (testMetrics, testStyleSet))
  , view           = \n -> fullView $ do
      emit (+1)
      drawText (RGBA 0 0 0 1) AlignLeft (T.pack (show n))
  , update         = modify
  }

-- Dispatches the number of key events seen this frame.
keyCountApp :: App () (Int -> Int) Int
keyCountApp = App
  { startUp        = pure 0

  , theme          = const (emptyTheme (testMetrics, testStyleSet))
  , view           = \_ -> fullView $ do
      input <- getInput
      emit (+ length (inputKeyEvents input))
  , update         = modify
  }

-- Reads scroll state as a counter, increments it, and dispatches the old value as app state.
uiStateApp :: App () (Int -> Int) Int
uiStateApp = App
  { startUp = pure 0
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \_ -> fullView $ do
      pos <- getScrollState ()
      requestScrollTo () (pos + 1)
      emit (\_ -> round pos)
  , update  = modify
  }

-- Emits two messages in one frame: appends "a" then "b" to the state.
multiEmitApp :: App () (String -> String) String
multiEmitApp = App
  { startUp        = pure ""

  , theme          = const (emptyTheme (testMetrics, testStyleSet))
  , view           = \_ -> fullView $ do
      emit (++ "a")
      emit (++ "b")
  , update         = modify
  }

-- Dispatches the animation delta as state so it can be observed.
deltaApp :: App () (Float -> Float) Float
deltaApp = App
  { startUp        = pure 999

  , theme          = const (emptyTheme (testMetrics, testStyleSet))
  , view           = \_ -> fullView $ do
      d <- getAnimDelta
      emit (const d)
  , update         = modify
  }

-- Acquires mouse capture unconditionally (standing in for a control that
-- captured on an earlier press, e.g. a drag) and draws whether it still
-- holds capture this frame, including the single frame the button comes up
-- on — the frame 'isDragging' is meant to still read 'True' for, so a
-- control can tell a drag-release apart from the button simply being up.
captureApp :: App () () ()
captureApp = App
  { startUp        = pure ()
  , theme          = const (emptyTheme (testMetrics, testStyleSet))
  , view           = \_ -> fullView $ do
      acquireCapture ()
      dragging <- isDragging ()
      drawText (RGBA 0 0 0 1) AlignLeft (if dragging then "dragging" else "idle")
  , update         = \_ -> pure ()
  }

mouseInput :: Bool -> FrameInput
mouseInput down = normalInput { mouseButtonDown = down }

-- | Wires a real 'checkbox' (not a toy view built from 'drawText'/'emit')
-- into an 'App', so clicking it is driven through 'Blink.App''s actual
-- frame orchestration -- real message-fold via 'update' and, in
-- event-driven mode, the real second render pass -- rather than only
-- through "Blink.Interaction"'s own simplified stepping loop, which every
-- other control test in this suite uses and which doesn't run either of
-- those. Every other app above exists to isolate one piece of that
-- orchestration; this one instead checks that a real control participates
-- in it correctly end to end.
checkboxApp :: App () (Bool -> Bool) Bool
checkboxApp = App
  { startUp = pure False
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \checked ->
      (checkbox () [isSelected checked, onSelectedChanged (postWith (\b -> (const b)))])
        { elLayout = Layout fill fill TopLeft }
  , update  = modify
  }

-- | Same rationale as 'checkboxApp', for a drag-driven continuous control
-- rather than a discrete toggle.
sliderApp :: App () (Double -> Double) Double
sliderApp = App
  { startUp = pure 0
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \v ->
      (Slider.slider () [Slider.value v, Slider.onValueChanged (postWith (\v' -> const v'))])
        { elLayout = Layout fill fill TopLeft }
  , update  = modify
  }

-- | Same rationale as 'checkboxApp', for a control driven by a stream of
-- typed-text frames rather than a single click.
textInputApp :: App () (Text -> Text) Text
textInputApp = App
  { startUp = pure ""
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \t ->
      (TextInput.textInput () [TextInput.value t, TextInput.onInput (postWith (\t' -> const t'))])
        { elLayout = Layout fill fill TopLeft }
  , update  = modify
  }

pointerAt :: Point -> Bool -> FrameInput
pointerAt p down = normalInput { mousePosition = p, mouseButtonDown = down }

typedInput :: Text -> FrameInput
typedInput t = normalInput { typedText = [t] }

tabInput :: FrameInput
tabInput = normalInput { keyEvents = [KeyEvent KeyTab [] False] }

data FocusElem = FocusA | FocusB deriving (Eq, Ord, Show)

-- | Same rationale as 'checkboxApp', for the third interaction shape none
-- of the above cover: a focus change, which (unlike a click or typed
-- character) is deferred by 'Blink.View' to the frame after the input that
-- triggered it, and delivers different messages to two different
-- elements ("A lost" / "B gained") from the same 'update' fold.
focusApp :: App FocusElem String [String]
focusApp = App
  { startUp = pure []
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \_ -> fullView $ do
      withBounds (Rectangle 0 0 50 100) $ void $ control $ resolve defaultControlConfig
        [ elementId FocusA
        , onFocusGained (post "A gained")
        , onFocusLost   (post "A lost")
        ]
      withBounds (Rectangle 50 0 50 100) $ void $ control $ resolve defaultControlConfig
        [ elementId FocusB
        , onFocusGained (post "B gained")
        ]
  , update  = \m -> modify (++ [m])
  }

-- A measurer that counts how many times a text size was requested, so a
-- test can observe how many times a frame's view actually ran.
countingMeasurer :: IORef Int -> TextMeasurer
countingMeasurer ref = noOpTextMeasurer
  { tmTextSize = \_ -> modifyIORef' ref (+1) >> pure (Size 0 0) }

-- Measures text every render (making render count observable via
-- 'countingMeasurer') and emits a message only when @emits@ is True.
viewCountApp :: Bool -> App () () ()
viewCountApp emits = App
  { startUp        = pure ()
  , theme          = const (emptyTheme (testMetrics, testStyleSet))
  , view           = \_ -> fullView $ do
      _ <- measureText "x"
      when emits (emit ())
  , update         = \_ -> pure ()
  }

spec :: Spec
spec = do
  describe "App integration" $ do
    describe "configureContinuous" $ do
      it "a normal frame returns Continue" $ do
        handle <- configureContinuous counterApp nullMeasurer
        result <- stepFrame handle normalInput
        isContinue result `shouldBe` True

      it "dispatched modifiers are applied to produce the frame state" $ do
        handle <- configureContinuous counterApp nullMeasurer
        result <- stepFrame handle normalInput
        resultState result `shouldBe` 1

      it "returns Quit when quitRequested is True" $ do
        handle <- configureContinuous counterApp nullMeasurer
        result <- stepFrame handle (mkInput True False)
        isQuit result `shouldBe` True

      it "draw commands from the view appear in the result" $ do
        let c = RGBA 1 0 0 1
        handle <- configureContinuous (drawingApp c) nullMeasurer
        result <- stepFrame handle normalInput
        resultDraws result `shouldContain` [FillRect (Rectangle 0 0 100 100) c]

      it "state accumulates correctly across multiple frames" $ do
        handle <- configureContinuous counterApp nullMeasurer
        _ <- stepFrame handle normalInput
        _ <- stepFrame handle normalInput
        r3 <- stepFrame handle normalInput
        resultState r3 `shouldBe` 3

      it "draw commands reflect the pre-dispatch app state" $ do
        handle <- configureContinuous stateDrawApp nullMeasurer
        result <- stepFrame handle normalInput
        drawnTexts result `shouldContain` ["0"]

      it "messages emitted in one frame are folded in emission order" $ do
        handle <- configureContinuous multiEmitApp nullMeasurer
        result <- stepFrame handle normalInput
        resultState result `shouldBe` "ab"

    describe "configureEventDriven" $ do
      it "a normal frame returns Continue" $ do
        handle <- configureEventDriven counterApp (pure ()) nullMeasurer
        result <- stepFrame handle normalInput
        isContinue result `shouldBe` True

      -- counterApp emits on every render regardless of input. Event-driven
      -- mode renders twice whenever a message is pending, so this counts
      -- twice per input event.
      it "dispatched modifiers are applied to produce the frame state" $ do
        handle <- configureEventDriven counterApp (pure ()) nullMeasurer
        result <- stepFrame handle normalInput
        resultState result `shouldBe` 2

      it "returns Quit when quitRequested is True" $ do
        handle <- configureEventDriven counterApp (pure ()) nullMeasurer
        result <- stepFrame handle (mkInput True False)
        isQuit result `shouldBe` True

      it "draw commands reflect the post-dispatch app state" $ do
        handle <- configureEventDriven stateDrawApp (pure ()) nullMeasurer
        result <- stepFrame handle normalInput
        drawnTexts result `shouldContain` ["1"]

      it "key events are not replayed in the second render pass" $ do
        handle <- configureEventDriven keyCountApp (pure ()) nullMeasurer
        let oneKey = normalInput { keyEvents = [KeyEvent KeyReturn [] False] }
        result <- stepFrame handle oneKey
        resultState result `shouldBe` 1

    describe "frame context progression" $ do
      it "view state written in frame N is readable in frame N+1" $ do
        handle <- configureContinuous uiStateApp nullMeasurer
        r1 <- stepFrame handle normalInput
        r2 <- stepFrame handle normalInput
        (resultState r1, resultState r2) `shouldBe` (0, 1)

      it "animation delta is 0 on non-tick frames" $ do
        handle <- configureContinuous deltaApp nullMeasurer
        result <- stepFrame handle normalInput
        resultState result `shouldBe` 0.0

    describe "capture across the render passes" $ do
      it "continuous mode's single pass still shows capture on the release frame" $ do
        handle <- configureContinuous captureApp nullMeasurer
        _      <- stepFrame handle (mouseInput True)
        result <- stepFrame handle (mouseInput False)
        drawnTexts result `shouldContain` ["dragging"]

      it "event-driven mode's second pass still shows capture on the release frame" $ do
        handle <- configureEventDriven captureApp (pure ()) nullMeasurer
        _      <- stepFrame handle (mouseInput True)
        result <- stepFrame handle (mouseInput False)
        drawnTexts result `shouldContain` ["dragging"]

    describe "second pass is skipped when nothing was queued" $ do
      it "runs the view once when a frame emits nothing" $ do
        ref    <- newIORef 0
        handle <- configureEventDriven (viewCountApp False) (pure ()) (countingMeasurer ref)
        _      <- stepFrame handle normalInput
        readIORef ref `shouldReturn` 1

      it "runs the view twice when a frame emits a message" $ do
        ref    <- newIORef 0
        handle <- configureEventDriven (viewCountApp True) (pure ()) (countingMeasurer ref)
        _      <- stepFrame handle normalInput
        readIORef ref `shouldReturn` 2

    describe "a real control driven through the actual frame loop" $ do
      it "clicking a real checkbox toggles the app's state and is reflected in that same click's draw commands" $ do
        handle <- configureEventDriven checkboxApp (pure ()) nullMeasurer
        _      <- stepFrame handle (pointerAt (Point 50 50) True)
        result <- stepFrame handle (pointerAt (Point 50 50) False)
        resultState result `shouldBe` True
        drawnTexts result `shouldContain` ["\10003"]

      it "dragging a real slider updates the app's state through the real update fold" $ do
        handle <- configureEventDriven sliderApp (pure ()) nullMeasurer
        result <- stepFrame handle (pointerAt (Point 50 50) True)
        resultState result `shouldBe` 0.5

      it "typing into a real text field updates the app's state through the real update fold" $ do
        handle <- configureEventDriven textInputApp (pure ()) nullMeasurer
        _      <- stepFrame handle normalInput -- claims focus, selects the (empty) value
        result <- stepFrame handle (typedInput "hi")
        resultState result `shouldBe` "hi"

      it "tabbing focus between two real controls updates the app's state through the real update fold" $ do
        handle <- configureEventDriven focusApp (pure ()) nullMeasurer
        _      <- stepFrame handle normalInput -- FocusA auto-claims
        result <- stepFrame handle tabInput
        resultState result `shouldBe` ["A gained", "A lost", "B gained"]

      -- Clicking B queues its focus change as a UiEffect that settles
      -- during the second render pass within this same stepFrame call.
      it "clicking to focus a real control still reaches update when it settles on the second pass" $ do
        handle <- configureEventDriven focusApp (pure ()) nullMeasurer
        _      <- stepFrame handle normalInput -- FocusA auto-claims
        result <- stepFrame handle (pointerAt (Point 75 50) True)
        resultState result `shouldBe` ["A gained", "A lost", "B gained"]
