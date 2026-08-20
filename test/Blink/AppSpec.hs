{-# LANGUAGE OverloadedStrings #-}
module Blink.AppSpec (spec) where

import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.Geometry (Point (..), Rectangle (..), Size (..), uniform)
import Blink.Input (Key (..), KeyEvent (..), InputState (..))
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Style (Style (..), StyleSet (..), emptyTheme, noBorder)
import Blink.UI
import Blink.Update (modify)

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
  , styleMargin       = uniform 0
  , stylePadding      = uniform 0
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

  , theme          = const (emptyTheme testStyleSet)
  , view           = \_ -> emit (+1)
  , update         = modify
  }

-- Emits a FillRect covering the full window bounds each frame.
drawingApp :: Colour -> App () () ()
drawingApp c = App
  { startUp        = pure ()

  , theme          = const (emptyTheme testStyleSet)
  , view           = \_ -> fillRect c
  , update         = \_ -> pure ()
  }

-- Dispatches (+1) and also draws the current app state as text.
-- The drawn value differs between continuous (pre-dispatch) and
-- event-driven (post-dispatch) modes.
stateDrawApp :: App () (Int -> Int) Int
stateDrawApp = App
  { startUp        = pure 0

  , theme          = const (emptyTheme testStyleSet)
  , view           = \n -> do
      emit (+1)
      drawText (RGBA 0 0 0 1) AlignLeft (T.pack (show n))
  , update         = modify
  }

-- Dispatches the number of key events seen this frame.
keyCountApp :: App () (Int -> Int) Int
keyCountApp = App
  { startUp        = pure 0

  , theme          = const (emptyTheme testStyleSet)
  , view           = \_ -> do
      input <- getInput
      emit (+ length (inputKeyEvents input))
  , update         = modify
  }

-- Reads scroll state as a counter, increments it, and dispatches the old value as app state.
uiStateApp :: App () (Int -> Int) Int
uiStateApp = App
  { startUp = pure 0
  , theme   = const (emptyTheme testStyleSet)
  , view    = \_ -> do
      pos <- getScrollState ()
      emitUi (ScrollTo () (pos + 1))
      emit (\_ -> round pos)
  , update  = modify
  }

-- Emits two messages in one frame: appends "a" then "b" to the state.
multiEmitApp :: App () (String -> String) String
multiEmitApp = App
  { startUp        = pure ""

  , theme          = const (emptyTheme testStyleSet)
  , view           = \_ -> do
      emit (++ "a")
      emit (++ "b")
  , update         = modify
  }

-- Dispatches the animation delta as state so it can be observed.
deltaApp :: App () (Float -> Float) Float
deltaApp = App
  { startUp        = pure 999

  , theme          = const (emptyTheme testStyleSet)
  , view           = \_ -> do
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
  , theme          = const (emptyTheme testStyleSet)
  , view           = \_ -> do
      acquireCapture ()
      dragging <- isDragging ()
      drawText (RGBA 0 0 0 1) AlignLeft (if dragging then "dragging" else "idle")
  , update         = \_ -> pure ()
  }

mouseInput :: Bool -> FrameInput
mouseInput down = normalInput { mouseButtonDown = down }

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
  , theme          = const (emptyTheme testStyleSet)
  , view           = \_ -> do
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

      it "dispatched modifiers are applied to produce the frame state" $ do
        handle <- configureEventDriven counterApp (pure ()) nullMeasurer
        result <- stepFrame handle normalInput
        resultState result `shouldBe` 1

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
        let oneKey = normalInput { keyEvents = [KeyEvent KeyReturn []] }
        result <- stepFrame handle oneKey
        resultState result `shouldBe` 1

    describe "frame context progression" $ do
      it "UI state written in frame N is readable in frame N+1" $ do
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
