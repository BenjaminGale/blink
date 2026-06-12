{-# LANGUAGE OverloadedStrings #-}
module Blink.AppSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.Geometry (Point (..), Rectangle (..), Size (..), uniform)
import Blink.Input (ButtonState (..), Key (..), KeyEvent (..), InputState (..))
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Style (Style (..), StyleSet (..), emptyTheme)
import Blink.UI

-- Test infrastructure

mkInput :: Bool -> Bool -> FrameInput
mkInput quit animTick = FrameInput
  { mousePosition   = Point 0 0
  , mouseButton     = ButtonUp
  , keyEvents       = []
  , typedText       = []
  , windowSize      = Size 100 100
  , quitRequested   = quit
  , isAnimationTick = animTick
  }

normalInput :: FrameInput
normalInput = mkInput False False

nullMeasurer :: TextMeasurer
nullMeasurer = TextMeasurer
  { measureFont  = \_ -> pure (FontMetrics 0 0 0)
  , measureText  = \_ _ -> pure (Size 0 0)
  , charOffset   = \_ _ _ -> pure 0
  , charAtOffset = \_ _ _ -> pure 0
  }

testStyle :: Style
testStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignLeft
  , styleMargin       = uniform 0
  , stylePadding      = uniform 0
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

counterApp :: App () () Int
counterApp = App
  { startUp        = pure 0
  , initialUIState = ()
  , theme          = const (emptyTheme testStyleSet)
  , view           = dispatch (+1)
  }

-- Emits a FillRect covering the full window bounds each frame.
drawingApp :: Colour -> App () () ()
drawingApp c = App
  { startUp        = pure ()
  , initialUIState = ()
  , theme          = const (emptyTheme testStyleSet)
  , view           = fillRect c
  }

-- Dispatches (+1) and also draws the current app state as text.
-- The drawn value differs between continuous (pre-dispatch) and
-- event-driven (post-dispatch) modes.
stateDrawApp :: App () () Int
stateDrawApp = App
  { startUp        = pure 0
  , initialUIState = ()
  , theme          = const (emptyTheme testStyleSet)
  , view           = do
      n <- getAppState
      dispatch (+1)
      drawText (RGBA 0 0 0 1) AlignLeft (T.pack (show n))
  }

-- Dispatches the number of key events seen this frame.
keyCountApp :: App () () Int
keyCountApp = App
  { startUp        = pure 0
  , initialUIState = ()
  , theme          = const (emptyTheme testStyleSet)
  , view           = do
      input <- getInput
      dispatch (+ length (inputKeyEvents input))
  }

-- Dispatches the current UI state (Int) as app state, then increments it.
uiStateApp :: App () Int Int
uiStateApp = App
  { startUp        = pure 0
  , initialUIState = 0
  , theme          = const (emptyTheme testStyleSet)
  , view           = do
      uiSt <- getUIState
      modifyUIState (+1)
      dispatch (const uiSt)
  }

-- Dispatches the animation delta as state so it can be observed.
deltaApp :: App () () Float
deltaApp = App
  { startUp        = pure 999
  , initialUIState = ()
  , theme          = const (emptyTheme testStyleSet)
  , view           = do
      d <- getAnimDelta
      dispatch (const d)
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

    describe "async dispatch" $ do
      it "an async modifier is applied at the start of the next frame" $ do
        done <- newEmptyMVar
        let asyncApp = App
              { startUp        = pure 0
              , initialUIState = ()
              , theme          = const (emptyTheme testStyleSet)
              , view           = do
                  s <- getAppState
                  when (s == 0) $ dispatchAsync $ \_ -> do
                    putMVar done ()
                    pure (+10)
              } :: App () () Int
        handle <- configureContinuous asyncApp nullMeasurer
        _ <- stepFrame handle normalInput
        takeMVar done
        r2 <- stepFrame handle normalInput
        resultState r2 `shouldBe` 10

      it "the notify callback is called when an async job completes" $ do
        notified <- newIORef False
        done     <- newEmptyMVar
        let notify = writeIORef notified True >> putMVar done ()
            asyncApp = App
              { startUp        = pure ()
              , initialUIState = ()
              , theme          = const (emptyTheme testStyleSet)
              , view           = dispatchAsync $ \_ -> pure id
              } :: App () () ()
        handle <- configureEventDriven asyncApp notify nullMeasurer
        _ <- stepFrame handle normalInput
        takeMVar done
        readIORef notified `shouldReturn` True

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
