module Blink.UISpec (spec) where

import Data.IORef
import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Geometry (Point (..), Rectangle (..), uniform)
import Blink.Input (ButtonState (..), InputState (..))
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.Controls (control)
import Blink.UI

data TwoElems = ElemA | ElemB deriving (Eq, Ord, Show)

twoElemTheme :: Theme TwoElems
twoElemTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = emptyStyleSet }

noInput :: InputState
noInput = InputState
  { inputMousePosition = Point 0 0
  , inputLeftButton = ButtonUp
  , inputKeyEvents = []
  , inputTypedText = []
  }

emptyStyle :: Style
emptyStyle = Style
  { styleBackground = RGBA 0 0 0 1
  , styleTextColour = RGBA 0 0 0 1
  , styleTextAlign = AlignCenter
  , styleMargin = uniform 0
  , stylePadding = uniform 0
  , styleBorderColour = Nothing
  , styleBorderWidth = 0
  }

emptyStyleSet :: StyleSet
emptyStyleSet = StyleSet
  { styleSetNormal = emptyStyle
  , styleSetHovered = emptyStyle
  , styleSetPressed = emptyStyle
  , styleSetFocused = emptyStyle
  , styleSetDisabled = emptyStyle
  }

emptyTheme :: Theme ()
emptyTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle = emptyStyleSet
  }

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

run :: UI () s a -> s -> (a, UIContext () s)
run ui s = runUI ui (emptyUIContext testBounds noInput emptyTheme s)

spec :: Spec
spec = describe "UI primitives" $ do
  describe "clipToCurrent" $ do
    -- In each test the control runs with testBounds (100×100) so the mouse is
    -- inside the element's own bounds; only the clip region should block hover.
    it "does not register hover when the mouse is inside bounds but outside the clip region" $
      let clipRect = Rectangle 0 0 100 50
          mouseOutsideClip = noInput { inputMousePosition = Point 50 75 }
          ctx = emptyUIContext testBounds mouseOutsideClip emptyTheme (0 :: Int)
          (_, ctx') = runUI (withBounds clipRect $ clipToCurrent $ withBounds testBounds $ control () (pure ())) ctx
      in ctxHoveredElement ctx' `shouldBe` Nothing

    it "registers hover when the mouse is inside both the bounds and the clip region" $
      let clipRect = Rectangle 0 0 100 50
          mouseInsideClip = noInput { inputMousePosition = Point 50 25 }
          ctx = emptyUIContext testBounds mouseInsideClip emptyTheme (0 :: Int)
          (_, ctx') = runUI (withBounds clipRect $ clipToCurrent $ withBounds testBounds $ control () (pure ())) ctx
      in ctxHoveredElement ctx' `shouldBe` Just ()

    it "intersects nested clip regions" $
      let outerClip = Rectangle 0 0 100 50
          innerClip = Rectangle 0 25 100 50
          -- intersection is y 25–50; mouse at (50, 10) is inside outerClip but outside intersection
          mouseOutside = noInput { inputMousePosition = Point 50 10 }
          ctx = emptyUIContext testBounds mouseOutside emptyTheme (0 :: Int)
          (_, ctx') = runUI
            (withBounds outerClip $ clipToCurrent $
             withBounds innerClip $ clipToCurrent $
             withBounds testBounds $ control () (pure ())) ctx
      in ctxHoveredElement ctx' `shouldBe` Nothing

  describe "hover suppression during drag" $ do
    let mouseOnA = noInput { inputMousePosition = Point 50 50, inputLeftButton = ButtonDown }

    it "does not hover an element when another element holds capture" $
      let ctx = (emptyUIContext testBounds mouseOnA twoElemTheme (0 :: Int))
                  { ctxCapturedElement = Just ElemB }
          (_, ctx') = runUI (control ElemA (pure ())) ctx
      in ctxHoveredElement ctx' `shouldBe` Nothing

    it "hovers an element when it is itself the captured element" $
      let ctx = (emptyUIContext testBounds mouseOnA twoElemTheme (0 :: Int))
                  { ctxCapturedElement = Just ElemA }
          (_, ctx') = runUI (control ElemA (pure ())) ctx
      in ctxHoveredElement ctx' `shouldBe` Just ElemA

    it "hovers an element when no capture is active" $
      let ctx = emptyUIContext testBounds mouseOnA twoElemTheme (0 :: Int)
          (_, ctx') = runUI (control ElemA (pure ())) ctx
      in ctxHoveredElement ctx' `shouldBe` Just ElemA

  it "getAppState returns the frame's starting state" $
    fst (run getAppState (42 :: Int)) `shouldBe` 42

  it "getAppState still sees the pre-dispatch state later in the same frame" $
    fst (run (dispatch (+ 1) >> getAppState) (0 :: Int)) `shouldBe` 0

  it "applyDispatches applies modifiers in dispatch order" $
    applyDispatches (snd (run (dispatch (+ 1) >> dispatch (* 10)) (0 :: Int))) `shouldBe` 10

  it "applyDispatches returns the starting state when nothing was dispatched" $
    applyDispatches (snd (run (pure ()) (0 :: Int))) `shouldBe` 0

  it "dispatchAsync queues the job without running it" $ do
    ref <- newIORef False
    let job s = writeIORef ref True >> pure (const s)
        ctx = snd (run (dispatchAsync job) (0 :: Int))
    length (getAsyncJobs ctx) `shouldBe` 1
    ran <- readIORef ref
    ran `shouldBe` False

  it "nextFrameContext clears queued dispatches and async jobs" $
    let ctx = snd (run (dispatch (+ 1) >> dispatchAsync (\s -> pure (const s))) (0 :: Int))
        ctx' = nextFrameContext testBounds noInput ctx
    in (applyDispatches ctx', length (getAsyncJobs ctx')) `shouldBe` (0, 0)

  describe "nextFrameContext capture" $ do
    let captured ctx = ctx { ctxCapturedElement = Just () }
        buttonDown  = noInput { inputLeftButton = ButtonDown }
        buttonRel   = noInput { inputLeftButton = ButtonReleased }

    it "auto-acquires capture when an element is hovered while the button is down" $
      -- Acquisition happens in setHovered during the frame, not via nextFrameContext.
      let ctx = snd (runUI (setHovered ()) (emptyUIContext testBounds buttonDown emptyTheme (0 :: Int)))
      in ctxCapturedElement ctx `shouldBe` Just ()

    it "carries existing capture forward on subsequent ButtonDown frames" $
      let ctx = nextFrameContext testBounds buttonDown (captured (snd (run (pure ()) (0 :: Int))))
      in ctxCapturedElement ctx `shouldBe` Just ()

    it "carries capture through ButtonReleased so focus logic can inspect it" $
      let ctx = nextFrameContext testBounds buttonRel (captured (snd (run (pure ()) (0 :: Int))))
      in ctxCapturedElement ctx `shouldBe` Just ()

    it "clears capture on ButtonUp" $
      let ctx = nextFrameContext testBounds noInput (captured (snd (run (pure ()) (0 :: Int))))
      in ctxCapturedElement ctx `shouldBe` Nothing
