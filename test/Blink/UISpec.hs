{-# LANGUAGE OverloadedStrings #-}
module Blink.UISpec (spec) where

import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, choose)

import Blink.Geometry (Point (..), Rectangle (..), uniform)
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.Controls (control)
import Blink.UI
import Blink.Generators ()

data TwoElems = ElemA | ElemB deriving (Eq, Ord, Show)

twoElemTheme :: Theme TwoElems
twoElemTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = emptyStyleSet }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 0 0
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

buttonDown :: InputState
buttonDown = noInput { inputLeftButtonDown = True }

mouseOnCenter :: InputState
mouseOnCenter = noInput { inputMousePosition = Point 50 50 }

mouseOnCenterDown :: InputState
mouseOnCenterDown = noInput { inputMousePosition = Point 50 50, inputLeftButtonDown = True }

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

run :: UI () s a -> s -> IO (a, UIContext () s)
run ui s = runUI ui (emptyUIContext testBounds noInput emptyTheme s noOpTextMeasurer)

runWith :: InputState -> UI () Int a -> IO (a, UIContext () Int)
runWith input ui = runUI ui (emptyUIContext testBounds input emptyTheme (0 :: Int) noOpTextMeasurer)

runTwoElem :: UI TwoElems Int a -> IO (a, UIContext TwoElems Int)
runTwoElem ui = runUI ui (emptyUIContext testBounds noInput twoElemTheme (0 :: Int) noOpTextMeasurer)

freshCtx :: IO (UIContext () Int)
freshCtx = snd <$> run (pure ()) (0 :: Int)

withCapture :: e -> UIContext e s -> UIContext e s
withCapture e ctx = ctx { ctxInteraction = (ctxInteraction ctx) { ixnCaptured = Just e } }

spec :: Spec
spec = describe "Blink.UI" $ do
  describe "clipToCurrent" $ do
    -- In each test the control runs with testBounds (100×100) so the mouse is
    -- inside the element's own bounds; only the clip region should block hover.
    it "does not register hover when the mouse is inside bounds but outside the clip region" $ do
      let clipRect = Rectangle 0 0 100 50
          mouseOutsideClip = noInput { inputMousePosition = Point 50 75 }
      (_, ctx') <- runWith mouseOutsideClip
        (withBounds clipRect $ clipToCurrent $ withBounds testBounds $ control () (pure ()))
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

    it "registers hover when the mouse is inside both the bounds and the clip region" $ do
      let clipRect = Rectangle 0 0 100 50
          mouseInsideClip = noInput { inputMousePosition = Point 50 25 }
      (_, ctx') <- runWith mouseInsideClip
        (withBounds clipRect $ clipToCurrent $ withBounds testBounds $ control () (pure ()))
      ixnHovered (ctxInteraction ctx') `shouldBe` Just ()

    it "wraps draw commands in PushClip / PopClip" $ do
      let clipRect = Rectangle 0 0 100 50
      (_, ctx) <- run (withBounds clipRect $ clipToCurrent $ fillRect (RGBA 1 0 0 1)) (0 :: Int)
      getDrawCommands ctx `shouldBe`
        [PushClip clipRect, FillRect clipRect (RGBA 1 0 0 1), PopClip]

    it "intersects nested clip regions" $ do
      let outerClip = Rectangle 0 0 100 50
          innerClip = Rectangle 0 25 100 50
          -- intersection is y 25–50; mouse at (50, 10) is inside outerClip but outside intersection
          mouseOutside = noInput { inputMousePosition = Point 50 10 }
      (_, ctx') <- runWith mouseOutside
        (withBounds outerClip $ clipToCurrent $
         withBounds innerClip $ clipToCurrent $
         withBounds testBounds $ control () (pure ()))
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

  describe "withBounds" $ do
    it "replaces the current bounds inside the sub-tree" $ do
      let inner = Rectangle 10 10 50 50
      (b, _) <- run (withBounds inner getBounds) (0 :: Int)
      b `shouldBe` inner

    it "restores the outer bounds after the sub-tree completes" $ do
      (b, _) <- run (withBounds (Rectangle 10 10 50 50) (pure ()) >> getBounds) (0 :: Int)
      b `shouldBe` testBounds

  describe "hover suppression during drag" $ do
    let base = emptyUIContext testBounds mouseOnCenterDown twoElemTheme (0 :: Int) noOpTextMeasurer

    it "does not hover an element when another element holds capture" $ do
      let ctx = withCapture ElemB base
      (_, ctx') <- runUI (control ElemA (pure ())) ctx
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

    it "hovers an element when it is itself the captured element" $ do
      let ctx = withCapture ElemA base
      (_, ctx') <- runUI (control ElemA (pure ())) ctx
      ixnHovered (ctxInteraction ctx') `shouldBe` Just ElemA

    it "hovers an element when no capture is active" $ do
      (_, ctx') <- runUI (control ElemA (pure ())) base
      ixnHovered (ctxInteraction ctx') `shouldBe` Just ElemA

  it "getAppState returns the frame's starting state" $ do
    (a, _) <- run getAppState (42 :: Int)
    a `shouldBe` 42

  it "getAppState still sees the pre-dispatch state later in the same frame" $ do
    (a, _) <- run (dispatch (+ 1) >> getAppState) (0 :: Int)
    a `shouldBe` 0

  it "applyDispatches applies modifiers in dispatch order" $ do
    (_, ctx) <- run (dispatch (+ 1) >> dispatch (* 10)) (0 :: Int)
    applyDispatches ctx `shouldBe` 10

  it "applyDispatches returns the starting state when nothing was dispatched" $ do
    (_, ctx) <- run (pure ()) (0 :: Int)
    applyDispatches ctx `shouldBe` 0

  it "dispatchAsync queues the job without running it" $ do
    ref <- newIORef False
    let job s = writeIORef ref True >> pure (const s)
    (_, ctx) <- run (dispatchAsync job) (0 :: Int)
    length (getAsyncJobs ctx) `shouldBe` 1
    ran <- readIORef ref
    ran `shouldBe` False

  it "nextFrameContext clears queued dispatches and async jobs" $ do
    (_, ctx) <- run (dispatch (+ 1) >> dispatchAsync (\s -> pure (const s))) (0 :: Int)
    let ctx' = nextFrameContext testBounds noInput ctx
    (applyDispatches ctx', length (getAsyncJobs ctx')) `shouldBe` (0, 0)

  describe "Selection helpers" $ do
    let sel a v = Selection a v

    describe "selectionLow" $ do
      it "returns the anchor when anchor < active" $
        selectionLow (sel 1 3) `shouldBe` 1
      it "returns the active when active < anchor" $
        selectionLow (sel 3 1) `shouldBe` 1
      it "returns the position when anchor == active" $
        selectionLow (sel 2 2) `shouldBe` 2

    describe "selectionHigh" $ do
      it "returns the active when active > anchor" $
        selectionHigh (sel 1 3) `shouldBe` 3
      it "returns the anchor when anchor > active" $
        selectionHigh (sel 3 1) `shouldBe` 3
      it "returns the position when anchor == active" $
        selectionHigh (sel 2 2) `shouldBe` 2

    describe "selectionHasExtent" $ do
      it "is True when anchor /= active" $
        selectionHasExtent (sel 1 3) `shouldBe` True
      it "is False when anchor == active" $
        selectionHasExtent (sel 2 2) `shouldBe` False

    describe "cursor" $ do
      it "creates a selection with equal anchor and active" $
        cursor 5 `shouldBe` Selection 5 5
      it "has no extent" $
        selectionHasExtent (cursor 3) `shouldBe` False

    describe "collapseToLow" $ do
      it "collapses to the lower bound" $
        collapseToLow (sel 1 4) `shouldBe` cursor 1
      it "collapses to the lower bound when active < anchor" $
        collapseToLow (sel 4 1) `shouldBe` cursor 1
      it "is a no-op on a cursor" $
        collapseToLow (sel 3 3) `shouldBe` cursor 3

    describe "collapseToHigh" $ do
      it "collapses to the upper bound" $
        collapseToHigh (sel 1 4) `shouldBe` cursor 4
      it "collapses to the upper bound when anchor > active" $
        collapseToHigh (sel 4 1) `shouldBe` cursor 4
      it "is a no-op on a cursor" $
        collapseToHigh (sel 3 3) `shouldBe` cursor 3

    describe "collapseToActive" $ do
      it "collapses to the active end" $
        collapseToActive (sel 1 4) `shouldBe` cursor 4
      it "collapses to the active end when active < anchor" $
        collapseToActive (sel 4 1) `shouldBe` cursor 1
      it "is a no-op on a cursor" $
        collapseToActive (sel 3 3) `shouldBe` cursor 3

    describe "extendActive" $ do
      it "applies the function to the active end" $
        extendActive (+1) (sel 2 3) `shouldBe` sel 2 4
      it "leaves the anchor unchanged" $
        selectionAnchor (extendActive (+1) (sel 2 3)) `shouldBe` 2
      it "can collapse a selection by moving active to anchor" $
        extendActive (const 2) (sel 2 5) `shouldBe` cursor 2

  describe "selection store" $ do
    it "returns [] when no selection has been recorded" $ do
      (ss, _) <- run (getSelections ()) (0 :: Int)
      ss `shouldBe` []

    it "returns the selections just written in the same frame" $ do
      let s = Selection 1 4
      (ss, _) <- run (setSelections () [s] >> getSelections ()) (0 :: Int)
      ss `shouldBe` [s]

    it "getSelection returns Nothing when no selection exists" $ do
      (s, _) <- run (getSelection ()) (0 :: Int)
      s `shouldBe` Nothing

    it "getSelection returns the first selection after setSelection" $ do
      (s, _) <- run (setSelection () (cursor 3) >> getSelection ()) (0 :: Int)
      s `shouldBe` Just (cursor 3)

    it "setSelection replaces any existing selections" $ do
      (ss, _) <- run (setSelections () [Selection 0 5, Selection 7 9] >> setSelection () (cursor 1) >> getSelections ()) (0 :: Int)
      ss `shouldBe` [cursor 1]

  describe "scroll state" $ do
    it "returns 0 when no position has been recorded" $ do
      (v, _) <- run (getScrollState ()) (0 :: Int)
      v `shouldBe` 0

    it "returns the value just written in the same frame" $ do
      (v, _) <- run (setScrollState () 0.5 >> getScrollState ()) (0 :: Int)
      v `shouldBe` 0.5

    it "keeps scroll positions separate per element" $ do
      (v, _) <- runTwoElem
        (setScrollState ElemA 0.3 >> setScrollState ElemB 0.7 >> getScrollState ElemA)
      v `shouldBe` 0.3

  describe "clampScrollPos" $ do
    it "clamps values below 0 to 0" $
      clampScrollPos (-0.5) `shouldBe` 0
    it "clamps values above 1 to 1" $
      clampScrollPos 1.5 `shouldBe` 1
    it "preserves values inside [0, 1]" $
      clampScrollPos 0.5 `shouldBe` 0.5
    it "preserves 0" $
      clampScrollPos 0 `shouldBe` 0
    it "preserves 1" $
      clampScrollPos 1 `shouldBe` 1

  describe "nextFrameContext capture" $ do
    it "auto-acquires capture when an element is hovered while the button is down" $ do
      -- Acquisition happens in setHovered during the frame, not via nextFrameContext.
      (_, ctx) <- runWith buttonDown (setHovered ())
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

    it "carries existing capture forward on subsequent ButtonDown frames" $ do
      ctx0 <- freshCtx
      let ctx = nextFrameContext testBounds buttonDown (withCapture () ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

    it "carries capture through the release frame so focus logic can inspect it" $ do
      -- Simulate: previous frame had button held, current frame it is released.
      (_, ctx0) <- runWith buttonDown (pure ())
      let ctx = nextFrameContext testBounds noInput (withCapture () ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

    it "clears capture once the button is fully up" $ do
      ctx0 <- freshCtx
      let ctx = nextFrameContext testBounds noInput (withCapture () ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Nothing

    it "clears the hovered element from the previous frame" $ do
      (_, ctx1) <- runWith mouseOnCenter (control () (pure ()))
      let ctx2 = nextFrameContext testBounds noInput ctx1
      ixnHovered (ctxInteraction ctx2) `shouldBe` Nothing

  describe "button interaction" $ do
    describe "state transitions via nextFrameContext" $ do
      it "ixnButtonDown is True when the button is currently held" $ do
        ctx0 <- freshCtx
        let ctx = nextFrameContext testBounds buttonDown ctx0
        ixnButtonDown (ctxInteraction ctx) `shouldBe` True

      it "ixnButtonReleased is True on the frame the button goes up" $ do
        (_, ctx0) <- runWith buttonDown (pure ())
        let ctx = nextFrameContext testBounds noInput ctx0
        ixnButtonReleased (ctxInteraction ctx) `shouldBe` True

      it "ixnButtonReleased is False when the button stays up" $ do
        ctx0 <- freshCtx
        let ctx = nextFrameContext testBounds noInput ctx0
        ixnButtonReleased (ctxInteraction ctx) `shouldBe` False

      it "ixnButtonReleased is False when the button stays down" $ do
        (_, ctx0) <- runWith buttonDown (pure ())
        let ctx = nextFrameContext testBounds buttonDown ctx0
        ixnButtonReleased (ctxInteraction ctx) `shouldBe` False

    it "isButtonDown returns True when the button is held" $ do
      (b, _) <- runWith buttonDown isButtonDown
      b `shouldBe` True

    it "isButtonReleased returns True on the release frame" $ do
      (_, ctx0) <- runWith buttonDown (pure ())
      let ctx = nextFrameContext testBounds noInput ctx0
      (b, _) <- runUI isButtonReleased ctx
      b `shouldBe` True

    it "isPressed is True when the element is hovered and the button is held" $ do
      (b, _) <- runWith mouseOnCenterDown (setHovered () >> isPressed ())
      b `shouldBe` True

    it "isPressed is False when the element is not hovered" $ do
      (b, _) <- runWith buttonDown (isPressed ())
      b `shouldBe` False

    it "isClicked is True when hovered and the button was just released" $ do
      (_, ctx1) <- runWith mouseOnCenterDown (setHovered ())
      let ctx2 = nextFrameContext testBounds mouseOnCenter ctx1
      (b, _) <- runUI (setHovered () >> isClicked ()) ctx2
      b `shouldBe` True

    it "isClicked is False when hovered but the button is still held" $ do
      (b, _) <- runWith mouseOnCenterDown (setHovered () >> isClicked ())
      b `shouldBe` False

    it "isDragging is True when the element holds capture" $ do
      ctx0 <- freshCtx
      let ctx = withCapture () ctx0
      (b, _) <- runUI (isDragging ()) ctx
      b `shouldBe` True

    it "isDragging is False when a different element holds capture" $ do
      (_, ctx0) <- runTwoElem (pure ())
      let ctx = withCapture ElemB ctx0
      (b, _) <- runUI (isDragging ElemA) ctx
      b `shouldBe` False

  describe "focus" $ do
    it "getFocus returns Nothing initially" $ do
      (f, _) <- run getFocus (0 :: Int)
      f `shouldBe` Nothing

    it "isFocused returns True after setFocus" $ do
      (b, _) <- run (setFocus () >> isFocused ()) (0 :: Int)
      b `shouldBe` True

    it "isFocused returns False for an element that does not hold focus" $ do
      (b, _) <- runTwoElem (setFocus ElemA >> isFocused ElemB)
      b `shouldBe` False

    it "clearFocus removes the focused element" $ do
      (f, _) <- run (setFocus () >> clearFocus >> getFocus) (0 :: Int)
      f `shouldBe` Nothing

    it "setFocusWhen does nothing when the condition is False" $ do
      (f, _) <- run (setFocusWhen False () >> getFocus) (0 :: Int)
      f `shouldBe` Nothing

    it "setFocusWhen sets focus when the condition is True" $ do
      (f, _) <- run (setFocusWhen True () >> getFocus) (0 :: Int)
      f `shouldBe` Just ()

    it "nextFrameContext carries focus forward when the element was visited this frame" $ do
      (_, ctx) <- run (setFocus ()) (0 :: Int)
      let ctx' = nextFrameContext testBounds noInput ctx
      focusedElement (ixnFocus (ctxInteraction ctx')) `shouldBe` Just ()

    it "nextFrameContext clears focus when the element was not visited this frame" $ do
      (_, ctx) <- run (pure ()) (0 :: Int)
      let staleCtx = ctx { ctxInteraction = (ctxInteraction ctx)
                             { ixnFocus = FocusState { focusedElement = Just (), focusedThisFrame = False } } }
          ctx' = nextFrameContext testBounds noInput staleCtx
      focusedElement (ixnFocus (ctxInteraction ctx')) `shouldBe` Nothing

  describe "drawing" $ do
    it "fillRect emits a FillRect command for the current bounds" $ do
      let colour = RGBA 1 0 0 1
      (_, ctx) <- run (fillRect colour) (0 :: Int)
      getDrawCommands ctx `shouldBe` [FillRect testBounds colour]

    it "strokeRect emits a StrokeRect command for the current bounds" $ do
      let colour = RGBA 0 1 0 1
      (_, ctx) <- run (strokeRect colour 2) (0 :: Int)
      getDrawCommands ctx `shouldBe` [StrokeRect testBounds colour 2]

    it "drawText emits a DrawText command for the current bounds" $ do
      let colour = RGBA 0 0 1 1
      (_, ctx) <- run (drawText colour AlignCenter "hello") (0 :: Int)
      getDrawCommands ctx `shouldBe` [DrawText testBounds "hello" colour AlignCenter]

    it "getDrawCommands returns commands in submission order" $ do
      let c1 = RGBA 1 0 0 1
          c2 = RGBA 0 1 0 1
      (_, ctx) <- run (fillRect c1 >> fillRect c2) (0 :: Int)
      getDrawCommands ctx `shouldBe` [FillRect testBounds c1, FillRect testBounds c2]

    it "nextFrameContext clears draw commands from the previous frame" $ do
      (_, ctx) <- run (fillRect (RGBA 1 0 0 1)) (0 :: Int)
      let ctx' = nextFrameContext testBounds noInput ctx
      getDrawCommands ctx' `shouldBe` []

    describe "withBackground" $ do
      it "emits a FillRect when the colour is opaque" $ do
        let colour = RGBA 1 0 0 1
        (_, ctx) <- run (withBackground colour (pure ())) (0 :: Int)
        getDrawCommands ctx `shouldBe` [FillRect testBounds colour]

      it "emits no FillRect when the colour is fully transparent" $ do
        (_, ctx) <- run (withBackground (RGBA 0 0 0 0) (pure ())) (0 :: Int)
        getDrawCommands ctx `shouldBe` []

    describe "withBorder" $ do
      it "strokes the border after the content" $ do
        let bgColour     = RGBA 1 0 0 1
            borderColour = RGBA 0 0 1 1
        (_, ctx) <- run (withBorder borderColour 1 (fillRect bgColour)) (0 :: Int)
        getDrawCommands ctx `shouldBe`
          [ FillRect testBounds bgColour
          , StrokeRect testBounds borderColour 1
          ]

  describe "disableWhen" $ do
    it "isDisabled is False by default" $ do
      (b, _) <- run isDisabled (0 :: Int)
      b `shouldBe` False

    it "isDisabled is True inside disableWhen True" $ do
      (b, _) <- run (disableWhen True isDisabled) (0 :: Int)
      b `shouldBe` True

    it "isDisabled is False inside disableWhen False" $ do
      (b, _) <- run (disableWhen False isDisabled) (0 :: Int)
      b `shouldBe` False

    it "restores the disabled flag to False after the sub-tree completes" $ do
      (b, _) <- run (disableWhen True (pure ()) >> isDisabled) (0 :: Int)
      b `shouldBe` False

    it "whenEnabled skips its body when the sub-tree is disabled" $ do
      (_, ctx) <- run (disableWhen True (whenEnabled (dispatch (const 1)))) (0 :: Int)
      applyDispatches ctx `shouldBe` 0

    it "whenEnabled runs its body when the sub-tree is enabled" $ do
      (_, ctx) <- run (whenEnabled (dispatch (const 1))) (0 :: Int)
      applyDispatches ctx `shouldBe` 1

  describe "isMouseFree" $ do
    it "is True when no element holds capture" $ do
      (result, _) <- run isMouseFree (0 :: Int)
      result `shouldBe` True

    it "is False when an element holds capture" $ do
      ctx0 <- freshCtx
      let ctx = withCapture () ctx0
      (result, _) <- runUI isMouseFree ctx
      result `shouldBe` False

  describe "regionHit" $ do
    it "is True when the mouse is inside the current bounds" $ do
      (hit, _) <- runWith mouseOnCenter regionHit
      hit `shouldBe` True

    it "is False when the mouse is outside the current bounds" $ do
      (hit, _) <- runWith (noInput { inputMousePosition = Point 200 200 }) regionHit
      hit `shouldBe` False

  describe "keyboard" $ do
    describe "consumeKey" $ do
      it "removes all events for the given key from the queue" $ do
        let input = noInput { inputKeyEvents = [ KeyEvent KeyTab [], KeyEvent KeyTab [] ] }
        (_, ctx') <- runWith input (consumeKey KeyTab)
        inputKeyEvents (ctxInput ctx') `shouldBe` []

      it "leaves events for other keys in the queue" $ do
        let tabEv    = KeyEvent KeyTab []
            returnEv = KeyEvent KeyReturn []
            input    = noInput { inputKeyEvents = [tabEv, returnEv] }
        (_, ctx') <- runWith input (consumeKey KeyTab)
        inputKeyEvents (ctxInput ctx') `shouldBe` [returnEv]

    describe "tab stop" $ do
      it "getPreviousTabStop returns Nothing initially" $ do
        (s, _) <- run getPreviousTabStop (0 :: Int)
        s `shouldBe` Nothing

      it "getPreviousTabStop returns the element after setPreviousTabStop" $ do
        (s, _) <- run (setPreviousTabStop () >> getPreviousTabStop) (0 :: Int)
        s `shouldBe` Just ()

  describe "styles" $ do
    let distinctStyles = StyleSet
          { styleSetNormal   = emptyStyle { styleBackground = RGBA 0 0 0 1 }
          , styleSetHovered  = emptyStyle { styleBackground = RGBA 1 0 0 1 }
          , styleSetPressed  = emptyStyle { styleBackground = RGBA 0 1 0 1 }
          , styleSetFocused  = emptyStyle { styleBackground = RGBA 0 0 1 1 }
          , styleSetDisabled = emptyStyle { styleBackground = RGBA 1 1 1 1 }
          }
        styledTheme = Theme
          { themeElementStyles = Map.singleton () distinctStyles
          , themeDefaultStyle  = emptyStyleSet
          }
        runStyled     ui       = runUI ui (emptyUIContext testBounds noInput       styledTheme (0 :: Int) noOpTextMeasurer)
        runStyledWith input ui = runUI ui (emptyUIContext testBounds input         styledTheme (0 :: Int) noOpTextMeasurer)

    describe "getStyleSet" $ do
      it "returns the element-specific style when registered" $ do
        (ss, _) <- runStyled (getStyleSet ())
        styleBackground (styleSetNormal ss) `shouldBe` RGBA 0 0 0 1

      it "falls back to the theme default when no element-specific style is registered" $ do
        (ss, _) <- run (getStyleSet ()) (0 :: Int)
        styleBackground (styleSetNormal ss) `shouldBe` styleBackground (styleSetNormal emptyStyleSet)

    describe "getStyle" $ do
      it "returns the normal style when no interaction is active" $ do
        (s, _) <- runStyled (getStyle ())
        styleBackground s `shouldBe` RGBA 0 0 0 1

      it "returns the hovered style when the element is hovered" $ do
        (s, _) <- runStyledWith mouseOnCenter (setHovered () >> getStyle ())
        styleBackground s `shouldBe` RGBA 1 0 0 1

      it "returns the focused style when the element is focused but not hovered" $ do
        (s, _) <- runStyled (setFocus () >> getStyle ())
        styleBackground s `shouldBe` RGBA 0 0 1 1

      it "pressed takes priority over hovered" $ do
        (s, _) <- runStyledWith mouseOnCenterDown (setHovered () >> getStyle ())
        styleBackground s `shouldBe` RGBA 0 1 0 1

      it "disabled takes priority over all other states" $ do
        (s, _) <- runStyledWith mouseOnCenterDown (disableWhen True (setHovered () >> getStyle ()))
        styleBackground s `shouldBe` RGBA 1 1 1 1

  describe "animation" $ do
    let tickCtx = (emptyUIContext testBounds noInput emptyTheme (0 :: Int) noOpTextMeasurer)
                    { ctxAnimation = AnimationState { animDelta = 0.016, animElapsed = 1.5, animIsTick = True } }
        nonTickCtx = (emptyUIContext testBounds noInput emptyTheme (0 :: Int) noOpTextMeasurer)
                       { ctxAnimation = AnimationState { animDelta = 0.016, animElapsed = 1.5, animIsTick = False } }

    it "getAnimDelta returns the frame delta" $ do
      (d, _) <- runUI getAnimDelta tickCtx
      d `shouldBe` 0.016

    it "getAnimElapsed returns the total elapsed time" $ do
      (e, _) <- runUI getAnimElapsed tickCtx
      e `shouldBe` 1.5

    it "withAnimationFrame runs its body on tick frames" $ do
      ref <- newIORef False
      runUI (withAnimationFrame (UI $ \ctx -> writeIORef ref True >> pure ((), ctx))) tickCtx
      readIORef ref `shouldReturn` True

    it "withAnimationFrame skips its body on non-tick frames" $ do
      ref <- newIORef False
      runUI (withAnimationFrame (UI $ \ctx -> writeIORef ref True >> pure ((), ctx))) nonTickCtx
      readIORef ref `shouldReturn` False

    it "requiresAnimation sets the animation continuation flag" $ do
      (_, ctx) <- runUI requiresAnimation tickCtx
      outRequiresAnimation (ctxOutputs ctx) `shouldBe` True

  describe "getHoveredElement" $ do
    it "returns Nothing when no element is hovered" $ do
      (result, _) <- run getHoveredElement (0 :: Int)
      result `shouldBe` Nothing

    it "returns the hovered element after setHovered" $ do
      (result, _) <- runWith mouseOnCenter (control () (pure ()) >> getHoveredElement)
      result `shouldBe` Just ()

  describe "properties" $ do
    prop "clampScrollPos is idempotent" $ \x ->
      clampScrollPos (clampScrollPos x) == (clampScrollPos x :: Double)

    prop "clampScrollPos result is always in [0, 1]" $ \x ->
      let v = clampScrollPos (x :: Double) in v >= 0 && v <= 1

    prop "selectionLow is never greater than selectionHigh" $
      forAll ((,) <$> choose (-100, 100) <*> choose (-100, 100)) $ \(a, v) ->
        selectionLow (Selection a v) <= selectionHigh (Selection a v)

    prop "collapseToLow always produces a cursor with no extent" $
      forAll ((,) <$> choose (-100, 100) <*> choose (-100, 100)) $ \(a, v) ->
        not (selectionHasExtent (collapseToLow (Selection a v)))

    prop "extendActive preserves the anchor" $
      forAll ((,) <$> choose (-100, 100) <*> choose (-100, 100)) $ \(a, v) ->
        selectionAnchor (extendActive (+1) (Selection a v)) == a
