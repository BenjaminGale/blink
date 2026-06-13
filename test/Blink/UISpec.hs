module Blink.UISpec (spec) where

import Data.IORef
import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Geometry (Point (..), Rectangle (..), uniform)
import Blink.Input (InputState (..))
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.Controls (control)
import Blink.UI

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

spec :: Spec
spec = describe "Blink.UI" $ do
  describe "clipToCurrent" $ do
    -- In each test the control runs with testBounds (100×100) so the mouse is
    -- inside the element's own bounds; only the clip region should block hover.
    it "does not register hover when the mouse is inside bounds but outside the clip region" $ do
      let clipRect = Rectangle 0 0 100 50
          mouseOutsideClip = noInput { inputMousePosition = Point 50 75 }
          ctx = emptyUIContext testBounds mouseOutsideClip emptyTheme (0 :: Int) noOpTextMeasurer
      (_, ctx') <- runUI (withBounds clipRect $ clipToCurrent $ withBounds testBounds $ control () (pure ())) ctx
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

    it "registers hover when the mouse is inside both the bounds and the clip region" $ do
      let clipRect = Rectangle 0 0 100 50
          mouseInsideClip = noInput { inputMousePosition = Point 50 25 }
          ctx = emptyUIContext testBounds mouseInsideClip emptyTheme (0 :: Int) noOpTextMeasurer
      (_, ctx') <- runUI (withBounds clipRect $ clipToCurrent $ withBounds testBounds $ control () (pure ())) ctx
      ixnHovered (ctxInteraction ctx') `shouldBe` Just ()

    it "intersects nested clip regions" $ do
      let outerClip = Rectangle 0 0 100 50
          innerClip = Rectangle 0 25 100 50
          -- intersection is y 25–50; mouse at (50, 10) is inside outerClip but outside intersection
          mouseOutside = noInput { inputMousePosition = Point 50 10 }
          ctx = emptyUIContext testBounds mouseOutside emptyTheme (0 :: Int) noOpTextMeasurer
      (_, ctx') <- runUI
        (withBounds outerClip $ clipToCurrent $
         withBounds innerClip $ clipToCurrent $
         withBounds testBounds $ control () (pure ())) ctx
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

  describe "hover suppression during drag" $ do
    let mouseOnA = noInput { inputMousePosition = Point 50 50, inputLeftButtonDown = True }

    it "does not hover an element when another element holds capture" $ do
      let base = emptyUIContext testBounds mouseOnA twoElemTheme (0 :: Int) noOpTextMeasurer
          ctx  = base { ctxInteraction = (ctxInteraction base) { ixnCaptured = Just ElemB } }
      (_, ctx') <- runUI (control ElemA (pure ())) ctx
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

    it "hovers an element when it is itself the captured element" $ do
      let base = emptyUIContext testBounds mouseOnA twoElemTheme (0 :: Int) noOpTextMeasurer
          ctx  = base { ctxInteraction = (ctxInteraction base) { ixnCaptured = Just ElemA } }
      (_, ctx') <- runUI (control ElemA (pure ())) ctx
      ixnHovered (ctxInteraction ctx') `shouldBe` Just ElemA

    it "hovers an element when no capture is active" $ do
      let ctx = emptyUIContext testBounds mouseOnA twoElemTheme (0 :: Int) noOpTextMeasurer
      (_, ctx') <- runUI (control ElemA (pure ())) ctx
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
      (v, _) <- runUI
        (setScrollState ElemA 0.3 >> setScrollState ElemB 0.7 >> getScrollState ElemA)
        (emptyUIContext testBounds noInput twoElemTheme (0 :: Int) noOpTextMeasurer)
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
    let captured ctx = ctx { ctxInteraction = (ctxInteraction ctx) { ixnCaptured = Just () } }
        buttonDown  = noInput { inputLeftButtonDown = True }

    it "auto-acquires capture when an element is hovered while the button is down" $ do
      -- Acquisition happens in setHovered during the frame, not via nextFrameContext.
      (_, ctx) <- runUI (setHovered ()) (emptyUIContext testBounds buttonDown emptyTheme (0 :: Int) noOpTextMeasurer)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

    it "carries existing capture forward on subsequent ButtonDown frames" $ do
      (_, ctx0) <- run (pure ()) (0 :: Int)
      let ctx = nextFrameContext testBounds buttonDown (captured ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

    it "carries capture through the release frame so focus logic can inspect it" $ do
      -- Simulate: previous frame had button held, current frame it is released.
      (_, ctx0) <- runUI (pure ()) (emptyUIContext testBounds buttonDown emptyTheme (0 :: Int) noOpTextMeasurer)
      let ctx = nextFrameContext testBounds noInput (captured ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

    it "clears capture once the button is fully up" $ do
      (_, ctx0) <- run (pure ()) (0 :: Int)
      let ctx = nextFrameContext testBounds noInput (captured ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Nothing

  describe "focus" $ do
    it "getFocus returns Nothing initially" $ do
      (f, _) <- run getFocus (0 :: Int)
      f `shouldBe` Nothing

    it "isFocused returns True after setFocus" $ do
      (b, _) <- run (setFocus () >> isFocused ()) (0 :: Int)
      b `shouldBe` True

    it "isFocused returns False for an element that does not hold focus" $ do
      (b, _) <- runUI
        (setFocus ElemA >> isFocused ElemB)
        (emptyUIContext testBounds noInput twoElemTheme (0 :: Int) noOpTextMeasurer)
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

  describe "isMouseFree" $ do
    it "is True when no element holds capture" $ do
      (result, _) <- run isMouseFree (0 :: Int)
      result `shouldBe` True

    it "is False when an element holds capture" $ do
      (_, ctx0) <- run (pure ()) (0 :: Int)
      let ctx = ctx0 { ctxInteraction = (ctxInteraction ctx0) { ixnCaptured = Just () } }
      (result, _) <- runUI isMouseFree ctx
      result `shouldBe` False

  describe "getHoveredElement" $ do
    it "returns Nothing when no element is hovered" $ do
      (result, _) <- run getHoveredElement (0 :: Int)
      result `shouldBe` Nothing

    it "returns the hovered element after setHovered" $ do
      let mouseInside = noInput { inputMousePosition = Point 50 50 }
          ctx = emptyUIContext testBounds mouseInside emptyTheme (0 :: Int) noOpTextMeasurer
      (result, _) <- runUI (control () (pure ()) >> getHoveredElement) ctx
      result `shouldBe` Just ()
