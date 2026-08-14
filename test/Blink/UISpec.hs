{-# LANGUAGE OverloadedStrings #-}
module Blink.UISpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, choose)

import Control.Monad (when)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, uniform, noBorder, uniformBorder)
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI
import Blink.Generators ()

-- | Minimal stand-in for the pre-migration @Blink.Controls.control@ (now
-- deleted): registers hover via the legacy single-owner 'setHovered' \/
-- 'ixnHovered' field these tests specifically target, gated the same way —
-- geometrically hit, and only when the mouse is free or this element itself
-- holds capture. The current 'Blink.Controls.control' no longer touches
-- this field at all (it uses the newer geometric hover model instead), so
-- these 'Blink.UI'-level primitives need their own minimal exerciser.
legacyHoverControl :: Ord e => e -> UI e msg () -> UI e msg ()
legacyHoverControl eid content = do
  free     <- isMouseFree
  dragging <- isDragging eid
  when (free || dragging) $ do
    s     <- getStyle eid
    r     <- getBounds
    isHit <- withBounds (insetRect (styleMargin s) r) isRegionHit
    when isHit $ setHovered eid
  content

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
  , styleBorderEdges = noBorder
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

run :: UI () msg a -> IO (a, UIContext () msg)
run ui = runUI ui (emptyUIContext testBounds noInput emptyTheme noOpTextMeasurer)

runWith :: InputState -> UI () msg a -> IO (a, UIContext () msg)
runWith input ui = runUI ui (emptyUIContext testBounds input emptyTheme noOpTextMeasurer)

runTwoElem :: UI TwoElems msg a -> IO (a, UIContext TwoElems msg)
runTwoElem ui = runUI ui (emptyUIContext testBounds noInput twoElemTheme noOpTextMeasurer)

freshCtx :: IO (UIContext () Int)
freshCtx = snd <$> run0 (pure ())

run0 :: UI () Int a -> IO (a, UIContext () Int)
run0 = run

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
        (withBounds clipRect $ clipToCurrent $ withBounds testBounds $ legacyHoverControl () (pure ()))
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

    it "registers hover when the mouse is inside both the bounds and the clip region" $ do
      let clipRect = Rectangle 0 0 100 50
          mouseInsideClip = noInput { inputMousePosition = Point 50 25 }
      (_, ctx') <- runWith mouseInsideClip
        (withBounds clipRect $ clipToCurrent $ withBounds testBounds $ legacyHoverControl () (pure ()))
      ixnHovered (ctxInteraction ctx') `shouldBe` Just ()

    it "wraps draw commands in PushClip / PopClip" $ do
      let clipRect = Rectangle 0 0 100 50
      (_, ctx) <- run0 (withBounds clipRect $ clipToCurrent $ fillRect (RGBA 1 0 0 1))
      getDrawCommands ctx `shouldBe`
        [PushClip clipRect, FillRect clipRect (RGBA 1 0 0 1), PopClip]

    it "does not register hover when the mouse is outside the intersection of nested clip regions" $ do
      let outerClip = Rectangle 0 0 100 50
          innerClip = Rectangle 0 25 100 50
          -- intersection is y 25–50; mouse at (50, 10) is inside outerClip but outside intersection
          mouseOutside = noInput { inputMousePosition = Point 50 10 }
      (_, ctx') <- runWith mouseOutside
        (withBounds outerClip $ clipToCurrent $
         withBounds innerClip $ clipToCurrent $
         withBounds testBounds $ legacyHoverControl () (pure ()))
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

  describe "withBounds" $ do
    it "replaces the current bounds inside the sub-tree" $ do
      let inner = Rectangle 10 10 50 50
      (b, _) <- run0 (withBounds inner getBounds)
      b `shouldBe` inner

    it "restores the outer bounds after the sub-tree completes" $ do
      (b, _) <- run0 (withBounds (Rectangle 10 10 50 50) (pure ()) >> getBounds)
      b `shouldBe` testBounds

  context "during a drag (another element holds capture)" $ do
    let base = emptyUIContext testBounds mouseOnCenterDown twoElemTheme noOpTextMeasurer :: UIContext TwoElems Int

    it "does not hover an element when another element holds capture" $ do
      let ctx = withCapture ElemB base
      (_, ctx') <- runUI (legacyHoverControl ElemA (pure ())) ctx
      ixnHovered (ctxInteraction ctx') `shouldBe` Nothing

    it "hovers an element when it is itself the captured element" $ do
      let ctx = withCapture ElemA base
      (_, ctx') <- runUI (legacyHoverControl ElemA (pure ())) ctx
      ixnHovered (ctxInteraction ctx') `shouldBe` Just ElemA

    it "hovers an element when no capture is active" $ do
      (_, ctx') <- runUI (legacyHoverControl ElemA (pure ())) base
      ixnHovered (ctxInteraction ctx') `shouldBe` Just ElemA

  it "getMessages returns emitted messages in emit order" $ do
    (_, ctx) <- run0 (emit (1 :: Int) >> emit 2)
    getMessages ctx `shouldBe` [1, 2]

  it "getMessages returns [] when nothing was emitted" $ do
    (_, ctx) <- run0 (pure ())
    getMessages ctx `shouldBe` []

  it "nextFrameContext clears queued messages" $ do
    (_, ctx) <- run0 (emit (1 :: Int))
    let ctx' = nextFrameContext testBounds noInput ctx
    getMessages ctx' `shouldBe` []

  describe "Selection helpers" $ do
    let sel = Selection

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
      (ss, _) <- run0 (getSelections ())
      ss `shouldBe` []

    it "getSelection returns Nothing when no selection exists" $ do
      (s, _) <- run0 (getSelection ())
      s `shouldBe` Nothing

    it "SetSelectionAt is queued rather than applied immediately" $ do
      (ss, ctx) <- run0 (emitUi (SetSelectionAt () (Selection 1 4)) >> getSelections ())
      ss `shouldBe` []
      getUiEffects ctx `shouldBe` [SetSelectionAt () (Selection 1 4)]

    it "a queued SetSelectionAt is visible via getSelections once applied" $ do
      (_, ctx) <- run0 (emitUi (SetSelectionAt () (Selection 1 4)))
      (ss, _) <- runUI (getSelections ()) (applyUiEffects (getUiEffects ctx) ctx)
      ss `shouldBe` [Selection 1 4]

    it "a later SetSelectionAt in the same frame overrides an earlier one" $ do
      (_, ctx) <- run0 (emitUi (SetSelectionAt () (Selection 0 5)) >> emitUi (SetSelectionAt () (cursor 1)))
      (ss, _) <- runUI (getSelections ()) (applyUiEffects (getUiEffects ctx) ctx)
      ss `shouldBe` [cursor 1]

  describe "scroll state" $ do
    it "returns 0 when no position has been recorded" $ do
      (v, _) <- run0 (getScrollState ())
      v `shouldBe` 0

    it "ScrollTo is queued rather than applied immediately" $ do
      (v, ctx) <- run0 (emitUi (ScrollTo () 0.5) >> getScrollState ())
      v `shouldBe` 0
      getUiEffects ctx `shouldBe` [ScrollTo () 0.5]

    it "a queued ScrollTo is visible via getScrollState once applied" $ do
      (_, ctx) <- run0 (emitUi (ScrollTo () 0.5))
      (v, _) <- runUI (getScrollState ()) (applyUiEffects (getUiEffects ctx) ctx)
      v `shouldBe` 0.5

    it "ScrollBy composes with the current position, clamped to [0, 1]" $ do
      (_, ctx) <- run0 (emitUi (ScrollTo () 0.5) >> emitUi (ScrollBy () 0.7))
      (v, _) <- runUI (getScrollState ()) (applyUiEffects (getUiEffects ctx) ctx)
      v `shouldBe` 1.0

    it "keeps scroll positions separate per element" $ do
      (_, ctx) <- runTwoElem (emitUi (ScrollTo ElemA 0.3) >> emitUi (ScrollTo ElemB 0.7))
      (v, _) <- runUI (getScrollState ElemA) (applyUiEffects (getUiEffects ctx) ctx)
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
    it "carries existing capture forward on continued ButtonDown frames" $ do
      (_, ctx0) <- runWith buttonDown (pure ())
      let ctx = nextFrameContext testBounds buttonDown (withCapture () ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

    it "clears capture on a fresh press even if capture was stale" $ do
      ctx0 <- freshCtx
      let ctx = nextFrameContext testBounds buttonDown (withCapture () ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Nothing

    it "clears capture on a fresh press (no capture existed)" $ do
      ctx0 <- freshCtx
      let ctx = nextFrameContext testBounds buttonDown ctx0
      ixnCaptured (ctxInteraction ctx) `shouldBe` Nothing

    it "carries capture through the release frame so focus logic can inspect it" $ do
      -- Simulate: previous frame had button held, current frame it is released.
      (_, ctx0) <- runWith buttonDown (pure ())
      let ctx = nextFrameContext testBounds noInput (withCapture () ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

    it "clears capture once the button is fully up" $ do
      ctx0 <- freshCtx
      let ctx = nextFrameContext testBounds noInput (withCapture () ctx0)
      ixnCaptured (ctxInteraction ctx) `shouldBe` Nothing

  describe "button interaction" $ do
    context "when advancing to the next frame" $ do
      it "isButtonDown is True when the button is currently held" $ do
        ctx0 <- freshCtx
        let ctx = nextFrameContext testBounds buttonDown ctx0
        (b, _) <- runUI isButtonDown ctx
        b `shouldBe` True

      it "isButtonReleased is True on the frame the button goes up" $ do
        (_, ctx0) <- runWith buttonDown (pure ())
        let ctx = nextFrameContext testBounds noInput ctx0
        (b, _) <- runUI isButtonReleased ctx
        b `shouldBe` True

      it "isButtonReleased is False when the button stays up" $ do
        ctx0 <- freshCtx
        let ctx = nextFrameContext testBounds noInput ctx0
        (b, _) <- runUI isButtonReleased ctx
        b `shouldBe` False

      it "isButtonReleased is False when the button stays down" $ do
        (_, ctx0) <- runWith buttonDown (pure ())
        let ctx = nextFrameContext testBounds buttonDown ctx0
        (b, _) <- runUI isButtonReleased ctx
        b `shouldBe` False

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

    it "setHovered auto-acquires capture when the button is down" $ do
      (_, ctx) <- runWith buttonDown (setHovered ())
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

  describe "setHot" $ do
    it "acquires capture when the button is down and nothing is captured" $ do
      (_, ctx) <- runWith buttonDown (setHot ())
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just ()

    it "does not acquire capture when another element already holds it" $ do
      (_, ctx0) <- runTwoElem (pure ())
      let ctx = withCapture ElemB ctx0
      (_, ctx') <- runUI (setHot ElemA) ctx
      ixnCaptured (ctxInteraction ctx') `shouldBe` Just ElemB

    it "does nothing when the button is not down" $ do
      (_, ctx) <- run0 (setHot ())
      ixnCaptured (ctxInteraction ctx) `shouldBe` Nothing

    it "does not mark the element as hovered" $ do
      (_, ctx) <- runWith buttonDown (setHot ())
      ixnHovered (ctxInteraction ctx) `shouldBe` Nothing

    it "makes the element dragging once capture is acquired" $ do
      (dragging, _) <- runWith buttonDown (setHot () >> isDragging ())
      dragging `shouldBe` True

  describe "focus" $ do
    it "getFocus returns Nothing initially" $ do
      (f, _) <- run0 getFocus
      f `shouldBe` Nothing

    it "isFocused returns True after setFocus" $ do
      (b, _) <- run0 (setFocus () >> isFocused ())
      b `shouldBe` True

    it "isFocused returns False for an element that does not hold focus" $ do
      (b, _) <- runTwoElem (setFocus ElemA >> isFocused ElemB)
      b `shouldBe` False

    it "clearFocus removes the focused element" $ do
      (f, _) <- run0 (setFocus () >> clearFocus >> getFocus)
      f `shouldBe` Nothing

    it "setFocusWhen does nothing when the condition is False" $ do
      (f, _) <- run0 (setFocusWhen False () >> getFocus)
      f `shouldBe` Nothing

    it "setFocusWhen sets focus when the condition is True" $ do
      (f, _) <- run0 (setFocusWhen True () >> getFocus)
      f `shouldBe` Just ()

    it "nextFrameContext carries focus forward when the element was visited this frame" $ do
      (_, ctx) <- run0 (setFocus ())
      let ctx' = nextFrameContext testBounds noInput ctx
      (f, _) <- runUI getFocus ctx'
      f `shouldBe` Just ()

    it "nextFrameContext clears focus when the element was not visited this frame" $ do
      (_, ctx0) <- run0 (setFocus ())
      let ctx1 = nextFrameContext testBounds noInput ctx0
      (_, ctx2) <- runUI (pure ()) ctx1
      let ctx3 = nextFrameContext testBounds noInput ctx2
      (f, _) <- runUI getFocus ctx3
      f `shouldBe` Nothing

  describe "drawing" $ do
    it "fillRect emits a FillRect command for the current bounds" $ do
      let colour = RGBA 1 0 0 1
      (_, ctx) <- run0 (fillRect colour)
      getDrawCommands ctx `shouldBe` [FillRect testBounds colour]

    it "strokeRect emits a StrokeBorder command for the current bounds" $ do
      let colour = RGBA 0 1 0 1
      (_, ctx) <- run0 (strokeRect colour (uniformBorder 2))
      getDrawCommands ctx `shouldBe` [StrokeBorder testBounds colour (uniformBorder 2)]

    it "drawText emits a DrawText command for the current bounds" $ do
      let colour = RGBA 0 0 1 1
      (_, ctx) <- run0 (drawText colour AlignCenter "hello")
      getDrawCommands ctx `shouldBe` [DrawText testBounds "hello" colour AlignCenter]

    it "getDrawCommands returns commands in submission order" $ do
      let c1 = RGBA 1 0 0 1
          c2 = RGBA 0 1 0 1
      (_, ctx) <- run0 (fillRect c1 >> fillRect c2)
      getDrawCommands ctx `shouldBe` [FillRect testBounds c1, FillRect testBounds c2]

    it "nextFrameContext clears draw commands from the previous frame" $ do
      (_, ctx) <- run0 (fillRect (RGBA 1 0 0 1))
      let ctx' = nextFrameContext testBounds noInput ctx
      getDrawCommands ctx' `shouldBe` []

    it "nextFrameContext clears the hovered element from the previous frame" $ do
      (_, ctx1) <- runWith mouseOnCenter (legacyHoverControl () (pure ()))
      let ctx2 = nextFrameContext testBounds noInput ctx1
      ixnHovered (ctxInteraction ctx2) `shouldBe` Nothing

    describe "withBackground" $ do
      it "emits a FillRect when the colour is opaque" $ do
        let colour = RGBA 1 0 0 1
        (_, ctx) <- run0 (withBackground colour (pure ()))
        getDrawCommands ctx `shouldBe` [FillRect testBounds colour]

      it "emits no FillRect when the colour is fully transparent" $ do
        (_, ctx) <- run0 (withBackground (RGBA 0 0 0 0) (pure ()))
        getDrawCommands ctx `shouldBe` []

    describe "withBorder" $ do
      it "strokes the border after the content" $ do
        let bgColour     = RGBA 1 0 0 1
            borderColour = RGBA 0 0 1 1
        (_, ctx) <- run0 (withBorder borderColour (uniformBorder 1) (fillRect bgColour))
        getDrawCommands ctx `shouldBe`
          [ FillRect testBounds bgColour
          , StrokeBorder testBounds borderColour (uniformBorder 1)
          ]

  describe "disableWhen" $ do
    it "isDisabled is False by default" $ do
      (b, _) <- run0 isDisabled
      b `shouldBe` False

    it "isDisabled is True inside disableWhen True" $ do
      (b, _) <- run0 (disableWhen True isDisabled)
      b `shouldBe` True

    it "isDisabled is False inside disableWhen False" $ do
      (b, _) <- run0 (disableWhen False isDisabled)
      b `shouldBe` False

    it "restores the disabled flag to False after the sub-tree completes" $ do
      (b, _) <- run0 (disableWhen True (pure ()) >> isDisabled)
      b `shouldBe` False

    it "whenEnabled skips its body when the sub-tree is disabled" $ do
      (_, ctx) <- run0 (disableWhen True (whenEnabled (emit 1)))
      getMessages ctx `shouldBe` []

    it "whenEnabled runs its body when the sub-tree is enabled" $ do
      (_, ctx) <- run0 (whenEnabled (emit 1))
      getMessages ctx `shouldBe` [1]

  describe "isMouseFree" $ do
    it "is True when no element holds capture" $ do
      (result, _) <- run0 isMouseFree
      result `shouldBe` True

    it "is False when an element holds capture" $ do
      ctx0 <- freshCtx
      let ctx = withCapture () ctx0
      (result, _) <- runUI isMouseFree ctx
      result `shouldBe` False

  describe "isRegionHit" $ do
    it "is True when the mouse is inside the current bounds" $ do
      (hit, _) <- runWith mouseOnCenter isRegionHit
      hit `shouldBe` True

    it "is False when the mouse is outside the current bounds" $ do
      (hit, _) <- runWith (noInput { inputMousePosition = Point 200 200 }) isRegionHit
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
      it "returns Nothing when no tab stop has been registered" $ do
        (s, _) <- run0 getPreviousTabStop
        s `shouldBe` Nothing

      it "returns the element registered as the previous tab stop" $ do
        (s, _) <- run0 (setPreviousTabStop () >> getPreviousTabStop)
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
        runStyled     ui       = runUI ui (emptyUIContext testBounds noInput       styledTheme noOpTextMeasurer)
        runStyledWith input ui = runUI ui (emptyUIContext testBounds input         styledTheme noOpTextMeasurer)

    describe "getStyleSet" $ do
      it "returns the element-specific style when registered" $ do
        (ss, _) <- runStyled (getStyleSet ())
        styleBackground (styleSetNormal ss) `shouldBe` RGBA 0 0 0 1

      it "falls back to the theme default when no element-specific style is registered" $ do
        (ss, _) <- run0 (getStyleSet ())
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
    let baseAnimCtx :: UIContext () Int
        baseAnimCtx = (emptyUIContext testBounds noInput emptyTheme noOpTextMeasurer)
                        { ctxAnimation = AnimationState { animDelta = 0.016, animElapsed = 1.5, animIsTick = False } }
        tickCtx    = baseAnimCtx { ctxAnimation = (ctxAnimation baseAnimCtx) { animIsTick = True } }
        nonTickCtx = baseAnimCtx

    it "getAnimDelta returns the frame delta" $ do
      (d, _) <- runUI getAnimDelta tickCtx
      d `shouldBe` 0.016

    it "getAnimElapsed returns the total elapsed time" $ do
      (e, _) <- runUI getAnimElapsed tickCtx
      e `shouldBe` 1.5

    it "withAnimationFrame runs its body on tick frames" $ do
      (_, ctx) <- runUI (withAnimationFrame (emit 1)) tickCtx
      getMessages ctx `shouldBe` [1]

    it "withAnimationFrame skips its body on non-tick frames" $ do
      (_, ctx) <- runUI (withAnimationFrame (emit 1)) nonTickCtx
      getMessages ctx `shouldBe` []

    it "requiresAnimation sets the animation continuation flag" $ do
      (_, ctx) <- runUI requiresAnimation tickCtx
      outRequiresAnimation (ctxOutputs ctx) `shouldBe` True

  describe "getHoveredElement" $ do
    it "returns Nothing when no element is hovered" $ do
      (result, _) <- run0 getHoveredElement
      result `shouldBe` Nothing

    it "returns the element currently registered as hovered" $ do
      (result, _) <- runWith mouseOnCenter (legacyHoverControl () (pure ()) >> getHoveredElement)
      result `shouldBe` Just ()

  describe "mouse-over memory (registerMouseOver / wasMouseOverLastFrame)" $ do
    it "is False when nothing has ever been registered" $ do
      (result, _) <- run0 (wasMouseOverLastFrame ())
      result `shouldBe` False

    it "is still False for an element registered only this frame" $ do
      (result, _) <- run0 (registerMouseOver () >> wasMouseOverLastFrame ())
      result `shouldBe` False

    it "is True on the frame after registration" $ do
      (_, ctx) <- run0 (registerMouseOver ())
      let ctx' = nextFrameContext testBounds noInput ctx
      (result, _) <- runUI (wasMouseOverLastFrame ()) ctx'
      result `shouldBe` True

    it "is False two frames after registration if not re-registered" $ do
      (_, ctx0) <- run0 (registerMouseOver ())
      let ctx1 = nextFrameContext testBounds noInput ctx0
      (_, ctx1') <- runUI (pure ()) ctx1
      let ctx2 = nextFrameContext testBounds noInput ctx1'
      (result, _) <- runUI (wasMouseOverLastFrame ()) ctx2
      result `shouldBe` False

    it "remembers every element registered in the same frame, not just the last" $ do
      (_, ctx) <- runTwoElem (registerMouseOver ElemA >> registerMouseOver ElemB)
      let ctx' = nextFrameContext testBounds noInput ctx
      (a, _) <- runUI (wasMouseOverLastFrame ElemA) ctx'
      (b, _) <- runUI (wasMouseOverLastFrame ElemB) ctx'
      (a, b) `shouldBe` (True, True)

    it "keeps results independent per element" $ do
      (_, ctx) <- runTwoElem (registerMouseOver ElemA)
      let ctx' = nextFrameContext testBounds noInput ctx
      (a, _) <- runUI (wasMouseOverLastFrame ElemA) ctx'
      (b, _) <- runUI (wasMouseOverLastFrame ElemB) ctx'
      (a, b) `shouldBe` (True, False)

  describe "isAnyMouseOver" $ do
    it "is False when nothing has registered mouse-over this frame" $ do
      (result, _) <- run0 isAnyMouseOver
      result `shouldBe` False

    it "is True once any element has registered mouse-over this frame" $ do
      (result, _) <- run0 (registerMouseOver () >> isAnyMouseOver)
      result `shouldBe` True

    it "is True when a different element registered, not just the one checked" $ do
      (result, _) <- runTwoElem (registerMouseOver ElemA >> isAnyMouseOver)
      result `shouldBe` True

    it "does not carry over from the previous frame without re-registering" $ do
      (_, ctx) <- run0 (registerMouseOver ())
      let ctx' = nextFrameContext testBounds noInput ctx
      (result, _) <- runUI isAnyMouseOver ctx'
      result `shouldBe` False

  describe "clampScrollPos properties" $ do
    prop "is idempotent" $ \x ->
      clampScrollPos (clampScrollPos x) == (clampScrollPos x :: Double)

    prop "result is always in [0, 1]" $ \x ->
      let v = clampScrollPos (x :: Double) in v >= 0 && v <= 1

  describe "Selection invariants" $ do
    prop "selectionLow is never greater than selectionHigh" $
      forAll ((,) <$> choose (-100, 100) <*> choose (-100, 100)) $ \(a, v) ->
        selectionLow (Selection a v) <= selectionHigh (Selection a v)

    prop "collapseToLow always produces a cursor with no extent" $
      forAll ((,) <$> choose (-100, 100) <*> choose (-100, 100)) $ \(a, v) ->
        not (selectionHasExtent (collapseToLow (Selection a v)))

    prop "extendActive preserves the anchor" $
      forAll ((,) <$> choose (-100, 100) <*> choose (-100, 100)) $ \(a, v) ->
        selectionAnchor (extendActive (+1) (Selection a v)) == a
