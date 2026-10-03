module Blink.View.MouseSpec (spec) where

import Test.Hspec

import Blink.Geometry (Point (..))
import Blink.Input (InputState (..))
import Blink.View
import Blink.View.Fixtures

-- | A context in which @()@ has just acquired real mouse capture at the
-- center point -- the common starting point for every test below that
-- exercises capture *after* it's been acquired, as opposed to the act of
-- acquiring it.
capturedCtx :: IO (ViewContext () Int)
capturedCtx = snd <$> runWith mouseOnCenterDown (acquireCapture ())

spec :: Spec
spec = describe "Blink.View.Mouse" $ do
  describe "nextFrameContext capture" $ do
    it "carries existing capture forward on continued ButtonDown frames" $ do
      ctx1 <- capturedCtx
      let ctx2 = advance mouseOnCenterDown ctx1
      contextCaptured ctx2 `shouldBe` MouseCapturedBy ()

    it "clears capture on a fresh press even if capture was stale" $ do
      ctx1 <- capturedCtx                                  -- Pressed: capture acquired
      let ctx2 = advance mouseOnCenter ctx1                -- Released: capture kept through this frame
          ctx3 = advance mouseOnCenterDown ctx2             -- fresh Pressed again
      contextCaptured ctx3 `shouldBe` MouseNotCaptured

    it "clears capture on a fresh press (no capture existed)" $ do
      ctx0 <- freshCtx
      let ctx = advance buttonDown ctx0
      contextCaptured ctx `shouldBe` MouseNotCaptured

    it "carries capture through the release frame so focus logic can inspect it" $ do
      ctx1 <- capturedCtx
      let ctx2 = advance mouseOnCenter ctx1
      contextCaptured ctx2 `shouldBe` MouseCapturedBy ()

    it "clears capture once the button is fully up" $ do
      ctx1 <- capturedCtx
      let ctx2 = advance mouseOnCenter ctx1  -- Released: capture kept through this frame
          ctx3 = advance mouseOnCenter ctx2  -- fully idle frame: capture cleared
      contextCaptured ctx3 `shouldBe` MouseNotCaptured

  describe "button interaction" $ do
    context "when advancing to the next frame" $ do
      it "isButtonDown is True when the button is currently held" $ do
        ctx0 <- freshCtx
        let ctx = advance buttonDown ctx0
        (b, _) <- runView isButtonDown ctx
        b `shouldBe` True

      it "isButtonDown remains True for as long as the button stays held, not just on the first frame" $ do
        (_, ctx1) <- runWith buttonDown (pure ())
        let ctx2 = advance buttonDown ctx1
            ctx3 = advance buttonDown ctx2
        (b, _) <- runView isButtonDown ctx3
        b `shouldBe` True

      it "isButtonReleased is True on the frame the button goes up" $ do
        (_, ctx0) <- runWith buttonDown (pure ())
        let ctx = advance noInput ctx0
        (b, _) <- runView isButtonReleased ctx
        b `shouldBe` True

      it "isButtonReleased is False when the button stays up" $ do
        ctx0 <- freshCtx
        let ctx = advance noInput ctx0
        (b, _) <- runView isButtonReleased ctx
        b `shouldBe` False

      it "isButtonReleased is False when the button stays down" $ do
        (_, ctx0) <- runWith buttonDown (pure ())
        let ctx = advance buttonDown ctx0
        (b, _) <- runView isButtonReleased ctx
        b `shouldBe` False

    it "isButtonDown returns True when the button is held" $ do
      (b, _) <- runWith buttonDown isButtonDown
      b `shouldBe` True

    it "isButtonReleased returns True on the release frame" $ do
      (_, ctx0) <- runWith buttonDown (pure ())
      let ctx = advance noInput ctx0
      (b, _) <- runView isButtonReleased ctx
      b `shouldBe` True

    it "isDragging is True when the element holds capture" $ do
      -- Acquire real capture on a down frame, then check it's retained
      -- through the following release frame — a genuinely realistic
      -- "captured but button now up" scenario.
      ctx1 <- capturedCtx
      let ctx2 = advance mouseOnCenter ctx1
      (dragging, _) <- runView (isDragging ()) ctx2
      dragging `shouldBe` True

    it "isDragging is False when a different element holds capture" $ do
      (_, ctx) <- runView (acquireCapture ElemB)
                    (emptyViewContext testBounds mouseOnCenterDown twoElemTheme)
      (dragging, _) <- runView (isDragging ElemA) ctx
      dragging `shouldBe` False

  describe "acquireCapture" $ do
    it "acquires capture when the button is down and nothing is captured" $ do
      (_, ctx) <- runWith buttonDown (acquireCapture ())
      contextCaptured ctx `shouldBe` MouseCapturedBy ()

    it "does not acquire capture when another element already holds it" $ do
      (_, ctx') <- runView (acquireCapture ElemB >> acquireCapture ElemA)
                     (emptyViewContext testBounds buttonDown twoElemTheme)
      contextCaptured ctx' `shouldBe` MouseCapturedBy ElemB

    it "does nothing when the button is not down" $ do
      (_, ctx) <- run0 (acquireCapture ())
      contextCaptured ctx `shouldBe` MouseNotCaptured

    it "makes the element dragging once capture is acquired" $ do
      (dragging, _) <- runWith buttonDown (acquireCapture () >> isDragging ())
      dragging `shouldBe` True

    it "still acquires capture if the press started elsewhere and the cursor moves onto the element while held" $ do
      -- Nobody claims capture on the first frame of the press (e.g. it
      -- started over empty space, or a different element that didn't
      -- acquire it); the element only becomes hit once the cursor moves
      -- onto it on a later frame, and should still be able to claim capture
      -- then.
      (_, ctx1) <- runWith mouseOnCenterDown (pure ())
      let ctx2 = advance mouseOnCenterDown ctx1
      (_, ctx3) <- runView (acquireCapture ()) ctx2
      contextCaptured ctx3 `shouldBe` MouseCapturedBy ()

  describe "isMouseFree" $ do
    it "is True when no element holds capture" $ do
      (result, _) <- run0 isMouseFree
      result `shouldBe` True

    it "is False when an element holds capture" $ do
      (_, ctx) <- runWith buttonDown (acquireCapture ())
      (result, _) <- runView isMouseFree ctx
      result `shouldBe` False

  describe "isRegionHit" $ do
    it "is True when the mouse is inside the current bounds" $ do
      (hit, _) <- runWith mouseOnCenter isRegionHit
      hit `shouldBe` True

    it "is False when the mouse is outside the current bounds" $ do
      (hit, _) <- runWith (noInput { inputMousePosition = Point 200 200 }) isRegionHit
      hit `shouldBe` False

  describe "mouse-over memory (registerMouseOver / wasMouseOverLastFrame)" $ do
    it "is False when nothing has ever been registered" $ do
      (result, _) <- run0 (wasMouseOverLastFrame ())
      result `shouldBe` False

    it "is still False for an element registered only this frame" $ do
      (result, _) <- run0 (registerMouseOver () >> wasMouseOverLastFrame ())
      result `shouldBe` False

    it "is True on the frame after registration" $ do
      (_, ctx) <- run0 (registerMouseOver ())
      let ctx' = advance noInput ctx
      (result, _) <- runView (wasMouseOverLastFrame ()) ctx'
      result `shouldBe` True

    it "is False two frames after registration if not re-registered" $ do
      (_, ctx0) <- run0 (registerMouseOver ())
      let ctx1 = advance noInput ctx0
      (_, ctx1') <- runView (pure ()) ctx1
      let ctx2 = advance noInput ctx1'
      (result, _) <- runView (wasMouseOverLastFrame ()) ctx2
      result `shouldBe` False

    it "remembers every element registered in the same frame, not just the last" $ do
      (_, ctx) <- runTwoElem (registerMouseOver ElemA >> registerMouseOver ElemB)
      let ctx' = advance noInput ctx
      (a, _) <- runView (wasMouseOverLastFrame ElemA) ctx'
      (b, _) <- runView (wasMouseOverLastFrame ElemB) ctx'
      (a, b) `shouldBe` (True, True)

    it "keeps results independent per element" $ do
      (_, ctx) <- runTwoElem (registerMouseOver ElemA)
      let ctx' = advance noInput ctx
      (a, _) <- runView (wasMouseOverLastFrame ElemA) ctx'
      (b, _) <- runView (wasMouseOverLastFrame ElemB) ctx'
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
      let ctx' = advance noInput ctx
      (result, _) <- runView isAnyMouseOver ctx'
      result `shouldBe` False
