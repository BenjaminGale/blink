module Blink.View.FocusSpec (spec) where

import Test.Hspec

import Blink.Style (Theme (..))
import Blink.View
import Blink.View.Fixtures

-- | A scope id (@Group@) plus two elements nested inside it, for testing
-- that a scoped focus change only affects that scope's own 'FocusState'.
-- 'Sibling' stands in for an unrelated element outside the scope.
data ScopeElems = Group | ItemA | ItemB | Sibling deriving (Eq, Ord, Show)

scopeTheme :: Theme ScopeElems
scopeTheme = mkTheme

-- | Whether @e@ has gained and\/or lost focus, as reported against @ctx@ --
-- the two 'hasGainedFocus'\/'hasLostFocus' queries needed to characterise
-- every element in a focus-change test, bundled so a test asserts on one
-- value per element instead of re-deriving the pair by hand each time.
focusReport :: Ord e => ViewContext e msg -> e -> IO (Bool, Bool)
focusReport ctx e = do
  (gained, _) <- runView (hasGainedFocus e) ctx
  (lost, _)   <- runView (hasLostFocus e) ctx
  pure (gained, lost)

spec :: Spec
spec = describe "Blink.View.Focus" $ do
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

    it "setFocus refuses to steal focus from a different element that already holds it this frame" $ do
      (f, _) <- runTwoElem (setFocus ElemA >> setFocus ElemB >> getFocus)
      f `shouldBe` Just ElemA

    it "setFocus still lets an element reaffirm itself after already claiming it this frame" $ do
      (f, _) <- runTwoElem (setFocus ElemA >> setFocus ElemA >> getFocus)
      f `shouldBe` Just ElemA

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
      let ctx' = advance noInput ctx
      (f, _) <- runView getFocus ctx'
      f `shouldBe` Just ()

    it "nextFrameContext clears focus when the element was not visited this frame" $ do
      (_, ctx0) <- run0 (setFocus ())
      let ctx1 = advance noInput ctx0
      (_, ctx2) <- runView (pure ()) ctx1
      let ctx3 = advance noInput ctx2
      (f, _) <- runView getFocus ctx3
      f `shouldBe` Nothing

  describe "tab stop" $ do
    it "returns Nothing when no tab stop has been registered" $ do
      (s, _) <- run0 getPreviousTabStop
      s `shouldBe` Nothing

    it "returns the element registered as the previous tab stop" $ do
      (s, _) <- run0 (setPreviousTabStop () >> getPreviousTabStop)
      s `shouldBe` Just ()

  describe "focus change (requestFocus / requestClearFocus)" $ do
    it "a focus request sets the new focus and reports it to the winner and the loser" $ do
      (_, ctx0') <- runTwoElem (setFocus ElemA)
      (_, ctx0) <- runView (requestFocus Nothing ElemB) ctx0'
      let ctx1 = settleEffects ctx0
      (newFocus, _)               <- runView getFocus ctx1
      (winnerGained, winnerLost)  <- focusReport ctx1 ElemB
      (loserGained, loserLost)    <- focusReport ctx1 ElemA
      newFocus     `shouldBe` Just ElemB
      winnerGained `shouldBe` True
      winnerLost   `shouldBe` False
      loserGained  `shouldBe` False
      loserLost    `shouldBe` True

    it "a clear removes focus and reports it to the loser, with no winner" $ do
      (_, ctx0') <- runTwoElem (setFocus ElemA)
      (_, ctx0) <- runView (requestClearFocus Nothing) ctx0'
      let ctx1 = settleEffects ctx0
      (newFocus, _)             <- runView getFocus ctx1
      (loserGained, loserLost)  <- focusReport ctx1 ElemA
      (otherGained, _)          <- focusReport ctx1 ElemB
      newFocus     `shouldBe` Nothing
      loserGained  `shouldBe` False
      loserLost    `shouldBe` True
      otherGained  `shouldBe` False

    it "a focus request with nothing previously focused reports no loser" $ do
      ctx0' <- snd <$> runTwoElem (pure ())
      (_, ctx0) <- runView (requestFocus Nothing ElemB) ctx0'
      let ctx1 = settleEffects ctx0
      (winnerGained, _) <- focusReport ctx1 ElemB
      (_, otherLost)    <- focusReport ctx1 ElemA
      winnerGained `shouldBe` True
      otherLost    `shouldBe` False

    it "the recorded change stays visible for exactly one more frame, then is cleared" $ do
      (_, ctx0') <- runTwoElem (setFocus ElemA)
      (_, ctx0) <- runView (requestFocus Nothing ElemB) ctx0'
      -- 'ctx2' settles the same queued request fresh from 'ctx0' rather
      -- than chaining off 'ctx1' -- 'settleEffects' doesn't clear the
      -- queue it just applied, so advancing 'ctx1' instead would reapply
      -- the same request a second time.
      let ctx1 = settleEffects ctx0
          ctx2 = advance noInput ctx0
          ctx3 = advance noInput ctx2
      (gainedAtApply, _)    <- focusReport ctx1 ElemB
      (_, lostAtApply)      <- focusReport ctx1 ElemA
      (gainedNextFrame, _)  <- focusReport ctx2 ElemB
      (_, lostNextFrame)    <- focusReport ctx2 ElemA
      (gainedFrameAfter, _) <- focusReport ctx3 ElemB
      (_, lostFrameAfter)   <- focusReport ctx3 ElemA
      gainedAtApply    `shouldBe` True
      lostAtApply      `shouldBe` True
      gainedNextFrame  `shouldBe` True
      lostNextFrame    `shouldBe` True
      gainedFrameAfter `shouldBe` False
      lostFrameAfter   `shouldBe` False

    it "a scoped focus request updates only that scope's FocusState, not root's" $ do
      let ctx0' = emptyViewContext testBounds noInput scopeTheme :: ViewContext ScopeElems ()
      (_, ctx0a) <- runView (requestFocus Nothing Group) ctx0'
      (_, ctx0)  <- runView (requestFocus (Just Group) ItemB) ctx0a
      let ctx1 = settleEffects ctx0
      (insideGained, _) <- runView (withFocusScope Group (hasGainedFocus ItemB)) ctx1
      (rootGained, _)   <- runView (hasGainedFocus ItemB) ctx1
      insideGained `shouldBe` True
      rootGained   `shouldBe` False

  describe "withFocusScope" $ do
    it "lets a child claim focus once the scope is already the focused element" $ do
      let ctx0' = emptyViewContext testBounds noInput scopeTheme :: ViewContext ScopeElems ()
      (_, ctx0) <- runView (requestFocus Nothing Group) ctx0'
      let ctx1 = settleEffects ctx0
      (inside, _) <- runView (withFocusScope Group (setFocus ItemA >> getFocus)) ctx1
      inside `shouldBe` Just ItemA

    it "blocks a child from claiming focus when nothing is focused anywhere" $ do
      let ctx0 = emptyViewContext testBounds noInput scopeTheme :: ViewContext ScopeElems ()
      (inside, _) <- runView (withFocusScope Group (setFocus ItemA >> getFocus)) ctx0
      inside `shouldNotBe` Just ItemA

    it "blocks a child from claiming focus when a different element holds root focus" $ do
      let ctx0' = emptyViewContext testBounds noInput scopeTheme :: ViewContext ScopeElems ()
      (_, ctx0)   <- runView (setFocus Sibling) ctx0'
      (inside, _) <- runView (withFocusScope Group (setFocus ItemA >> getFocus)) ctx0
      inside `shouldNotBe` Just ItemA

    it "leaves root's own focus untouched by a blocked child's claim attempt" $ do
      let ctx0' = emptyViewContext testBounds noInput scopeTheme :: ViewContext ScopeElems ()
      (_, ctx0) <- runView (setFocus Sibling) ctx0'
      (_, ctx1) <- runView (withFocusScope Group (setFocus ItemA)) ctx0
      (root, _) <- runView getFocus ctx1
      root `shouldBe` Just Sibling

    it "reads the scope's own id as focused once a child inside it is claimed" $ do
      let ctx0' = emptyViewContext testBounds noInput scopeTheme :: ViewContext ScopeElems ()
      (_, ctx0) <- runView (requestFocus Nothing Group) ctx0'
      let ctx1 = settleEffects ctx0
      (_, ctx2) <- runView (withFocusScope Group (setFocus ItemA)) ctx1
      (groupFocused, _) <- runView (isFocused Group) ctx2
      groupFocused `shouldBe` True

    it "does not claim or persist state while the surrounding context is disabled" $ do
      let ctx0' = emptyViewContext testBounds noInput scopeTheme :: ViewContext ScopeElems ()
      (_, ctx0) <- runView (requestFocus Nothing Group) ctx0'
      let ctx1 = settleEffects ctx0
      (_, ctx2) <- runView (disableWhen True (withFocusScope Group (setFocus ItemA))) ctx1
      (inside, _) <- runView (withFocusScope Group getFocus) ctx2
      inside `shouldBe` Nothing

    it "keeps a claim made while live after a later render where the scope is blocked" $ do
      let ctx0' = emptyViewContext testBounds noInput scopeTheme :: ViewContext ScopeElems ()
      (_, ctxA0) <- runView (requestFocus Nothing Group) ctx0'
      let ctxA = settleAndClearEffects ctxA0
      (_, ctxB) <- runView (withFocusScope Group (setFocus ItemA)) ctxA
      -- Root focus moves away from Group, so the scope is blocked on its
      -- next render.
      (_, ctxC0) <- runView (requestFocus Nothing Sibling) ctxB
      let ctxC = settleAndClearEffects ctxC0
      (_, ctxD) <- runView (withFocusScope Group (pure ())) ctxC
      -- Root focus returns to Group, so the scope is live again.
      (_, ctxE0) <- runView (requestFocus Nothing Group) ctxD
      let ctxE = settleAndClearEffects ctxE0
      (stillFocused, _) <- runView (withFocusScope Group (isFocused ItemA)) ctxE
      stillFocused `shouldBe` True
