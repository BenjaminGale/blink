module Blink.View.SelectionSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (choose, forAll)

import Blink.View
import Blink.View.Fixtures
import Blink.View.Selection
  ( collapseToActive, collapseToHigh, collapseToLow, cursor, extendActive
  , selectionHasExtent, selectionHigh, selectionLow
  )

spec :: Spec
spec = describe "Blink.View.Selection" $ do
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
    it "getSelection returns Nothing when no selection exists" $ do
      (s, _) <- run0 (getSelection ())
      s `shouldBe` Nothing

    it "requestSelectionAt is deferred rather than applied immediately" $ do
      (s, _) <- run0 (requestSelectionAt () (Selection 1 4) >> getSelection ())
      s `shouldBe` Nothing

    it "a requested selection is visible via getSelection once settled" $ do
      (_, ctx) <- run0 (requestSelectionAt () (Selection 1 4))
      (s, _) <- runView (getSelection ()) (settleEffects ctx)
      s `shouldBe` Just (Selection 1 4)

    it "a later requestSelectionAt in the same frame overrides an earlier one" $ do
      (_, ctx) <- run0 (requestSelectionAt () (Selection 0 5) >> requestSelectionAt () (cursor 1))
      (s, _) <- runView (getSelection ()) (settleEffects ctx)
      s `shouldBe` Just (cursor 1)

    it "a selection for a different element replaces the one held before" $ do
      (_, ctx)   <- runTwoElem (requestSelectionAt ElemA (Selection 0 5))
      let ctx' = settleEffects ctx
      (_, ctx'') <- runView (requestSelectionAt ElemB (cursor 1)) ctx'
      let settled = settleEffects ctx''
      contextSelection ElemA settled `shouldBe` Nothing
      contextSelection ElemB settled `shouldBe` Just (cursor 1)

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
