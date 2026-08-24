{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The shared "raises the right raw event for the right interaction"
-- contract every 'Blink.Controls.Element.element'-based primitive must satisfy.
-- 'Blink.ElementSpec' runs this against 'Blink.Controls.Element.element' directly;
-- anything built on top (a control, and every widget built on that) reuses
-- it to confirm the same raw facts still surface through its own attrs
-- type, on top of whatever that layer adds.
module Blink.Controls.ElementBehaviour
  ( elementBehaviourSpec
  , tagged
  ) where

import Test.Hspec

import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import Blink.Controls.Element
  ( HasElementEvents
  , onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed
  , onFocusGained, onFocusLost
  )
import Blink.Generators (genPointIn)
import Blink.Geometry (Point, Rectangle)
import Blink.Input (Key (KeySpace))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.UI

-- | Tags every raw event a reaction built on 'Blink.Controls.Element.element' can
-- raise with a plain label naming it, discarding any payload -- enough to
-- assert "this fired" declaratively without a bespoke message type per
-- caller.
tagged :: HasElementEvents e String cfg => [cfg]
tagged =
  [ onMouseEntered (const [OutMsg "MouseEntered"])
  , onMouseExited  (const [OutMsg "MouseExited"])
  , onMouseDown    (const [OutMsg "MouseDown"])
  , onMouseUp      (const [OutMsg "MouseUp"])
  , onClicked      (const [OutMsg "Clicked"])
  , onKeyPressed   (const [OutMsg "KeyPressed"])
  , onFocusGained  (const [OutMsg "FocusGained"])
  , onFocusLost    (const [OutMsg "FocusLost"])
  ]

-- | The raw-event contract: given how to render the thing under test with
-- a given attrs list, asserts that moving the cursor, pressing keys, and
-- changing focus raise the right raw events -- and nothing while disabled.
-- Every check that involves a point inside the thing's hit area picks one
-- at random from @insideRect@ on each run, rather than always the same
-- fixed spot, so a control whose hit area spans more than one visually
-- distinct part (e.g. a checkbox's glyph and caption) can't pass just
-- because one particular point happens to work.
elementBehaviourSpec
  :: (Ord e, HasElementEvents e String cfg)
  => Rectangle                                    -- ^ bounds the thing under test renders at
  -> UIContext e String                           -- ^ starting context (theme\/measurer already set up)
  -> e                                             -- ^ element id under test
  -> Rectangle                                     -- ^ the region making up its hit area
  -> Point                                         -- ^ a point outside its bounds
  -> ([cfg] -> UI e String ())                     -- ^ render the thing under test with these attrs
  -> Spec
elementBehaviourSpec bounds ctx eid insideRect outside render = do
  describe "hover" $ do
    it "raises a mouse enter event when the cursor moves into its bounds" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render tagged) [] [MoveTo p])
      assert (resultMessages result == ["MouseEntered"])

    it "raises a mouse exit event when the cursor moves back out" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render tagged) [MoveTo p] [MoveTo outside])
      assert (resultMessages result == ["MouseExited"])

    it "raises nothing while disabled, even with the cursor over it" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (disableWhen True (render tagged)) [] [MoveTo p])
      assert (resultMessages result == [])

  describe "press and click" $ do
    it "raises a press event when pressed while the cursor is over it" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render tagged) [] [MouseDown p])
      assert ("MouseDown" `elem` resultMessages result)

    it "raises a release event when released while the cursor is over it" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render tagged) [MouseDown p] [MouseUp p])
      assert ("MouseUp" `elem` resultMessages result)

    it "raises a click event when pressed and released without the cursor leaving its bounds" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render tagged) [] [ClickAt p])
      assert ("Clicked" `elem` resultMessages result)

  describe "keyboard" $ do
    it "raises a key event while it holds focus" $ do
      focused <- runInteractions bounds ctx (setFocus eid) [] []
      result  <- runInteractions bounds (resultContext focused) (render tagged) [] [PressKey KeySpace []]
      resultMessages result `shouldBe` ["KeyPressed"]

    it "raises nothing while it doesn't hold focus" $ do
      result <- runInteractions bounds ctx (render tagged) [] [PressKey KeySpace []]
      resultMessages result `shouldBe` []

    it "raises nothing while disabled, even while focused" $ do
      focused <- runInteractions bounds ctx (setFocus eid) [] []
      result  <- runInteractions bounds (resultContext focused) (disableWhen True (render tagged)) [] [PressKey KeySpace []]
      resultMessages result `shouldBe` []

  -- Priming a focus change and observing it happen within the same
  -- continuous 'runInteractions' call (setup phase queues it, test phase
  -- observes it) rather than two calls glued together via 'resultContext'
  -- -- see "Blink.InteractionSpec"'s "chaining two runInteractions calls
  -- via resultContext" for why the latter corrupts a focus change's origin.
  describe "focus" $ do
    it "raises a focus gained event when given focus" $ do
      result <- runInteractions bounds ctx (requestFocus Nothing eid >> render tagged) [Wait 1] []
      resultMessages result `shouldBe` ["FocusGained"]

    it "raises a focus lost event when its focus is cleared" $ do
      result <- runInteractions bounds ctx (setFocus eid >> requestClearFocus Nothing >> render tagged) [Wait 1] []
      resultMessages result `shouldBe` ["FocusLost"]
