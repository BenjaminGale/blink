{-# LANGUAGE OverloadedStrings #-}
-- | The shared focus\/hit-region contract every 'Blink.Control.control'-based
-- control must satisfy, on top of the raw-event contract every element
-- already satisfies (see 'Blink.ElementBehaviour.elementBehaviourSpec').
-- 'Blink.ControlSpec' runs this against 'Blink.Control.control' directly;
-- any widget built on top reuses it to confirm the same focus\/hit-region
-- behaviour still holds through its own attrs list.
module Blink.ControlBehaviour
  ( controlBehaviourSpec
  ) where

import Test.Hspec

import Blink.Attributes (Attr, HasControlConfig, isTabStop)
import Blink.Element (HasElementEvent)
import Blink.ElementBehaviour (elementBehaviourSpec, tagged)
import Blink.Geometry (Point, Rectangle)
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.UI

-- | The focus\/hit-region contract: given how to render the control under
-- test with a given attrs list, asserts it claims and gives up focus the
-- way every control should, and that its hit region respects its margin.
controlBehaviourSpec
  :: (Ord e, Show e, HasElementEvent ev, HasControlConfig e cfg)
  => Rectangle                                   -- ^ bounds the control renders at
  -> UIContext e String                          -- ^ starting context (theme\/measurer already set up)
  -> e                                             -- ^ element id under test
  -> Point                                         -- ^ a point inside its margin (not part of its hit area)
  -> Point                                         -- ^ a point inside its margin-inset hit area
  -> Point                                         -- ^ a point outside its bounds entirely
  -> ([Attr e ev String cfg] -> UI e String ())    -- ^ render the control under test with these attrs
  -> Spec
controlBehaviourSpec bounds ctx eid marginPoint insidePoint outsidePoint render = do
  -- A control auto-claims focus the moment nothing else holds it, which
  -- would otherwise leak an incidental focus-gained event into every one
  -- of these raw-fact checks. 'isTabStop' 'False' keeps the reused
  -- contract about the same raw facts 'Blink.ElementSpec' checks, not
  -- about this control's own focus-claiming behaviour (covered below).
  elementBehaviourSpec bounds ctx eid insidePoint outsidePoint (\attrs -> render (isTabStop False : attrs))

  describe "focus claiming" $ do
    it "raises a focus gained event when nothing else is focused" $ do
      result <- runInteractions bounds ctx (render tagged) [] []
      resultMessages result `shouldBe` ["FocusGained"]

    it "raises nothing while disabled, even with nothing else focused" $ do
      result <- runInteractions bounds ctx (disableWhen True (render tagged)) [] []
      resultMessages result `shouldBe` []

    it "raises nothing when it isn't a tab stop, even with nothing else focused" $ do
      result <- runInteractions bounds ctx (render (isTabStop False : tagged)) [] []
      resultMessages result `shouldBe` []

    it "raises no focus lost event across further interactions that don't move focus away" $ do
      result <- runInteractions bounds ctx (render tagged) [Wait 1] [MoveTo outsidePoint, MoveTo insidePoint]
      resultMessages result `shouldNotContain` ["FocusLost"]

  describe "click and keyboard focus" $ do
    it "raises a focus gained event when clicked, even if it isn't a tab stop" $ do
      result <- runInteractions bounds ctx (render (isTabStop False : tagged)) [] [ClickAt insidePoint, Wait 1]
      resultMessages result `shouldContain` ["FocusGained"]

    it "raises no focus gained event from a click while disabled" $ do
      result <- runInteractions bounds ctx (disableWhen True (render (isTabStop False : tagged))) [] [ClickAt insidePoint, Wait 1]
      resultMessages result `shouldNotContain` ["FocusGained"]

    it "raises a focus lost event when Tab is pressed while focused" $ do
      -- Primed with a frame first so the control is already focused
      -- *before* Tab is pressed -- auto-claiming and giving it up again
      -- on the very same frame is a different, deliberately-ignored case
      -- (see 'Blink.Control.control').
      result <- runInteractions bounds ctx (render tagged) [Wait 1] [Tab]
      resultMessages result `shouldBe` ["FocusLost"]

  describe "hit region" $ do
    it "does not raise a click event for a press and release inside its margin" $ do
      result <- runInteractions bounds ctx (render tagged) [] [ClickAt marginPoint]
      resultMessages result `shouldNotContain` ["Clicked"]

    it "raises a click event for a press and release inside its margin-inset area" $ do
      result <- runInteractions bounds ctx (render tagged) [] [ClickAt insidePoint]
      resultMessages result `shouldContain` ["Clicked"]
