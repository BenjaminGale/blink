{-# LANGUAGE OverloadedStrings #-}
-- | The shared "activating it changes its selected state and reports the
-- new value" contract every toggle-style control must satisfy, on top of
-- the activation contract every button-like control already satisfies (see
-- 'Blink.ButtonBehaviour.buttonBehaviourSpec'). 'Blink.Button.toggleButton',
-- 'Blink.Checkbox.checkbox', and 'Blink.RadioButton.radioButton' all reuse
-- this, each passing the function describing how activating it changes its
-- own selected state (see 'toggleBehaviourSpec').
module Blink.ToggleBehaviour
  ( toggleBehaviourSpec
  ) where

import Test.Hspec
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import Blink.Control (HasControlConfig)
import Blink.Button (HasToggleConfig, ToggleEvent, isSelected, onSelectedChanged)
import Blink.Element (Attr)
import Blink.ButtonBehaviour (buttonBehaviourSpec)
import Blink.ElementBehaviour (tagged)
import Blink.Generators (genPointIn)
import Blink.Geometry (Point, Rectangle)
import Blink.Input (Key (KeyReturn))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.UI

-- | Every raw\/activation reaction, plus a tagged reaction to
-- 'onSelectedChanged' naming the value it changed to.
taggedToggle :: HasToggleConfig cfg => [Attr e ToggleEvent String cfg]
taggedToggle = onSelectedChanged (\b -> [OutMsg ("SelectedChanged:" ++ show b)]) : tagged

-- | 'True' when @msgs@ reports activating a control starting at @current@
-- the way @next@ says it should: the changed-to value when that's actually
-- a change (see 'Blink.Button.toggleBase'), or no 'SelectedChanged' at all
-- when it isn't. Other tagged messages (hover, click, focus, ...) may
-- freely appear alongside either way.
reportsSelectedChange :: (Bool -> Bool) -> Bool -> [String] -> Bool
reportsSelectedChange next current msgs
  | next current /= current = tag (next current) `elem` msgs
  | otherwise                = tag True `notElem` msgs && tag False `notElem` msgs
  where tag b = "SelectedChanged:" ++ show b

-- | The toggle contract: given how to render the control under test with a
-- given attrs list, and how activating it (by click or by Enter while
-- focused) changes its selected state from its current value -- @not@ for
-- a control that flips every time, @const True@ for one that only ever
-- becomes selected -- asserts it reports exactly that change, or nothing
-- when the value would stay the same.
toggleBehaviourSpec
  :: (Ord e, Show e, HasControlConfig e cfg, HasToggleConfig cfg)
  => (Bool -> Bool)                                       -- ^ how activating it changes its selected state
  -> Rectangle                                          -- ^ bounds the control renders at
  -> UIContext e String                                 -- ^ starting context (theme\/measurer already set up)
  -> e                                                    -- ^ element id under test
  -> Point                                                -- ^ a point inside its margin (not part of its hit area)
  -> Rectangle                                            -- ^ the region making up its margin-inset hit area
  -> Point                                                -- ^ a point outside its bounds entirely
  -> ([Attr e ToggleEvent String cfg] -> UI e String ())  -- ^ render the control under test with these attrs
  -> Spec
toggleBehaviourSpec next bounds ctx eid marginPoint insideRect outsidePoint render = do
  buttonBehaviourSpec bounds ctx eid marginPoint insideRect outsidePoint (\attrs -> render (isSelected False : attrs))

  describe "toggle" $ do
    it "reports the change from unselected" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render (isSelected False : taggedToggle)) [] [ClickAt p, Wait 1])
      assert (reportsSelectedChange next False (resultMessages result))

    it "reports the change from selected" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render (isSelected True : taggedToggle)) [] [ClickAt p, Wait 1])
      assert (reportsSelectedChange next True (resultMessages result))

    it "also reports the change when activated via Enter while focused" $ do
      result <- runInteractions bounds ctx (render (isSelected False : taggedToggle)) [Wait 1] [PressKey KeyReturn []]
      reportsSelectedChange next False (resultMessages result) `shouldBe` True
