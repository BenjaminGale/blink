{-# LANGUAGE OverloadedStrings #-}
-- | The shared "flips its selected state and reports the new value" contract
-- every flipping toggle-style control must satisfy, on top of the
-- activation contract every button-like control already satisfies (see
-- 'Blink.ButtonBehaviour.buttonBehaviourSpec'). 'Blink.Button.toggleButton'
-- and 'Blink.Checkbox.checkbox' both reuse this; 'Blink.RadioButton.radioButton'
-- doesn't, since it only ever moves from unselected to selected rather than
-- flipping, and is the only control that behaves that way so far.
module Blink.ToggleBehaviour
  ( toggleBehaviourSpec
  ) where

import Test.Hspec
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import Blink.Attributes (Attr, HasControlConfig)
import Blink.Button (HasToggleConfig, ToggleEvent, isSelected, onSelectedChanged)
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

-- | The flipping-toggle contract: given how to render the control under
-- test with a given attrs list, asserts that activating it -- by click or
-- by Enter while focused -- flips its selected state and reports the new
-- value, regardless of which state it started from.
toggleBehaviourSpec
  :: (Ord e, Show e, HasControlConfig e cfg, HasToggleConfig cfg)
  => Rectangle                                          -- ^ bounds the control renders at
  -> UIContext e String                                 -- ^ starting context (theme\/measurer already set up)
  -> e                                                    -- ^ element id under test
  -> Point                                                -- ^ a point inside its margin (not part of its hit area)
  -> Rectangle                                            -- ^ the region making up its margin-inset hit area
  -> Point                                                -- ^ a point outside its bounds entirely
  -> ([Attr e ToggleEvent String cfg] -> UI e String ())  -- ^ render the control under test with these attrs
  -> Spec
toggleBehaviourSpec bounds ctx eid marginPoint insideRect outsidePoint render = do
  buttonBehaviourSpec bounds ctx eid marginPoint insideRect outsidePoint (\attrs -> render (isSelected False : attrs))

  describe "toggle" $ do
    it "raises a selected-changed event with True when activated while unselected" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render (isSelected False : taggedToggle)) [] [ClickAt p, Wait 1])
      assert ("SelectedChanged:True" `elem` resultMessages result)

    it "raises a selected-changed event with False when activated while selected" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render (isSelected True : taggedToggle)) [] [ClickAt p, Wait 1])
      assert ("SelectedChanged:False" `elem` resultMessages result)

    it "also raises a selected-changed event when activated via Enter while focused" $ do
      result <- runInteractions bounds ctx (render (isSelected False : taggedToggle)) [Wait 1] [PressKey KeyReturn []]
      resultMessages result `shouldContain` ["SelectedChanged:True"]
