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

import Blink.Attributes (Attr, HasControlConfig)
import Blink.Button (HasToggleConfig, ToggleEvent, isSelected, onSelectedChanged)
import Blink.ButtonBehaviour (buttonBehaviourSpec)
import Blink.ElementBehaviour (tagged)
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
  -> Point                                                -- ^ a point inside its margin-inset hit area
  -> Point                                                -- ^ a point outside its bounds entirely
  -> ([Attr e ToggleEvent String cfg] -> UI e String ())  -- ^ render the control under test with these attrs
  -> Spec
toggleBehaviourSpec bounds ctx eid marginPoint insidePoint outsidePoint render = do
  buttonBehaviourSpec bounds ctx eid marginPoint insidePoint outsidePoint (\attrs -> render (isSelected False : attrs))

  describe "toggle" $ do
    it "raises a selected-changed event with True when activated while unselected" $ do
      result <- runInteractions bounds ctx (render (isSelected False : taggedToggle)) [] [ClickAt insidePoint, Wait 1]
      resultMessages result `shouldContain` ["SelectedChanged:True"]

    it "raises a selected-changed event with False when activated while selected" $ do
      result <- runInteractions bounds ctx (render (isSelected True : taggedToggle)) [] [ClickAt insidePoint, Wait 1]
      resultMessages result `shouldContain` ["SelectedChanged:False"]

    it "also raises a selected-changed event when activated via Enter while focused" $ do
      result <- runInteractions bounds ctx (render (isSelected False : taggedToggle)) [Wait 1] [PressKey KeyReturn []]
      resultMessages result `shouldContain` ["SelectedChanged:True"]
