{-# LANGUAGE OverloadedStrings #-}
-- | The shared "activated by a click or by Enter while focused" contract
-- every 'Blink.Button.buttonBase'-based control must satisfy, on top of
-- the focus\/hit-region contract every control already satisfies (see
-- 'Blink.ControlBehaviour.controlBehaviourSpec'). 'Blink.ButtonSpec' runs
-- this against 'Blink.Button.button'; 'toggleButton', 'checkbox', and
-- 'radioButton' reuse it too, on top of their own toggle-specific contract.
module Blink.ButtonBehaviour
  ( buttonBehaviourSpec
  ) where

import Test.Hspec

import Blink.Attributes (Attr, HasControlConfig, isFocusable)
import Blink.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Element (HasElementEvent)
import Blink.ElementBehaviour (tagged)
import Blink.Geometry (Point, Rectangle)
import Blink.Input (Key (KeyReturn))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.UI

-- | The activation contract: given how to render the control under test
-- with a given attrs list, asserts it's activated by Enter while focused
-- the same way it's activated by a click -- and not while unfocused or
-- disabled.
buttonBehaviourSpec
  :: (Ord e, Show e, HasElementEvent ev, HasControlConfig e cfg)
  => Rectangle                                   -- ^ bounds the control renders at
  -> UIContext e String                          -- ^ starting context (theme\/measurer already set up)
  -> e                                             -- ^ element id under test
  -> Point                                         -- ^ a point inside its margin (not part of its hit area)
  -> Rectangle                                     -- ^ the region making up its margin-inset hit area
  -> Point                                         -- ^ a point outside its bounds entirely
  -> ([Attr e ev String cfg] -> UI e String ())    -- ^ render the control under test with these attrs
  -> Spec
buttonBehaviourSpec bounds ctx eid marginPoint insideRect outsidePoint render = do
  controlBehaviourSpec defaultControlBehaviourConfig bounds ctx eid marginPoint insideRect outsidePoint render

  describe "keyboard activation" $ do
    it "raises a click event when Enter is pressed while focused" $ do
      result <- runInteractions bounds ctx (render tagged) [Wait 1] [PressKey KeyReturn []]
      resultMessages result `shouldContain` ["Clicked"]

    it "raises no click event from Enter while it doesn't hold focus" $ do
      result <- runInteractions bounds ctx (render (isFocusable False : tagged)) [] [PressKey KeyReturn []]
      resultMessages result `shouldBe` []

    it "raises no click event from Enter while disabled, even while already focused" $ do
      result <- runInteractions bounds ctx (setFocus eid >> disableWhen True (render tagged)) [] [PressKey KeyReturn []]
      resultMessages result `shouldBe` []
