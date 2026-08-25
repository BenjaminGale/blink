{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The shared "activated by a click or by Enter while focused" contract
-- every 'Blink.Controls.Button.buttonBase'-based control must satisfy, on
-- top of the raw-event\/focus\/hit-region contract every control already
-- satisfies (see 'Blink.Controls.ControlBehaviour.controlBehaviourSpec').
-- 'Blink.Controls.ButtonSpec' runs this against 'Blink.Controls.Button.button';
-- 'toggleButton', 'checkbox', and 'radioButton' reuse it too, on top of
-- their own toggle-specific contract.
module Blink.Controls.ButtonBehaviour
  ( buttonBehaviourSpec
  ) where

import Test.Hspec

import Blink.Controls.Button (HasButtonConfig, onActivated)
import Blink.Controls.Control (Attr, HasControlConfig, HasElementConfig, isFocusable)
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Controls.ElementBehaviour (tagged)
import Blink.Geometry (Point, Rectangle)
import Blink.Input (Key (KeyReturn))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.UI

-- | Every raw\/focus reaction (including 'Blink.Controls.Element.onClicked',
-- via 'tagged'), plus a tagged reaction to 'onActivated'.
taggedActivated :: (HasElementConfig e String cfg, HasButtonConfig e String cfg) => [Attr cfg]
taggedActivated = onActivated (const [OutMsg "Activated"]) : tagged

-- | The activation contract: given how to render the control under test
-- with a given attrs list, asserts it's activated by Enter while focused
-- the same way it's activated by a click -- and not while unfocused or
-- disabled -- and that Enter raises 'onActivated' only, never
-- 'Blink.Controls.Element.onClicked' (mouse-only, per the split between the
-- two).
buttonBehaviourSpec
  :: (Ord e, Show e, HasControlConfig e String cfg, HasElementConfig e String cfg, HasButtonConfig e String cfg)
  => Rectangle                                   -- ^ bounds the control renders at
  -> UIContext e String                          -- ^ starting context (theme\/measurer already set up)
  -> e                                             -- ^ element id under test
  -> Point                                         -- ^ a point inside its margin (not part of its hit area)
  -> Rectangle                                     -- ^ the region making up its margin-inset hit area
  -> Point                                         -- ^ a point outside its bounds entirely
  -> ([Attr cfg] -> UI e String ())                -- ^ render the control under test with these attrs
  -> Spec
buttonBehaviourSpec bounds ctx eid marginPoint insideRect outsidePoint render = do
  controlBehaviourSpec defaultControlBehaviourConfig bounds ctx eid marginPoint insideRect outsidePoint render

  describe "keyboard activation" $ do
    it "raises Activated when Enter is pressed while focused" $ do
      result <- runInteractions bounds ctx (render taggedActivated) [Wait 1] [PressKey KeyReturn []]
      resultMessages result `shouldContain` ["Activated"]

    it "does not raise Clicked from Enter -- onClicked is mouse-only, unlike onActivated" $ do
      result <- runInteractions bounds ctx (render taggedActivated) [Wait 1] [PressKey KeyReturn []]
      resultMessages result `shouldNotContain` ["Clicked"]

    it "raises no Activated event from Enter while it doesn't hold focus" $ do
      result <- runInteractions bounds ctx (render (isFocusable False : taggedActivated)) [] [PressKey KeyReturn []]
      resultMessages result `shouldBe` []

    it "raises no Activated event from Enter while disabled, even while already focused" $ do
      result <- runInteractions bounds ctx (setFocus eid >> disableWhen True (render taggedActivated)) [] [PressKey KeyReturn []]
      resultMessages result `shouldBe` []
