{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The shared "activated by a click or by Enter while focused" contract
-- every 'Blink.Controls.Button.buttonBase'-based control must satisfy, on
-- top of the raw-event\/focus\/hit-region contract every control already
-- satisfies (see 'Blink.Controls.ControlBehaviour.controlBehaviourSpec').
-- 'Blink.Controls.ButtonSpec' runs this against 'Blink.Controls.Button.button';
-- 'Blink.Controls.Toggle.toggleButton', 'Blink.Controls.Checkbox.checkbox',
-- 'Blink.Controls.RadioButton.radioButton', and
-- 'Blink.Controls.RepeatButton.repeatButton' reuse it too, on top of their
-- own control-specific contract -- the last passing a 'ButtonBehaviourConfig'
-- reflecting its 'Blink.Controls.Button.ActivateOnPress' Enter-repeat
-- behaviour, since it genuinely differs from the rest.
module Blink.Controls.ButtonBehaviour
  ( ButtonBehaviourConfig (..)
  , defaultButtonBehaviourConfig
  , buttonBehaviourSpec
  ) where

import Test.Hspec

import Blink.Controls.Button (HasButtonConfig, onActivated)
import Blink.Controls.Control (Attribute, HasControlConfig, HasElementConfig, isFocusable)
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Controls.ElementBehaviour (tagged)
import Blink.Geometry (Point, Rectangle)
import Blink.Input (InputState (..), Key (KeyReturn), KeyEvent (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.UI

-- | Every raw\/focus reaction (including 'Blink.Controls.Element.onClicked',
-- via 'tagged'), plus a tagged reaction to 'onActivated'.
taggedActivated :: (HasElementConfig e String cfg, HasButtonConfig e String cfg) => [Attribute cfg]
taggedActivated = onActivated (const [OutMsg "Activated"]) : tagged

-- | How a control's Enter-activation deviates from the plain
-- ('Blink.Controls.Button.ActivateOnClick'-based) default -- passed by
-- 'Blink.Controls.Button.ActivateOnPress'-based controls, i.e.
-- 'Blink.Controls.RepeatButton.repeatButton'.
newtype ButtonBehaviourConfig = ButtonBehaviourConfig
  { bbcRepeatsOnHeldEnter :: Bool
    -- ^ Whether a platform auto-repeat of a held Enter activates the
    -- control again. 'False' (the default) for an
    -- 'Blink.Controls.Button.ActivateOnClick'-based control, where a
    -- single logical key press is the whole interaction; 'True' for an
    -- 'Blink.Controls.Button.ActivateOnPress'-based one, where holding
    -- Enter is the keyboard equivalent of holding the mouse down -- see
    -- 'Blink.Controls.Button.ButtonActivation'.
  }

-- | 'bbcRepeatsOnHeldEnter' 'False' -- what every 'Blink.Controls.Button.ActivateOnClick'-based
-- control (every button-family control except 'Blink.Controls.RepeatButton.repeatButton') does.
defaultButtonBehaviourConfig :: ButtonBehaviourConfig
defaultButtonBehaviourConfig = ButtonBehaviourConfig { bbcRepeatsOnHeldEnter = False }

-- | The activation contract: given how to render the control under test
-- with a given attrs list, asserts it's activated by Enter while focused
-- the same way it's activated by a click -- and not while unfocused or
-- disabled -- and that Enter raises 'onActivated' only, never
-- 'Blink.Controls.Element.onClicked' (mouse-only, per the split between the
-- two).
buttonBehaviourSpec
  :: (Ord e, Show e, HasControlConfig e String cfg, HasElementConfig e String cfg, HasButtonConfig e String cfg)
  => ButtonBehaviourConfig                       -- ^ how this control's Enter-repeat behaviour deviates, if at all
  -> Rectangle                                   -- ^ bounds the control renders at
  -> UIContext e String                          -- ^ starting context (theme\/measurer already set up)
  -> e                                             -- ^ element id under test
  -> Point                                         -- ^ a point inside its margin (not part of its hit area)
  -> Rectangle                                     -- ^ the region making up its margin-inset hit area
  -> Point                                         -- ^ a point outside its bounds entirely
  -> ([Attribute cfg] -> UI e String ())                -- ^ render the control under test with these attrs
  -> Spec
buttonBehaviourSpec cfg bounds ctx eid marginPoint insideRect outsidePoint render = do
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

    let repeatDescription
          | bbcRepeatsOnHeldEnter cfg = "raises a second Activated from the platform's own auto-repeat of a held Enter"
          | otherwise                 = "does not raise a second Activated from the platform's own auto-repeat of a held Enter"
        repeatAssertion
          | bbcRepeatsOnHeldEnter cfg = (`shouldContain` ["Activated"])
          | otherwise                 = (`shouldNotContain` ["Activated"])

    it repeatDescription $ do
      -- 'PressKey' always synthesizes a fresh, non-repeat 'KeyEvent' (see
      -- its own Haddock) -- there's no 'Interaction' for a platform
      -- auto-repeat, so this drives one raw frame directly, the same way
      -- 'runInteractions' itself does internally.
      primed <- runInteractions bounds ctx (render taggedActivated) [Wait 1] [PressKey KeyReturn []]
      let primedCtx  = resultContext primed
          repeatInput = (contextInput primedCtx) { inputKeyEvents = [KeyEvent KeyReturn [] True] }
          repeatCtx   = nextFrameContext bounds repeatInput (contextTheme primedCtx) (contextAnimation primedCtx) primedCtx
      (_, afterRepeat) <- runUI (render taggedActivated) repeatCtx
      repeatAssertion (getMessages afterRepeat)
