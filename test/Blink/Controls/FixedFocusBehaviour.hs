{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The contract for a control whose 'Blink.Controls.Control.isFocusable' is fixed
-- rather than a default -- it always appends its own value last, so nothing
-- a caller passes can change it (e.g. 'Blink.Controls.Label.label',
-- 'Blink.Controls.ProgressBar.progressBar'). 'Blink.ControlBehaviour.controlBehaviourSpec'
-- already covers the resulting behaviour with no 'isFocusable' passed at
-- all; this adds the one case that doesn't: a caller explicitly passing
-- 'isFocusable' 'True' still has no effect.
module Blink.Controls.FixedFocusBehaviour
  ( fixedNotFocusableSpec
  ) where

import Test.Hspec

import Blink.Controls.Control (HasControlConfig, isFocusable)
import Blink.Controls.Element (HasElementEvents)
import Blink.Controls.ElementBehaviour (tagged)
import Blink.Geometry (Rectangle)
import Blink.Interaction (InteractionResult (..), runInteractions)
import Blink.UI

-- | Asserts that passing 'isFocusable' 'True' has no effect on a control
-- whose focus behaviour is fixed to never-focusable: it still doesn't
-- auto-claim focus when nothing else holds it.
fixedNotFocusableSpec
  :: (Ord e, HasControlConfig e cfg, HasElementEvents e String cfg)
  => Rectangle                   -- ^ bounds the control renders at
  -> UIContext e String          -- ^ starting context (theme\/measurer already set up)
  -> ([cfg] -> UI e String ())   -- ^ render the control under test with these attrs
  -> Spec
fixedNotFocusableSpec bounds ctx render =
  it "still never claims focus when isFocusable True is explicitly passed" $ do
    result <- runInteractions bounds ctx (render (isFocusable True : tagged)) [] []
    resultMessages result `shouldBe` []
