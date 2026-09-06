{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The contract for a control whose 'Blink.View.Controls.Control.isFocusable' is fixed
-- rather than a default -- it always overrides its own value last, so
-- nothing a caller passes can change it (e.g. 'Blink.View.Controls.Label.label',
-- 'Blink.View.Controls.ProgressBar.progressBar'). 'Blink.View.Controls.ControlBehaviour.controlBehaviourSpec'
-- already covers the resulting behaviour with no 'isFocusable' passed at
-- all; this adds the one case that doesn't: a caller explicitly passing
-- 'isFocusable' 'True' still has no effect.
module Blink.View.Controls.FixedFocusBehaviour
  ( fixedNotFocusableSpec
  ) where

import Test.Hspec

import Blink.View.Controls.Control (Attribute, HasControlConfig, HasElementConfig, isFocusable)
import Blink.View.Controls.ElementBehaviour (tagged)
import Blink.Geometry (Rectangle)
import Blink.Interaction (InteractionResult (..), runInteractions)
import Blink.View

-- | Asserts that passing 'isFocusable' 'True' has no effect on a control
-- whose focus behaviour is fixed to never-focusable: it still doesn't
-- auto-claim focus when nothing else holds it.
fixedNotFocusableSpec
  :: (Ord e, HasControlConfig e String cfg, HasElementConfig e String cfg)
  => Rectangle                          -- ^ bounds the control renders at
  -> ViewContext e String                 -- ^ starting context (theme\/measurer already set up)
  -> ([Attribute cfg] -> View e String ())     -- ^ render the control under test with these attrs
  -> Spec
fixedNotFocusableSpec bounds ctx render =
  it "still never claims focus when isFocusable True is explicitly passed" $ do
    result <- runInteractions bounds ctx (render (isFocusable True : tagged)) [] []
    resultMessages result `shouldBe` []
