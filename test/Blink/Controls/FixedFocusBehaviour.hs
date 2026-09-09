{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The contract for a control whose 'Blink.Controls.Control.focusPolicy' is fixed
-- rather than a default -- it always overrides its own value last, so
-- nothing a caller passes can change it (e.g. 'Blink.Controls.Label.label',
-- 'Blink.Controls.ProgressBar.progressBar'). 'Blink.Controls.ControlBehaviour.controlBehaviourSpec'
-- already covers the resulting behaviour with no 'focusPolicy' passed at
-- all; this adds the one case that doesn't: a caller explicitly passing
-- 'focusPolicy' 'Focusable' still has no effect.
module Blink.Controls.FixedFocusBehaviour
  ( fixedNotFocusableSpec
  ) where

import Test.Hspec

import Blink.Controls.Control (Attribute, FocusPolicy (..), HasControlConfig, focusPolicy)
import Blink.Controls.ElementBehaviour (tagged)
import Blink.Geometry (Rectangle)
import Blink.Interaction (InteractionResult (..), runInteractions)
import Blink.View

-- | Asserts that passing 'focusPolicy' 'Focusable' has no effect on a control
-- whose focus behaviour is fixed to never-focusable: it still doesn't
-- auto-claim focus when nothing else holds it.
fixedNotFocusableSpec
  :: (Ord e, HasControlConfig e String cfg)
  => Rectangle                          -- ^ bounds the control renders at
  -> ViewContext e String                 -- ^ starting context (theme\/measurer already set up)
  -> ([Attribute cfg] -> View e String ())     -- ^ render the control under test with these attrs
  -> Spec
fixedNotFocusableSpec bounds ctx render =
  it "still never claims focus when focusPolicy Focusable is explicitly passed" $ do
    result <- runInteractions bounds ctx (render (focusPolicy Focusable : tagged)) [] []
    resultMessages result `shouldBe` []
