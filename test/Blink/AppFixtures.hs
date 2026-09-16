-- | Shared fixtures for the "Blink.App"-driven test suite
-- (test/Blink/AppSpec.hs and the Blink.Controls.Menu*Spec files): the
-- small harness nearly every test there uses to run an 'App' and inspect
-- its 'FrameResult'.
module Blink.AppFixtures
  ( nullMsgQueue
  , resultState
  , resultDraws
  , drawnTexts
  ) where

import Data.Text (Text)

import Blink.App (FrameResult (..), MsgQueue (..))
import Blink.Rendering (DrawCommand (..))

-- | A 'MsgQueue' that holds nothing -- fine for any test app that never
-- requests a 'Cmd' via 'cmd'.
nullMsgQueue :: MsgQueue msg
nullMsgQueue = MsgQueue { enqueueMsg = \_ -> pure (), drainMsgs = pure [] }

resultState :: FrameResult s -> s
resultState (Continue _ _ s) = s
resultState (Quit _ _ s)     = s

resultDraws :: FrameResult s -> [DrawCommand]
resultDraws (Continue ds _ _) = ds
resultDraws (Quit ds _ _)     = ds

drawnTexts :: FrameResult s -> [Text]
drawnTexts r = [t | DrawText _ t _ _ <- resultDraws r]
