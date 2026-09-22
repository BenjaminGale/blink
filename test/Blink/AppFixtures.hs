-- | Shared fixtures for the "Blink.App"-driven test suite
-- (test/Blink/AppSpec.hs and the Blink.Controls.Menu*Spec files): the
-- small harness nearly every test there uses to run an 'App' and inspect
-- its 'FrameResult'.
module Blink.AppFixtures
  ( nullMsgQueue
  , resultState
  , resultDraws
  , drawnTexts
  , logAddedBetween
  , testStyle
  , testMetrics
  , testStyleSet
  ) where

import Data.Text (Text)

import Blink.App (FrameResult (..), MsgQueue (..))
import Blink.Geometry (uniform)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), noBorder)

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

-- | The log entries added between two frames of an app whose state pairs
-- some value with an append-only log.
logAddedBetween :: FrameResult (a, [Text]) -> FrameResult (a, [Text]) -> [Text]
logAddedBetween earlier later = drop (length (snd (resultState earlier))) (snd (resultState later))

-- | The plain black, left-aligned, chrome-less style/metrics nearly every
-- 'App'-driven test theme here starts from, via
-- @emptyTheme (testMetrics, testStyleSet)@.
testStyle :: Style
testStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignLeft
  , styleBorder       = noBorder
  }

testMetrics :: Metrics
testMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet { styleBase = testStyle, styleOverrides = mempty }
