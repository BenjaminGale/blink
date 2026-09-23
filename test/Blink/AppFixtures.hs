-- | Shared fixtures for the "Blink.App"-driven test suite
-- (test/Blink/AppSpec.hs and the Blink.Controls.Menu*Spec files): the
-- small harness nearly every test there uses to run an 'App' and inspect
-- its 'FrameResult'.
module Blink.AppFixtures
  ( nullMsgQueue
  , startApp
  , resultState
  , resultDraws
  , drawnTexts
  , resultLog
  , logAddedBetween
  , testStyle
  , testMetrics
  , testStyleSet
  , solidPalette
  ) where

import Data.Text (Text)

import Blink.App (App, BlinkHandle, FrameResult (..), MsgQueue (..), configureEventDriven)
import Blink.Geometry (uniform)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..), noOpMeasurers)
import Blink.Style (Metrics (..), Palette (..), Style (..), StyleSet (..), noBorder)

-- | A 'MsgQueue' that holds nothing -- fine for any test app that never
-- requests a 'Cmd' via 'cmd'.
nullMsgQueue :: MsgQueue msg
nullMsgQueue = MsgQueue { enqueueMsg = \_ -> pure (), drainMsgs = pure [] }

-- | Starts @app@ event-driven, with no 'Cmd' queue, animation wake-up or
-- text measurement.
startApp :: Ord e => App e msg s -> IO (BlinkHandle s)
startApp app = configureEventDriven app nullMsgQueue (pure ()) noOpMeasurers

resultState :: FrameResult s -> s
resultState (Continue _ _ s) = s
resultState (Quit _ _ s)     = s

resultDraws :: FrameResult s -> [DrawCommand]
resultDraws (Continue ds _ _) = ds
resultDraws (Quit ds _ _)     = ds

drawnTexts :: FrameResult s -> [Text]
drawnTexts r = [t | DrawText _ t _ _ <- resultDraws r]

-- | The log of an app whose state pairs some value with an append-only log.
resultLog :: FrameResult (a, [Text]) -> [Text]
resultLog = snd . resultState

-- | The log entries added between two frames of such an app.
logAddedBetween :: FrameResult (a, [Text]) -> FrameResult (a, [Text]) -> [Text]
logAddedBetween earlier later = drop (length (resultLog earlier)) (resultLog later)

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

-- | Every colour set to @c@, for a test that needs the library's real
-- 'Blink.Style.Defaults.defaultTheme' but not any particular colours.
-- Record-update the fields a test needs to tell apart.
solidPalette :: Colour -> Palette
solidPalette c = Palette
  { paletteAccent = c, paletteFocusRing = c, paletteSurface = c, paletteSurfaceHover = c
  , paletteSurfaceDisabled = c, paletteTextPrimary = c, paletteTextMuted = c, paletteTextOnAccent = c
  , paletteBorder = c, paletteBorderHover = c, paletteIcon = c, paletteIconHover = c
  }
