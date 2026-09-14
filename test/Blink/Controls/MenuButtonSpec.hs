{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.MenuButtonSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.Controls.Control (postWith)
import Blink.Controls.Label (text)
import Blink.Controls.MenuButton (MenuButtonPart (..), isOpen, itemAttrs, items, menuButton, onOpenChanged)
import Blink.Element (elLayout)
import Blink.Geometry (Alignment (TopLeft), Point (..), Size (..), uniform)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..), noOpMeasurers)
import Blink.Style (Metrics (..), Style (..), StyleSet (..), emptyTheme, noBorder)
import Blink.Update (put)

data Item = Open | Save deriving (Eq, Ord, Show)

testStyle :: Style
testStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignLeft
  , styleBorderColour = Nothing
  }

testMetrics :: Metrics
testMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet { styleBase = testStyle, styleOverrides = mempty }

-- | A single menu button, filling the whole window so any point in it
-- reliably hits the trigger, with two items -- 'Open' and 'Save'.
menuApp :: App (MenuButtonPart Item) Bool Bool
menuApp = App
  { startUp = pure False
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \open ->
      (menuButton id
        [ text "File"
        , isOpen open
        , onOpenChanged (postWith id)
        , items [Open, Save]
        , itemAttrs (\i -> [text (T.pack (show i))])
        ]) { elLayout = Layout fill fill TopLeft }
  , update  = put
  }

mkInput :: Point -> Bool -> FrameInput
mkInput p down = FrameInput
  { mousePosition   = p
  , mouseButtonDown = down
  , keyEvents       = []
  , typedText       = []
  , wheelDelta      = 0
  , windowSize      = Size 100 100
  , quitRequested   = False
  , isAnimationTick = False
  }

nullMsgQueue :: MsgQueue msg
nullMsgQueue = MsgQueue { enqueueMsg = \_ -> pure (), drainMsgs = pure [] }

resultDraws :: FrameResult s -> [DrawCommand]
resultDraws (Continue ds _ _) = ds
resultDraws (Quit ds _ _)     = ds

drawnTexts :: FrameResult s -> [Text]
drawnTexts r = [t | DrawText _ t _ _ <- resultDraws r]

triggerPoint :: Point
triggerPoint = Point 50 50

spec :: Spec
spec = describe "Blink.Controls.MenuButton.menuButton" $ do
  it "opens on click, rendering its items' text through the popup layer" $ do
    handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
    _      <- stepFrame handle (mkInput triggerPoint True)
    result <- stepFrame handle (mkInput triggerPoint False)
    drawnTexts result `shouldContain` ["Open", "Save"]

  it "does not draw its items before being clicked" $ do
    handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
    result <- stepFrame handle (mkInput (Point 200 200) False)
    drawnTexts result `shouldNotContain` ["Open", "Save"]

  it "closes on a second click, no longer drawing its items" $ do
    handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
    _      <- stepFrame handle (mkInput triggerPoint True)
    _      <- stepFrame handle (mkInput triggerPoint False) -- opens
    _      <- stepFrame handle (mkInput triggerPoint True)
    result <- stepFrame handle (mkInput triggerPoint False) -- closes
    drawnTexts result `shouldNotContain` ["Open", "Save"]
