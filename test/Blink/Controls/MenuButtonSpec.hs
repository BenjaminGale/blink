{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.MenuButtonSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.Controls.Button (onActivated)
import Blink.Controls.Control (onFocusGained, post, postWith)
import Blink.Controls.Label (text)
import Blink.Controls.MenuButton (MenuButtonPart (..), isOpen, itemAttrs, items, menuButton, onOpenChanged)
import Blink.Element (elLayout)
import Blink.Geometry (Alignment (TopLeft), Point (..), Size (..), uniform)
import Blink.Input (Key (KeyDown, KeyEscape, KeyReturn, KeyUp), KeyEvent (..))
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..), noOpMeasurers)
import Blink.Style (Metrics (..), Style (..), StyleSet (..), emptyTheme, noBorder)
import Blink.Update (modify, put)

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

-- | What changes the open flag, or logs a keyboard-navigation event, in
-- 'navApp'.
data NavEvent = SetOpen Bool | Logged Text

-- | Same shape as 'menuApp', but every focus-gained and activation event
-- (trigger and items alike) appends a tagged entry to a log, so a test can
-- observe keyboard navigation and closing behaviour precisely -- the same
-- log-of-tags approach 'Blink.AppSpec.focusApp' uses for the same reason.
navApp :: App (MenuButtonPart Item) NavEvent (Bool, [Text])
navApp = App
  { startUp = pure (False, [])
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \(open, _) ->
      (menuButton id
        [ text "File"
        , isOpen open
        , onOpenChanged (postWith SetOpen)
        , onFocusGained (post (Logged "Trigger focused"))
        , items [Open, Save]
        , itemAttrs (\i ->
            [ text (T.pack (show i))
            , onFocusGained (post (Logged (T.pack (show i) <> " focused")))
            , onActivated   (post (Logged (T.pack (show i) <> " activated")))
            ])
        ]) { elLayout = Layout fill fill TopLeft }
  , update  = \m -> modify $ \(open, log') -> case m of
      SetOpen b -> (b, log' ++ ["Open=" <> T.pack (show b)])
      Logged t  -> (open, log' ++ [t])
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

resultState :: FrameResult s -> s
resultState (Continue _ _ s) = s
resultState (Quit _ _ s)     = s

resultDraws :: FrameResult s -> [DrawCommand]
resultDraws (Continue ds _ _) = ds
resultDraws (Quit ds _ _)     = ds

drawnTexts :: FrameResult s -> [Text]
drawnTexts r = [t | DrawText _ t _ _ <- resultDraws r]

triggerPoint :: Point
triggerPoint = Point 50 50

-- | A single key press, with the mouse left resting on the trigger (not
-- pressed) so it never spuriously reclicks anything.
keyInput :: Key -> FrameInput
keyInput k = (mkInput triggerPoint False) { keyEvents = [KeyEvent k [] False] }

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

  describe "keyboard navigation while open" $ do
    it "opening the menu focuses the first item" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput triggerPoint True)
      result <- stepFrame handle (mkInput triggerPoint False)
      snd (resultState result) `shouldContain` ["Open focused"]

    it "Down moves the highlight to the next item" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput triggerPoint True)
      _      <- stepFrame handle (mkInput triggerPoint False) -- opens, focuses Open
      result <- stepFrame handle (keyInput KeyDown)
      last (snd (resultState result)) `shouldBe` "Save focused"

    it "Down on the last item does not wrap back to the first" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput triggerPoint True)
      _      <- stepFrame handle (mkInput triggerPoint False) -- opens, focuses Open
      atSave <- stepFrame handle (keyInput KeyDown)            -- Open -> Save
      atEnd  <- stepFrame handle (keyInput KeyDown)            -- Save -> nowhere
      snd (resultState atEnd) `shouldBe` snd (resultState atSave)

    it "Up on the first item does not wrap to the last" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput triggerPoint True)
      atOpen <- stepFrame handle (mkInput triggerPoint False) -- opens, focuses Open
      result <- stepFrame handle (keyInput KeyUp)
      snd (resultState result) `shouldBe` snd (resultState atOpen)

    it "Enter on the highlighted item activates it, closes the menu, and returns focus to the trigger" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput triggerPoint True)
      _      <- stepFrame handle (mkInput triggerPoint False) -- opens, focuses Open
      _      <- stepFrame handle (keyInput KeyDown)            -- focuses Save
      result <- stepFrame handle (keyInput KeyReturn)
      let (open, log') = resultState result
      open `shouldBe` False
      log' `shouldContain` ["Save activated"]
      last log' `shouldBe` "Trigger focused"

    it "Escape closes the menu without activating anything, and returns focus to the trigger" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput triggerPoint True)
      _      <- stepFrame handle (mkInput triggerPoint False) -- opens, focuses Open
      result <- stepFrame handle (keyInput KeyEscape)
      let (open, log') = resultState result
      open `shouldBe` False
      log' `shouldNotContain` ["Open activated", "Save activated"]
      last log' `shouldBe` "Trigger focused"
