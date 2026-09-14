{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.MenuButtonSpec (spec) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.Controls.Button (onActivated)
import Blink.Controls.Control
  ( ControlInteraction (ciMouseDown)
  , control, defaultControlConfig, elementId, isEnabled, onFocusGained, post, postWith, resolve
  )
import Blink.Controls.Label (text)
import Blink.Controls.MenuButton (MenuButtonPart (..), isOpen, itemAttrs, items, menuButton, onOpenChanged)
import Blink.Element (Element, elLayout, elementWithLayout, height, runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Size (..), uniform)
import Blink.Input (Key (KeyDown, KeyEscape, KeyReturn, KeyUp), KeyEvent (..))
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..), noOpMeasurers)
import Blink.Style (Metrics (..), Palette (..), Style (..), StyleSet (..), emptyTheme, noBorder)
import Blink.Style.Defaults (defaultTheme)
import Blink.Update (modify, put)
import Blink.View (View, emit)

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

-- | 'menuApp', but disabled -- clicking its trigger must not open it, the
-- same as any other disabled control refusing to activate.
disabledMenuApp :: App (MenuButtonPart Item) Bool Bool
disabledMenuApp = menuApp
  { view = \open ->
      (menuButton id
        [ text "File"
        , isOpen open
        , onOpenChanged (postWith id)
        , items [Open, Save]
        , itemAttrs (\i -> [text (T.pack (show i))])
        , isEnabled False
        ]) { elLayout = Layout fill fill TopLeft }
  }

-- | What changes the open flag, or logs a keyboard-navigation event, in
-- 'navApp'.
data NavEvent = SetOpen Bool | Logged Text

-- | Same shape as 'menuApp', but every focus-gained and activation event
-- (trigger and items alike) appends a tagged entry to a log, so a test can
-- observe keyboard navigation and closing behaviour precisely -- the same
-- log-of-tags approach 'Blink.AppSpec.focusApp' uses for the same reason.
-- Unlike 'menuApp', the trigger and each item have real, fixed sizes (not
-- fill-the-window) so a click can land unambiguously on one of them, or on
-- neither -- see 'navTriggerPoint', 'navItemPoint', and 'navOutsidePoint'.
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
            , width (exactly 40), height (exactly 20)
            , onFocusGained (post (Logged (T.pack (show i) <> " focused")))
            , onActivated   (post (Logged (T.pack (show i) <> " activated")))
            ])
        ]) { elLayout = Layout (exactly 40) (exactly 20) TopLeft }
  , update  = \m -> modify $ \(open, log') -> case m of
      SetOpen b -> (b, log' ++ ["Open=" <> T.pack (show b)])
      Logged t  -> (open, log' ++ [t])
  }

-- | Every app below fills the whole test bounds; only the body varies.
fullView :: View e msg a -> Element e msg
fullView = elementWithLayout (Layout fill fill TopLeft) . void

data ClickThroughElem = Background | Menu (MenuButtonPart Item) deriving (Eq, Ord, Show)

-- | An arbitrary palette -- this app needs the library's real
-- 'defaultTheme', not this file's usual chrome-less 'testStyleSet', so the
-- popup's panel actually has the padding\/border\/margin
-- 'menuButtonListStyleKey' resolves to in real use (see 'clickThroughApp').
testPalette :: Palette
testPalette = Palette
  { paletteAccent = c, paletteFocusRing = c, paletteSurface = c, paletteSurfaceHover = c
  , paletteSurfaceDisabled = c, paletteTextPrimary = c, paletteTextMuted = c, paletteTextOnAccent = c
  , paletteBorder = c, paletteBorderHover = c, paletteIcon = c, paletteIconHover = c
  }
  where c = RGBA 0 0 0 1

-- | A window-filling background control behind an always-open menu, whose
-- item list is anchored at (0,20)-(60,80) (a 40x20 trigger, two 40x20
-- items, and 'containerStyle's real chrome -- margin 3, border 1, padding
-- 6, so 10px inset on every side): 'backgroundPoint' falls on the popup's
-- own panel background, inside its chrome but outside both items.
clickThroughApp :: App ClickThroughElem Bool Bool
clickThroughApp = App
  { startUp = pure False
  , theme   = const (defaultTheme testPalette)
  , view    = \_ -> fullView $ do
      ci <- control (resolve defaultControlConfig [elementId Background])
      when (ciMouseDown ci) (emit True)
      runElement
        ((menuButton Menu
          [ text "File", isOpen True, onOpenChanged (const [])
          , items [Open, Save]
          , itemAttrs (\i -> [text (T.pack (show i)), width (exactly 40), height (exactly 20)])
          ]) { elLayout = Layout (exactly 40) (exactly 20) TopLeft })
  , update  = \down -> modify (|| down)
  }

backgroundPoint :: Point
backgroundPoint = Point 5 25

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

-- | 'navApp'-only points: its trigger sits at (0,0)-(40,20); the item list,
-- placed below it by default, spans (0,20)-(40,60) (two 40x20 items
-- stacked); 'navOutsidePoint' falls outside both.
navTriggerPoint, navItemPoint, navOutsidePoint :: Point
navTriggerPoint = Point 20 10
navItemPoint     = Point 20 30 -- within the first item's own (0,20)-(40,40)
navOutsidePoint  = Point 80 80

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

  it "does not open on click while disabled" $ do
    handle <- configureEventDriven disabledMenuApp nullMsgQueue (pure ()) noOpMeasurers
    _      <- stepFrame handle (mkInput triggerPoint True)
    result <- stepFrame handle (mkInput triggerPoint False)
    drawnTexts result `shouldNotContain` ["Open", "Save"]

  describe "keyboard navigation while open" $ do
    it "opening the menu focuses the first item" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      result <- stepFrame handle (mkInput navTriggerPoint False)
      snd (resultState result) `shouldContain` ["Open focused"]

    it "Down moves the highlight to the next item" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      _      <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
      result <- stepFrame handle (keyInput KeyDown)
      last (snd (resultState result)) `shouldBe` "Save focused"

    it "Down on the last item does not wrap back to the first" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      _      <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
      atSave <- stepFrame handle (keyInput KeyDown)            -- Open -> Save
      atEnd  <- stepFrame handle (keyInput KeyDown)            -- Save -> nowhere
      snd (resultState atEnd) `shouldBe` snd (resultState atSave)

    it "Up on the first item does not wrap to the last" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      atOpen <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
      result <- stepFrame handle (keyInput KeyUp)
      snd (resultState result) `shouldBe` snd (resultState atOpen)

    it "Enter on the highlighted item activates it, closes the menu, and returns focus to the trigger" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      _      <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
      _      <- stepFrame handle (keyInput KeyDown)            -- focuses Save
      result <- stepFrame handle (keyInput KeyReturn)
      let (open, log') = resultState result
      open `shouldBe` False
      log' `shouldContain` ["Save activated"]
      last log' `shouldBe` "Trigger focused"

    it "Escape closes the menu without activating anything, and returns focus to the trigger" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      _      <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
      result <- stepFrame handle (keyInput KeyEscape)
      let (open, log') = resultState result
      open `shouldBe` False
      log' `shouldNotContain` ["Open activated", "Save activated"]
      last log' `shouldBe` "Trigger focused"

  describe "outside-click dismissal" $ do
    it "closes without activating anything when a click completes outside the trigger and the item list" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      _      <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
      _      <- stepFrame handle (mkInput navOutsidePoint True)
      result <- stepFrame handle (mkInput navOutsidePoint False)
      let (open, log') = resultState result
      open `shouldBe` False
      log' `shouldNotContain` ["Open activated", "Save activated"]
      last log' `shouldBe` "Trigger focused"

    it "still activates an item when the click lands on it, rather than being treated as outside" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      _      <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
      _      <- stepFrame handle (mkInput navItemPoint False) -- hovers the item first, as a real click would
      _      <- stepFrame handle (mkInput navItemPoint True)
      result <- stepFrame handle (mkInput navItemPoint False)
      snd (resultState result) `shouldContain` ["Open activated"]

  describe "the open item list's own panel background" $
    it "does not let a click reach a control behind the popup, even off any item" $ do
      handle <- configureEventDriven clickThroughApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput backgroundPoint False) -- primes the panel's own hit-rect
      _      <- stepFrame handle (mkInput backgroundPoint True)
      result <- stepFrame handle (mkInput backgroundPoint False)
      resultState result `shouldBe` False
