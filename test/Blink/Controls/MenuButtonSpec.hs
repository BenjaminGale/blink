{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.MenuButtonSpec (spec) where

import Control.Monad (void, when)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.AppFixtures (drawnTexts, nullMsgQueue, resultDraws, resultState, testMetrics, testStyleSet)
import Blink.Controls.Button (onActivated)
import Blink.Controls.Control
  ( ControlInteraction (ciMouseDown)
  , control, defaultControlConfig, elementId, isEnabled, onFocusGained, onFocusLost, onMouseEntered
  , post, postWith, resolve
  )
import Blink.Controls.Label (text)
import Blink.Controls.MenuButton (MenuButtonPart (..), isOpen, itemAttrs, items, menuButton, onOpenChanged)
import Blink.Element (Element, elLayout, elementWithLayout, height, runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..))
import Blink.Input (Key (KeyDown, KeyEscape, KeyReturn, KeyTab, KeyUp), KeyEvent (..), Modifier (Shift))
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..), DrawCommand (..), noOpMeasurers)
import Blink.Style (Palette (..), Style (styleBackground), StyleSet (styleOverrides), VisualState (..), emptyTheme)
import Blink.Style.Defaults (defaultTheme)
import Blink.Update (modify, put)
import Blink.View (View, emit, withBounds)

data Item = Open | Save deriving (Eq, Ord, Show)

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
            , onFocusGained  (post (Logged (T.pack (show i) <> " focused")))
            , onActivated    (post (Logged (T.pack (show i) <> " activated")))
            , onMouseEntered (post (Logged (T.pack (show i) <> " hovered")))
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

-- | The colour drawn while an item is hovered, in 'hoverStyleSet' --
-- distinct from 'testStyle's own resting background so a test can tell
-- which one a draw used.
hoverColour :: Colour
hoverColour = RGBA 1 0 0 1

-- | 'testStyleSet' plus a 'CommonMouseOver' override, so a hovered
-- control's chrome-less background switches to 'hoverColour'.
hoverStyleSet :: StyleSet
hoverStyleSet = testStyleSet
  { styleOverrides = Map.fromList [(CommonMouseOver, \s -> s { styleBackground = hoverColour })] }

-- | Same geometry as 'navApp' (40x20 trigger, two 40x20 items below it),
-- always open, with a hover-distinguishing style so a test can tell
-- whether an item's hover style is actually being drawn.
hoverApp :: App (MenuButtonPart Item) Bool Bool
hoverApp = App
  { startUp = pure False
  , theme   = const (emptyTheme (testMetrics, hoverStyleSet))
  , view    = \_ -> fullView $
      runElement
        ((menuButton id
          [ text "File", isOpen True, onOpenChanged (const [])
          , items [Open, Save]
          , itemAttrs (\i -> [text (T.pack (show i)), width (exactly 40), height (exactly 20)])
          ]) { elLayout = Layout (exactly 40) (exactly 20) TopLeft })
  , update  = const (pure ())
  }

-- | Two independent 'menuButton's, side by side at root scope, each with a
-- single item, both logging their own trigger's and item's focus-gained
-- events -- for the outside-click race in 'twoMenusApp' below: menu A's
-- trigger sits at (0,0)-(40,20) with its item at (0,20)-(40,40); menu B's
-- at (100,0)-(140,20) and (100,20)-(140,40), far enough apart that a click
-- on one is unambiguously "outside" the other.
data TwoMenuElem = MenuAPart (MenuButtonPart Item) | MenuBPart (MenuButtonPart Item)
  deriving (Eq, Ord, Show)

data TwoMenuEvent = SetOpenA Bool | SetOpenB Bool | TwoMenuLogged Text

twoMenusApp :: App TwoMenuElem TwoMenuEvent (Bool, Bool, [Text])
twoMenusApp = App
  { startUp = pure (False, False, [])
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \(openA, openB, _) -> fullView $ do
      withBounds (Rectangle 0 0 40 20) $ void $
        runElement (oneMenu MenuAPart "A" openA (postWith SetOpenA))
      withBounds (Rectangle 100 0 40 20) $ void $
        runElement (oneMenu MenuBPart "B" openB (postWith SetOpenB))
  , update  = \m -> modify $ \(openA, openB, log') -> case m of
      SetOpenA b     -> (b, openB, log')
      SetOpenB b     -> (openA, b, log')
      TwoMenuLogged t -> (openA, openB, log' ++ [t])
  }
  where
    oneMenu tag label open onOpen =
      (menuButton tag
        [ text label
        , isOpen open
        , onOpenChanged onOpen
        , onFocusGained (post (TwoMenuLogged (label <> " trigger focused")))
        , items [Open]
        , itemAttrs (const
            [ text "X", width (exactly 40), height (exactly 20)
            , onFocusGained (post (TwoMenuLogged (label <> " item focused")))
            ])
        ]) { elLayout = Layout (exactly 40) (exactly 20) TopLeft }

menuATriggerPoint, menuBTriggerPoint :: Point
menuATriggerPoint = Point 20 10
menuBTriggerPoint = Point 120 10

mkInput :: Point -> Bool -> FrameInput
mkInput p down = emptyFrameInput
  { mousePosition   = p
  , mouseButtonDown = down
  , windowSize      = Size 100 100
  }

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

-- | A plain focusable control on either side of the menu button.
data SiblingElem = SiblingBefore | SiblingAfter | SiblingMenu (MenuButtonPart Item)
  deriving (Eq, Ord, Show)

-- | What changes the open flag, or logs a keyboard-navigation event, in
-- 'navSiblingApp'.
data SiblingEvent = SiblingOpen Bool | SiblingLog Text

-- | Like 'navApp', but with a plain focusable control placed before and
-- after the menu button in the root focus scope: @Before(0,0)-(40,20)@,
-- trigger @(50,0)-(90,20)@, items below the trigger, @After(100,0)-(140,20)@.
navSiblingApp :: App SiblingElem SiblingEvent (Bool, [Text])
navSiblingApp = App
  { startUp = pure (False, [])
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \(open, _) -> fullView $ do
      withBounds (Rectangle 0 0 40 20) $ void $ control $ resolve defaultControlConfig
        [ elementId SiblingBefore
        , onFocusGained (post (SiblingLog "Before focused"))
        , onFocusLost   (post (SiblingLog "Before lost"))
        ]
      withBounds (Rectangle 50 0 40 20) $ void $ runElement
        ((menuButton SiblingMenu
          [ text "File"
          , isOpen open
          , onOpenChanged (postWith SiblingOpen)
          , onFocusGained (post (SiblingLog "Trigger focused"))
          , onFocusLost   (post (SiblingLog "Trigger lost"))
          , items [Open, Save]
          , itemAttrs (\i ->
              [ text (T.pack (show i))
              , width (exactly 40), height (exactly 20)
              , onFocusGained (post (SiblingLog (T.pack (show i) <> " focused")))
              , onFocusLost   (post (SiblingLog (T.pack (show i) <> " lost")))
              ])
          ]) { elLayout = Layout (exactly 40) (exactly 20) TopLeft })
      withBounds (Rectangle 100 0 40 20) $ void $ control $ resolve defaultControlConfig
        [ elementId SiblingAfter
        , onFocusGained (post (SiblingLog "After focused"))
        , onFocusLost   (post (SiblingLog "After lost"))
        ]
  , update  = \m -> modify $ \(open, log') -> case m of
      SiblingOpen b -> (b, log' ++ ["Open=" <> T.pack (show b)])
      SiblingLog t  -> (open, log' ++ [t])
  }

-- | 'navSiblingApp'-only point: the trigger sits at (50,0)-(90,20).
siblingTriggerPoint :: Point
siblingTriggerPoint = Point 70 10

-- | A single key press (with optional modifiers), mouse left resting on the
-- trigger so it never spuriously reclicks anything.
siblingKeyInput :: Key -> [Modifier] -> FrameInput
siblingKeyInput k mods = (mkInput siblingTriggerPoint False) { keyEvents = [KeyEvent k mods False] }

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

    it "Down on the last item wraps to the first, in a single keypress" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      _      <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
      _      <- stepFrame handle (keyInput KeyDown)            -- Open -> Save
      result <- stepFrame handle (keyInput KeyDown)            -- Save -> Open
      last (snd (resultState result)) `shouldBe` "Open focused"

    it "Up on the first item wraps to the last, in a single keypress" $ do
      handle <- configureEventDriven navApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput navTriggerPoint True)
      _      <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
      result <- stepFrame handle (keyInput KeyUp)              -- Open -> Save
      last (snd (resultState result)) `shouldBe` "Save focused"

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

  describe "keyboard navigation across the menu's own focus scope boundary" $ do
    it "Tab from the last item closes the menu and returns focus to the trigger, not a random root-scope control" $ do
      handle <- configureEventDriven navSiblingApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput siblingTriggerPoint True)
      _      <- stepFrame handle (mkInput siblingTriggerPoint False) -- opens, focuses Open
      _      <- stepFrame handle (siblingKeyInput KeyDown [])        -- Open -> Save (last item)
      result <- stepFrame handle (siblingKeyInput KeyTab [])
      let (open, log') = resultState result
      open `shouldBe` False
      last log' `shouldBe` "Trigger focused"

    it "Shift-Tab from the first item closes the menu and returns focus to the trigger, not a random root-scope control" $ do
      handle <- configureEventDriven navSiblingApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput siblingTriggerPoint True)
      _      <- stepFrame handle (mkInput siblingTriggerPoint False) -- opens, focuses Open
      result <- stepFrame handle (siblingKeyInput KeyTab [Shift])
      let (open, log') = resultState result
      open `shouldBe` False
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

    it "focuses the newly opened menu, not the trigger of the one an outside click just closed" $ do
      handle <- configureEventDriven twoMenusApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput menuATriggerPoint True)
      _      <- stepFrame handle (mkInput menuATriggerPoint False) -- opens A, focuses "A item"
      _      <- stepFrame handle (mkInput menuBTriggerPoint True)
      result <- stepFrame handle (mkInput menuBTriggerPoint False) -- closes A, opens B
      let (openA, openB, log') = resultState result
      openA `shouldBe` False
      openB `shouldBe` True
      last log' `shouldBe` "B item focused"

  it "still draws an item's hover style once the popup's own panel is already registered" $ do
    handle <- configureEventDriven hoverApp nullMsgQueue (pure ()) noOpMeasurers
    _      <- stepFrame handle (mkInput navTriggerPoint True)
    _      <- stepFrame handle (mkInput navTriggerPoint False) -- opens, focuses Open
    _      <- stepFrame handle (mkInput navItemPoint False)    -- registers the panel's hit-rect
    result <- stepFrame handle (mkInput navItemPoint False)    -- steady-state hover, panel already in prev
    [c | FillRect _ c <- resultDraws result] `shouldContain` [hoverColour]

  describe "the open item list's own panel background" $
    it "does not let a click reach a control behind the popup, even off any item" $ do
      handle <- configureEventDriven clickThroughApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- stepFrame handle (mkInput backgroundPoint False) -- primes the panel's own hit-rect
      _      <- stepFrame handle (mkInput backgroundPoint True)
      result <- stepFrame handle (mkInput backgroundPoint False)
      resultState result `shouldBe` False
