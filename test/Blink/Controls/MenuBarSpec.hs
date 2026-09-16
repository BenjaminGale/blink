{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.MenuBarSpec (spec) where

import Data.Char (toUpper)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.AppFixtures (drawnTexts, nullMsgQueue, resultState, testMetrics, testStyleSet)
import Blink.Controls.Control (postWith)
import Blink.Controls.Label (mnemonic, text)
import Blink.Controls.MenuBar (MenuBarPart (..), itemAttrs, labelAttrs, menuBar, menuItems, menus, onOpenMenuChanged, openMenu)
import Blink.Element (elLayout, height, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Size (..))
import Blink.Input (Key (KeyChar, KeyLeft, KeyRight), KeyEvent (..), Modifier (Alt))
import Blink.Layout.Constraints (Layout (..), exactly)
import Blink.Rendering (noOpMeasurers)
import Blink.Style (emptyTheme)
import Blink.Update (put)

data TopMenu = FileMenu | EditMenu deriving (Eq, Ord, Show)

data Item = Open | Save | Cut | Copy deriving (Eq, Ord, Show)

labelText :: TopMenu -> T.Text
labelText FileMenu = "File"
labelText EditMenu = "Edit"

labelMnemonic :: TopMenu -> Char
labelMnemonic FileMenu = 'F'
labelMnemonic EditMenu = 'E'

itemsFor :: TopMenu -> [Item]
itemsFor FileMenu = [Open, Save]
itemsFor EditMenu = [Cut, Copy]

-- | A menu bar with two menus -- File(Open, Save) and Edit(Cut, Copy) --
-- each label a fixed 40x20, laid out left to right, so File sits at
-- (0,0)-(40,20) and Edit at (40,0)-(80,20). Each menu's own dropdown,
-- placed below its label by default, spans (0,20)-(40,60) for File and
-- (40,20)-(80,60) for Edit (two 40x20 items stacked).
menuBarApp :: App (MenuBarPart TopMenu Item) (Maybe TopMenu) (Maybe TopMenu)
menuBarApp = App
  { startUp = pure Nothing
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \open ->
      (menuBar id
        [ menus [FileMenu, EditMenu]
        , labelAttrs (\m -> [text (labelText m), mnemonic (labelMnemonic m), width (exactly 40), height (exactly 20)])
        , menuItems itemsFor
        , itemAttrs (\_ i -> [text (T.pack (show i)), width (exactly 40), height (exactly 20)])
        , openMenu open
        , onOpenMenuChanged (postWith id)
        ]) { elLayout = Layout (exactly 80) (exactly 20) TopLeft }
  , update  = put
  }

mkInput :: Point -> Bool -> FrameInput
mkInput p down = emptyFrameInput
  { mousePosition   = p
  , mouseButtonDown = down
  , windowSize      = Size 100 100
  }

fileTriggerPoint, editTriggerPoint, fileItemPoint :: Point
fileTriggerPoint = Point 20 10
editTriggerPoint = Point 60 10
fileItemPoint    = Point 20 30 -- within File's first item, (0,20)-(40,40)

-- | Hovers @p@ for a frame before pressing -- as a real click would --
-- so the occlusion tracking (based on the *previous* frame's registered
-- hit-rects) already knows a nested label\/item sits on top of the bar's
-- own row container by the time the press frame runs; see
-- 'Blink.Controls.MenuButtonSpec.navItemPoint's own click for the same
-- reason.
click :: BlinkHandle s -> Point -> IO (FrameResult s)
click handle p = do
  _ <- stepFrame handle (mkInput p False)
  _ <- stepFrame handle (mkInput p True)
  stepFrame handle (mkInput p False)

-- | A single key press, with the mouse left resting on File's trigger (not
-- pressed) so it never spuriously reclicks anything.
keyInput :: Key -> FrameInput
keyInput k = (mkInput fileTriggerPoint False) { keyEvents = [KeyEvent k [] False] }

-- | Alt held with a mnemonic letter (as the backend would report it, always
-- uppercase -- see 'Blink.Input.KeyChar'), mouse resting off every
-- label/item.
altKeyInput :: Char -> FrameInput
altKeyInput c = (mkInput (Point 90 90) False) { keyEvents = [KeyEvent (KeyChar (toUpper c)) [Alt] False] }

spec :: Spec
spec = describe "Blink.Controls.MenuBar.menuBar" $ do
  it "opens a menu's dropdown on click, drawing its items' text" $ do
    handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
    result <- click handle fileTriggerPoint
    resultState result `shouldBe` Just FileMenu
    drawnTexts result `shouldContain` ["Open", "Save"]

  it "does not draw any dropdown before a label is clicked" $ do
    handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
    result <- click handle (Point 90 90)
    drawnTexts result `shouldNotContain` ["Open", "Save"]
    drawnTexts result `shouldNotContain` ["Cut", "Copy"]

  it "closes a menu's dropdown on a second click of the same label" $ do
    handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
    _      <- click handle fileTriggerPoint -- opens File
    result <- click handle fileTriggerPoint -- closes it
    drawnTexts result `shouldNotContain` ["Open", "Save"]

  it "moving the pointer to a different label switches which dropdown is open, before any click completes" $ do
    handle  <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
    _       <- click handle fileTriggerPoint -- opens File
    -- hovering Edit already switches to it (see hover-to-switch below); the
    -- click that follows lands on a label already open, so it closes it
    -- right back -- the same "second click on an open label closes it"
    -- rule 'closes a menu's dropdown on a second click of the same label'
    -- already covers, generalised to whichever label switching just opened.
    midClick <- stepFrame handle (mkInput editTriggerPoint False)
    resultState midClick `shouldBe` Just EditMenu
    drawnTexts midClick `shouldContain` ["Cut", "Copy"]
    final <- click handle editTriggerPoint
    resultState final `shouldBe` Nothing

  it "clicking an item activates it and closes the menu" $ do
    handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
    _      <- click handle fileTriggerPoint -- opens File
    result <- click handle fileItemPoint
    resultState result `shouldBe` Nothing

  describe "left/right menu-switching while a dropdown is open" $ do
    it "Right moves to the next menu" $ do
      handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (keyInput KeyRight)
      resultState result `shouldBe` Just EditMenu
      drawnTexts result `shouldContain` ["Cut", "Copy"]

    it "Right on the last menu wraps to the first, in a single keypress" $ do
      handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- click handle fileTriggerPoint      -- opens File
      _      <- stepFrame handle (keyInput KeyRight) -- File -> Edit
      result <- stepFrame handle (keyInput KeyRight) -- Edit -> File
      resultState result `shouldBe` Just FileMenu

    it "Left moves to the previous menu, wrapping from the first to the last" $ do
      handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (keyInput KeyLeft) -- File -> Edit (wraps backward)
      resultState result `shouldBe` Just EditMenu

  describe "mnemonics" $ do
    it "Alt+letter opens the matching top-level menu" $ do
      handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
      result <- stepFrame handle (altKeyInput 'F')
      resultState result `shouldBe` Just FileMenu
      drawnTexts result `shouldContain` ["Open", "Save"]

    it "Alt+letter switches to a different menu while one is already open" $ do
      handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (altKeyInput 'E')
      resultState result `shouldBe` Just EditMenu
      drawnTexts result `shouldContain` ["Cut", "Copy"]

    it "Alt+letter of the already-open menu leaves it open rather than closing it" $ do
      handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (altKeyInput 'F')
      resultState result `shouldBe` Just FileMenu

  describe "hover-to-switch while a dropdown is open" $ do
    it "hovering a different label switches to its dropdown without a click" $ do
      handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (mkInput editTriggerPoint False) -- hovers Edit, no click
      resultState result `shouldBe` Just EditMenu
      drawnTexts result `shouldContain` ["Cut", "Copy"]

    it "hovering a label does nothing while no dropdown is open" $ do
      handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
      result <- stepFrame handle (mkInput editTriggerPoint False) -- just hovering, nothing open
      resultState result `shouldBe` Nothing
