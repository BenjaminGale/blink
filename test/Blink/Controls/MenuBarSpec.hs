{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.MenuBarSpec (spec) where

import Control.Monad (void)
import Data.Char (toUpper)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.AppFixtures (drawnTexts, logAddedBetween, resultState, startApp, testMetrics, testStyleSet)
import Blink.Controls.Button (ButtonConfig)
import Blink.Controls.Control (Attribute, control, defaultControlConfig, elementId, onFocusGained, onFocusLost, post, postWith, resolve)
import Blink.Controls.ControlBehaviour (ControlBehaviourConfig (..), controlBehaviourSpec, styleAttributeSpec)
import Blink.Controls.Fixtures (contentRectFor, hitRectFor, mkTestTheme, noInput, plainStyle, plainStyleSet, standardMetrics, testColour)
import Blink.Controls.Label (mnemonic, text)
import Blink.Controls.MenuBar (MenuBarConfig, MenuBarPart (..), itemAttrs, labelAttrs, menuBar, menuItems, menus, onOpenMenuChanged, openMenu, submenuItems)
import Blink.Element (Element, elLayout, elementWithLayout, height, runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..))
import Blink.Input (Key (KeyChar, KeyLeft, KeyRight), KeyEvent (..), Modifier (Alt))
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Style (Theme, emptyTheme)
import Blink.Update (modify, put)
import Blink.View (View, ViewContext, emptyViewContext, withBounds)

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
-- (40,20)-(80,60) for Edit (two 40x20 items stacked). @tag@ builds the
-- bar's element ids, @extraLabelAttrs@ adds to each label's own
-- attributes, @extraAttrs@ to the bar's.
testMenuBar
  :: Ord e
  => (MenuBarPart TopMenu Item -> e)
  -> (TopMenu -> [Attribute (ButtonConfig e msg)])
  -> [Attribute (MenuBarConfig e TopMenu Item msg)]
  -> Element e msg
testMenuBar tag extraLabelAttrs extraAttrs =
  (menuBar tag (
    [ menus [FileMenu, EditMenu]
    , labelAttrs (\m -> [text (labelText m), mnemonic (labelMnemonic m), width (exactly 40), height (exactly 20)] ++ extraLabelAttrs m)
    , menuItems itemsFor
    , itemAttrs (\_ i -> [text (T.pack (show i)), width (exactly 40), height (exactly 20)])
    ] ++ extraAttrs)) { elLayout = Layout (exactly 80) (exactly 20) TopLeft }

menuBarApp :: App (MenuBarPart TopMenu Item) (Maybe TopMenu) (Maybe TopMenu)
menuBarApp = menuBarAppWith []

-- | 'menuBarApp' with @extraAttrs@ appended to the bar's own attributes.
menuBarAppWith
  :: [Attribute (MenuBarConfig (MenuBarPart TopMenu Item) TopMenu Item (Maybe TopMenu))]
  -> App (MenuBarPart TopMenu Item) (Maybe TopMenu) (Maybe TopMenu)
menuBarAppWith extraAttrs = App
  { startUp = pure Nothing
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \open -> testMenuBar id (const []) ([openMenu open, onOpenMenuChanged (postWith id)] ++ extraAttrs)
  , update  = put
  }

data BarEvent = SetOpen (Maybe TopMenu) | Logged T.Text

data FocusElem = BarPart (MenuBarPart TopMenu Item) | Sibling deriving (Eq, Ord, Show)

-- | 'menuBarApp' that logs each label gaining focus, plus a focusable
-- control below both dropdowns at (0,70)-(40,90) that logs its own focus
-- changes.
focusLoggingApp :: App FocusElem BarEvent (Maybe TopMenu, [T.Text])
focusLoggingApp = App
  { startUp = pure (Nothing, [])
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \(open, _) -> elementWithLayout (Layout fill fill TopLeft) $ do
      runElement $ testMenuBar BarPart (\m -> [onFocusGained (post (Logged (labelText m <> " focused")))])
        [openMenu open, onOpenMenuChanged (postWith SetOpen)]
      withBounds (Rectangle 0 70 40 20) $ void $ control $ resolve defaultControlConfig
        [ elementId Sibling
        , onFocusGained (post (Logged "Sibling focused"))
        , onFocusLost   (post (Logged "Sibling lost"))
        ]
  , update  = \msg -> modify $ \(open, log') -> case msg of
      SetOpen m -> (m, log')
      Logged t  -> (open, log' ++ [t])
  }

-- | 'menuBarApp' with File's Save item carrying a submenu of Cut and Copy.
submenuApp :: App (MenuBarPart TopMenu Item) (Maybe TopMenu) (Maybe TopMenu)
submenuApp = menuBarAppWith [submenuItems (\m i -> if m == FileMenu && i == Save then [Cut, Copy] else [])]

mkInput :: Point -> Bool -> FrameInput
mkInput p down = emptyFrameInput
  { mousePosition   = p
  , mouseButtonDown = down
  , windowSize      = Size 100 100
  }

fileTriggerPoint, editTriggerPoint, fileItemPoint, siblingPoint, saveItemPoint :: Point
fileTriggerPoint = Point 20 10
editTriggerPoint = Point 60 10
fileItemPoint    = Point 20 30 -- within File's first item, (0,20)-(40,40)
siblingPoint     = Point 20 80 -- within 'focusLoggingApp's sibling control
saveItemPoint    = Point 20 50 -- within File's second item, (0,40)-(40,60)

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

data ContractElem = ContractPart (MenuBarPart Int Int) | FocusHolder deriving (Eq, Ord, Show)

-- | Real margin, unlike 'testMetrics', for the hit-region contract below.
contractTheme :: Theme ContractElem
contractTheme = mkTestTheme standardMetrics (plainStyleSet (plainStyle testColour))

contractBounds :: Rectangle
contractBounds = Rectangle 0 0 100 40

contractCtx :: ViewContext ContractElem String
contractCtx = emptyViewContext contractBounds noInput contractTheme

-- | 'contractBounds' inset by 'standardMetrics'.
contractHitRect :: Rectangle
contractHitRect = hitRectFor contractBounds

-- | No menus, so nothing occludes 'contractHitRect'; a fixed height, since
-- 'fitContent' would otherwise collapse to just chrome.
renderEmptyMenuBar :: [Attribute (MenuBarConfig ContractElem Int Int String)] -> View ContractElem String ()
renderEmptyMenuBar attrs = runElement $ menuBar ContractPart (menus [] : height (exactly 40) : attrs)

-- | Tall enough that a single label filling the bar's content area keeps
-- a non-empty hit area inside its own margin.
labelContractBounds :: Rectangle
labelContractBounds = Rectangle 0 0 100 100

-- | A single label filling the bar's content area, with @attrs@ passed to
-- it through 'labelAttrs'.
renderSingleLabel :: [Attribute (ButtonConfig ContractElem String)] -> View ContractElem String ()
renderSingleLabel attrs = runElement $ menuBar ContractPart
  [menus [0], height fill, labelAttrs (const (width fill : height fill : attrs))]

-- | The bar's own container is fixed 'NotFocusable'.
contractSpec :: Spec
contractSpec = do
  controlBehaviourSpec (ControlBehaviourConfig { cbcAutoClaims = False, cbcClickFocuses = False })
    contractBounds contractCtx (ContractPart MenuBar) FocusHolder (Point 5 5) contractHitRect (Point 200 200) renderEmptyMenuBar
  describe "label" $
    styleAttributeSpec labelContractBounds contractCtx (hitRectFor (contentRectFor labelContractBounds)) renderSingleLabel

spec :: Spec
spec = describe "Blink.Controls.MenuBar.menuBar" $ do
  contractSpec

  it "opens a menu's dropdown on click, drawing its items' text" $ do
    handle <- startApp menuBarApp
    result <- click handle fileTriggerPoint
    resultState result `shouldBe` Just FileMenu
    drawnTexts result `shouldContain` ["Open", "Save"]

  it "does not draw any dropdown before a label is clicked" $ do
    handle <- startApp menuBarApp
    result <- click handle (Point 90 90)
    drawnTexts result `shouldNotContain` ["Open", "Save"]
    drawnTexts result `shouldNotContain` ["Cut", "Copy"]

  it "closes a menu's dropdown on a second click of the same label" $ do
    handle <- startApp menuBarApp
    _      <- click handle fileTriggerPoint -- opens File
    result <- click handle fileTriggerPoint -- closes it
    drawnTexts result `shouldNotContain` ["Open", "Save"]

  it "moving the pointer to a different label switches which dropdown is open, before any click completes" $ do
    handle  <- startApp menuBarApp
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
    handle <- startApp menuBarApp
    _      <- click handle fileTriggerPoint -- opens File
    result <- click handle fileItemPoint
    resultState result `shouldBe` Nothing

  describe "left/right menu-switching while a dropdown is open" $ do
    it "Right moves to the next menu" $ do
      handle <- startApp menuBarApp
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (keyInput KeyRight)
      resultState result `shouldBe` Just EditMenu
      drawnTexts result `shouldContain` ["Cut", "Copy"]

    it "Right on the last menu wraps to the first, in a single keypress" $ do
      handle <- startApp menuBarApp
      _      <- click handle fileTriggerPoint      -- opens File
      _      <- stepFrame handle (keyInput KeyRight) -- File -> Edit
      result <- stepFrame handle (keyInput KeyRight) -- Edit -> File
      resultState result `shouldBe` Just FileMenu

    it "Left moves to the previous menu, wrapping from the first to the last" $ do
      handle <- startApp menuBarApp
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (keyInput KeyLeft) -- File -> Edit (wraps backward)
      resultState result `shouldBe` Just EditMenu

  describe "mnemonics" $ do
    it "Alt+letter opens the matching top-level menu" $ do
      handle <- startApp menuBarApp
      result <- stepFrame handle (altKeyInput 'F')
      resultState result `shouldBe` Just FileMenu
      drawnTexts result `shouldContain` ["Open", "Save"]

    it "Alt+letter switches to a different menu while one is already open" $ do
      handle <- startApp menuBarApp
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (altKeyInput 'E')
      resultState result `shouldBe` Just EditMenu
      drawnTexts result `shouldContain` ["Cut", "Copy"]

    it "Alt+letter of the already-open menu leaves it open rather than closing it" $ do
      handle <- startApp menuBarApp
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (altKeyInput 'F')
      resultState result `shouldBe` Just FileMenu

  describe "re-clicking an open menu's label" $ do
    it "does not focus the label while the closing press is still held" $ do
      handle  <- startApp focusLoggingApp
      opened  <- click handle fileTriggerPoint
      pressed <- stepFrame handle (mkInput fileTriggerPoint True)
      let logSinceOpen = logAddedBetween opened pressed
      logSinceOpen `shouldNotContain` ["File focused"]

    it "closes the menu and focuses the label on release" $ do
      handle <- startApp focusLoggingApp
      _      <- click handle fileTriggerPoint
      _      <- stepFrame handle (mkInput fileTriggerPoint True)
      result <- stepFrame handle (mkInput fileTriggerPoint False)
      let (open, log') = resultState result
      open `shouldBe` Nothing
      last log' `shouldBe` "File focused"

  describe "pressing another control while a dropdown is open" $ do
    it "closes the dropdown on the press" $ do
      handle <- startApp focusLoggingApp
      _      <- click handle fileTriggerPoint
      _      <- stepFrame handle (mkInput siblingPoint False)
      result <- stepFrame handle (mkInput siblingPoint True)
      fst (resultState result) `shouldBe` Nothing
      drawnTexts result `shouldNotContain` ["Open", "Save"]

    it "leaves focus on the pressed control" $ do
      handle <- startApp focusLoggingApp
      opened <- click handle fileTriggerPoint
      result <- click handle siblingPoint
      let logSinceOpen = logAddedBetween opened result
      logSinceOpen `shouldContain` ["Sibling focused"]
      logSinceOpen `shouldNotContain` ["Sibling lost"]

  describe "hover-to-switch while a dropdown is open" $ do
    it "hovering a different label switches to its dropdown without a click" $ do
      handle <- startApp menuBarApp
      _      <- click handle fileTriggerPoint -- opens File
      result <- stepFrame handle (mkInput editTriggerPoint False) -- hovers Edit, no click
      resultState result `shouldBe` Just EditMenu
      drawnTexts result `shouldContain` ["Cut", "Copy"]

    it "hovering a label does nothing while no dropdown is open" $ do
      handle <- startApp menuBarApp
      result <- stepFrame handle (mkInput editTriggerPoint False) -- just hovering, nothing open
      resultState result `shouldBe` Nothing

  describe "submenus" $ do
    it "hovering an item with a submenu opens it when the pointer arrives from another item" $ do
      handle <- startApp submenuApp
      _      <- click handle fileTriggerPoint
      _      <- stepFrame handle (mkInput fileItemPoint False)
      _      <- stepFrame handle (mkInput saveItemPoint False)
      result <- stepFrame handle (mkInput saveItemPoint False)
      drawnTexts result `shouldContain` ["Cut", "Copy"]

    it "hovering an item with a submenu opens it when the pointer arrives from the bar" $ do
      handle <- startApp submenuApp
      _      <- click handle fileTriggerPoint
      _      <- stepFrame handle (mkInput saveItemPoint False)
      result <- stepFrame handle (mkInput saveItemPoint False)
      drawnTexts result `shouldContain` ["Cut", "Copy"]


