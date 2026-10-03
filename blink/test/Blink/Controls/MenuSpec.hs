{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.MenuSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.AppFixtures (drawnTexts, resultDraws, resultState, solidPalette, startApp, testMetrics, testStyleSet)
import Blink.Controls.Button (ButtonConfig, onActivated)
import Blink.Controls.Control (onFocusGained, post)
import Blink.Controls.Label (mnemonic, text)
import Blink.Controls.Menu (MenuItems (..), defaultStyleEntries, menuListWithSubmenus)
import Blink.Element (Attribute, elLayout, elementWithLayout, height, runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..))
import Blink.Input (Key (..), KeyEvent (..), Modifier (Alt))
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..), DrawCommand (..))
import Blink.Style (Palette (..), StyleKey (..), Theme (..), emptyTheme)
import Blink.Update (modify)
import Blink.View (emit, getFocus, requestFocus)

-- | Every item, top-level and nested alike -- 'Export' is the only one
-- with a submenu (see 'submenuFor'); 'Csv'\/'Pdf'\/'Json' are its own
-- items, themselves with no further submenu.
data Item = Open | Save | Export | Csv | Pdf | Json deriving (Eq, Ord, Show)

-- | Every item's own element id ('ItemPart'), plus 'Export's own submenu
-- list\/focus scope id ('ExportSubmenu') and the top-level list's own
-- ('TopList') -- 'itemId' below builds the
-- former for every item regardless of nesting depth, since 'Item' is a
-- single flat type with one constructor per item.
data Part = ItemPart Item | ExportSubmenu | TopList deriving (Eq, Ord, Show)

testStyleKey :: StyleKey Part
testStyleKey = Class "testMenu"

-- | 'Export's own submenu -- its focus-scope\/panel id, and its items.
-- Every other item has none.
submenuFor :: Item -> Maybe (Part, [Item])
submenuFor Export = Just (ExportSubmenu, [Csv, Pdf, Json])
submenuFor _       = Nothing

-- | Appends a tagged log entry -- what a test observes, the same
-- log-of-tags approach 'Blink.Controls.MenuButtonSpec.navApp' uses for the
-- same reason: every focus-gained and activation event, plus 'menuApp's own
-- top-level 'close'.
data Event = Logged Text

-- | Renders 'Open'\/'Save'\/'Export' stacked from (0,0) in a list 100 wide
-- that fills the window's height. Items stretch across the list, so
-- @Open (0,0)-(100,20)@, @Save (0,20)-(100,40)@, @Export (0,40)-(100,60)@.
-- The submenu of 'Export' opens against its right edge at the menu's
-- minimum width of 160, so its items sit at @Csv (100,40)-(260,60)@,
-- @Pdf (100,60)-(260,80)@, @Json (100,80)-(260,100)@.
--
-- Nothing here plays the role 'Blink.Controls.MenuButton.menuButton's own
-- trigger does -- moving root's own focus onto the list's scope the moment
-- it opens -- so this claims it once itself, the one frame nothing is
-- focused yet, and every frame after simply reaffirms whatever the list's
-- own navigation has since settled on.
menuApp :: App Part Event [Text]
menuApp = App
  { startUp = pure []
  , theme   = const menuTheme
  , view    = \_ -> elementWithLayout (Layout fill fill TopLeft) $ do
      cur <- getFocus
      case cur of
        Nothing -> requestFocus Nothing TopList
        Just _  -> pure ()
      runElement $
        (menuListWithSubmenus testStyleKey menu (emit (Logged "Closed")) False)
          { elLayout = Layout (exactly 100) fill TopLeft }
  , update  = \(Logged t) -> modify (++ [t])
  }
  where
    menu = MenuItems
      { miListId    = TopList
      , miItemId    = ItemPart
      , miItems     = [Open, Save, Export]
      , miItemAttrs = itemAttrsFor
      , miSubmenu   = submenuFor
      }

    itemAttrsFor :: Item -> [Attribute (ButtonConfig Part Event)]
    itemAttrsFor item =
      [ text (T.pack (show item)), mnemonic (itemMnemonic item), width (exactly 40), height (exactly 20)
      , onFocusGained (post (Logged (T.pack (show item) <> " focused")))
      , onActivated    (post (Logged (T.pack (show item) <> " activated")))
      ]

-- | The chrome-less test style for everything but the items, which get the
-- library's real menu item style so its highlight can be observed.
menuTheme :: Theme Part
menuTheme = (emptyTheme (testMetrics, testStyleSet))
  { themeElementStyles = Map.fromList (defaultStyleEntries highlightPalette) }

-- | Black everywhere except 'highlightColour', so an item's highlight fill
-- is the only thing drawn in that colour.
highlightPalette :: Palette
highlightPalette = (solidPalette (RGBA 0 0 0 1)) { paletteSurfaceHover = highlightColour }

highlightColour :: Colour
highlightColour = RGBA 1 0 0 1

-- | The highlight fill over an item occupying the given rectangle of
-- 'menuApp's layout.
highlightOver :: Rectangle -> DrawCommand
highlightOver r = FillRect r highlightColour

saveRect, exportRect :: Rectangle
saveRect   = Rectangle 0 20 100 20
exportRect = Rectangle 0 40 100 20

-- | Each item's own mnemonic letter -- the initial of its name, distinct
-- across the whole set (top-level and submenu alike) so a test can target
-- any one of them unambiguously.
itemMnemonic :: Item -> Char
itemMnemonic Open   = 'O'
itemMnemonic Save   = 'S'
itemMnemonic Export = 'E'
itemMnemonic Csv    = 'C'
itemMnemonic Pdf    = 'P'
itemMnemonic Json   = 'J'

-- | Holds the app clock at 0, so no time passes between frames unless a
-- test uses 'mkInputAt'.
mkInput :: Point -> Bool -> FrameInput
mkInput p down = emptyFrameInput
  { mousePosition   = p
  , mouseButtonDown = down
  , windowSize      = Size 300 300
  , frameTime       = Just 0
  }

-- | The pointer resting at @p@, @seconds@ into the app clock.
mkInputAt :: Double -> Point -> FrameInput
mkInputAt seconds p = (mkInput p False) { frameTime = Just (round (seconds * 1.0e9)) }

-- | A single key press, mouse left resting off every item so it never
-- spuriously reclicks anything.
keyInput :: Key -> FrameInput
keyInput = keyInputAt (Point 200 200)

-- | A single key press with the pointer resting at @p@.
keyInputAt :: Point -> Key -> FrameInput
keyInputAt p k = (mkInput p False) { keyEvents = [KeyEvent k [] False] }

-- | Alt held with a mnemonic letter (as the backend would report it, always
-- uppercase -- see 'Blink.Input.KeyChar'), mouse resting off every item.
altKeyInput :: Char -> FrameInput
altKeyInput c = (mkInput (Point 200 200) False) { keyEvents = [KeyEvent (KeyChar c) [Alt] False] }

savePoint, exportPoint, pdfPoint, betweenItemsPoint, offMenuPoint :: Point
savePoint   = Point 20 30   -- within Save's own (0,20)-(100,40)
exportPoint = Point 20 50   -- within Export's own (0,40)-(100,60)
pdfPoint    = Point 150 70  -- within Pdf's own (100,60)-(260,80)
betweenItemsPoint = Point 20 150 -- on the list's own panel, below every item
offMenuPoint      = Point 400 400 -- outside the list's own panel, which fills the window

-- | Frame 1 always just settles the initial auto-focus (see 'menuApp');
-- every test below runs it first, off any item, before doing anything the
-- test itself cares about.
settle :: BlinkHandle [Text] -> IO (FrameResult [Text])
settle handle = stepFrame handle (mkInput (Point 200 200) False)

-- | Settles, then hovers 'Export' until its submenu is open.
openExportByHover :: BlinkHandle [Text] -> IO (FrameResult [Text])
openExportByHover handle = do
  _ <- settle handle
  _ <- stepFrame handle (mkInput exportPoint False)
  stepFrame handle (mkInput exportPoint False)

-- | Settles, moves the highlight to 'Export', then opens its submenu with
-- Right-arrow, focusing 'Csv'.
openExportByKeyboard :: BlinkHandle [Text] -> IO (FrameResult [Text])
openExportByKeyboard handle = do
  _ <- settle handle
  _ <- downTimes handle 2
  stepFrame handle (keyInput KeyRight)

-- | Settles, hovers 'Save' so it takes the highlight, then presses Down
-- without moving the pointer, moving the highlight to 'Export'.
downWithPointerOnSave :: BlinkHandle [Text] -> IO (FrameResult [Text])
downWithPointerOnSave handle = do
  _ <- settle handle
  _ <- stepFrame handle (mkInput savePoint False)
  stepFrame handle (keyInputAt savePoint KeyDown)

-- | Presses Down @n@ times, moving the highlight from 'Open' (the first
-- item, focused by 'settle') onto the @n@th item after it.
downTimes :: BlinkHandle [Text] -> Int -> IO (FrameResult [Text])
downTimes handle n = go n
  where
    go 0 = stepFrame handle (mkInput (Point 200 200) False)
    go k = stepFrame handle (keyInput KeyDown) >> go (k - 1)

spec :: Spec
spec = describe "Blink.Controls.Menu.menuListWithSubmenus" $ do
  it "opens the first item's highlight on its own, with no trigger control involved" $ do
    handle <- startApp menuApp
    result <- settle handle
    last (resultState result) `shouldBe` "Open focused"

  describe "opening a submenu" $ do
    it "does not draw a submenu item before its parent item's submenu opens" $ do
      handle <- startApp menuApp
      _      <- settle handle
      result <- stepFrame handle (mkInput (Point 200 200) False)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]

    it "opens on Right-arrow while the item holding a submenu is highlighted, focusing its first item" $ do
      handle <- startApp menuApp
      _      <- settle handle
      _      <- downTimes handle 2 -- Open -> Save -> Export
      _      <- stepFrame handle (keyInput KeyRight)
      result <- stepFrame handle (mkInput (Point 200 200) False)
      drawnTexts result `shouldContain` ["Csv", "Pdf", "Json"]
      last (resultState result) `shouldBe` "Csv focused"

    it "does not open on Right-arrow while a different item is highlighted" $ do
      handle <- startApp menuApp
      _      <- settle handle -- Open highlighted, not Export
      result <- stepFrame handle (keyInput KeyRight)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]

    it "opens on hovering the item, without needing it highlighted or Right-arrow first" $ do
      handle <- startApp menuApp
      _      <- settle handle -- Open highlighted, not Export
      _      <- stepFrame handle (mkInput exportPoint False)
      result <- stepFrame handle (mkInput exportPoint False)
      drawnTexts result `shouldContain` ["Csv", "Pdf", "Json"]

  describe "closing a submenu back to its parent list" $ do
    it "Escape closes the submenu without closing the whole menu, and returns the highlight to Export" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      result <- stepFrame handle (keyInput KeyEscape)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]
      drawnTexts result `shouldContain` ["Open", "Save", "Export"]
      resultState result `shouldNotContain` ["Closed"]
      last (resultState result) `shouldBe` "Export focused"

    it "Left-arrow closes the submenu without closing the whole menu, and returns the highlight to Export" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      result <- stepFrame handle (keyInput KeyLeft)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]
      resultState result `shouldNotContain` ["Closed"]
      last (resultState result) `shouldBe` "Export focused"

    it "Left-arrow does nothing at the top level, which has no parent to back out to" $ do
      handle <- startApp menuApp
      _      <- settle handle
      result <- stepFrame handle (keyInput KeyLeft)
      drawnTexts result `shouldContain` ["Open", "Save", "Export"]
      resultState result `shouldNotContain` ["Closed"]

  describe "the submenu's own independent arrow-key wraparound" $ do
    it "Down on the last submenu item wraps to the first, in a single keypress" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      _      <- stepFrame handle (keyInput KeyDown)  -- Csv -> Pdf
      result <- stepFrame handle (keyInput KeyDown)  -- Pdf -> Json
      last (resultState result) `shouldBe` "Json focused"
      result2 <- stepFrame handle (keyInput KeyDown) -- Json -> Csv, wraps
      last (resultState result2) `shouldBe` "Csv focused"

    it "Up on the first submenu item wraps to the last, in a single keypress" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      result <- stepFrame handle (keyInput KeyUp)    -- Csv -> Json, wraps
      last (resultState result) `shouldBe` "Json focused"

    it "does not move the parent list's own highlight while navigating within the submenu" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      result <- stepFrame handle (keyInput KeyDown)  -- Csv -> Pdf
      -- Everything logged since the submenu opened (dropping the initial
      -- Open\/Save\/Export highlight-nav entries) is the submenu's own, not
      -- a refocus of any top-level item.
      drop 3 (resultState result) `shouldBe` ["Csv focused", "Pdf focused"]

  describe "activating a submenu item" $ do
    it "closes the whole menu (every level), not just the submenu" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      result <- stepFrame handle (keyInput KeyReturn)
      let log' = resultState result
      log' `shouldContain` ["Csv activated"]
      last log' `shouldBe` "Closed"

    it "still activates the item clicked, rather than being treated as outside" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      _      <- stepFrame handle (mkInput pdfPoint False) -- hovers Pdf first, as a real click would
      _      <- stepFrame handle (mkInput pdfPoint True)
      result <- stepFrame handle (mkInput pdfPoint False)
      resultState result `shouldContain` ["Pdf activated"]

  describe "mnemonics" $ do
    it "Alt+letter activates a plain item and closes the whole menu, without needing it highlighted first" $ do
      handle <- startApp menuApp
      _      <- settle handle -- Open highlighted, not Save
      result <- stepFrame handle (altKeyInput 'S')
      let log' = resultState result
      log' `shouldContain` ["Save activated"]
      last log' `shouldBe` "Closed"

    it "Alt+letter on an item with a submenu opens it instead of activating it" $ do
      handle <- startApp menuApp
      _      <- settle handle -- Open highlighted, not Export
      _      <- stepFrame handle (altKeyInput 'E')
      result <- stepFrame handle (mkInput (Point 200 200) False)
      drawnTexts result `shouldContain` ["Csv", "Pdf", "Json"]
      last (resultState result) `shouldBe` "Csv focused"
      resultState result `shouldNotContain` ["Closed"]

    it "Alt+letter for a submenu item activates it once its submenu is open" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      result <- stepFrame handle (altKeyInput 'P')
      let log' = resultState result
      log' `shouldContain` ["Pdf activated"]
      last log' `shouldBe` "Closed"

  describe "clicking the parent list while a submenu is open" $ do
    it "does not treat a click on the submenu's own item as outside and close everything" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      _      <- stepFrame handle (mkInput exportPoint False)
      _      <- stepFrame handle (mkInput exportPoint True)
      result <- stepFrame handle (mkInput exportPoint False)
      resultState result `shouldNotContain` ["Closed"]

    it "activates another item of the parent list" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      _      <- stepFrame handle (mkInput savePoint False)
      _      <- stepFrame handle (mkInput savePoint True)
      result <- stepFrame handle (mkInput savePoint False)
      resultState result `shouldContain` ["Save activated"]

  describe "outside-click dismissal" $ do
    it "a click completing outside every level closes the whole menu" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      _      <- stepFrame handle (mkInput offMenuPoint False)
      _      <- stepFrame handle (mkInput offMenuPoint True)
      result <- stepFrame handle (mkInput offMenuPoint False)
      resultState result `shouldContain` ["Closed"]

    it "closes on the press, before the button is released" $ do
      handle <- startApp menuApp
      _      <- settle handle
      result <- stepFrame handle (mkInput offMenuPoint True)
      resultState result `shouldContain` ["Closed"]

  describe "the single highlight shared by pointer and keyboard" $ do
    it "moves onto an item when the pointer enters it" $ do
      handle <- startApp menuApp
      _      <- settle handle
      result <- stepFrame handle (mkInput savePoint False)
      last (resultState result) `shouldBe` "Save focused"

    context "when Down-arrow is pressed with the pointer resting on an item" $ do
      it "leaves the item under the pointer unhighlighted" $ do
        handle <- startApp menuApp
        result <- downWithPointerOnSave handle
        resultDraws result `shouldNotContain` [highlightOver saveRect]

      it "highlights the next item" $ do
        handle <- startApp menuApp
        _      <- downWithPointerOnSave handle
        result <- stepFrame handle (mkInput savePoint False)
        resultDraws result `shouldContain` [highlightOver exportRect]

      it "stays on the next item while the pointer rests without moving" $ do
        handle <- startApp menuApp
        _      <- downWithPointerOnSave handle
        result <- stepFrame handle (mkInput savePoint False)
        last (resultState result) `shouldBe` "Export focused"

      it "moves back onto the item under the pointer when the pointer moves within it" $ do
        handle <- startApp menuApp
        _      <- downWithPointerOnSave handle
        result <- stepFrame handle (mkInput (Point 21 30) False)
        last (resultState result) `shouldBe` "Save focused"

    it "keeps the item that owns an open submenu highlighted while the highlight is inside that submenu" $ do
      handle <- startApp menuApp
      _      <- openExportByKeyboard handle
      result <- stepFrame handle (mkInput (Point 200 200) False)
      resultDraws result `shouldContain` [highlightOver exportRect]

  describe "at the top level, unaffected by submenu support" $
    it "Escape still closes the whole menu" $ do
      handle <- startApp menuApp
      _      <- settle handle
      result <- stepFrame handle (keyInput KeyEscape)
      resultState result `shouldContain` ["Closed"]

  describe "switching away from an open submenu by hovering another item" $ do
    it "closes the submenu and highlights the hovered item once the pointer rests there past the delay" $ do
      handle <- startApp menuApp
      _      <- openExportByHover handle
      _      <- stepFrame handle (mkInput savePoint False)
      _      <- restOn handle savePoint
      result <- stepFrame handle (mkInputAt restTime savePoint)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]
      last (resultState result) `shouldBe` "Save focused"

    it "keeps the submenu open while the pointer crosses another item quicker than the delay" $ do
      handle <- startApp menuApp
      _      <- openExportByHover handle
      _      <- stepFrame handle (mkInput savePoint False)
      result <- stepFrame handle (mkInput savePoint False)
      drawnTexts result `shouldContain` ["Csv", "Pdf", "Json"]

    it "keeps the submenu open when the pointer leaves the menu entirely" $ do
      handle <- startApp menuApp
      _      <- openExportByHover handle
      _      <- restOn handle offMenuPoint
      result <- stepFrame handle (mkInputAt restTime offMenuPoint)
      drawnTexts result `shouldContain` ["Csv", "Pdf", "Json"]

    it "counts time spent on the list between items towards the delay" $ do
      handle <- startApp menuApp
      _      <- openExportByHover handle
      _      <- restOn handle betweenItemsPoint
      _      <- stepFrame handle (mkInputAt restTime savePoint)
      result <- stepFrame handle (mkInputAt restTime savePoint)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]
      last (resultState result) `shouldBe` "Save focused"

    it "restarts the delay when the pointer returns to the item that owns the submenu" $ do
      handle <- startApp menuApp
      _      <- openExportByHover handle
      _      <- restOn handle betweenItemsPoint
      _      <- stepFrame handle (mkInputAt restTime exportPoint)
      _      <- stepFrame handle (mkInputAt restTime savePoint)
      result <- stepFrame handle (mkInputAt restTime savePoint)
      drawnTexts result `shouldContain` ["Csv", "Pdf", "Json"]

-- | Holds the pointer at @p@ from 0.1s to 'restTime' on the app clock,
-- long enough to pass the menu's submenu-switch delay. Steps 0.1s at a
-- time because the app clock advances by at most 0.1s per frame.
restOn :: BlinkHandle [Text] -> Point -> IO ()
restOn handle p = mapM_ (\t -> stepFrame handle (mkInputAt t p)) [0.1, 0.2, 0.3, 0.4, restTime]

restTime :: Double
restTime = 0.5
