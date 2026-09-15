{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.MenuSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.Controls.Button (ButtonConfig, onActivated)
import Blink.Controls.Control (onFocusGained, post)
import Blink.Controls.Label (text)
import Blink.Controls.Menu (menuListWithSubmenus)
import Blink.Element (Attribute, elLayout, elementWithLayout, height, runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Size (..), uniform)
import Blink.Input (Key (..), KeyEvent (..))
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..), noOpMeasurers)
import Blink.Style (Metrics (..), Style (..), StyleKey (..), StyleSet (..), emptyTheme, noBorder)
import Blink.Update (modify)
import Blink.View (emit, getFocus, requestFocus)

-- | Every item, top-level and nested alike -- 'Export' is the only one
-- with a submenu (see 'submenuFor'); 'Csv'\/'Pdf'\/'Json' are its own
-- items, themselves with no further submenu.
data Item = Open | Save | Export | Csv | Pdf | Json deriving (Eq, Ord, Show)

-- | Every item's own element id ('ItemPart'), plus 'Export's own submenu
-- list\/focus scope id ('ExportSubmenu') -- 'itemId' below builds the
-- former for every item regardless of nesting depth, since 'Item' is a
-- single flat type with one constructor per item.
data Part = ItemPart Item | ExportSubmenu deriving (Eq, Ord, Show)

testStyleKey :: StyleKey Part
testStyleKey = Class "testMenu"

testStyle :: Style
testStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignLeft
  , styleBorderColour = Nothing
  }

testMetrics :: Metrics
testMetrics = Metrics { metricsMargin = uniform 0, metricsPadding = uniform 0, metricsBorderEdges = noBorder }

testStyleSet :: StyleSet
testStyleSet = StyleSet { styleBase = testStyle, styleOverrides = mempty }

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

-- | Renders 'Open'\/'Save'\/'Export' stacked from (0,0), each 40x20 -- the
-- same geometry 'Blink.Controls.MenuButtonSpec.navApp' uses, so a
-- click\/hover point lands unambiguously on one of them:
-- @Open (0,0)-(40,20)@, @Save (0,20)-(40,40)@, @Export (0,40)-(40,60)@.
-- 'Export's own submenu, once open, renders via 'Blink.Popup.popup'
-- side-anchored to 'Export's own bounds, so its items sit at
-- @Csv (40,40)-(80,60)@, @Pdf (40,60)-(80,80)@, @Json (40,80)-(80,100)@.
--
-- Nothing here plays the role 'Blink.Controls.MenuButton.menuButton's own
-- trigger does -- moving root's own focus onto the list's scope the moment
-- it opens -- so this claims it once itself, the one frame nothing is
-- focused yet, and every frame after simply reaffirms whatever the list's
-- own navigation has since settled on.
menuApp :: App Part Event [Text]
menuApp = App
  { startUp = pure []
  , theme   = const (emptyTheme (testMetrics, testStyleSet))
  , view    = \_ -> elementWithLayout (Layout fill fill TopLeft) $ do
      cur <- getFocus
      case cur of
        Nothing -> requestFocus Nothing (ItemPart Open)
        Just _  -> pure ()
      runElement $
        (menuListWithSubmenus testStyleKey (ItemPart Open) ItemPart [Open, Save, Export] itemAttrsFor submenuFor
          (emit (Logged "Closed")) False)
          { elLayout = Layout fill fill TopLeft }
  , update  = \(Logged t) -> modify (++ [t])
  }
  where
    itemAttrsFor :: Item -> [Attribute (ButtonConfig Part Event)]
    itemAttrsFor item =
      [ text (T.pack (show item)), width (exactly 40), height (exactly 20)
      , onFocusGained (post (Logged (T.pack (show item) <> " focused")))
      , onActivated    (post (Logged (T.pack (show item) <> " activated")))
      ]

mkInput :: Point -> Bool -> FrameInput
mkInput p down = FrameInput
  { mousePosition   = p
  , mouseButtonDown = down
  , keyEvents       = []
  , typedText       = []
  , wheelDelta      = 0
  , windowSize      = Size 300 300
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

-- | A single key press, mouse left resting off every item so it never
-- spuriously reclicks anything.
keyInput :: Key -> FrameInput
keyInput k = (mkInput (Point 200 200) False) { keyEvents = [KeyEvent k [] False] }

exportPoint, pdfPoint :: Point
exportPoint = Point 20 50   -- within Export's own (0,40)-(40,60)
pdfPoint    = Point 60 70   -- within Pdf's own (40,60)-(80,80)

-- | Frame 1 always just settles the initial auto-focus (see 'menuApp');
-- every test below runs it first, off any item, before doing anything the
-- test itself cares about.
settle :: BlinkHandle [Text] -> IO (FrameResult [Text])
settle handle = stepFrame handle (mkInput (Point 200 200) False)

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
    handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
    result <- settle handle
    last (resultState result) `shouldBe` "Open focused"

  describe "opening a submenu" $ do
    it "does not draw a submenu item before its parent item's submenu opens" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      result <- stepFrame handle (mkInput (Point 200 200) False)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]

    it "opens on Right-arrow while the item holding a submenu is highlighted, focusing its first item" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2 -- Open -> Save -> Export
      _      <- stepFrame handle (keyInput KeyRight)
      result <- stepFrame handle (mkInput (Point 200 200) False)
      drawnTexts result `shouldContain` ["Csv", "Pdf", "Json"]
      last (resultState result) `shouldBe` "Csv focused"

    it "does not open on Right-arrow while a different item is highlighted" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle -- Open highlighted, not Export
      result <- stepFrame handle (keyInput KeyRight)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]

    it "opens on hovering the item, without needing it highlighted or Right-arrow first" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle -- Open highlighted, not Export
      _      <- stepFrame handle (mkInput exportPoint False)
      result <- stepFrame handle (mkInput exportPoint False)
      drawnTexts result `shouldContain` ["Csv", "Pdf", "Json"]

  describe "closing a submenu back to its parent list" $ do
    it "Escape closes the submenu without closing the whole menu, and returns the highlight to Export" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2
      _      <- stepFrame handle (keyInput KeyRight) -- opens, focuses Csv
      result <- stepFrame handle (keyInput KeyEscape)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]
      drawnTexts result `shouldContain` ["Open", "Save", "Export"]
      resultState result `shouldNotContain` ["Closed"]
      last (resultState result) `shouldBe` "Export focused"

    it "Left-arrow closes the submenu without closing the whole menu, and returns the highlight to Export" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2
      _      <- stepFrame handle (keyInput KeyRight) -- opens, focuses Csv
      result <- stepFrame handle (keyInput KeyLeft)
      drawnTexts result `shouldNotContain` ["Csv", "Pdf", "Json"]
      resultState result `shouldNotContain` ["Closed"]
      last (resultState result) `shouldBe` "Export focused"

    it "Left-arrow does nothing at the top level, which has no parent to back out to" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      result <- stepFrame handle (keyInput KeyLeft)
      drawnTexts result `shouldContain` ["Open", "Save", "Export"]
      resultState result `shouldNotContain` ["Closed"]

  describe "the submenu's own independent arrow-key wraparound" $ do
    it "Down on the last submenu item wraps to the first, in a single keypress" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2
      _      <- stepFrame handle (keyInput KeyRight) -- opens, focuses Csv
      _      <- stepFrame handle (keyInput KeyDown)  -- Csv -> Pdf
      result <- stepFrame handle (keyInput KeyDown)  -- Pdf -> Json
      last (resultState result) `shouldBe` "Json focused"
      result2 <- stepFrame handle (keyInput KeyDown) -- Json -> Csv, wraps
      last (resultState result2) `shouldBe` "Csv focused"

    it "Up on the first submenu item wraps to the last, in a single keypress" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2
      _      <- stepFrame handle (keyInput KeyRight) -- opens, focuses Csv
      result <- stepFrame handle (keyInput KeyUp)    -- Csv -> Json, wraps
      last (resultState result) `shouldBe` "Json focused"

    it "does not move the parent list's own highlight while navigating within the submenu" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2
      _      <- stepFrame handle (keyInput KeyRight) -- opens, focuses Csv
      result <- stepFrame handle (keyInput KeyDown)  -- Csv -> Pdf
      -- Everything logged since the submenu opened (dropping the initial
      -- Open\/Save\/Export highlight-nav entries) is the submenu's own, not
      -- a refocus of any top-level item.
      drop 3 (resultState result) `shouldBe` ["Csv focused", "Pdf focused"]

  describe "activating a submenu item" $ do
    it "closes the whole menu (every level), not just the submenu" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2
      _      <- stepFrame handle (keyInput KeyRight) -- opens, focuses Csv
      result <- stepFrame handle (keyInput KeyReturn)
      let log' = resultState result
      log' `shouldContain` ["Csv activated"]
      last log' `shouldBe` "Closed"

    it "still activates the item clicked, rather than being treated as outside" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2
      _      <- stepFrame handle (keyInput KeyRight) -- opens, focuses Csv
      _      <- stepFrame handle (mkInput pdfPoint False) -- hovers Pdf first, as a real click would
      _      <- stepFrame handle (mkInput pdfPoint True)
      result <- stepFrame handle (mkInput pdfPoint False)
      resultState result `shouldContain` ["Pdf activated"]

  describe "clicking the parent item while its submenu is open" $
    it "does not treat the click as outside and close everything" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2
      _      <- stepFrame handle (keyInput KeyRight) -- opens, focuses Csv
      _      <- stepFrame handle (mkInput exportPoint False)
      _      <- stepFrame handle (mkInput exportPoint True)
      result <- stepFrame handle (mkInput exportPoint False)
      resultState result `shouldNotContain` ["Closed"]

  describe "outside-click dismissal" $
    it "a click completing outside every level closes the whole menu" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      _      <- downTimes handle 2
      _      <- stepFrame handle (keyInput KeyRight) -- opens, focuses Csv
      _      <- stepFrame handle (mkInput (Point 250 250) False)
      _      <- stepFrame handle (mkInput (Point 250 250) True)
      result <- stepFrame handle (mkInput (Point 250 250) False)
      resultState result `shouldContain` ["Closed"]

  describe "at the top level, unaffected by submenu support" $
    it "Escape still closes the whole menu" $ do
      handle <- configureEventDriven menuApp nullMsgQueue (pure ()) noOpMeasurers
      _      <- settle handle
      result <- stepFrame handle (keyInput KeyEscape)
      resultState result `shouldContain` ["Closed"]
