{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.MenuBarSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Blink.App
import Blink.Controls.Control (postWith)
import Blink.Controls.Label (text)
import Blink.Controls.MenuBar (MenuBarPart (..), itemAttrs, labelAttrs, menuBar, menuItems, menus, onOpenMenuChanged, openMenu)
import Blink.Element (elLayout, height, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Size (..), uniform)
import Blink.Layout.Constraints (Layout (..), exactly)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..), noOpMeasurers)
import Blink.Style (Metrics (..), Style (..), StyleSet (..), emptyTheme, noBorder)
import Blink.Update (put)

data TopMenu = FileMenu | EditMenu deriving (Eq, Ord, Show)

data Item = Open | Save | Cut | Copy deriving (Eq, Ord, Show)

labelText :: TopMenu -> T.Text
labelText FileMenu = "File"
labelText EditMenu = "Edit"

itemsFor :: TopMenu -> [Item]
itemsFor FileMenu = [Open, Save]
itemsFor EditMenu = [Cut, Copy]

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
        , labelAttrs (\m -> [text (labelText m), width (exactly 40), height (exactly 20)])
        , menuItems itemsFor
        , itemAttrs (\_ i -> [text (T.pack (show i)), width (exactly 40), height (exactly 20)])
        , openMenu open
        , onOpenMenuChanged (postWith id)
        ]) { elLayout = Layout (exactly 80) (exactly 20) TopLeft }
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

resultState :: FrameResult s -> s
resultState (Continue _ _ s) = s
resultState (Quit _ _ s)     = s

resultDraws :: FrameResult s -> [DrawCommand]
resultDraws (Continue ds _ _) = ds
resultDraws (Quit ds _ _)     = ds

drawnTexts :: FrameResult s -> [T.Text]
drawnTexts r = [t | DrawText _ t _ _ <- resultDraws r]

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

  it "clicking a different label switches which dropdown is open" $ do
    handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
    _      <- click handle fileTriggerPoint -- opens File
    result <- click handle editTriggerPoint -- switches to Edit
    drawnTexts result `shouldContain` ["Cut", "Copy"]
    drawnTexts result `shouldNotContain` ["Open", "Save"]

  it "clicking an item activates it and closes the menu" $ do
    handle <- configureEventDriven menuBarApp nullMsgQueue (pure ()) noOpMeasurers
    _      <- click handle fileTriggerPoint -- opens File
    result <- click handle fileItemPoint
    resultState result `shouldBe` Nothing
