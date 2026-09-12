{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ToggleGroupSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Test.Hspec
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import Blink.Controls.Control (Attribute, StyleKey (..), control, defaultControlConfig, elementId, isEnabled, postWith, resolve)
import Blink.Controls.ElementBehaviour (tagged)
import Blink.Controls.Label (text)
import Blink.Controls.ToggleGroup
  ( ToggleGroupConfig, ToggleGroupPart (..), allowDeselect, defaultToggleGroupConfig
  , items, onSelectionChanged, selectedItem, tggSelected, toggleAttributes, toggleButtonGroup
  )
import Blink.Generators (genPointIn)
import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Layout.Constraints (exactly, fill)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.View
import Blink.Element (height, runElement, width)

-- | The data a group under test picks from -- deliberately not an id type,
-- so tests can't accidentally rely on the id and the data being the same
-- thing.
data Size = Small | Medium | Large deriving (Eq, Ord, Show, Enum, Bounded)

sizes :: [Size]
sizes = [minBound .. maxBound]

-- | The element ids a test group's items resolve to, plus 'Before' -- a
-- plain unrelated control preceding the group in some tests, standing in
-- for the rest of a real form.
data TestElement = Before | Group | Item Size deriving (Eq, Ord, Show)

tag :: ToggleGroupPart Size -> TestElement
tag ToggleGroup         = Group
tag (ToggleGroupItem s) = Item s

testTheme :: Theme TestElement
testTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  = (emptyMetrics, StyleSet emptyStyle Map.empty)
  }
  where
    emptyStyle = Style
      { styleBackground   = RGBA 0 0 0 1
      , styleTextColour   = RGBA 0 0 0 1
      , styleTextAlign    = AlignCenter
      , styleBorderColour = Nothing
      }
    emptyMetrics = Metrics
      { metricsMargin      = uniform 0
      , metricsPadding     = uniform 0
      , metricsBorderEdges = noBorder
      }

noInput :: InputState
noInput = InputState (Point 200 200) False [] [] 0

-- | Three equal 100px-wide slots, filling 'groupBounds' with no gaps: Small
-- at x 0-100, Medium at 100-200, Large at 200-300.
groupBounds :: Rectangle
groupBounds = Rectangle 0 0 300 100

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext groupBounds noInput testTheme noOpTextMeasurer

type Attribute' = Attribute (ToggleGroupConfig TestElement Size String)

-- | A point inside the given size's own 100px slot.
inSlot :: Size -> Point
inSlot Small  = Point 50  50
inSlot Medium = Point 150 50
inSlot Large  = Point 250 50

render :: [Attribute'] -> View TestElement String ()
render attrs = runElement $ toggleButtonGroup tag
  ( items sizes
  : toggleAttributes (\s -> [text (Text.pack (show s)), width (exactly 100), height fill])
  : attrs
  )

selectionAttr :: Attribute'
selectionAttr = onSelectionChanged (postWith (\m -> ("SelectionChanged:" ++ show m)))

-- | Scene bounds wide enough for 'Before' (0-100) followed by the group
-- (100-400), for tests about Tab moving in and out of the group.
sceneBounds :: Rectangle
sceneBounds = Rectangle 0 0 400 100

rectBefore :: Rectangle
rectBefore = Rectangle 0 0 100 100

rectGroup :: Rectangle
rectGroup = Rectangle 100 0 300 100

renderScene :: [Attribute'] -> View TestElement String ()
renderScene attrs = do
  withBounds rectBefore (() <$ control (resolve defaultControlConfig [elementId Before]))
  withBounds rectGroup (render attrs)

focusedOn :: TestElement -> InteractionResult TestElement String a -> Bool
focusedOn eid result = case contextFocusChain (resultContext result) of
  [] -> False
  xs -> last xs == eid

spec :: Spec
spec = describe "Blink.Controls.ToggleGroup" $ do
  describe "defaults" $
    it "starts with nothing selected" $
      tggSelected (resolve (defaultToggleGroupConfig (Class "test")) []) `shouldBe` (Nothing :: Maybe Size)

  -- Every click below is preceded by a 'MoveTo' at the same point, as
  -- setup (a real frame, discarded from 'resultMessages') rather than
  -- clicking cold -- see 'Blink.Interaction.ClickAt'. 'ToggleGroup' wraps
  -- its items inside its own outer
  -- 'Blink.Controls.Control.control', which watches its own
  -- interaction before its content ever runs the items' own 'control'
  -- calls; without a prior frame at the click point, that outer control
  -- can end up holding mouse capture instead of the item, the same as
  -- real mouse input would never produce (moving the cursor there always
  -- happens before pressing the button).
  describe "selection" $ do
    it "selects a clicked item that wasn't already selected" $ do
      result <- runInteractions groupBounds seedCtx
        (render [selectedItem (Just Small), selectionAttr])
        [MoveTo (inSlot Medium)]
        [ClickAt (inSlot Medium)]
      resultMessages result `shouldBe` ["SelectionChanged:Just Medium"]

    it "clicking the already-selected item does nothing when deselecting isn't allowed" $ do
      result <- runInteractions groupBounds seedCtx
        (render [selectedItem (Just Small), selectionAttr])
        [MoveTo (inSlot Small)]
        [ClickAt (inSlot Small)]
      resultMessages result `shouldBe` []

    it "clicking the already-selected item deselects when allowDeselect is set" $ do
      result <- runInteractions groupBounds seedCtx
        (render [selectedItem (Just Small), allowDeselect True, selectionAttr])
        [MoveTo (inSlot Small)]
        [ClickAt (inSlot Small)]
      resultMessages result `shouldBe` ["SelectionChanged:Nothing"]

    it "clicking an unselected item reports it as selected even with nothing selected yet" $ do
      result <- runInteractions groupBounds seedCtx
        (render [selectedItem Nothing, selectionAttr])
        [MoveTo (inSlot Large)]
        [ClickAt (inSlot Large)]
      resultMessages result `shouldBe` ["SelectionChanged:Just Large"]

    it "only ever reports one item as selected, regardless of click order" $ monadicIO $ do
      p <- pick (genPointIn (Rectangle 100 0 200 100)) -- Medium or Large
      result <- run $ runInteractions groupBounds seedCtx
        (render [selectedItem (Just Small), allowDeselect True, selectionAttr])
        [MoveTo p]
        [ClickAt p]
      assert (resultMessages result `elem` [["SelectionChanged:Just Medium"], ["SelectionChanged:Just Large"]])

  describe "disabling" $
    it "disables every item when the group itself is disabled" $ do
      result <- runInteractions groupBounds seedCtx
        (render [selectedItem (Just Small), isEnabled False, selectionAttr])
        [MoveTo (inSlot Medium)]
        [ClickAt (inSlot Medium)]
      resultMessages result `shouldBe` []

  describe "as a control" $
    it "still raises its own raw mouse events, since it's built on `control`" $ monadicIO $ do
      p <- pick (genPointIn groupBounds)
      result <- run $ runInteractions groupBounds seedCtx (render (selectedItem Nothing : tagged)) [] [MoveTo p]
      assert ("MouseEntered" `elem` resultMessages result)

  describe "focus" $ do
    it "is not itself a tab stop -- Tab from before lands directly on the first item" $ do
      result <- runInteractions sceneBounds seedCtx (renderScene [selectedItem Nothing]) [Wait 1] [Tab, Wait 1]
      focusedOn (Item Small) result `shouldBe` True

    it "items remain independently focusable -- Tab moves from one item straight to the next" $ do
      result <- runInteractions sceneBounds seedCtx (renderScene [selectedItem Nothing])
        [Wait 1, Tab, Wait 1] [Tab, Wait 1]
      focusedOn (Item Medium) result `shouldBe` True

    it "Shift-Tab leaves the group for Before, regardless of which item is selected" $ do
      result <- runInteractions sceneBounds seedCtx (renderScene [selectedItem (Just Medium)]) [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
      focusedOn Before result `shouldBe` True
