{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ListSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.List
import Blink.Element (Element (..), runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..), noBorder, uniform)
import Blink.Input (InputState (..), Key (..), Modifier (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View

-- * Pure model tests

spec :: Spec
spec = describe "Blink.Controls.List" $ do
  singleSpec
  requiredSpec
  multiSpec
  rangeSpec
  widgetSpec

singleSpec :: Spec
singleSpec = describe "SingleSelection" $ do
  it "selectFirst selects the first item" $
    selectFirst [1, 2, 3 :: Int] `shouldBe` Selected [] 1 [2, 3]

  it "selectAt selects by position" $
    selectAt 1 [1, 2, 3 :: Int] `shouldBe` Selected [1] 2 [3]

  it "selectAt out of range yields Unselected" $
    selectAt 5 [1, 2, 3 :: Int] `shouldBe` Unselected [1, 2, 3]

  it "selectItem selects by equality" $
    selectItem 2 [1, 2, 3 :: Int] `shouldBe` Selected [1] 2 [3]

  it "singleSelection with an absent item yields Unselected" $
    singleSelection [1, 2, 3 :: Int] (Just 9) `shouldBe` Unselected [1, 2, 3]

  it "moveCursor Next advances the selection" $
    moveCursor Next (selectFirst [1, 2, 3 :: Int]) `shouldBe` Selected [1] 2 [3]

  it "moveCursor Prev at the first item is a no-op" $ do
    let s = selectFirst [1, 2, 3 :: Int]
    moveCursor Prev s `shouldBe` s

  it "moveCursor Next at the last item is a no-op" $ do
    let s = selectAt 2 [1, 2, 3 :: Int]
    moveCursor Next s `shouldBe` s

  it "moveCursor on Unselected selects the first item regardless of direction" $
    moveCursor Prev (unselected [1, 2, 3 :: Int]) `shouldBe` Selected [] 1 [2, 3]

  it "activate on an absent item leaves the model unchanged" $ do
    let s = selectFirst [1, 2, 3 :: Int]
    activate 9 s `shouldBe` s

  it "selectedItems/cursorItem agree with the selected item" $ do
    let s = selectAt 1 [1, 2, 3 :: Int]
    selectedItems s `shouldBe` [2]
    cursorItem s `shouldBe` Just 2

requiredSpec :: Spec
requiredSpec = describe "RequiredSelection" $ do
  it "requireFirst selects the first item of a non-empty list" $
    requireFirst (1 :| [2, 3 :: Int]) `shouldBe` RequiredSelection [] 1 [2, 3]

  it "requireItem on an absent item is Nothing" $
    requireItem 9 [1, 2, 3 :: Int] `shouldBe` Nothing

  it "requireAt out of range is Nothing" $
    requireAt 5 [1, 2, 3 :: Int] `shouldBe` Nothing

  it "moveCursor at either end is a no-op" $ do
    let first = requireFirst (1 :| [2, 3 :: Int])
        Just lastSel = requireAt 2 [1, 2, 3 :: Int]
    moveCursor Prev first `shouldBe` first
    moveCursor Next lastSel `shouldBe` lastSel

multiSpec :: Spec
multiSpec = describe "MultiSelection" $ do
  it "multiSelection starts with nothing selected, cursor on the first item" $
    multiSelection [1, 2, 3 :: Int] `shouldBe` MultiSelection [] (False, 1) [(False, 2), (False, 3)]

  it "multiSelected marks the given subset" $
    selectedItems (multiSelected [1, 2, 3 :: Int] [2]) `shouldBe` [2]

  it "activate toggles the target item on, moving the cursor to it" $ do
    let s = activate 2 (multiSelection [1, 2, 3 :: Int])
    selectedItems s `shouldBe` [2]
    cursorItem s `shouldBe` Just 2

  it "activating a selected item toggles it back off" $ do
    let s = activate 2 (activate 2 (multiSelection [1, 2, 3 :: Int]))
    selectedItems s `shouldBe` []

  it "moveCursor never changes the selection" $ do
    let s = activate 1 (multiSelection [1, 2, 3 :: Int])
    selectedItems (moveCursor Next s) `shouldBe` [1]
    cursorItem (moveCursor Next s) `shouldBe` Just 2

rangeSpec :: Spec
rangeSpec = describe "RangeSelection" $ do
  it "rangeAt selects a single-item run" $
    rangeAt 2 [1, 2, 3, 4 :: Int] `shouldBe` Range [1] (2 :| []) [3, 4] AtEnd

  it "activate collapses to a single item, wherever the previous run was" $ do
    let extended = extendTo 4 (rangeAt 2 [1, 2, 3, 4, 5 :: Int])
    activate 1 extended `shouldBe` Range [] (1 :| []) [2, 3, 4, 5] AtEnd

  it "extendTo grows the run from the anchor to the target" $
    extendTo 4 (rangeAt 2 [1, 2, 3, 4, 5 :: Int]) `shouldBe` Range [1] (2 :| [3, 4]) [5] AtEnd

  it "extendTo the other way flips the cursor end" $
    extendTo 1 (rangeAt 3 [1, 2, 3, 4, 5 :: Int]) `shouldBe` Range [] (1 :| [2, 3]) [4, 5] AtStart

  it "extendCursor grows one step away from the anchor" $ do
    let s = rangeAt 2 [1, 2, 3, 4, 5 :: Int]
    extendCursor Next s `shouldBe` Range [1] (2 :| [3]) [4, 5] AtEnd

  it "extendCursor toward the anchor shrinks the run" $ do
    let s = extendCursor Next (rangeAt 2 [1, 2, 3, 4, 5 :: Int])
    extendCursor Prev s `shouldBe` Range [1] (2 :| []) [3, 4, 5] AtEnd

  it "extendCursor past the anchor crosses over and continues on the other side" $ do
    let s = rangeAt 2 [1, 2, 3, 4, 5 :: Int]
    extendCursor Prev s `shouldBe` Range [] (1 :| [2]) [3, 4, 5] AtStart

  it "extendCursor at the very end of the list is a no-op" $ do
    let s = extendTo 5 (rangeAt 4 [1, 2, 3, 4, 5 :: Int])
    extendCursor Next s `shouldBe` s

  it "moveCursor collapses the run to the cursor item, then steps once more" $ do
    let s = extendTo 3 (rangeAt 1 [1, 2, 3, 4, 5 :: Int]) -- run {1,2,3}, cursor 3
    moveCursor Prev s `shouldBe` Range [1] (2 :| []) [3, 4, 5] AtEnd

  it "selectedItems is exactly the contiguous run" $
    selectedItems (extendTo 4 (rangeAt 2 [1, 2, 3, 4, 5 :: Int])) `shouldBe` [2, 3, 4]

-- * Widget behaviour

-- | Rows are plain 20px-tall, full-width slots with no chrome (see
-- 'testTheme'), so three items give exactly a 100x60 list with rows at
-- y 0-20\/20-40\/40-60.
newtype TestElem = Part (ListPart Int) deriving (Eq, Ord, Show)

testItems :: [Int]
testItems = [1, 2, 3]

testTheme :: Theme TestElem
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

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 60

noInput :: InputState
noInput = InputState (Point 200 200) False [] []

seedCtx :: ViewContext TestElem String
seedCtx = emptyViewContext testBounds noInput testTheme noOpTextMeasurer

-- | A point inside item @n@'s own 20px-tall row (1-indexed).
inRow :: Int -> Point
inRow n = Point 50 (fromIntegral (n - 1) * 20 + 10)

-- | A row 20px tall regardless of what space it's offered -- unlike
-- 'Blink.Element.elementWithLayout', which reports 'noIntrinsicSize' (no
-- fixed height to measure), this actually answers the 'fitContent'
-- measurement 'list' resolves each row's height through.
fixedRow :: Element TestElem String
fixedRow = Element
  { elLayout  = Layout fill (exactly 20) TopLeft
  , elMeasure = const (pure (Size 0 20))
  , elRun     = pure ()
  }

renderList :: (SelectionModel sel, Eq (sel Int)) => [Attribute (ListConfig sel TestElem String Int)] -> View TestElem String ()
renderList attrs = runElement $ list Part
  ( renderItem (const fixedRow)
  : width (exactly 100)
  : attrs
  )

selectedMsg :: Show (sel Int) => sel Int -> String
selectedMsg s = "Selected:" ++ show s

activatedMsg :: Int -> String
activatedMsg x = "Activated:" ++ show x

reactions :: (Show (sel Int)) => [Attribute (ListConfig sel TestElem String Int)]
reactions =
  [ onSelectionChanged (\s -> [OutMsg (selectedMsg s)])
  , onItemActivated (\x -> [OutMsg (activatedMsg x)])
  ]

widgetSpec :: Spec
widgetSpec = describe "list" $ do
  let start = selectFirst testItems

  it "clicking an unselected row selects and activates it" $ do
    result <- runInteractions testBounds seedCtx
      (renderList (selection start : reactions))
      [MoveTo (inRow 2)]
      [ClickAt (inRow 2)]
    resultMessages result `shouldBe` [selectedMsg (activate 2 start), activatedMsg 2]

  it "clicking the already-selected row only activates it, since the model doesn't change" $ do
    result <- runInteractions testBounds seedCtx
      (renderList (selection start : reactions))
      [MoveTo (inRow 1)]
      [ClickAt (inRow 1)]
    resultMessages result `shouldBe` [activatedMsg 1]

  it "Down moves the cursor and fires only onSelectionChanged" $ do
    result <- runInteractions testBounds seedCtx
      (renderList (selection start : reactions))
      [Wait 1]
      [PressKey KeyDown []]
    resultMessages result `shouldBe` [selectedMsg (moveCursor Next start)]

  it "Enter on the already-selected (cursor) row only activates it" $ do
    result <- runInteractions testBounds seedCtx
      (renderList (selection start : reactions))
      [Wait 1]
      [PressKey KeyReturn []]
    resultMessages result `shouldBe` [activatedMsg 1]

  it "Shift-Down on a RangeSelection extends the run" $ do
    let rstart = rangeAt 1 testItems
    result <- runInteractions testBounds seedCtx
      (renderList (selection rstart : reactions))
      [Wait 1]
      [PressKey KeyDown [Shift]]
    resultMessages result `shouldBe` [selectedMsg (extendCursor Next rstart)]
