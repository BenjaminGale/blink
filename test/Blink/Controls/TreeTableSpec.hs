{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.TreeTableSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree (Tree (..))
import Test.Hspec

import Blink.Controls.Control (Attribute, postWith)
import Blink.Controls.List
  (Direction (..), ItemState, SingleSelection, isItem, moveCursor, onSelectionChanged, rowHeight, selectItem, selection, unselected)
import Blink.Controls.Table (ColumnConfig (..), ColumnWidth (..), cell, cellWidth, column, sortable)
import Blink.Controls.TreeTable
import Blink.Element (Element (..), runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..), noBorder, uniform)
import Blink.Input (InputState (..), Key (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View

-- | src
--   |- src/List.hs
--   test
forest0 :: [Tree String]
forest0 =
  [ Node "src"
      [ Node "src/List.hs" [] ]
  , Node "test" []
  ]

newtype TestElem = Part (TreeTablePart String) deriving (Eq, Ord, Show)

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

noInput :: InputState
noInput = InputState (Point 200 200) False [] [] 0

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 90

seedCtx :: ViewContext TestElem String
seedCtx = emptyViewContext testBounds noInput testTheme noOpTextMeasurer

at :: Double -> Double -> Point
at = Point

-- | Reports a label plus its own left edge -- lets a test check both
-- that a cell ran at all and where 'treeTable' positioned it.
marker :: String -> Element TestElem String
marker label = Element
  { elLayout  = Layout fill fill TopLeft
  , elMeasure = const (pure (Size 0 0))
  , elRun     = do
      b <- getBounds
      emit (label ++ "@" ++ show (rectX b))
  }

nameCell :: ItemState String -> Element TestElem String
nameCell st = marker (isItem st)

qtyCell :: ItemState String -> Element TestElem String
qtyCell st = marker ("Qty:" ++ isItem st)

-- | Real cell markers (for checking row alignment), silent headers --
-- header alignment is 'table''s own concern, not this row-focused test.
testColumns :: [ColumnConfig TestElem String String]
testColumns =
  [ column [cellWidth (ColumnFixed 60), cell nameCell, sortable True]
  , column [cellWidth (ColumnFixed 40), cell qtyCell, sortable True]
  ]

silentColumns :: [ColumnConfig TestElem String String]
silentColumns =
  [ column [cellWidth (ColumnFixed 60), sortable True]
  , column [cellWidth (ColumnFixed 40), sortable True]
  ]

items :: [String]
items = ["src", "src/List.hs", "test"]

renderTreeTable :: [Attribute (TreeTableConfig SingleSelection TestElem String String)] -> View TestElem String ()
renderTreeTable attrs = runElement $ treeTable Part
  ( columns testColumns
  : width (exactly 100)
  : rowHeight 20
  : forest forest0
  : attrs
  )

-- | Like 'renderTreeTable', but with silent cells\/header -- for tests
-- that only care about a click or key's reaction.
renderSilentTreeTable :: [Attribute (TreeTableConfig SingleSelection TestElem String String)] -> View TestElem String ()
renderSilentTreeTable attrs = runElement $ treeTable Part
  ( columns silentColumns
  : width (exactly 100)
  : rowHeight 20
  : forest forest0
  : attrs
  )

widgetSpec :: Spec
widgetSpec = describe "treeTable" $
  it "indents column 0 by depth while column 1+ cells stay aligned across depths" $ do
    result <- runInteractions testBounds seedCtx
      (renderTreeTable [expanded (Set.singleton "src"), selection (unselected items)])
      []
      [Wait 1]
    resultMessages result `shouldBe`
      [ "src@16.0"
      , "Qty:src@60.0"
      , "src/List.hs@32.0"
      , "Qty:src/List.hs@60.0"
      , "test@16.0"
      , "Qty:test@60.0"
      ]

expansionSpec :: Spec
expansionSpec = describe "treeTable expansion" $
  it "clicking a node's chevron fires onExpansionChanged with that node's membership toggled" $ do
    -- The 20px header pushes row 1 ("src") to y 20-40.
    let atRow1Chevron = at 8 30
    result <- runInteractions testBounds seedCtx
      (renderSilentTreeTable
        [ expanded (Set.singleton "src")
        , selection (unselected items)
        , onExpansionChanged (postWith (\s -> ("Expanded:" ++ show (Set.toList s))))
        ])
      [MoveTo atRow1Chevron]
      [ClickAt atRow1Chevron]
    resultMessages result `shouldBe` ["Expanded:[]"]

keyboardSpec :: Spec
keyboardSpec = describe "treeTable keyboard expand/collapse" $
  it "Right expands the collapsed root and moves into its child, Left moves back to the parent" $ do
    let cursorOnSrc = selectItem "src" items

    step1 <- runInteractions testBounds seedCtx
      (renderSilentTreeTable
        [ expanded Set.empty
        , selection cursorOnSrc
        , onExpansionChanged (postWith (\s -> ("Expanded:" ++ show (Set.toList s))))
        ])
      []
      [PressKey KeyRight []]
    resultMessages step1 `shouldBe` ["Expanded:[\"src\"]"]

    step2 <- runInteractions testBounds (resultContext step1)
      (renderSilentTreeTable
        [ expanded (Set.singleton "src")
        , selection cursorOnSrc
        , onSelectionChanged (postWith (\s -> ("Selected:" ++ show s)))
        ])
      []
      [PressKey KeyRight []]
    let cursorOnFirstChild = moveCursor Next cursorOnSrc
    resultMessages step2 `shouldBe` ["Selected:" ++ show cursorOnFirstChild]

    -- Left on that leaf child: moves the cursor back to its parent, "src".
    step3 <- runInteractions testBounds (resultContext step2)
      (renderSilentTreeTable
        [ expanded (Set.singleton "src")
        , selection cursorOnFirstChild
        , onSelectionChanged (postWith (\s -> ("Selected:" ++ show s)))
        ])
      []
      [PressKey KeyLeft []]
    resultMessages step3 `shouldBe` ["Selected:" ++ show (moveCursor Prev cursorOnFirstChild)]

resizingSpec :: Spec
resizingSpec = describe "treeTable column resizing" $
  it "dragging the handle between two columns resizes them and leaves the rows' own layout otherwise unchanged" $ do
    -- Column 0 (0-60) and column 1 (60-100): the handle sits at 60-65,
    -- centred at 62.5.
    let handleCentre = at 62.5 10

    dragged <- runInteractions testBounds seedCtx
      (renderSilentTreeTable [expanded (Set.singleton "src"), selection (unselected items)])
      [MoveTo handleCentre]
      [MouseDown handleCentre, DragTo (at 52.5 10)]

    result <- runInteractions testBounds (resultContext dragged)
      (renderTreeTable [expanded (Set.singleton "src"), selection (unselected items)])
      []
      [Wait 1]

    resultMessages result `shouldBe`
      [ "src@16.0"
      , "Qty:src@50.0"
      , "src/List.hs@32.0"
      , "Qty:src/List.hs@50.0"
      , "test@16.0"
      , "Qty:test@50.0"
      ]

sortingSpec :: Spec
sortingSpec = describe "treeTable column-click sorting" $
  it "clicking a sortable column's header fires onColumnSortRequested with Ascending" $ do
    result <- runInteractions testBounds seedCtx
      (renderSilentTreeTable
        [ expanded (Set.singleton "src")
        , selection (unselected items)
        , onColumnSortRequested (postWith (\s -> ("Sort:" ++ show s)))
        ])
      [MoveTo (at 30 10)]
      [ClickAt (at 30 10)]
    resultMessages result `shouldBe` ["Sort:(0,Ascending)"]

spec :: Spec
spec = describe "Blink.Controls.TreeTable" $ do
  widgetSpec
  expansionSpec
  keyboardSpec
  resizingSpec
  sortingSpec
