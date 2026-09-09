{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.TableSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.List (ItemState, SingleSelection, isItem, onSelectionChanged, rowHeight, selection, unselected)
import Blink.Controls.Table
import Blink.Element (Element (..), emptyElement, runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..), noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View

newtype TestElem = Part (TablePart Int) deriving (Eq, Ord, Show)

-- | Three 20px rows (60px content), a 30px header -- 90px total, so an
-- 80px-tall scene forces scrolling (see 'scrollingSpec') while a 90px
-- one shows everything at once (see 'widgetSpec').
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
noInput = InputState (Point 200 200) False [] []

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 90

seedCtx :: ViewContext TestElem String
seedCtx = emptyViewContext testBounds noInput testTheme noOpTextMeasurer

-- | A point at @(x, y)@ within the scene.
at :: Double -> Double -> Point
at = Point

-- | Reports a label plus its own bounds -- lets a test check both that
-- a cell\/header ran at all and exactly where 'table' positioned it.
marker :: String -> Element TestElem String
marker label = Element
  { elLayout  = Layout fill fill TopLeft
  , elMeasure = const (pure (Size 0 0))
  , elRun     = do
      b <- getBounds
      emit (label ++ "@" ++ show (rectX b) ++ "," ++ show (rectY b) ++ "+" ++ show (rectWidth b))
  }

cellMarker :: String -> ItemState Int -> Element TestElem String
cellMarker col st = marker (col ++ show (isItem st))

items :: [Int]
items = [1, 2, 3]

testColumns :: [ColumnConfig TestElem String Int]
testColumns =
  [ ColumnConfig { colHeader = marker "H-Name", colWidth = exactly 40, colCell = cellMarker "Name" }
  , ColumnConfig { colHeader = marker "H-Age",  colWidth = exactly 60, colCell = cellMarker "Age" }
  ]

renderTable :: [Attribute (TableConfig SingleSelection TestElem String Int)] -> View TestElem String ()
renderTable attrs = runElement $ table Part
  ( columns testColumns
  : width (exactly 100)
  : rowHeight 20
  : attrs
  )

silentColumns :: [ColumnConfig TestElem String Int]
silentColumns =
  [ ColumnConfig { colHeader = emptyElement, colWidth = exactly 40, colCell = const emptyElement }
  , ColumnConfig { colHeader = emptyElement, colWidth = exactly 60, colCell = const emptyElement }
  ]

-- | Like 'renderTable', but with silent cells\/header -- for tests that
-- only care about a click's reaction, not what each cell draws (every
-- simulated frame re-renders every cell, so 'marker' would otherwise
-- add a message per cell per frame).
renderSilentTable :: [Attribute (TableConfig SingleSelection TestElem String Int)] -> View TestElem String ()
renderSilentTable attrs = runElement $ table Part
  ( columns silentColumns
  : width (exactly 100)
  : rowHeight 20
  : attrs
  )

widgetSpec :: Spec
widgetSpec = describe "table" $
  it "renders one cell per column at each column's own width, with a header aligned to them" $ do
    result <- runInteractions testBounds seedCtx
      (renderTable [selection (unselected items)])
      []
      [Wait 1]
    resultMessages result `shouldBe`
      [ "H-Name@0.0,0.0+40.0"
      , "H-Age@41.0,0.0+60.0" -- +1 for the divider between header cells
      , "Name1@0.0,20.0+40.0"
      , "Age1@40.0,20.0+60.0"
      , "Name2@0.0,40.0+40.0"
      , "Age2@40.0,40.0+60.0"
      , "Name3@0.0,60.0+40.0"
      , "Age3@40.0,60.0+60.0"
      ]

scrollingSpec :: Spec
scrollingSpec = describe "table header" $ do
  -- Header (20px) leaves a 30px row viewport, shorter than the 60px of
  -- content (3 rows), so the body scrolls while the header does not.
  let shortBounds = Rectangle 0 0 100 50

  it "does not scroll with the body rows" $ do
    result <- runInteractions shortBounds seedCtx
      (renderTable [selection (unselected items)])
      []
      [Wait 1]
    let headerMsgs = filter (\m -> take 2 m == "H-") (resultMessages result)
    -- Always at y 0, full column widths -- the header sits in its own
    -- fixed slot above the (here, scrolling) rows, not inside them.
    headerMsgs `shouldBe` ["H-Name@0.0,0.0+40.0", "H-Age@41.0,0.0+60.0"]

  it "is not itself a selectable row -- clicking it fires nothing" $ do
    result <- runInteractions shortBounds seedCtx
      (renderSilentTable
        [ selection (unselected items)
        , onSelectionChanged (\s -> [OutMsg ("Selected:" ++ show s)])
        ])
      [MoveTo (at 10 10)]
      [ClickAt (at 10 10)]
    resultMessages result `shouldBe` []

spec :: Spec
spec = describe "Blink.Controls.Table" $ do
  widgetSpec
  scrollingSpec
