{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.TableSpec (spec) where

import Test.Hspec

import Blink.Controls.Control (Attribute, postWith)
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Controls.List
  ( ItemState, ListPart (List), SingleSelection, isItem, onSelectionChanged, rowHeight, selectAt, selectFirst
  , selection, unselected
  )
import Blink.Controls.Table
import Blink.Controls.Fixtures (hitRectFor, mkTestTheme, noInput, plainStyle, plainStyleSet, standardMetrics, testColour, zeroMetrics)
import Blink.Element (Element (..), height, runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..))
import Blink.AppFixtures (solidPalette)
import Blink.Style (Palette, Theme)
import Blink.Style.Defaults (defaultTheme)
import Blink.View

data TestElem = Part (TablePart Int) | FocusHolder deriving (Eq, Ord, Show)

-- | Three 20px rows (60px content), a 30px header -- 90px total, so an
-- 80px-tall scene forces scrolling (see 'scrollingSpec') while a 90px
-- one shows everything at once (see 'widgetSpec').
testTheme :: Theme TestElem
testTheme = mkTestTheme zeroMetrics (plainStyleSet (plainStyle testColour))

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 90

seedCtx :: ViewContext TestElem String
seedCtx = emptyViewContext testBounds noInput testTheme

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
  [ column [header (marker "H-Name"), cellWidth (ColumnFixed 40), cell (cellMarker "Name")]
  , column [header (marker "H-Age"), cellWidth (ColumnFixed 60), cell (cellMarker "Age")]
  ]

renderTable :: [Attribute (TableConfig SingleSelection TestElem String Int)] -> View TestElem String ()
renderTable attrs = runElement $ table Part
  ( columns testColumns
  : width (exactly 100)
  : rowHeight 20
  : attrs
  )

-- | Real margin, unlike 'testTheme', for the hit-region contract below.
contractTheme :: Theme TestElem
contractTheme = mkTestTheme standardMetrics (plainStyleSet (plainStyle testColour))

contractCtx :: ViewContext TestElem String
contractCtx = emptyViewContext testBounds noInput contractTheme

-- | 'renderEmptyTable's fixed 100x80 outer rect, inset by 'standardMetrics'.
contractHitRect :: Rectangle
contractHitRect = hitRectFor (Rectangle 0 0 100 80)

-- | No columns or items, so nothing occludes 'contractHitRect'.
renderEmptyTable :: [Attribute (TableConfig SingleSelection TestElem String Int)] -> View TestElem String ()
renderEmptyTable attrs = renderTable (columns [] : selection (unselected []) : height (exactly 80) : attrs)

-- | 'table' discards any elementId in favour of its own root id.
contractSpec :: Spec
contractSpec = controlBehaviourSpec defaultControlBehaviourConfig
  testBounds contractCtx (Part (TableRow List)) FocusHolder (Point 5 5) contractHitRect (Point 200 200) renderEmptyTable

silentColumns :: [ColumnConfig TestElem String Int]
silentColumns =
  [ column [cellWidth (ColumnFixed 40)]
  , column [cellWidth (ColumnFixed 60)]
  ]

sortableColumns :: [ColumnConfig TestElem String Int]
sortableColumns =
  [ column [cellWidth (ColumnFixed 40), sortable True]
  , column [cellWidth (ColumnFixed 60), sortable True]
  ]

-- | Like 'renderSilentTable', but with both columns sortable -- for
-- 'sortingSpec'.
renderSortableTable :: [Attribute (TableConfig SingleSelection TestElem String Int)] -> View TestElem String ()
renderSortableTable attrs = runElement $ table Part
  ( columns sortableColumns
  : width (exactly 100)
  : rowHeight 20
  : attrs
  )

-- | Like 'testColumns', but sortable -- for 'scrollOnSortSpec', which needs
-- both a sort and, via 'cellMarker', each row's actually-rendered position.
sortableMarkedColumns :: [ColumnConfig TestElem String Int]
sortableMarkedColumns =
  [ column [header (marker "H-Name"), cellWidth (ColumnFixed 40), cell (cellMarker "Name"), sortable True]
  , column [header (marker "H-Age"), cellWidth (ColumnFixed 60), cell (cellMarker "Age"), sortable True]
  ]

renderSortableMarkedTable :: [Attribute (TableConfig SingleSelection TestElem String Int)] -> View TestElem String ()
renderSortableMarkedTable attrs = runElement $ table Part
  ( columns sortableMarkedColumns
  : width (exactly 100)
  : rowHeight 20
  : attrs
  )

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
      , "H-Age@45.0,0.0+60.0" -- +5 for the resize handle between header cells
      , "Name1@0.0,20.0+40.0"
      , "Age1@45.0,20.0+60.0" -- +5 for the matching 'columnSpacer' between row cells
      , "Name2@0.0,40.0+40.0"
      , "Age2@45.0,40.0+60.0"
      , "Name3@0.0,60.0+40.0"
      , "Age3@45.0,60.0+60.0"
      ]

-- | 'testTheme' zeroes margin\/padding\/border on every 'StyleKey', so a
-- mismatch between the chrome a header cell resolves and the chrome a
-- row resolves (e.g. one padded per cell, the other padded once as a
-- whole) can never show up in 'widgetSpec' or any other test in this
-- file -- both always come out to zero regardless. A palette-driven
-- 'defaultTheme' gives every 'StyleKey' real, independently-set chrome,
-- so this is the only test here that would catch that kind of
-- regression.
chromePalette :: Palette
chromePalette = solidPalette (RGBA 0 0 0 1)

chromeAlignmentSpec :: Spec
chromeAlignmentSpec = describe "table header/row chrome" $
  it "insets every column's cell content the same as its header cell, not just column 0's" $ do
    result <- runInteractions testBounds (emptyViewContext testBounds noInput (defaultTheme chromePalette))
      (renderTable [selection (unselected items)])
      []
      [Wait 1]
    let msgs      = resultMessages result
        stripY m  = let rest = drop 1 (dropWhile (/= '@') m)
                        x    = takeWhile (/= ',') rest
                        w    = drop 1 (dropWhile (/= '+') rest)
                    in x ++ "+" ++ w
        headerAge = head (filter (\m -> take 5 m == "H-Age") msgs)
        rowAge1   = head (filter (\m -> take 4 m == "Age1") msgs)
    stripY headerAge `shouldBe` stripY rowAge1

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
    headerMsgs `shouldBe` ["H-Name@0.0,0.0+40.0", "H-Age@45.0,0.0+60.0"]

  it "is not itself a selectable row -- clicking it fires nothing" $ do
    result <- runInteractions shortBounds seedCtx
      (renderSilentTable
        [ selection (unselected items)
        , onSelectionChanged (postWith (\s -> ("Selected:" ++ show s)))
        ])
      [MoveTo (at 10 10)]
      [ClickAt (at 10 10)]
    resultMessages result `shouldBe` []

resizingSpec :: Spec
resizingSpec = describe "table column resizing" $ do
  -- The handle between "Name" (0-40) and "Age" (40-100) spans x 40-45,
  -- centred at 42.5.
  let handleCentre = at 42.5 10

  it "dragging the handle between two columns resizes them and leaves other columns' widths unchanged" $ do
    -- Dragging its centre 10px right should grow "Name" by 10 and
    -- shrink "Age" by 10, leaving their combined 100px (and every
    -- row's) unchanged.
    dragged <- runInteractions testBounds seedCtx
      (renderSilentTable [selection (unselected items)])
      [MoveTo handleCentre]
      [MouseDown handleCentre, DragTo (at 52.5 10)]

    result <- runInteractions testBounds (resultContext dragged)
      (renderTable [selection (unselected items)])
      []
      [Wait 1]

    resultMessages result `shouldBe`
      [ "H-Name@0.0,0.0+50.0"
      , "H-Age@55.0,0.0+50.0"
      , "Name1@0.0,20.0+50.0"
      , "Age1@55.0,20.0+50.0"
      , "Name2@0.0,40.0+50.0"
      , "Age2@55.0,40.0+50.0"
      , "Name3@0.0,60.0+50.0"
      , "Age3@55.0,60.0+50.0"
      ]

  it "shows a resize cursor while hovering the handle" $ do
    result <- runInteractions testBounds seedCtx
      (renderSilentTable [selection (unselected items)])
      [MoveTo handleCentre]
      [Wait 1]
    getCursorShape (resultContext result) `shouldBe` CursorResizeHorizontal

  it "shows a resize cursor while dragging the handle, even once the pointer leaves it" $ do
    result <- runInteractions testBounds seedCtx
      (renderSilentTable [selection (unselected items)])
      [MoveTo handleCentre]
      [MouseDown handleCentre, DragTo (at 90 10)]
    getCursorShape (resultContext result) `shouldBe` CursorResizeHorizontal

  it "leaves the default cursor when the pointer is away from the handle" $ do
    result <- runInteractions testBounds seedCtx
      (renderSilentTable [selection (unselected items)])
      [MoveTo (at 10 10)]
      [Wait 1]
    getCursorShape (resultContext result) `shouldBe` CursorArrow

sortingSpec :: Spec
sortingSpec = describe "table column-click sorting" $
  it "cycles Ascending/Descending on repeat clicks of a column, resets to Ascending on a different one" $ do
    let headerClick x = at x 10
        onSort        = onColumnSortRequested (postWith (\s -> ("Sort:" ++ show s)))

    step1 <- runInteractions testBounds seedCtx
      (renderSortableTable [selection (unselected items), onSort])
      [MoveTo (headerClick 10)]
      [ClickAt (headerClick 10)]
    resultMessages step1 `shouldBe` ["Sort:(0,Ascending)"]

    step2 <- runInteractions testBounds (resultContext step1)
      (renderSortableTable [selection (unselected items), sortedBy (Just (0, Ascending)), onSort])
      [MoveTo (headerClick 10)]
      [ClickAt (headerClick 10)]
    resultMessages step2 `shouldBe` ["Sort:(0,Descending)"]

    -- A third click on the same, already-descending column cycles back
    -- to Ascending rather than clearing the sort or repeating Descending.
    step3 <- runInteractions testBounds (resultContext step2)
      (renderSortableTable [selection (unselected items), sortedBy (Just (0, Descending)), onSort])
      [MoveTo (headerClick 10)]
      [ClickAt (headerClick 10)]
    resultMessages step3 `shouldBe` ["Sort:(0,Ascending)"]

    step4 <- runInteractions testBounds (resultContext step3)
      (renderSortableTable [selection (unselected items), sortedBy (Just (0, Ascending)), onSort])
      [MoveTo (headerClick 70)]
      [ClickAt (headerClick 70)]
    resultMessages step4 `shouldBe` ["Sort:(1,Ascending)"]

scrollOnSortSpec :: Spec
scrollOnSortSpec = describe "table sorting scrolls the selection into view" $
  it "scrolls a row a sort moved off-screen back into view" $ do
    -- Header (20px) leaves a 30px row viewport (1.5 of 5 20px rows).
    let shortBounds     = Rectangle 0 0 100 50
        viewportTop     = 20 -- header height
        viewportBottom  = 50 -- shortBounds height
        fiveItems       = [1, 2, 3, 4, 5]
        cursorOnFirst   = selectFirst fiveItems
        -- The Y out of a marker message like "Name5@0.0,80.0+40.0".
        rowY m = read (takeWhile (/= '+') (drop 1 (dropWhile (/= ',') m))) :: Double

    step1 <- runInteractions shortBounds seedCtx
      (renderSortableMarkedTable [selection cursorOnFirst])
      []
      [Wait 1]

    -- Simulates a sort moving the selected item from the top row to the
    -- last, with no click or key press on the table itself driving it.
    result <- runInteractions shortBounds (resultContext step1)
      (renderSortableMarkedTable [selection (selectAt 4 fiveItems), sortedBy (Just (0, Ascending))])
      []
      [Wait 1]

    let selectedRowY = rowY (head (filter (\m -> take 5 m == "Name5") (resultMessages result)))
    selectedRowY `shouldSatisfy` \y -> y >= viewportTop && y < viewportBottom

focusSpec :: Spec
focusSpec = describe "table header focus" $
  it "clicking a header cell never claims focus for it, since it's NotFocusable" $ do
    let headerPoint = at 10 10

    -- The table's own root auto-claims focus by the end of setup, since
    -- nothing else is focused.
    settled <- runInteractions testBounds seedCtx
      (renderSilentTable [selection (unselected items)])
      []
      [Wait 1]
    let chainBeforeClick = contextFocusChain (resultContext settled)
    chainBeforeClick `shouldNotBe` []

    -- A header cell taking focus on click (were it wrongly Focusable)
    -- would change this chain; it must not.
    result <- runInteractions testBounds (resultContext settled)
      (renderSilentTable [selection (unselected items)])
      [MoveTo headerPoint]
      [ClickAt headerPoint, Wait 1]
    contextFocusChain (resultContext result) `shouldBe` chainBeforeClick

columnCountEdgeSpec :: Spec
columnCountEdgeSpec = describe "table column count edge cases" $ do
  it "renders no header and no cells at all when there are no columns" $ do
    result <- runInteractions testBounds seedCtx
      (runElement $ table Part [columns [], selection (unselected items), width (exactly 100), rowHeight 20])
      []
      [Wait 1]
    resultMessages result `shouldBe` []

  it "renders a single column with no divider woven in" $ do
    let oneColumn = [ column [header (marker "H-Solo"), cellWidth (ColumnFixed 40), cell (cellMarker "Solo")] ]
    result <- runInteractions testBounds seedCtx
      (runElement $ table Part [columns oneColumn, selection (unselected items), width (exactly 100), rowHeight 20])
      []
      [Wait 1]
    resultMessages result `shouldBe`
      [ "H-Solo@0.0,0.0+40.0"
      , "Solo1@0.0,20.0+40.0"
      , "Solo2@0.0,40.0+40.0"
      , "Solo3@0.0,60.0+40.0"
      ]

spec :: Spec
spec = describe "Blink.Controls.Table" $ do
  contractSpec
  widgetSpec
  chromeAlignmentSpec
  scrollingSpec
  resizingSpec
  sortingSpec
  scrollOnSortSpec
  focusSpec
  columnCountEdgeSpec
