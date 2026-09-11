{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ListSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec

import Blink.Controls.Control (Attribute, resolve)
import Blink.Controls.List
import Blink.Controls.ScrollBar (ScrollBarPart (..))
import Blink.Element (Element (..), height, runElement, width)
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
  scrollingSpec

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

renderList :: (SelectionModel sel, EmptySelection sel, Eq (sel Int)) => [Attribute (ListConfig sel TestElem String Int)] -> View TestElem String ()
renderList attrs = runElement $ list Part
  ( renderItem (const fixedRow)
  : width (exactly 100)
  : rowHeight 20
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

  it "listBase reports this frame's keyboard-driven selection/activation, matching the same-frame reactions" $ do
    let cfg = resolve defaultListConfig
          ( renderItem (const fixedRow)
          : width (exactly 100)
          : rowHeight 20
          : selection start
          : reactions
          )
        render' = do
          li <- listBase Part cfg
          emit ("Interaction:" ++ show (liSelection li) ++ "/" ++ show (liActivated li))
    result <- runInteractions testBounds seedCtx render' [Wait 1] [PressKey KeyDown []]
    resultMessages result `shouldBe`
      [ selectedMsg (moveCursor Next start)
      , "Interaction:" ++ show (moveCursor Next start) ++ "/[]"
      ]

  it "rowHeight forces every row to that height, regardless of what renderItem itself requests" $ do
    let tallRow = Element (Layout fill (exactly 999) TopLeft) (const (pure (Size 0 999))) (pure ())
        render' = runElement $ list Part
          ( renderItem (const tallRow)
          : width (exactly 100)
          : rowHeight 10
          : selection start
          : reactions
          )
        -- With rows forced to 10px each, item 2's row spans y 10-20; if
        -- 'renderItem's own 999px request leaked through instead, item 1
        -- alone would still occupy this point.
        inTallRow2 = Point 50 15
    result <- runInteractions testBounds seedCtx render' [MoveTo inTallRow2] [ClickAt inTallRow2]
    resultMessages result `shouldBe` [selectedMsg (activate 2 start), activatedMsg 2]

-- * Scrolling

-- | Five 20px rows (see 'fixedRow') is 100px of content, bounded to a 60px
-- list -- exactly the 'testBounds' the scene itself renders into, so
-- scrolling is visible without also having to bound the scene smaller
-- than the list.
scrollItems :: [Int]
scrollItems = [1, 2, 3, 4, 5]

-- | The id 'Blink.Controls.List.list' itself reads\/writes its scrollbar's
-- position under (see its module header) -- the same @tag ('ListScrollBar'
-- 'ScrollBar')@ pattern 'Blink.Controls.ScrollBar.scrollBar' documents for
-- its own composite.
listScrollEid :: TestElem
listScrollEid = Part (ListScrollBar ScrollBar)

renderScrollList :: (SelectionModel sel, EmptySelection sel, Eq (sel Int)) => [Attribute (ListConfig sel TestElem String Int)] -> View TestElem String ()
renderScrollList attrs = runElement $ list Part
  ( renderItem (const fixedRow)
  : width (exactly 100)
  : rowHeight 20
  : height (exactly 60)
  : attrs
  )

scrollingSpec :: Spec
scrollingSpec = describe "list scrolling" $ do
  let start = selectFirst scrollItems

  it "reserves no width for a scrollbar when every row already fits the bounds" $ do
    let fitStart = selectFirst testItems
    result <- runInteractions testBounds seedCtx
      (renderList (selection fitStart : reactions))
      [MoveTo (Point 90 10)]
      [ClickAt (Point 90 10)]
    resultMessages result `shouldBe` [activatedMsg 1]

  it "composites a scrollbar (reserving its own width) once the rows overflow the bounds" $ do
    result <- runInteractions testBounds seedCtx
      (renderScrollList (selection start : reactions))
      [MoveTo (Point 90 10)]
      [ClickAt (Point 90 10)]
    resultMessages result `shouldBe` []

  it "offsets and clips the rows by the scrollbar's own scroll position" $ do
    ctx <- resultContext <$> runInteractions testBounds seedCtx (requestScrollTo listScrollEid 1) [] []
    -- Scrolled all the way down (40px of the 100px content is out of
    -- view), item 3's row is now the first one visible, at the top of
    -- the viewport.
    result <- runInteractions testBounds ctx
      (renderScrollList (selection start : reactions))
      [MoveTo (Point 50 10)]
      [ClickAt (Point 50 10)]
    resultMessages result `shouldBe` [selectedMsg (activate 3 start), activatedMsg 3]

  it "only builds and runs the rows currently intersecting the viewport" $ do
    -- A marker row that reports its own item purely by being run at all
    -- -- unlike 'fixedRow', which draws nothing observable, this proves
    -- whether a row's 'Blink.Controls.Control.control' (and so its own
    -- 'renderItem') ran this frame, not just where it would have been
    -- positioned.
    let marker item = Element
          { elLayout  = Layout fill fill TopLeft
          , elMeasure = const (pure (Size 0 0))
          , elRun     = emit ("Rendered:" ++ show item)
          }
        renderMarked :: View TestElem String ()
        renderMarked = runElement $ list Part
          ( renderItem (marker . isItem)
          : width (exactly 100)
          : rowHeight 20
          : height (exactly 60)
          : [selection start]
          )

    atTop <- runInteractions testBounds seedCtx renderMarked [] [Wait 1]
    resultMessages atTop `shouldBe` ["Rendered:1", "Rendered:2", "Rendered:3"]

    ctx <- resultContext <$> runInteractions testBounds seedCtx (requestScrollTo listScrollEid 1) [] []
    atBottom <- runInteractions testBounds ctx renderMarked [] [Wait 1]
    resultMessages atBottom `shouldBe` ["Rendered:3", "Rendered:4", "Rendered:5"]

  it "keeps the scrollbar's thumb geometry the same no matter which rows are currently virtualised" $ do
    -- Not the track's exact vertical centre -- that point maps to 0.5
    -- for *any* thumb length (see 'Blink.Controls.ScrollBar.fractionAt':
    -- centre minus half the thumb, over the track length minus the
    -- thumb, is always exactly half), so it can't actually distinguish a
    -- wrong thumb length from a right one. y 28 sits off-centre in the
    -- 16-44 track, where the mapped fraction (0.25, for the 20px thumb
    -- 'contentHeight' 100 \/ viewport 60 always produces here) does
    -- depend on the thumb's own length -- so if virtualisation ever let
    -- 'contentHeight' drift with which rows happen to be built, clicking
    -- this same point would land on a different value depending on
    -- which scroll position -- and so which rows -- was already in
    -- effect.
    let trackPoint = Point 92 28
        clickTrack ctx = runInteractions testBounds ctx
          (renderScrollList [selection start])
          [MoveTo trackPoint]
          [MouseDown trackPoint]

    atTop <- clickTrack seedCtx
    contextScrollState listScrollEid (resultContext atTop) `shouldBe` 0.25

    seededAtEnd <- resultContext <$> runInteractions testBounds seedCtx (requestScrollTo listScrollEid 1) [] []
    atBottom <- clickTrack seededAtEnd
    contextScrollState listScrollEid (resultContext atBottom) `shouldBe` 0.25

  it "does not move the scroll position while the keyboard cursor stays within the viewport" $ do
    result <- runInteractions testBounds seedCtx
      (renderScrollList [selection start])
      [Wait 1]
      [PressKey KeyDown []]
    contextScrollState listScrollEid (resultContext result) `shouldBe` 0

  it "scrolls down just enough to keep a cursor moved below the viewport visible" $ do
    -- Cursor starts on item 3 (y 40-60), the viewport's own bottom row;
    -- one Down moves it to item 4 (y 60-80), one row past the bottom
    -- edge.
    let atRow3 = selectAt 2 scrollItems
    result <- runInteractions testBounds seedCtx
      (renderScrollList [selection atRow3])
      [Wait 1]
      [PressKey KeyDown []]
    contextScrollState listScrollEid (resultContext result) `shouldBe` 0.5

  it "scrolls up just enough to keep a cursor moved above the viewport visible" $ do
    let atRow3 = selectAt 2 scrollItems -- cursor on item 3
    seeded <- resultContext <$> runInteractions testBounds seedCtx (requestScrollTo listScrollEid 1) [] []
    -- Scrolled to the bottom, items 3-5 (y 40-100) are in view; one Up
    -- moves the cursor from item 3 to item 2 (y 20-40), one row above
    -- the top edge.
    result <- runInteractions testBounds seeded
      (renderScrollList [selection atRow3])
      [Wait 1]
      [PressKey KeyUp []]
    contextScrollState listScrollEid (resultContext result) `shouldBe` 0.5

  describe "clicking a row only partly in view" $ do
    -- Scrolled 10px into the 100px content (a quarter of the 40px
    -- scrollable range): item 1's row (y 0-20) now straddles the top
    -- edge, showing only its bottom 10px (y 0-10 in the viewport); item
    -- 4's row (y 60-80) straddles the bottom edge, showing only its top
    -- 10px (y 50-60). Both are still clickable on their visible sliver
    -- -- virtualisation only excludes rows with *no* overlap at all (see
    -- 'visibleRows') -- and neither is fully in view yet.
    let seededPartway = resultContext <$> runInteractions testBounds seedCtx (requestScrollTo listScrollEid 0.25) [] []

    it "scrolls a row straddling the top edge fully into view on click" $ do
      seeded <- seededPartway
      let clickPoint = Point 50 5
      result <- runInteractions testBounds seeded
        (renderScrollList (selection start : reactions))
        [MoveTo clickPoint]
        [ClickAt clickPoint]
      -- Item 1 is already the cursor/selection, so only the click itself
      -- fires -- see "clicking the already-selected row only activates
      -- it" above.
      resultMessages result `shouldBe` [activatedMsg 1]
      contextScrollState listScrollEid (resultContext result) `shouldBe` 0

    it "scrolls a row straddling the bottom edge fully into view on click" $ do
      seeded <- seededPartway
      let clickPoint = Point 50 55
      result <- runInteractions testBounds seeded
        (renderScrollList (selection start : reactions))
        [MoveTo clickPoint]
        [ClickAt clickPoint]
      resultMessages result `shouldBe` [selectedMsg (activate 4 start), activatedMsg 4]
      contextScrollState listScrollEid (resultContext result) `shouldBe` 0.5

  it "scrolls the cursor into view when the caller moves it directly, with no click or key press driving it" $ do
    -- Establishes the list's persisted cursor position at item 1 --
    -- already fully in view, so nothing about this first render scrolls
    -- anything.
    step1 <- runInteractions testBounds seedCtx
      (renderScrollList [selection start])
      []
      [Wait 1]

    -- The app jumps the cursor straight to the last item -- e.g. after
    -- re-sorting its own items -- with no key press or click of this
    -- list's own driving it.
    let jumpedToLast = selectAt 4 scrollItems
    result <- runInteractions testBounds (resultContext step1)
      (renderScrollList [selection jumpedToLast])
      []
      [Wait 1]

    contextScrollState listScrollEid (resultContext result) `shouldBe` 1
