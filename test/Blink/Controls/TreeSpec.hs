{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.TreeSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree (Tree (..))
import Test.Hspec

import Blink.Controls.Control (Attribute, postWith)
import Blink.Controls.List
  ( Direction (..), ListPart (..), MultiSelection, SingleSelection, isItem, moveCursor, multiSelected
  , onSelectionChanged, rowHeight, selectItem, selectedItems, selection, unselected
  )
import Blink.Controls.List.Style (listStyleKey)
import Blink.Controls.ScrollBar (ScrollBarPart (..))
import Blink.Controls.Tree
import Blink.Element (Element (..), runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..), noBorder, uniform)
import Blink.Input (InputState (..), Key (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View

-- * flattenVisible

-- | src
--   |- src/List.hs
--   |- src/Controls
--        |- src/Controls/Button.hs
--   test
forest0 :: [Tree String]
forest0 =
  [ Node "src"
      [ Node "src/List.hs" []
      , Node "src/Controls"
          [ Node "src/Controls/Button.hs" [] ]
      ]
  , Node "test" []
  ]

flattenVisibleSpec :: Spec
flattenVisibleSpec = describe "flattenVisible" $ do
  it "shows only the roots when nothing is expanded" $
    flattenVisible forest0 Set.empty `shouldBe`
      [ ("src", 0), ("test", 0) ]

  it "descends into an expanded node's children, but not a collapsed grandchild's" $
    flattenVisible forest0 (Set.singleton "src") `shouldBe`
      [ ("src", 0), ("src/List.hs", 1), ("src/Controls", 1), ("test", 0) ]

  it "descends further once a deeper node is also expanded" $
    flattenVisible forest0 (Set.fromList ["src", "src/Controls"]) `shouldBe`
      [ ("src", 0)
      , ("src/List.hs", 1)
      , ("src/Controls", 1)
      , ("src/Controls/Button.hs", 2)
      , ("test", 0)
      ]

  it "yields nothing for an empty forest" $
    flattenVisible ([] :: [Tree String]) (Set.singleton "src") `shouldBe` []

-- * Widget behaviour

newtype TestElem = Part (TreePart String) deriving (Eq, Ord, Show)

-- | Rows are 20px tall, and with "src" expanded there are exactly four
-- visible rows (see 'flattenVisibleSpec' above), so an 80px-tall scene
-- shows them all without triggering scrolling.
testBounds :: Rectangle
testBounds = Rectangle 0 0 100 80

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

-- | 'testTheme', but with real padding on the list's own chrome -- a
-- regression case for the bug where a Right\/Left-driven scroll read the
-- list's outer, pre-chrome bounds instead of the padded interior
-- 'listBase' itself actually scrolls rows within (see 'tree').
chromeTheme :: Theme TestElem
chromeTheme = testTheme
  { themeElementStyles = Map.singleton listStyleKey (chromeMetrics, snd (themeDefaultStyle testTheme)) }
  where
    chromeMetrics = Metrics
      { metricsMargin      = uniform 0
      , metricsPadding     = uniform 8
      , metricsBorderEdges = noBorder
      }

noInput :: InputState
noInput = InputState (Point 200 200) False [] [] 0

seedCtx :: ViewContext TestElem String
seedCtx = emptyViewContext testBounds noInput testTheme

-- | A point at x-offset @x@ within row @n@'s own 20px-tall row (1-indexed).
atRow :: Int -> Double -> Point
atRow n x = Point x (fromIntegral (n - 1) * 20 + 10)

-- | Reports its own item and left edge -- lets a test check both that a
-- row's content ran at all and where 'tree' positioned it (see its
-- indent).
markerNode :: TreeItemState String -> Element TestElem String
markerNode tis = Element
  { elLayout  = Layout fill fill TopLeft
  , elMeasure = const (pure (Size 0 0))
  , elRun     = do
      b <- getBounds
      emit (isItem (tisState tis) ++ "@" ++ show (rectX b))
  }

renderTree :: [Attribute (TreeConfig SingleSelection TestElem String String)] -> View TestElem String ()
renderTree attrs = runElement $ tree Part
  ( renderNode markerNode
  : width (exactly 100)
  : rowHeight 20
  : forest forest0
  : attrs
  )

-- | Like 'renderTree', but with a silent node render -- for tests that
-- only care about a click's reaction, not what each row draws (every
-- simulated frame re-renders every row, so 'markerNode' would otherwise
-- add a marker message per row per frame).
renderSilentTree :: [Attribute (TreeConfig SingleSelection TestElem String String)] -> View TestElem String ()
renderSilentTree attrs = runElement $ tree Part
  ( width (exactly 100)
  : rowHeight 20
  : forest forest0
  : attrs
  )

items :: [String]
items = ["src", "src/List.hs", "src/Controls", "test"]

widgetSpec :: Spec
widgetSpec = describe "tree" $ do
  it "indents each row's content proportional to its depth, past a fixed chevron column" $ do
    result <- runInteractions testBounds seedCtx
      (renderTree [expanded (Set.singleton "src"), selection (unselected items)])
      []
      [Wait 1]
    resultMessages result `shouldBe`
      [ "src@16.0"
      , "src/List.hs@32.0"
      , "src/Controls@32.0"
      , "test@16.0"
      ]

  it "clicking a node's chevron fires onExpansionChanged with that node's membership toggled" $ do
    result <- runInteractions testBounds seedCtx
      (renderSilentTree
        [ expanded (Set.singleton "src")
        , selection (unselected items)
        , onExpansionChanged (postWith (\s -> ("Expanded:" ++ show (Set.toList s))))
        ])
      [MoveTo (atRow 1 8)]
      [ClickAt (atRow 1 8)]
    resultMessages result `shouldBe` ["Expanded:[]"]

  it "a leaf row's chevron column is inert -- there's no chevron there to click" $ do
    result <- runInteractions testBounds seedCtx
      (renderSilentTree
        [ expanded (Set.singleton "src")
        , selection (unselected items)
        , onExpansionChanged (postWith (\s -> ("Expanded:" ++ show (Set.toList s))))
        ])
      [MoveTo (atRow 2 24)]
      [ClickAt (atRow 2 24)]
    resultMessages result `shouldBe` []

  it "draws an expanded node's chevron as expand_more.svg and a collapsed one as chevron_right.svg" $ do
    result <- runInteractions testBounds seedCtx
      (renderSilentTree [expanded (Set.singleton "src"), selection (unselected items)])
      [] [Wait 1]
    let restColour = RGBA 0 0 0 1
    -- Row 1 ("src", depth 0): expanded, has children.
    resultDraws result `shouldContain`
      [DrawImage (Rectangle 0 0 16 20) "assets/icons/expand_more.svg" restColour]
    -- Row 3 ("src/Controls", depth 1): collapsed (not in the expanded
    -- set), has children -- chevron column starts after its indent.
    resultDraws result `shouldContain`
      [DrawImage (Rectangle 16 40 16 20) "assets/icons/chevron_right.svg" restColour]

expandedMsg :: Set.Set String -> String
expandedMsg s = "Expanded:" ++ show (Set.toList s)

selectedMsg :: SingleSelection String -> String
selectedMsg s = "Selected:" ++ show s

keyReactions :: [Attribute (TreeConfig SingleSelection TestElem String String)]
keyReactions =
  [ onExpansionChanged (postWith (\s -> (expandedMsg s)))
  , onSelectionChanged (postWith (\s -> (selectedMsg s)))
  ]

keyboardSpec :: Spec
keyboardSpec = describe "tree keyboard expand/collapse" $
  it "Right expands a collapsed parent, Right again moves into its first child, Left moves back to the parent" $ do
    let cursorOnSrc = selectItem "src" items

    -- Right on the collapsed root: expands it, cursor stays on "src".
    step1 <- runInteractions testBounds seedCtx
      (renderSilentTree (expanded Set.empty : selection cursorOnSrc : keyReactions))
      []
      [PressKey KeyRight []]
    resultMessages step1 `shouldBe` [expandedMsg (Set.singleton "src")]

    -- Right again, now that "src" is expanded: moves the cursor to the
    -- very next visible row, its first child "src/List.hs".
    step2 <- runInteractions testBounds (resultContext step1)
      (renderSilentTree (expanded (Set.singleton "src") : selection cursorOnSrc : keyReactions))
      []
      [PressKey KeyRight []]
    let cursorOnFirstChild = moveCursor Next cursorOnSrc
    resultMessages step2 `shouldBe` [selectedMsg cursorOnFirstChild]

    -- Left on that leaf child: moves the cursor back to its parent, "src"
    -- -- via 'moveCursor', not 'activate' (see 'Blink.Controls.Tree.tree'
    -- for why: 'activate' carries model-specific side effects a plain
    -- cursor move must never have for a 'MultiSelection'\/'RangeSelection'
    -- caller).
    step3 <- runInteractions testBounds (resultContext step2)
      (renderSilentTree (expanded (Set.singleton "src") : selection cursorOnFirstChild : keyReactions))
      []
      [PressKey KeyLeft []]
    resultMessages step3 `shouldBe` [selectedMsg (moveCursor Prev cursorOnFirstChild)]

-- | 'Blink.Controls.List.activate' would be the wrong primitive for a
-- parent-jump: for a 'MultiSelection' it also toggles the target's own
-- checked state, which navigating the tree's shape must never do.
multiKeyboardSpec :: Spec
multiKeyboardSpec = describe "tree keyboard with MultiSelection" $
  it "Left's parent-jump moves the cursor without toggling the parent's checked state" $ do
    let cursorOnFirstChild = moveCursor Next (multiSelected items ["src"])
        render' :: [Attribute (TreeConfig MultiSelection TestElem String String)] -> View TestElem String ()
        render' attrs = runElement $ tree Part (width (exactly 100) : rowHeight 20 : forest forest0 : attrs)
    result <- runInteractions testBounds seedCtx
      (render'
        [ expanded (Set.singleton "src")
        , selection cursorOnFirstChild
        , onSelectionChanged (postWith (\s -> ("Selected:" ++ show s)))
        ])
      []
      [PressKey KeyLeft []]
    let expected = moveCursor Prev cursorOnFirstChild
    resultMessages result `shouldBe` ["Selected:" ++ show expected]
    selectedItems expected `shouldBe` ["src"]

-- | The tree's own scrollbar id -- 'tree' reads\/writes its position
-- under @mkId (TreeRow (ListScrollBar ScrollBar))@, the same @tag@
-- pattern 'Blink.Controls.List.list' documents for its own.
treeScrollEid :: TestElem
treeScrollEid = Part (TreeRow (ListScrollBar ScrollBar))

-- | Narrower than 'testBounds' -- with both "src" and "src/Controls"
-- expanded there are 5 rows (100px), so a 40px viewport (2 rows) is
-- scrollable.
scrollTestBounds :: Rectangle
scrollTestBounds = Rectangle 0 0 100 40

scrollingKeyboardSpec :: Spec
scrollingKeyboardSpec = describe "tree keyboard scrolling" $ do
  it "scrolls a Left-driven parent jump into view, the same way Up/Down already does" $ do
    let expandedBoth   = Set.fromList ["src", "src/Controls"]
        visibleItems   = map fst (flattenVisible forest0 expandedBoth)
        cursorOnButton = selectItem "src/Controls/Button.hs" visibleItems

    -- Scrolled all the way down: rows 0-2 ("src".."src/Controls") are out
    -- of view, cursor starts on the last row, "src/Controls/Button.hs".
    seeded <- resultContext <$> runInteractions scrollTestBounds seedCtx
      (requestScrollTo treeScrollEid 1)
      []
      []

    -- Left moves the cursor up one row to its parent, "src/Controls" (row
    -- 2 of 5, y 40-60) -- above the current 40px window (y 60-100) -- so
    -- this should scroll just enough to bring its top edge into view.
    result <- runInteractions scrollTestBounds seeded
      (renderSilentTree
        [ expanded expandedBoth
        , selection cursorOnButton
        , onSelectionChanged (postWith (\s -> (selectedMsg s)))
        ])
      []
      [PressKey KeyLeft []]

    resultMessages result `shouldBe` [selectedMsg (moveCursor Prev cursorOnButton)]
    contextScrollState treeScrollEid (resultContext result) `shouldBe` (2 / 3)

  it "scrolls a Right-driven move into a sibling below the viewport into view" $ do
    let expandedBoth  = Set.fromList ["src", "src/Controls"]
        visibleItems  = map fst (flattenVisible forest0 expandedBoth)
        cursorOnChild = selectItem "src/List.hs" visibleItems

    -- Starts scrolled to the top: rows 0-1 ("src", "src/List.hs") are in
    -- view, cursor on the second (a leaf, no chevron to expand).
    result <- runInteractions scrollTestBounds seedCtx
      (renderSilentTree
        [ expanded expandedBoth
        , selection cursorOnChild
        , onSelectionChanged (postWith (\s -> (selectedMsg s)))
        ])
      []
      [PressKey KeyRight []]

    -- Right moves the cursor down one row to "src/Controls" (row 3 of 5,
    -- y 40-60) -- below the current 40px window (y 0-40) -- so this
    -- should scroll just enough to bring its bottom edge into view.
    resultMessages result `shouldBe` [selectedMsg (moveCursor Next cursorOnChild)]
    contextScrollState treeScrollEid (resultContext result) `shouldBe` (1 / 3)

  it "scrolls the child revealed by expanding a bottom-edge row into view once Right navigates onto it" $ do
    let expandedSrc      = Set.singleton "src"
        expandedBoth     = Set.fromList ["src", "src/Controls"]
        visibleBefore    = map fst (flattenVisible forest0 expandedSrc)
        cursorOnControls = selectItem "src/Controls" visibleBefore

    -- Scrolled so "src/Controls" (row 2 of 4, y 40-60) is the last row in
    -- the 40px window (y 20-60) -- at the bottom edge, still fully
    -- visible, before it has any children of its own in view.
    step1 <- runInteractions scrollTestBounds seedCtx
      (requestScrollTo treeScrollEid (1 / 2))
      []
      []

    -- Right expands "src/Controls": the cursor stays put (still fully in
    -- view), so nothing scrolls yet -- its new child,
    -- "src/Controls/Button.hs", is now the row just below the viewport.
    step2 <- runInteractions scrollTestBounds (resultContext step1)
      (renderSilentTree
        [ expanded expandedSrc
        , selection cursorOnControls
        , onExpansionChanged (postWith (\s -> (expandedMsg s)))
        ])
      []
      [PressKey KeyRight []]
    resultMessages step2 `shouldBe` [expandedMsg expandedBoth]
    contextScrollState treeScrollEid (resultContext step2) `shouldBe` (1 / 2)

    -- Right again, now that "src/Controls" is expanded and the app has
    -- passed the updated set back in: moves the cursor onto that child,
    -- which should scroll it fully into view.
    let visibleAfter    = map fst (flattenVisible forest0 expandedBoth)
        cursorOnControls' = selectItem "src/Controls" visibleAfter
    step3 <- runInteractions scrollTestBounds (resultContext step2)
      (renderSilentTree
        [ expanded expandedBoth
        , selection cursorOnControls'
        , onSelectionChanged (postWith (\s -> (selectedMsg s)))
        ])
      []
      [PressKey KeyRight []]
    resultMessages step3 `shouldBe` [selectedMsg (moveCursor Next cursorOnControls')]
    contextScrollState treeScrollEid (resultContext step3) `shouldBe` (2 / 3)

  it "scrolls a later, unrelated sibling pushed down by an earlier expansion into view" $ do
    let expandedBoth = Set.fromList ["src", "src/Controls"]
        visibleItems  = map fst (flattenVisible forest0 expandedBoth)
        cursorOnButton = selectItem "src/Controls/Button.hs" visibleItems

    -- Scrolled so the window shows rows 2-3 of 5 ("src/Controls" and
    -- "src/Controls/Button.hs", y 40-80); cursor on the last of those, a
    -- leaf. "test" (row 4, y 80-100) -- not a descendant of anything just
    -- expanded, simply the next later sibling, pushed down by "src" and
    -- "src/Controls" both being expanded -- sits just below the window.
    seeded <- resultContext <$> runInteractions scrollTestBounds seedCtx
      (requestScrollTo treeScrollEid (2 / 3))
      []
      []

    result <- runInteractions scrollTestBounds seeded
      (renderSilentTree
        [ expanded expandedBoth
        , selection cursorOnButton
        , onSelectionChanged (postWith (\s -> (selectedMsg s)))
        ])
      []
      [PressKey KeyRight []]

    resultMessages result `shouldBe` [selectedMsg (moveCursor Next cursorOnButton)]
    contextScrollState treeScrollEid (resultContext result) `shouldBe` 1

chromeSeedCtx :: ViewContext TestElem String
chromeSeedCtx = emptyViewContext scrollTestBounds noInput chromeTheme

chromeSpec :: Spec
chromeSpec = describe "tree keyboard scrolling with list chrome" $
  it "accounts for the list's own padding, not just its outer bounds, when scrolling a row into view" $ do
    let expandedBoth = Set.fromList ["src", "src/Controls"]
        visibleItems  = map fst (flattenVisible forest0 expandedBoth)
        cursorOnSrc   = selectItem "src" visibleItems

    result <- runInteractions scrollTestBounds chromeSeedCtx
      (renderSilentTree
        [ expanded expandedBoth
        , selection cursorOnSrc
        , onSelectionChanged (postWith (\s -> (selectedMsg s)))
        ])
      []
      [PressKey KeyRight []]

    resultMessages result `shouldBe` [selectedMsg (moveCursor Next cursorOnSrc)]
    -- 40px outer bounds, 8px padding each side -> 24px usable viewport;
    -- content 100px -> max offset 76px. Row 1 (y 20-40) doesn't fit the
    -- 24px window, so this must scroll -- reading the outer 40px bounds
    -- alone would wrongly conclude it already fits.
    contextScrollState treeScrollEid (resultContext result) `shouldBe` (16 / 76)

spec :: Spec
spec = describe "Blink.Controls.Tree" $ do
  flattenVisibleSpec
  widgetSpec
  keyboardSpec
  multiKeyboardSpec
  scrollingKeyboardSpec
  chromeSpec
