{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.TreeSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree (Tree (..))
import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.List
  ( Direction (..), SingleSelection, activate, isItem, moveCursor, onSelectionChanged, rowHeight, selectItem
  , selection, unselected
  )
import Blink.Controls.Tree
import Blink.Element (Element (..), runElement, width)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..), noBorder, uniform)
import Blink.Input (InputState (..), Key (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (Colour (..), TextAlign (..))
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

noInput :: InputState
noInput = InputState (Point 200 200) False [] []

seedCtx :: ViewContext TestElem String
seedCtx = emptyViewContext testBounds noInput testTheme noOpTextMeasurer

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
        , onExpansionChanged (\s -> [OutMsg ("Expanded:" ++ show (Set.toList s))])
        ])
      [MoveTo (atRow 1 8)]
      [ClickAt (atRow 1 8)]
    resultMessages result `shouldBe` ["Expanded:[]"]

  it "a leaf row's chevron column is inert -- there's no chevron there to click" $ do
    result <- runInteractions testBounds seedCtx
      (renderSilentTree
        [ expanded (Set.singleton "src")
        , selection (unselected items)
        , onExpansionChanged (\s -> [OutMsg ("Expanded:" ++ show (Set.toList s))])
        ])
      [MoveTo (atRow 2 24)]
      [ClickAt (atRow 2 24)]
    resultMessages result `shouldBe` []

expandedMsg :: Set.Set String -> String
expandedMsg s = "Expanded:" ++ show (Set.toList s)

selectedMsg :: SingleSelection String -> String
selectedMsg s = "Selected:" ++ show s

keyReactions :: [Attribute (TreeConfig SingleSelection TestElem String String)]
keyReactions =
  [ onExpansionChanged (\s -> [OutMsg (expandedMsg s)])
  , onSelectionChanged (\s -> [OutMsg (selectedMsg s)])
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

    -- Left on that leaf child: moves the cursor back to its parent, "src".
    step3 <- runInteractions testBounds (resultContext step2)
      (renderSilentTree (expanded (Set.singleton "src") : selection cursorOnFirstChild : keyReactions))
      []
      [PressKey KeyLeft []]
    resultMessages step3 `shouldBe` [selectedMsg (activate "src" cursorOnFirstChild)]

spec :: Spec
spec = describe "Blink.Controls.Tree" $ do
  flattenVisibleSpec
  widgetSpec
  keyboardSpec
