{-# LANGUAGE OverloadedStrings #-}
-- | Tests both 'itemsLayout' and 'selectionControl' from
-- "Blink.ItemsControl". They share a "renders 'items' via a template, in
-- order" behaviour, exercised once via 'itemOrderingSpec' and
-- 'emptyItemsSpec' and instantiated for each control, rather than
-- duplicated per-control — 'selectionControl' is built directly on top of
-- 'itemsLayout' and forwards 'items'\/'itemsPanel' straight through, so
-- this also doubles as a check that the sharing actually works.
module Blink.ItemsControlSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Blink.Controls (orientation)
import Blink.ControlsTestSupport
  ( TestElement (..)
  , dispatchCount
  , drawnTexts
  , noInput
  , mouseAt
  , settle
  , testColour
  , withButtonReleased
  )
import Blink.Geometry (Alignment (..), Orientation (..), Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState)
import Blink.ItemsControl
  ( SelectionEvent (..)
  , SelectionState (..)
  , itemContainer
  , itemTemplate
  , items
  , itemsLayout
  , itemsPanel
  , onSelect
  , selected
  , selectedIndex
  , selectionControl
  )
import Blink.Layout (BoxConfig (..), Layout (..), Length (..), defaultBoxConfig)
import Blink.Rendering (DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

itemLabels :: [Text]
itemLabels = ["Alpha", "Beta", "Gamma"]

-- Zero margin/padding so the whole 100x90 rect is evenly split three ways,
-- matching the existing selector/radioGroup/listBox test fixtures' geometry
-- (Blink.ControlsSpec) so click points are easy to reason about.
zeroChromeStyle :: Style
zeroChromeStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignLeft
  , styleMargin       = uniform 0
  , stylePadding      = uniform 0
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

zeroChromeStyleSet :: StyleSet
zeroChromeStyleSet = StyleSet
  { styleSetNormal   = zeroChromeStyle
  , styleSetHovered  = zeroChromeStyle
  , styleSetPressed  = zeroChromeStyle
  , styleSetFocused  = zeroChromeStyle
  , styleSetDisabled = zeroChromeStyle
  }

listRect :: Rectangle
listRect = Rectangle 0 0 100 90

-- 'itemsLayout' has no element id, so it never looks anything up in the
-- theme; 'Int' is just a convenient, concrete element type for
-- 'selectionControl's per-item ids (mirrors 'Blink.ControlsSpec's
-- radioGroup/listBox fixtures, which use @Int@ with @mkId = id@).
listTheme :: Theme Int
listTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = zeroChromeStyleSet }

mkListCtx :: InputState -> UIContext Int msg
mkListCtx input = emptyUIContext listRect input listTheme noOpTextMeasurer

isFillRect :: DrawCommand -> Bool
isFillRect (FillRect {}) = True
isFillRect _             = False

-- | Draws only the item's label -- used to check ordering/content.
itemsRenderItem :: Int -> Text -> (Layout, UI Int String ())
itemsRenderItem _ lbl = (Layout Fill Fill TopLeft, drawText testColour AlignLeft lbl)

-- | Draws its own bounds as @(x, y)@ -- used to check panel arrangement.
boundsRenderItem :: Int -> Text -> (Layout, UI Int String ())
boundsRenderItem _ _ =
  ( Layout Fill Fill TopLeft
  , do
      r <- getBounds
      drawText testColour AlignLeft (T.pack (show (rectX r, rectY r)))
  )

runItemsLayout attrs = fmap (settle . snd) . runUI (itemsLayout attrs)

selectionRenderItem :: Int -> SelectionState -> Text -> (Layout, UI Int (Int, Text) ())
selectionRenderItem _ st lbl =
  (Layout Fill Fill TopLeft, drawText testColour AlignLeft ((if st == Selected then "SEL:" else "UNSEL:") <> lbl))

dispatchActivated :: Int -> Text -> [Out Int (Int, Text)]
dispatchActivated idx val = [OutMsg (idx, val)]

runSelectionControl attrs = fmap (settle . snd) . runUI (selectionControl id attrs)

-- | Shared behaviour: renders 'items' via the template, in order.
itemOrderingSpec :: (UIContext e msg -> IO (UIContext e msg)) -> UIContext e msg -> [Text] -> Spec
itemOrderingSpec run baseCtx expectedTexts =
  it "renders each item's content, in order" $ do
    ctx' <- run baseCtx
    drawnTexts ctx' `shouldBe` expectedTexts

-- | Shared behaviour: renders nothing when 'items' is left at its default
-- (empty).
emptyItemsSpec :: (UIContext e msg -> IO (UIContext e msg)) -> UIContext e msg -> Spec
emptyItemsSpec run baseCtx =
  it "renders nothing when items is empty" $ do
    ctx' <- run baseCtx
    drawnTexts ctx' `shouldBe` []

-- | Shared behaviour: neither control is a chrome-drawing control -- see
-- the module header of "Blink.ItemsControl".
noChromeSpec :: (UIContext e msg -> IO (UIContext e msg)) -> UIContext e msg -> Spec
noChromeSpec run baseCtx =
  it "draws no background or border of its own" $ do
    ctx' <- run baseCtx
    filter isFillRect (getDrawCommands ctx') `shouldBe` []

spec :: Spec
spec = describe "Blink.ItemsControl" $ do
  describe "itemsLayout" $ do
    let run      = runItemsLayout [items itemLabels, itemTemplate itemsRenderItem]
        runEmpty = runItemsLayout [itemTemplate itemsRenderItem]

    describe "items" $ do
      itemOrderingSpec run (mkListCtx noInput) itemLabels
      emptyItemsSpec runEmpty (mkListCtx noInput)

    noChromeSpec run (mkListCtx noInput)

    describe "orientation / itemsPanel" $ do
      it "arranges items in a vertical stack by default" $ do
        ctx' <- runItemsLayout [items itemLabels, itemTemplate boundsRenderItem] (mkListCtx noInput)
        let coords = map (read . T.unpack) (drawnTexts ctx') :: [(Double, Double)]
        map fst coords `shouldSatisfy` \xs -> all (== head xs) xs
        map snd coords `shouldSatisfy` \ys -> and (zipWith (<) ys (tail ys))

      it "arranges items in a horizontal row with orientation Horizontal" $ do
        ctx' <- runItemsLayout
          [items itemLabels, orientation Horizontal, itemTemplate boundsRenderItem]
          (mkListCtx noInput)
        let coords = map (read . T.unpack) (drawnTexts ctx') :: [(Double, Double)]
        map snd coords `shouldSatisfy` \ys -> all (== head ys) ys
        map fst coords `shouldSatisfy` \xs -> and (zipWith (<) xs (tail xs))

      it "still stacks vertically when itemsPanel changes spacing" $ do
        ctx' <- runItemsLayout
          [items itemLabels, itemsPanel (defaultBoxConfig { boxSpacing = 2 }), itemTemplate boundsRenderItem]
          (mkListCtx noInput)
        let coords = map (read . T.unpack) (drawnTexts ctx') :: [(Double, Double)]
        map fst coords `shouldSatisfy` \xs -> all (== head xs) xs
        map snd coords `shouldSatisfy` \ys -> and (zipWith (<) ys (tail ys))

  describe "selectionControl" $ do
    let run      = runSelectionControl [items itemLabels, itemContainer selectionRenderItem, onSelect dispatchActivated]
        runEmpty = runSelectionControl [itemContainer selectionRenderItem, onSelect dispatchActivated]

    describe "items" $ do
      itemOrderingSpec run (mkListCtx noInput) (map ("UNSEL:" <>) itemLabels)
      emptyItemsSpec runEmpty (mkListCtx noInput)

    noChromeSpec run (mkListCtx noInput)

    describe "selection" $ do
      it "defaults to nothing selected" $ do
        ctx' <- run (mkListCtx noInput)
        drawnTexts ctx' `shouldBe` map ("UNSEL:" <>) itemLabels

      it "marks the item matching 'selected' as Selected" $ do
        ctx' <- runSelectionControl
          [items itemLabels, selected "Beta", itemContainer selectionRenderItem, onSelect dispatchActivated]
          (mkListCtx noInput)
        drawnTexts ctx' `shouldBe` ["UNSEL:Alpha", "SEL:Beta", "UNSEL:Gamma"]

      it "marks the item at 'selectedIndex' as Selected regardless of value" $ do
        ctx' <- runSelectionControl
          [items itemLabels, selectedIndex 2, itemContainer selectionRenderItem, onSelect dispatchActivated]
          (mkListCtx noInput)
        drawnTexts ctx' `shouldBe` ["UNSEL:Alpha", "UNSEL:Beta", "SEL:Gamma"]

    describe "click detection" $ do
      it "fires Activated with the clicked item's index and value" $ do
        ctx' <- run (withButtonReleased (mkListCtx (mouseAt (Point 50 45) False [])))
        getMessages ctx' `shouldBe` [(1, "Beta")]

      it "does not dispatch when there is no interaction" $ do
        ctx' <- run (mkListCtx noInput)
        dispatchCount ctx' `shouldBe` 0

      it "does not dispatch when clicked while disabled" $ do
        (_, ctx') <- runUI
          (disableWhen True (selectionControl id [items itemLabels, itemContainer selectionRenderItem, onSelect dispatchActivated]))
          (withButtonReleased (mkListCtx (mouseAt (Point 50 45) False [])))
        dispatchCount (settle ctx') `shouldBe` 0
