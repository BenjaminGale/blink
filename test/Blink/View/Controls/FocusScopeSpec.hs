{-# LANGUAGE OverloadedStrings #-}
module Blink.View.Controls.FocusScopeSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.View.Controls.Control
import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.View.Rendering (Colour (..), TextAlign (..))
import Blink.View.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View

data TestElement
  = Before | After
  | Composite  | Child1 | Child2
  | Composite2 | Child3 | Child4
  deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 200 100

rectBefore, rectComposite, rectChild1, rectChild2, rectAfter :: Rectangle
rectBefore    = Rectangle 0   0 40 100
rectComposite = Rectangle 40  0 80 100
rectChild1    = Rectangle 40  0 40 100
rectChild2    = Rectangle 80  0 40 100
rectAfter     = Rectangle 120 0 40 100

rectComposite2, rectChild3, rectChild4 :: Rectangle
rectComposite2 = Rectangle 120 0 80 100
rectChild3     = Rectangle 120 0 40 100
rectChild4     = Rectangle 160 0 40 100

emptyStyle :: Style
emptyStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignCenter
  , styleBorderColour = Nothing
  }

emptyMetrics :: Metrics
emptyMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

testTheme :: Theme TestElement
testTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  = (emptyMetrics, StyleSet emptyStyle Map.empty)
  }

noInput :: InputState
noInput = InputState (Point 500 500) False [] []

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme noOpTextMeasurer

type Attribute' = Attribute (ControlConfig TestElement String)

-- | A plain leaf at @rect@, identified by @eid@, with the given attrs.
leaf :: TestElement -> Rectangle -> [Attribute'] -> View TestElement String ()
leaf eid rect attrs = withBounds rect (() <$ control (resolve defaultControlConfig (elementId eid : attrs)))

-- | A 'FocusScope' composite at 'rectComposite', containing 'Child1' then
-- 'Child2' (each a plain leaf, no attrs of their own beyond an optional
-- override), with the given 'FocusPolicy'.
composite :: FocusPolicy -> [Attribute'] -> [Attribute'] -> View TestElement String ()
composite policy child1Attrs child2Attrs = withBounds rectComposite (() <$ control cfg)
  where
    cfg = (resolve defaultControlConfig [elementId Composite, focusPolicy policy])
      { ccContent = const $ do
          leaf Child1 rectChild1 child1Attrs
          leaf Child2 rectChild2 child2Attrs
      }

-- | A second 'FocusScope' composite at 'rectComposite2', containing
-- 'Child3' then 'Child4'.
composite2 :: FocusPolicy -> View TestElement String ()
composite2 policy = withBounds rectComposite2 (() <$ control cfg)
  where
    cfg = (resolve defaultControlConfig [elementId Composite2, focusPolicy policy])
      { ccContent = const $ do
          leaf Child3 rectChild3 []
          leaf Child4 rectChild4 []
      }

-- | 'Before', the composite, and 'After', in that order.
renderAll :: FocusPolicy -> View TestElement String ()
renderAll policy = do
  leaf Before rectBefore []
  composite policy [] []
  leaf After rectAfter []

-- | Whether @eid@ is the innermost currently-focused element, following
-- the chain through any nested 'FocusScope's rather than only checking
-- the outermost ambient claim.
focusedOn :: TestElement -> InteractionResult TestElement String a -> Bool
focusedOn eid result = case contextFocusChain (resultContext result) of
  [] -> False
  xs -> last xs == eid

spec :: Spec
spec = describe "Blink.View.Controls.Control.FocusScope" $
  describe "Continue" $ do
    let policy = FocusScope Continue

    describe "a composite as the first control, nothing focused yet" $ do
      it "Child1 is focused" $ do
        result <- runInteractions testBounds seedCtx (composite policy [] []) [] []
        focusedOn Child1 result `shouldBe` True

      it "Composite's own focus-gained handler fires too, via focus-within" $ do
        let cfg = (resolve defaultControlConfig [elementId Composite, focusPolicy policy, onFocusGained (const [OutMsg ("Composite gained" :: String)])])
              { ccContent = const $ do
                  leaf Child1 rectChild1 []
                  leaf Child2 rectChild2 []
              }
        result <- runInteractions testBounds seedCtx (() <$ control cfg) [] []
        resultMessages result `shouldBe` ["Composite gained"]

    describe "a composite between two plain controls" $ do
      it "Before -> Composite lands on Child1" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1] [Tab]
        focusedOn Child1 result `shouldBe` True

      it "Child1 -> Child2" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1] [Tab, Wait 1, Tab]
        focusedOn Child2 result `shouldBe` True

      it "Child2 -> Child1 (Shift-Tab)" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        focusedOn Child1 result `shouldBe` True

      it "Child2 -> After" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1, Tab, Wait 1] [Tab, Wait 1]
        focusedOn After result `shouldBe` True

      it "Child1 -> Before (Shift-Tab)" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        focusedOn Before result `shouldBe` True

      it "Child2 disabled, Child1 -> After" $ do
        let render = do
              leaf Before rectBefore []
              composite policy [] [isEnabled False]
              leaf After rectAfter []
        result <- runInteractions testBounds seedCtx render [Wait 1, Tab, Wait 1] [Tab, Wait 1]
        focusedOn After result `shouldBe` True

      it "clicking Child2 focuses it directly" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [] [ClickAt (Point 100 50), Wait 1]
        focusedOn Child2 result `shouldBe` True

      it "Child2 (focused via click) -> After" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [ClickAt (Point 100 50), Wait 1] [Tab, Wait 1]
        focusedOn After result `shouldBe` True

    describe "two composites in sequence" $ do
      let renderTwo = do
            composite policy [] []
            composite2 policy

      it "Child2 -> Child3 (second composite's first child)" $ do
        result <- runInteractions testBounds seedCtx renderTwo [Wait 1, Tab, Wait 1] [Tab, Wait 1]
        focusedOn Child3 result `shouldBe` True

      it "Child3 -> Child4" $ do
        result <- runInteractions testBounds seedCtx renderTwo [Wait 1, Tab, Wait 1, Tab, Wait 1] [Tab, Wait 1]
        focusedOn Child4 result `shouldBe` True
