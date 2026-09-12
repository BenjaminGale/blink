{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.FocusScopeSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control
import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..), Key (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
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
noInput = InputState (Point 500 500) False [] [] 0

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

-- | Like 'composite', but the children don't fully cover the composite's
-- own bounds -- leaving a real "background" strip (here, 40-50 and 110-120)
-- that hits the composite's own hit region without hitting either child.
compositeWithGap :: FocusPolicy -> View TestElement String ()
compositeWithGap policy = withBounds rectComposite (() <$ control cfg)
  where
    cfg = (resolve defaultControlConfig [elementId Composite, focusPolicy policy])
      { ccContent = const $ do
          leaf Child1 (Rectangle 50 0 30 100) []
          leaf Child2 (Rectangle 80 0 30 100) []
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

rectOption1, rectOption2, rectOption3, rectOption4 :: Rectangle
rectOption1 = Rectangle 40  0 20 100
rectOption2 = Rectangle 60  0 20 100
rectOption3 = Rectangle 80  0 20 100
rectOption4 = Rectangle 100 0 20 100

-- | A 'FocusScope' composite with four children (reusing 'Child1'..'Child4').
-- Unlike the two-child 'composite' above -- where "whichever child was
-- selected" and "whichever child renders last" are always the same child
-- by construction -- this has enough children to actually distinguish the
-- two when testing 'EnterRemembered'.
fourOptionsComposite :: FocusPolicy -> View TestElement String ()
fourOptionsComposite policy = withBounds rectComposite (() <$ control cfg)
  where
    cfg = (resolve defaultControlConfig [elementId Composite, focusPolicy policy])
      { ccContent = const $ do
          leaf Child1 rectOption1 []
          leaf Child2 rectOption2 []
          leaf Child3 rectOption3 []
          leaf Child4 rectOption4 []
      }

-- | 'Before', 'fourOptionsComposite', and 'After', in that order.
renderFourOptions :: FocusPolicy -> View TestElement String ()
renderFourOptions policy = do
  leaf Before rectBefore []
  fourOptionsComposite policy
  leaf After rectAfter []

-- | A 'ContainedNavigation' scheme using Down\/Up as the forward\/backward
-- keys (Tab stays owned by the composite as a whole), with the given
-- wrap and entry policies.
arrowNav :: WrapPolicy -> EntryPolicy -> ContainedNavigation
arrowNav wrap entry = ContainedNavigation
  { navForward  = (KeyDown, [])
  , navBackward = (KeyUp, [])
  , navWrap     = wrap
  , navEntry    = entry
  }

-- | Whether @eid@ is the innermost currently-focused element, following
-- the chain through any nested 'FocusScope's rather than only checking
-- the outermost ambient claim.
focusedOn :: TestElement -> InteractionResult TestElement String a -> Bool
focusedOn eid result = case contextFocusChain (resultContext result) of
  [] -> False
  xs -> last xs == eid

spec :: Spec
spec = describe "Blink.Controls.Control.FocusScope" $ do
  describe "Continue" $ do
    let policy = FocusScope Continue

    describe "a composite as the first control, nothing focused yet" $ do
      it "Child1 is focused" $ do
        result <- runInteractions testBounds seedCtx (composite policy [] []) [] []
        focusedOn Child1 result `shouldBe` True

      it "Composite's own focus-gained handler fires too, via focus-within" $ do
        let cfg = (resolve defaultControlConfig [elementId Composite, focusPolicy policy, onFocusGained (post ("Composite gained" :: String))])
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

    describe "mousedown on the composite's own background (not any child)" $ do
      -- 'Before' auto-claims root focus the instant nothing else has it --
      -- standing in for e.g. a sidebar's own first control, which would
      -- otherwise be free to steal focus back the moment the composite's
      -- own claim is (even momentarily) given up.
      let renderWithSidebar = do
            leaf Before rectBefore []
            compositeWithGap policy
            leaf After rectAfter []
          gapPoint = Point 45 50

      it "Before auto-claims at start" $ do
        result <- runInteractions testBounds seedCtx renderWithSidebar [] [Wait 1]
        focusedOn Before result `shouldBe` True

      it "press, hold across a frame, then release lands on Child1 -- not back on Before" $ do
        result <- runInteractions testBounds seedCtx renderWithSidebar []
          [MouseDown gapPoint, Wait 1, Wait 1, MouseUp gapPoint, Wait 1]
        focusedOn Child1 result `shouldBe` True

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

  describe "Contained" $ do
    describe "entering and leaving the group" $ do
      let policy = FocusScope (Contained (arrowNav WrapCycle EnterFirst))

      it "Tab from Before lands on Child1 (EnterFirst)" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1] [Tab, Wait 1]
        focusedOn Child1 result `shouldBe` True

      it "Tab leaves the group for After, regardless of which child was selected" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1, PressKey KeyDown [], Wait 1] [Tab, Wait 1]
        focusedOn After result `shouldBe` True

      it "Shift-Tab leaves the group for Before, regardless of which child was selected" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        focusedOn Before result `shouldBe` True

    describe "moving between children with the arrow keys" $ do
      let policy = FocusScope (Contained (arrowNav WrapCycle EnterFirst))

      it "Down moves Child1 -> Child2" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1] [PressKey KeyDown [], Wait 1]
        focusedOn Child2 result `shouldBe` True

      it "Up moves Child2 -> Child1" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1, PressKey KeyDown [], Wait 1] [PressKey KeyUp [], Wait 1]
        focusedOn Child1 result `shouldBe` True

    describe "WrapCycle" $ do
      let policy = FocusScope (Contained (arrowNav WrapCycle EnterFirst))

      it "Down from the last child wraps to the first" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1, PressKey KeyDown [], Wait 1] [PressKey KeyDown [], Wait 1]
        focusedOn Child1 result `shouldBe` True

      it "Up from the first child wraps to the last" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1] [PressKey KeyUp [], Wait 1]
        focusedOn Child2 result `shouldBe` True

    describe "mousedown on the group's own background while a child is already focused" $ do
      let policy = FocusScope (Contained (arrowNav WrapCycle EnterFirst))
          gapPoint = Point 45 50
          -- Tab in (EnterFirst lands on Child1), then move to Child2, so a
          -- later reset-to-first would be distinguishable from "stayed put".
          selectChild2 = [Wait 1, Tab, Wait 1, PressKey KeyDown [], Wait 1]

      it "doesn't reset the selection back to Child1 (EnterFirst re-firing on every click)" $ do
        result <- runInteractions testBounds seedCtx (compositeWithGap policy) selectChild2
          [MouseDown gapPoint, Wait 1, MouseUp gapPoint, Wait 1]
        focusedOn Child2 result `shouldBe` True

    describe "WrapStop" $ do
      let policy = FocusScope (Contained (arrowNav WrapStop EnterFirst))

      it "Down from the last child stays put" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1, PressKey KeyDown [], Wait 1] [PressKey KeyDown [], Wait 1]
        focusedOn Child2 result `shouldBe` True

      it "Up from the first child stays put" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1] [PressKey KeyUp [], Wait 1]
        focusedOn Child1 result `shouldBe` True

      it "Tab still leaves the group even when stopped at the last child" $ do
        result <- runInteractions testBounds seedCtx (renderAll policy) [Wait 1, Tab, Wait 1, PressKey KeyDown [], Wait 1] [Tab, Wait 1]
        focusedOn After result `shouldBe` True

    describe "EnterRemembered" $ do
      let policy = FocusScope (Contained (arrowNav WrapCycle EnterRemembered))
          -- Before -> Child1 -> Child2 -> Child3, then leave to After.
          -- Child3 is a middle child -- neither first nor last -- so
          -- landing back on it can't be confused with either "always
          -- resets to first" (EnterFirst) or "always ends up on
          -- whichever child renders last" (the bug this is guarding
          -- against: 'Child4' would render last in this fixture).
          selectChild3 =
            [ Wait 1, Tab, Wait 1
            , PressKey KeyDown [], Wait 1
            , PressKey KeyDown [], Wait 1
            , Tab, Wait 1
            ]

      it "remembers a middle selection, not whichever child renders last" $ do
        result <- runInteractions testBounds seedCtx (renderFourOptions policy) selectChild3 [ShiftTab, Wait 1]
        focusedOn Child3 result `shouldBe` True

      it "keeps remembering across several frames away, same page" $ do
        result <- runInteractions testBounds seedCtx (renderFourOptions policy)
          (selectChild3 ++ [Wait 1, Wait 1, Wait 1, Wait 1, Wait 1])
          [ShiftTab, Wait 1]
        focusedOn Child3 result `shouldBe` True

      it "EnterFirst, unlike EnterRemembered, always resets to the first child" $ do
        let firstPolicy = FocusScope (Contained (arrowNav WrapCycle EnterFirst))
        result <- runInteractions testBounds seedCtx (renderFourOptions firstPolicy) selectChild3 [ShiftTab, Wait 1]
        focusedOn Child1 result `shouldBe` True
