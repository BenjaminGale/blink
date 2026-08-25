{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ControlSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control
  ( Attr, ControlConfig (..), FocusOnClick (..)
  , controlBase, defaultControlConfig, isEnabled, isFocusable
  , onFocusGained, onFocusLost, onKeyPressed, resolve
  )
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement
  = ElemA | ElemB | ElemC
  deriving (Eq, Ord, Show)

rectA, rectB, rectC :: Rectangle
rectA = Rectangle 0 0 50 100
rectB = Rectangle 50 0 50 100
rectC = Rectangle 100 0 50 100

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignCenter
  , styleMargin       = uniform 10
  , stylePadding      = uniform 5
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet
  { styleSetNormal   = testStyle
  , styleSetHovered  = testStyle
  , styleSetPressed  = testStyle
  , styleSetFocused  = testStyle
  , styleSetDisabled = testStyle
  }

testTheme :: Theme TestElement
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = testStyleSet }

pressedColour :: Colour
pressedColour = RGBA 1 1 1 1

-- | Like 'testTheme', but with a 'styleSetPressed' background distinct
-- from every other variant, so a test can tell whether the pressed style
-- was actually the one drawn.
pressedTestTheme :: Theme TestElement
pressedTestTheme = testTheme
  { themeDefaultStyle = testStyleSet { styleSetPressed = testStyle { styleBackground = pressedColour } } }

onA, onB :: Point
onA = Point 10 50
onB = Point 60 50

type Attr' = Attr (ControlConfig TestElement String)

-- | Renders a single control at whatever bounds are current, with its
-- attrs resolved against 'defaultControlConfig'.
renderAt :: TestElement -> [Attr'] -> UI TestElement String ()
renderAt eid attrs = () <$ controlBase eid (resolve defaultControlConfig attrs)

-- | Renders a single control the same way, but with 'ccFocusOnClick'
-- overridden afterward -- there's no @focusOnClick@ attribute any more (see
-- "Blink.Controls.Control"), so a spec that wants to vary it constructs the
-- config directly instead of resolving it as an attr.
renderFoc :: FocusOnClick TestElement -> TestElement -> [Attr'] -> UI TestElement String ()
renderFoc foc eid attrs = () <$ controlBase eid (resolve defaultControlConfig attrs) { ccFocusOnClick = foc }

-- | Renders 'ElemA' at 'rectA' and 'ElemB' at 'rectB' with the given
-- per-element 'FocusOnClick' and attrs.
both :: FocusOnClick TestElement -> [Attr'] -> FocusOnClick TestElement -> [Attr'] -> UI TestElement String ()
both focA attrsA focB attrsB = do
  withBounds rectA (renderFoc focA ElemA attrsA)
  withBounds rectB (renderFoc focB ElemB attrsB)

-- | Renders a single 'ElemA' at 'testBounds' with the given attrs -- the
-- same way every real widget built on 'controlBase' does.
renderControl :: [Attr'] -> UI TestElement String ()
renderControl = renderAt ElemA

three :: [Attr'] -> [Attr'] -> [Attr'] -> UI TestElement String ()
three attrsA attrsB attrsC = do
  withBounds rectA (renderAt ElemA attrsA)
  withBounds rectB (renderAt ElemB attrsB)
  withBounds rectC (renderAt ElemC attrsC)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer
  where
    noInput = InputState (Point 200 200) False [] []

pressedSeedCtx :: UIContext TestElement String
pressedSeedCtx = emptyUIContext testBounds noInput pressedTestTheme noOpTextMeasurer
  where
    noInput = InputState (Point 200 200) False [] []

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

spec :: Spec
spec = describe "Blink.Controls.Control.controlBase" $ do
  controlBehaviourSpec defaultControlBehaviourConfig testBounds seedCtx ElemA (Point 5 5) hitRect (Point 200 200) renderControl

  describe "chrome" $ do
    it "draws background via renderStyled, inset by margin" $ do
      ctx <- snd <$> runUI (renderControl []) seedCtx
      getDrawCommands ctx `shouldContain` [FillRect (insetRect (uniform 10) testBounds) testColour]

    it "draws in its pressed style while the mouse is held down over it" $ do
      result <- runInteractions testBounds pressedSeedCtx (renderControl []) [] [MouseDown (Point 50 50)]
      getDrawCommands (resultContext result) `shouldContain` [FillRect (insetRect (uniform 10) testBounds) pressedColour]

  describe "auto-claim" $
    it "raises a focus gained event for only the first of several simultaneously-eligible controls" $ do
      let attrsA = [onFocusGained (const [OutMsg ("A gained" :: String)])]
          attrsB = [onFocusGained (const [OutMsg ("B gained" :: String)])]
      result <- runInteractions testBounds seedCtx (both FocusSelf attrsA FocusSelf attrsB) [] []
      resultMessages result `shouldBe` ["A gained"]

  describe "click-to-focus" $ do
    let attrsA = [onFocusLost   (const [OutMsg ("A lost"   :: String)])]
        attrsB = [onFocusGained (const [OutMsg ("B gained" :: String)])]
        render = both FocusSelf attrsA FocusSelf attrsB

    it "does not take effect on the click's own frame" $ do
      result <- runInteractions testBounds seedCtx render [] [ClickAt onB]
      resultMessages result `shouldBe` []

    it "takes effect one frame after the click, firing FocusLost/FocusGained for the right elements" $ do
      result <- runInteractions testBounds seedCtx render [] [ClickAt onB, Wait 1]
      resultMessages result `shouldBe` ["A lost", "B gained"]

    it "FocusTarget redirects focus to the named element instead of the clicker" $ do
      let taggedA = [isFocusable False, onFocusGained (const [OutMsg ("A gained" :: String)])]
          taggedB = [isFocusable False, onFocusGained (const [OutMsg ("B gained" :: String)])]
      result <- runInteractions testBounds seedCtx (both (FocusTarget ElemB) taggedA FocusSelf taggedB) [] [ClickAt onA, Wait 1]
      resultMessages result `shouldBe` ["B gained"]

    it "NoFocus leaves focus unchanged when clicked" $ do
      let attrs :: [Attr']
          attrs = [isFocusable False, onFocusGained (const [OutMsg ("gained" :: String)])]
      result <- runInteractions testBounds seedCtx (renderFoc NoFocus ElemA attrs) [] [ClickAt onA, Wait 1]
      resultMessages result `shouldBe` []

  describe "keyboard navigation" $ do
    let attrsA = [onFocusLost   (const [OutMsg ("A lost"   :: String)])]
        attrsB = [onFocusGained (const [OutMsg ("B gained" :: String)])]
        render = both FocusSelf attrsA FocusSelf attrsB

    it "Tab gives up focus immediately, letting the next control auto-claim in the same frame" $ do
      result <- runInteractions testBounds seedCtx render [Wait 1] [Tab]
      resultMessages result `shouldBe` ["A lost", "B gained"]

    it "does not hand focus to the previous tab stop on the Shift-Tab frame itself" $ do
      result <- runInteractions testBounds seedCtx render [Wait 1] [ShiftTab]
      resultMessages result `shouldBe` []

    it "hands focus to the previous tab stop one frame after Shift-Tab" $ do
      result <- runInteractions testBounds seedCtx render [Wait 1] [ShiftTab, Wait 1]
      resultMessages result `shouldBe` ["A lost", "B gained"]

    it "does not report Tab as a key event to the control it moves focus away from" $ do
      let keyAttrs = [onKeyPressed (\k -> [OutMsg (show k)])]
      result <- runInteractions testBounds seedCtx (both FocusSelf keyAttrs FocusSelf []) [Wait 1] [Tab]
      resultMessages result `shouldBe` []

    it "does not report Shift-Tab as a key event to the control it moves focus away from" $ do
      let keyAttrs = [onKeyPressed (\k -> [OutMsg (show k)])]
      result <- runInteractions testBounds seedCtx (both FocusSelf keyAttrs FocusSelf []) [Wait 1] [ShiftTab]
      resultMessages result `shouldBe` []

    describe "Shift-Tab past a disabled control" $ do
      let tagged e = [onFocusGained (const [OutMsg (show e ++ " gained")]), onFocusLost (const [OutMsg (show e ++ " lost")])]
          renderWithDisabledMiddle = three (tagged ElemA) (isEnabled False : tagged ElemB) (tagged ElemC)

      it "Tab from the first control skips the disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithDisabledMiddle [Wait 1] [Tab, Wait 1]
        resultMessages result `shouldBe` ["ElemA lost", "ElemC gained"]

      it "Shift-Tab back from the last control also skips the disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithDisabledMiddle [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        resultMessages result `shouldBe` ["ElemA gained", "ElemC lost"]

    describe "Shift-Tab past a control disabled via an ambient disableWhen" $ do
      let tagged e = [onFocusGained (const [OutMsg (show e ++ " gained")]), onFocusLost (const [OutMsg (show e ++ " lost")])]
          renderWithAmbientlyDisabledMiddle = do
            withBounds rectA (renderAt ElemA (tagged ElemA))
            disableWhen True (withBounds rectB (renderAt ElemB (tagged ElemB)))
            withBounds rectC (renderAt ElemC (tagged ElemC))

      it "Tab from the first control skips the ambiently-disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithAmbientlyDisabledMiddle [Wait 1] [Tab, Wait 1]
        resultMessages result `shouldBe` ["ElemA lost", "ElemC gained"]

      it "Shift-Tab back from the last control also skips the ambiently-disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithAmbientlyDisabledMiddle [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        resultMessages result `shouldBe` ["ElemA gained", "ElemC lost"]
