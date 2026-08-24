{-# LANGUAGE OverloadedStrings #-}
module Blink.ControlSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Control
  ( ControlAttrs, FocusOnClick (..)
  , control, focusOnClick, isFocusable
  , onFocusGained, onFocusLost, onKeyPressed
  )
import Blink.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement
  = ElemA | ElemB
  deriving (Eq, Ord, Show)

rectA, rectB :: Rectangle
rectA = Rectangle 0 0 50 100
rectB = Rectangle 50 0 50 100

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

onA, onB :: Point
onA = Point 10 50
onB = Point 60 50

type Attr' = ControlAttrs TestElement String

-- | Renders 'ElemA' at 'rectA' and 'ElemB' at 'rectB' with the given
-- per-element 'FocusOnClick' and attrs.
both :: FocusOnClick TestElement -> [Attr'] -> FocusOnClick TestElement -> [Attr'] -> UI TestElement String ()
both focA attrsA focB attrsB = do
  withBounds rectA (control ElemA (focusOnClick focA : attrsA))
  withBounds rectB (control ElemB (focusOnClick focB : attrsB))

-- | Renders a single 'ElemA' at 'testBounds' with the given attrs -- the
-- same way every real widget built on 'control' does.
renderControl :: [Attr'] -> UI TestElement String ()
renderControl attrs = control ElemA attrs

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer
  where
    noInput = InputState (Point 200 200) False [] []

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

spec :: Spec
spec = describe "Blink.Control" $ do
  controlBehaviourSpec defaultControlBehaviourConfig testBounds seedCtx ElemA (Point 5 5) hitRect (Point 200 200) renderControl

  describe "chrome" $
    it "draws background via styledElement, inset by margin" $ do
      ctx <- snd <$> runUI (renderControl []) seedCtx
      getDrawCommands ctx `shouldContain` [FillRect (insetRect (uniform 10) testBounds) testColour]

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
      result <- runInteractions testBounds seedCtx (control ElemA (focusOnClick NoFocus : attrs)) [] [ClickAt onA, Wait 1]
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
