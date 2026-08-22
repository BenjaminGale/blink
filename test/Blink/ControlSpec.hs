{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Blink.ControlSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Attributes
  ( Attr, HasControlConfig (..), ControlConfig (..), FocusOnClick (..), NavigationMode (..)
  , configure, defaultControlConfig
  )
import Blink.Control (control)
import Blink.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Element (ElementEvent, onFocusGained, onFocusLost, onKeyPressed)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..), Key (..), Modifier (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement
  = ElemA | ElemB
  | Container | ChildA | ChildB | ChildC | Sibling
  | Outer | Inner | InnerA | InnerB | OuterChildB | OuterSibling
  deriving (Eq, Ord, Show)

-- | The shared config type every test control uses -- just a
-- 'ControlConfig' wrapper, since 'control' needs a resolved @cfg@ to read
-- 'ccIsTabStop'\/'ccFocusOnClick' off.
newtype TestConfig e = TestConfig { testControlConfig :: ControlConfig e }

defaultTestConfig :: TestConfig e
defaultTestConfig = TestConfig defaultControlConfig

instance HasControlConfig e (TestConfig e) where
  controlConfig    = testControlConfig
  setControlConfig cc c = c { testControlConfig = cc }

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

type Attr' = Attr TestElement ElementEvent String (TestConfig TestElement)

-- | Renders 'ElemA' at 'rectA' and 'ElemB' at 'rectB' with the given
-- per-element config and attrs.
both :: TestConfig TestElement -> [Attr'] -> TestConfig TestElement -> [Attr'] -> UI TestElement String ()
both cfgA attrsA cfgB attrsB = do
  withBounds rectA (control ElemA cfgA attrsA (pure ()))
  withBounds rectB (control ElemB cfgB attrsB (pure ()))

-- | Renders a single 'ElemA' at 'testBounds', resolving its @cfg@ from
-- @attrs@ the same way every real widget built on 'control' does --
-- @attrs@ alone (e.g. 'isTabStop') is enough to configure it.
renderControl :: [Attr'] -> UI TestElement String ()
renderControl attrs = control ElemA cfg attrs (pure ())
  where
    cfg = configure defaultTestConfig attrs

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer
  where
    noInput = InputState (Point 200 200) False [] []

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

-- | Tags an element's own focus events with its name -- these tests are
-- keyboard-only, so every element here shares the same bounds; position
-- never matters.
tag :: TestElement -> [Attr']
tag e =
  [ onFocusGained (const [OutMsg (show e ++ " gained")])
  , onFocusLost   (const [OutMsg (show e ++ " lost")])
  ]

containedCfg :: TestConfig TestElement
containedCfg = TestConfig defaultControlConfig { ccTabNavigation = Contained }

-- | 'Container' (Contained) holding 'ChildA'\/'ChildB'\/'ChildC', followed
-- by a plain 'Sibling' -- the basic shape for testing Tab\/Shift-Tab
-- containment and cycling, and Ctrl+Tab\/Ctrl+Shift+Tab escaping to the
-- next\/previous thing at the enclosing level.
containedTree :: UI TestElement String ()
containedTree = do
  control Container containedCfg (tag Container) $
    mapM_ (\c -> control c defaultTestConfig (tag c) (pure ())) [ChildA, ChildB, ChildC]
  control Sibling defaultTestConfig (tag Sibling) (pure ())

-- | Same shape as 'containedTree', but with arrow-key cycling also enabled.
arrowTree :: UI TestElement String ()
arrowTree = control Container arrowCfg (tag Container) $
  mapM_ (\c -> control c defaultTestConfig (tag c) (pure ())) [ChildA, ChildB, ChildC]
  where
    arrowCfg = TestConfig defaultControlConfig { ccTabNavigation = Contained, ccArrowNavigation = True }

-- | 'Outer' (Contained) holds a nested 'Inner' (also Contained, with its
-- own children 'InnerA'\/'InnerB') plus a plain 'OuterChildB', followed by
-- a root-level 'OuterSibling' -- for testing that Ctrl+Tab from deep
-- inside only escapes the nearest scope, landing on the next thing within
-- the enclosing one rather than jumping all the way out.
nestedTree :: UI TestElement String ()
nestedTree = do
  control Outer containedCfg (tag Outer) $ do
    control Inner containedCfg (tag Inner) $
      mapM_ (\c -> control c defaultTestConfig (tag c) (pure ())) [InnerA, InnerB]
    control OuterChildB defaultTestConfig (tag OuterChildB) (pure ())
  control OuterSibling defaultTestConfig (tag OuterSibling) (pure ())

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
      result <- runInteractions testBounds seedCtx (both defaultTestConfig attrsA defaultTestConfig attrsB) [] []
      resultMessages result `shouldBe` ["A gained"]

  describe "click-to-focus" $ do
    let attrsA = [onFocusLost   (const [OutMsg ("A lost"   :: String)])]
        attrsB = [onFocusGained (const [OutMsg ("B gained" :: String)])]
        render = both defaultTestConfig attrsA defaultTestConfig attrsB

    it "does not take effect on the click's own frame" $ do
      result <- runInteractions testBounds seedCtx render [] [ClickAt onB]
      resultMessages result `shouldBe` []

    it "takes effect one frame after the click, firing FocusLost/FocusGained for the right elements" $ do
      result <- runInteractions testBounds seedCtx render [] [ClickAt onB, Wait 1]
      resultMessages result `shouldBe` ["A lost", "B gained"]

    it "FocusTarget redirects focus to the named element instead of the clicker" $ do
      let cfgA = TestConfig defaultControlConfig { ccIsTabStop = False, ccFocusOnClick = FocusTarget ElemB }
          cfgB = TestConfig defaultControlConfig { ccIsTabStop = False }
          taggedA = [onFocusGained (const [OutMsg ("A gained" :: String)])]
          taggedB = [onFocusGained (const [OutMsg ("B gained" :: String)])]
      result <- runInteractions testBounds seedCtx (both cfgA taggedA cfgB taggedB) [] [ClickAt onA, Wait 1]
      resultMessages result `shouldBe` ["B gained"]

    it "NoFocus leaves focus unchanged when clicked" $ do
      let cfgA = TestConfig defaultControlConfig { ccIsTabStop = False, ccFocusOnClick = NoFocus }
          attrs :: [Attr']
          attrs = [onFocusGained (const [OutMsg ("gained" :: String)])]
      result <- runInteractions testBounds seedCtx (control ElemA cfgA attrs (pure ())) [] [ClickAt onA, Wait 1]
      resultMessages result `shouldBe` []

  describe "keyboard navigation" $ do
    let attrsA = [onFocusLost   (const [OutMsg ("A lost"   :: String)])]
        attrsB = [onFocusGained (const [OutMsg ("B gained" :: String)])]
        render = both defaultTestConfig attrsA defaultTestConfig attrsB

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
      result <- runInteractions testBounds seedCtx (both defaultTestConfig keyAttrs defaultTestConfig []) [Wait 1] [Tab]
      resultMessages result `shouldBe` []

    it "does not report Shift-Tab as a key event to the control it moves focus away from" $ do
      let keyAttrs = [onKeyPressed (\k -> [OutMsg (show k)])]
      result <- runInteractions testBounds seedCtx (both defaultTestConfig keyAttrs defaultTestConfig []) [Wait 1] [ShiftTab]
      resultMessages result `shouldBe` []

  describe "container navigation (tabNavigation Contained)" $ do
    it "entering a Contained container immediately focuses its first child too, in the same press" $ do
      result <- runInteractions testBounds seedCtx containedTree [] []
      resultMessages result `shouldBe` ["Container gained", "ChildA gained"]

    it "Tab cycles forward through the container's children without escaping" $ do
      result <- runInteractions testBounds seedCtx containedTree [Wait 1] [Tab]
      resultMessages result `shouldBe` ["ChildA lost", "ChildB gained"]

    it "Tab wraps from the last child back to the first, never escaping" $ do
      -- The last child giving up focus doesn't let anything claim it on
      -- that same press (nothing renders after it that frame, same as
      -- plain Tab from the last of several root-level controls) -- the
      -- wrap to the first only shows up on the next frame.
      result <- runInteractions testBounds seedCtx containedTree [Wait 1] [Tab, Tab, Tab, Wait 1]
      resultMessages result `shouldBe`
        [ "ChildA lost", "ChildB gained"
        , "ChildB lost", "ChildC gained"
        , "ChildC lost"
        , "ChildA gained"
        ]

    it "Shift-Tab moves backward, wrapping to the last child, one frame later" $ do
      result <- runInteractions testBounds seedCtx containedTree [Wait 1] [ShiftTab, Wait 1]
      resultMessages result `shouldBe` ["ChildA lost", "ChildC gained"]

    it "does not fire the container's own FocusLost/FocusGained when focus just moves among its children" $ do
      result <- runInteractions testBounds seedCtx containedTree [Wait 1] [Tab, Tab]
      resultMessages result `shouldNotContain` ["Container lost"]
      resultMessages result `shouldNotContain` ["Container gained"]

    it "arrow keys do nothing within the container by default" $ do
      result <- runInteractions testBounds seedCtx containedTree [Wait 1] [PressKey KeyDown []]
      resultMessages result `shouldBe` []

    it "arrow keys also cycle within the container when arrowNavigation is enabled" $ do
      result <- runInteractions testBounds seedCtx arrowTree [Wait 1] [PressKey KeyDown []]
      resultMessages result `shouldBe` ["ChildA lost", "ChildB gained"]

    it "Ctrl+Tab escapes the container entirely to the next sibling, regardless of which child is focused" $ do
      result <- runInteractions testBounds seedCtx containedTree [Wait 1, Tab] [PressKey KeyTab [Ctrl]]
      resultMessages result `shouldBe` ["Container lost", "Sibling gained"]

    it "Ctrl+Shift+Tab escapes the container to the previous sibling, one frame later" $ do
      result <- runInteractions testBounds seedCtx containedTree [Wait 1, Tab] [PressKey KeyTab [Ctrl, Shift], Wait 1]
      resultMessages result `shouldBe` ["Container lost", "Sibling gained"]

    it "Ctrl+Tab from a nested Contained container only escapes the nearest scope, not outer ones" $ do
      result <- runInteractions testBounds seedCtx nestedTree [Wait 1] [PressKey KeyTab [Ctrl]]
      resultMessages result `shouldBe` ["Inner lost", "OuterChildB gained"]

    it "has no effect when the container isn't a tab stop -- its children participate directly in the enclosing sequence" $ do
      let notTabStopCfg = TestConfig defaultControlConfig { ccIsTabStop = False, ccTabNavigation = Contained }
          tree = do
            control Container notTabStopCfg (tag Container) $
              mapM_ (\c -> control c defaultTestConfig (tag c) (pure ())) [ChildA, ChildB]
            control Sibling defaultTestConfig (tag Sibling) (pure ())
      entry <- runInteractions testBounds seedCtx tree [] []
      resultMessages entry `shouldBe` ["ChildA gained"]
      result <- runInteractions testBounds seedCtx tree [Wait 1] [Tab, Tab]
      resultMessages result `shouldBe` ["ChildA lost", "ChildB gained", "ChildB lost", "Sibling gained"]
