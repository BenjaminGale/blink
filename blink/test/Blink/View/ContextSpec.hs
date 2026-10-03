module Blink.View.ContextSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Geometry (Rectangle (..))
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Rendering (Colour (..))
import Blink.Style (Style (..), StyleKey (..), StyleSet (..), Theme (..), VisualState (..))
import Blink.View
import Blink.View.Fixtures

spec :: Spec
spec = describe "Blink.View.Context" $ do
  describe "withBounds" $ do
    it "replaces the current bounds inside the sub-tree" $ do
      let inner = Rectangle 10 10 50 50
      (b, _) <- run0 (withBounds inner getBounds)
      b `shouldBe` inner

    it "restores the outer bounds after the sub-tree completes" $ do
      (b, _) <- run0 (withBounds (Rectangle 10 10 50 50) (pure ()) >> getBounds)
      b `shouldBe` testBounds

  describe "getWindowSize" $ do
    it "returns the window rectangle passed to emptyViewContext" $ do
      (w, _) <- run0 getWindowSize
      w `shouldBe` testBounds

    it "is unaffected by withBounds narrowing the current bounds" $ do
      (w, _) <- run0 (withBounds (Rectangle 10 10 50 50) getWindowSize)
      w `shouldBe` testBounds

    it "reflects the new window rectangle after nextFrameContext advances it" $ do
      (_, ctx) <- run0 (pure ())
      let resized = Rectangle 0 0 200 150
          ctx'    = nextFrameContext resized noInput (contextTheme ctx) (contextAnimation ctx) ctx
      (w, _) <- runView getWindowSize ctx'
      w `shouldBe` resized

  it "getMessages returns emitted messages in emit order" $ do
    (_, ctx) <- run0 (emit (1 :: Int) >> emit 2)
    getMessages ctx `shouldBe` [1, 2]

  it "getMessages returns [] when nothing was emitted" $ do
    (_, ctx) <- run0 (pure ())
    getMessages ctx `shouldBe` []

  it "nextFrameContext clears queued messages" $ do
    (_, ctx) <- run0 (emit (1 :: Int))
    let ctx' = advance noInput ctx
    getMessages ctx' `shouldBe` []

  describe "disableWhen" $ do
    it "isDisabled is False by default" $ do
      (b, _) <- run0 isDisabled
      b `shouldBe` False

    it "isDisabled is True inside disableWhen True" $ do
      (b, _) <- run0 (disableWhen True isDisabled)
      b `shouldBe` True

    it "isDisabled is False inside disableWhen False" $ do
      (b, _) <- run0 (disableWhen False isDisabled)
      b `shouldBe` False

    it "restores the disabled flag to False after the sub-tree completes" $ do
      (b, _) <- run0 (disableWhen True (pure ()) >> isDisabled)
      b `shouldBe` False

    it "whenEnabled skips its body when the sub-tree is disabled" $ do
      (_, ctx) <- run0 (disableWhen True (whenEnabled (emit 1)))
      getMessages ctx `shouldBe` []

    it "whenEnabled runs its body when the sub-tree is enabled" $ do
      (_, ctx) <- run0 (whenEnabled (emit 1))
      getMessages ctx `shouldBe` [1]

  describe "keyboard" $ do
    describe "consumeKey" $ do
      it "removes all events for the given key from the queue" $ do
        let input = noInput { inputKeyEvents = [ KeyEvent KeyTab [] False, KeyEvent KeyTab [] False ] }
        (remaining, _) <- runWith input (consumeKey KeyTab >> getInput)
        inputKeyEvents remaining `shouldBe` []

      it "leaves events for other keys in the queue" $ do
        let tabEv    = KeyEvent KeyTab [] False
            returnEv = KeyEvent KeyReturn [] False
            input    = noInput { inputKeyEvents = [tabEv, returnEv] }
        (remaining, _) <- runWith input (consumeKey KeyTab >> getInput)
        inputKeyEvents remaining `shouldBe` [returnEv]

    describe "withoutKeyEvents" $ do
      it "hides the given keys from the wrapped action" $ do
        let tabEv    = KeyEvent KeyTab [] False
            returnEv = KeyEvent KeyReturn [] False
            input    = noInput { inputKeyEvents = [tabEv, returnEv] }
        (seen, _) <- runWith input (withoutKeyEvents [(KeyTab, [])] getInput)
        inputKeyEvents seen `shouldBe` [returnEv]

      it "restores the hidden keys once the wrapped action completes" $ do
        let tabEv    = KeyEvent KeyTab [] False
            returnEv = KeyEvent KeyReturn [] False
            input    = noInput { inputKeyEvents = [tabEv, returnEv] }
        (_, ctx) <- runWith input (withoutKeyEvents [(KeyTab, [])] (pure ()))
        inputKeyEvents (contextInput ctx) `shouldBe` [tabEv, returnEv]

      it "keeps a key consumed inside the scope consumed after it exits" $ do
        let tabEv    = KeyEvent KeyTab [] False
            returnEv = KeyEvent KeyReturn [] False
            input    = noInput { inputKeyEvents = [tabEv, returnEv] }
        (_, ctx) <- runWith input (withoutKeyEvents [(KeyTab, [])] (consumeKey KeyReturn))
        inputKeyEvents (contextInput ctx) `shouldBe` [tabEv]

  describe "styles" $ do
    let distinctStyles = StyleSet
          { styleBase = emptyStyle { styleBackground = RGBA 0 0 0 1 }
          , styleOverrides = Map.fromList
              [ (CommonMouseOver, \s -> s { styleBackground = RGBA 1 0 0 1 })
              , (CommonPressed,   \s -> s { styleBackground = RGBA 0 1 0 1 })
              , (FocusFocused,    \s -> s { styleBackground = RGBA 0 0 1 1 })
              , (CommonDisabled,  \s -> s { styleBackground = RGBA 1 1 1 1 })
              ]
          }
        styledTheme = Theme
          { themeElementStyles = Map.singleton (ElementId ()) (emptyMetrics, distinctStyles)
          , themeDefaultStyle  = (emptyMetrics, emptyStyleSet)
          }
        runStyled ui = runView ui (emptyViewContext testBounds noInput styledTheme)

    describe "getStyleSet" $ do
      it "returns the element-specific style when registered" $ do
        ((_, ss), _) <- runStyled (getStyleSet (ElementId ()))
        styleBackground (styleBase ss) `shouldBe` RGBA 0 0 0 1

      it "falls back to the theme default when no element-specific style is registered" $ do
        ((_, ss), _) <- run0 (getStyleSet (ElementId ()))
        styleBackground (styleBase ss) `shouldBe` styleBackground (styleBase emptyStyleSet)
