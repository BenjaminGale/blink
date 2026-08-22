{-# LANGUAGE OverloadedStrings #-}
module Blink.InteractionSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Geometry (Point (..), Rectangle (..), uniform, noBorder)
import Blink.Input (InputState (..), Key (..), KeyEvent (..), Modifier (..))
import Blink.Interaction
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

emptyStyle :: Style
emptyStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignCenter
  , styleMargin       = uniform 0
  , stylePadding      = uniform 0
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

testTheme :: Theme ()
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = testStyleSet }
  where
    testStyleSet = StyleSet emptyStyle emptyStyle emptyStyle emptyStyle emptyStyle

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

seedAt :: Point -> UIContext () msg
seedAt p = emptyUIContext testBounds (InputState p False [] []) testTheme noOpTextMeasurer

seedAt0 :: UIContext () msg
seedAt0 = seedAt (Point 0 0)

-- | Records the frame's raw input as a message every time it runs — lets
-- these tests observe exactly which frames 'runInteractions' drove, and
-- with what input, purely through the public 'resultMessages' output.
probe :: UI () InputState ()
probe = getInput >>= emit

-- | Emits a fixed value every frame it runs, for tests only interested in
-- how many frames ran and when.
tick :: UI () () ()
tick = emit ()

-- | Reports the most recent focus change still visible this frame, if any.
probeFocusChange :: UI () (Maybe (FocusChange ())) ()
probeFocusChange = getFocusChange >>= emit

posDown :: InputState -> (Point, Bool)
posDown i = (inputMousePosition i, inputLeftButtonDown i)

spec :: Spec
spec = describe "Blink.Interaction" $ do
  describe "Click / ClickAt" $ do
    it "Click presses then releases at the current tracked position" $ do
      result <- runInteractions testBounds (seedAt (Point 50 50)) probe [] [Click]
      map posDown (resultMessages result) `shouldBe` [(Point 50 50, True), (Point 50 50, False)]

    it "ClickAt moves to the given point for both the press and release frame" $ do
      result <- runInteractions testBounds seedAt0 probe [] [ClickAt (Point 30 40)]
      map posDown (resultMessages result) `shouldBe` [(Point 30 40, True), (Point 30 40, False)]

  describe "DragTo" $ do
    it "keeps the button down and only changes position after MouseDown" $ do
      result <- runInteractions testBounds seedAt0 probe []
                  [MouseDown (Point 10 10), DragTo (Point 90 90)]
      map posDown (resultMessages result) `shouldBe` [(Point 10 10, True), (Point 90 90, True)]

  describe "Tab / ShiftTab" $ do
    it "Tab produces a single frame with an unmodified KeyTab event" $ do
      result <- runInteractions testBounds seedAt0 probe [] [Tab]
      map inputKeyEvents (resultMessages result) `shouldBe` [[KeyEvent KeyTab []]]

    it "ShiftTab produces a single frame with a Shift-modified KeyTab event" $ do
      result <- runInteractions testBounds seedAt0 probe [] [ShiftTab]
      map inputKeyEvents (resultMessages result) `shouldBe` [[KeyEvent KeyTab [Shift]]]

  describe "key events and typed text do not leak between frames" $ do
    it "a PressKey's KeyEvent is gone on the following frame" $ do
      result <- runInteractions testBounds seedAt0 probe [] [PressKey KeyReturn [], Wait 1]
      map inputKeyEvents (resultMessages result) `shouldBe` [[KeyEvent KeyReturn []], []]

    it "a TypeText's text is gone on the following frame" $ do
      result <- runInteractions testBounds seedAt0 probe [] [TypeText "hi", Wait 1]
      map inputTypedText (resultMessages result) `shouldBe` [["hi"], []]

  describe "setup vs test phase" $ do
    it "discards messages emitted during the setup phase" $ do
      result <- runInteractions testBounds seedAt0 tick [Wait 3] [Wait 1]
      resultMessages result `shouldBe` [()]

    it "accumulates messages across every test-phase frame, not just the last" $ do
      result <- runInteractions testBounds seedAt0 tick [] [Wait 1, Wait 1, Wait 1]
      resultMessages result `shouldBe` [(), (), ()]

    it "still runs the action once when the test interaction list is empty" $ do
      result <- runInteractions testBounds seedAt0 tick [] []
      resultMessages result `shouldBe` [()]

  describe "auto-settle" $ do
    it "makes a deferred ScrollTo visible immediately in resultContext" $ do
      result <- runInteractions testBounds seedAt0 (emitUi (ScrollTo () 0.5)) [] []
      contextScrollPosition () (resultContext result) `shouldBe` 0.5

  describe "chaining two runInteractions calls via resultContext" $ do
    -- 'resultContext' settles (applies) its queued effects but doesn't
    -- clear them, so a second 'runInteractions' call seeded from it
    -- re-applies the same effect again via its own opening frame. Harmless
    -- for an idempotent effect like ScrollTo (re-setting the same absolute
    -- position changes nothing) -- but a focus change's "from" is
    -- recomputed fresh at apply time, so a second application reports it
    -- coming from wherever it already ended up, not where it actually
    -- started.
    it "still reports the same scroll position after being carried into a second call" $ do
      seeded <- runInteractions testBounds seedAt0 (emitUi (ScrollTo () 0.5)) [] []
      result <- runInteractions testBounds (resultContext seeded) tick [] []
      contextScrollPosition () (resultContext result) `shouldBe` 0.5

    it "reports a focus change as coming from nowhere when carried into a second call, even though it really came from a focused element" $ do
      seeded  <- runInteractions testBounds seedAt0 (setFocus () >> requestClearFocus Nothing) [] []
      chained <- runInteractions testBounds (resultContext seeded) probeFocusChange [] []
      resultMessages chained `shouldBe` [Just (FocusChange Nothing Nothing)]

    it "reports the real origin when the change is primed and observed within one continuous call instead" $ do
      result <- runInteractions testBounds seedAt0
                  (setFocus () >> requestClearFocus Nothing >> probeFocusChange)
                  [Wait 1] []
      resultMessages result `shouldBe` [Just (FocusChange (Just ()) Nothing)]
