module Blink.MouseSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Mouse

data Elem = ElemA | ElemB deriving (Eq, Ord, Show)

spec :: Spec
spec = do
  describe "nextButtonState" $ do
    it "goes to ButtonDown on the down edge" $
      nextButtonState False True MouseNotCaptured `shouldBe` ButtonDown (MouseNotCaptured :: MouseCapture Elem)

    it "carries capture into ButtonDown on the down edge" $
      nextButtonState False True (MouseCapturedBy ElemA) `shouldBe` ButtonDown (MouseCapturedBy ElemA)

    it "goes from down to held while still held" $
      nextButtonState True True (MouseCapturedBy ElemA) `shouldBe` ButtonHeld (MouseCapturedBy ElemA)

    it "goes to ButtonReleased on the up edge" $
      nextButtonState True False (MouseCapturedBy ElemA) `shouldBe` ButtonReleased (MouseCapturedBy ElemA)

    it "goes from released to up once the button has been up for a frame" $
      nextButtonState False False MouseNotCaptured `shouldBe` (ButtonUp :: ButtonState Elem)

  describe "captureOf" $ do
    it "is MouseNotCaptured for ButtonUp" $
      captureOf (ButtonUp :: ButtonState Elem) `shouldBe` MouseNotCaptured

    it "recovers the capture carried by ButtonDown/Held/Released" $ do
      captureOf (ButtonDown (MouseCapturedBy ElemA)) `shouldBe` MouseCapturedBy ElemA
      captureOf (ButtonHeld (MouseCapturedBy ElemA)) `shouldBe` MouseCapturedBy ElemA
      captureOf (ButtonReleased (MouseCapturedBy ElemA)) `shouldBe` MouseCapturedBy ElemA

  describe "nextHoverState" $ do
    it "goes to Entered on the enter edge" $
      nextHoverState NotOver True `shouldBe` Entered

    it "goes from Entered to Over while still hit" $
      nextHoverState Entered True `shouldBe` Over

    it "goes to Exited on the exit edge" $
      nextHoverState Over False `shouldBe` Exited

    it "goes from Exited to NotOver once not hit for a frame" $
      nextHoverState Exited False `shouldBe` NotOver

    it "stays NotOver when never hit" $
      nextHoverState NotOver False `shouldBe` NotOver

    it "stays Over while continuously hit" $
      nextHoverState Over True `shouldBe` Over

  describe "wasHit" $
    it "is true only for Entered and Over" $ do
      wasHit Entered `shouldBe` True
      wasHit Over `shouldBe` True
      wasHit NotOver `shouldBe` False
      wasHit Exited `shouldBe` False

  describe "advanceMouse" $ do
    it "starts empty with no button held and nothing hovered" $ do
      mouseButton (emptyMouse :: Mouse Elem) `shouldBe` ButtonUp
      mouseHoverPrev (emptyMouse :: Mouse Elem) `shouldBe` Map.empty

    it "advances the button state alongside hover" $ do
      let mouse0 = emptyMouse { mouseHoverNext = Map.fromList [(ElemA, Entered)] }
          mouse1 = advanceMouse False True mouse0
      mouseButton mouse1 `shouldBe` ButtonDown MouseNotCaptured
      mouseHoverPrev mouse1 `shouldBe` Map.fromList [(ElemA, Entered)]
      mouseHoverNext mouse1 `shouldBe` Map.empty

    it "drops a hover entry that wasn't touched this frame, so a stale state isn't resumed" $ do
      -- ElemA was Over last frame (in mouseHoverPrev) but wasn't visited
      -- this frame, so mouseHoverNext never got an entry for it. Advancing
      -- should drop it rather than carry the stale Over forward: the next
      -- time ElemA is visited, a fresh Map.findWithDefault NotOver lookup
      -- (as 'Blink.Element' will do) correctly starts it from NotOver
      -- instead of resuming as if it had been continuously hovered.
      let mouse0 = emptyMouse
            { mouseButton    = ButtonUp
            , mouseHoverPrev = Map.fromList [(ElemA, Over)]
            , mouseHoverNext = Map.empty
            }
          mouse1 = advanceMouse False False mouse0
      mouseHoverPrev mouse1 `shouldBe` Map.empty

    it "only carries forward hover entries touched this frame" $ do
      let mouse0 = emptyMouse
            { mouseHoverPrev = Map.fromList [(ElemA, Over), (ElemB, Over)]
            , mouseHoverNext = Map.fromList [(ElemA, Over)]
            }
          mouse1 = advanceMouse False False mouse0
      mouseHoverPrev mouse1 `shouldBe` Map.fromList [(ElemA, Over)]
