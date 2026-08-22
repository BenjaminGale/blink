module Blink.MouseSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Mouse

data Elem = ElemA | ElemB deriving (Eq, Ord, Show)

spec :: Spec
spec = do
  describe "nextButtonState" $ do
    it "reports the button as just pressed the first frame it's held" $
      nextButtonState False True MouseNotCaptured `shouldBe` ButtonDown (MouseNotCaptured :: MouseCapture Elem)

    it "never lets a new press inherit capture left over from a previous one" $
      nextButtonState False True (MouseCapturedBy ElemA) `shouldBe` ButtonDown (MouseNotCaptured :: MouseCapture Elem)

    it "no longer reports a fresh press once the button has been held more than one frame" $
      nextButtonState True True (MouseCapturedBy ElemA) `shouldBe` ButtonHeld (MouseCapturedBy ElemA)

    it "reports the button as just released the frame it comes up" $
      nextButtonState True False (MouseCapturedBy ElemA) `shouldBe` ButtonReleased (MouseCapturedBy ElemA)

    it "no longer reports a release once the button has been up more than one frame" $
      nextButtonState False False MouseNotCaptured `shouldBe` (ButtonUp :: ButtonState Elem)

  describe "captureOf" $ do
    it "reports no capture while the button isn't held" $
      captureOf (ButtonUp :: ButtonState Elem) `shouldBe` MouseNotCaptured

    it "reports whichever element is holding capture while the button is pressed, held, or just released" $ do
      captureOf (ButtonDown (MouseCapturedBy ElemA)) `shouldBe` MouseCapturedBy ElemA
      captureOf (ButtonHeld (MouseCapturedBy ElemA)) `shouldBe` MouseCapturedBy ElemA
      captureOf (ButtonReleased (MouseCapturedBy ElemA)) `shouldBe` MouseCapturedBy ElemA

  describe "nextHoverState" $ do
    it "reports an element as just entered the first frame it's hit" $
      nextHoverState NotOver True `shouldBe` Entered

    it "no longer reports a fresh entry once an element has been hit more than one frame" $
      nextHoverState Entered True `shouldBe` Over

    it "reports an element as just exited the frame it stops being hit" $
      nextHoverState Over False `shouldBe` Exited

    it "no longer reports an exit once an element has been un-hit more than one frame" $
      nextHoverState Exited False `shouldBe` NotOver

    it "keeps reporting an element as not hovered while it's never hit" $
      nextHoverState NotOver False `shouldBe` NotOver

    it "keeps reporting an element as hovered while it stays hit" $
      nextHoverState Over True `shouldBe` Over

  describe "wasHit" $
    it "treats only a freshly-entered or continuously-hovered element as currently hit" $ do
      wasHit Entered `shouldBe` True
      wasHit Over `shouldBe` True
      wasHit NotOver `shouldBe` False
      wasHit Exited `shouldBe` False

  describe "advanceMouse" $ do
    it "starts with nothing held and nothing hovered" $ do
      mouseButton (emptyMouse :: Mouse Elem) `shouldBe` ButtonUp
      mouseHoverPrev (emptyMouse :: Mouse Elem) `shouldBe` Map.empty

    it "advances the button and hover together in one step" $ do
      let mouse0 = emptyMouse { mouseHoverNext = Map.fromList [(ElemA, Entered)] }
          mouse1 = advanceMouse False True mouse0
      mouseButton mouse1 `shouldBe` ButtonDown MouseNotCaptured
      mouseHoverPrev mouse1 `shouldBe` Map.fromList [(ElemA, Entered)]
      mouseHoverNext mouse1 `shouldBe` Map.empty

    it "forgets an element's hover once it stops being visited, rather than resuming it as still hovered" $ do
      -- ElemA was hovered last frame but wasn't visited this frame at all
      -- (e.g. it stopped being rendered). Advancing should forget it rather
      -- than carry the old hovered reading forward: the next time ElemA is
      -- visited, it should be treated as freshly entering rather than as
      -- having been continuously hovered the whole time.
      let mouse0 = emptyMouse
            { mouseButton    = ButtonUp
            , mouseHoverPrev = Map.fromList [(ElemA, Over)]
            , mouseHoverNext = Map.empty
            }
          mouse1 = advanceMouse False False mouse0
      mouseHoverPrev mouse1 `shouldBe` Map.empty

    it "treats each element's hover independently, so one dropping out doesn't affect another still being visited" $ do
      let mouse0 = emptyMouse
            { mouseHoverPrev = Map.fromList [(ElemA, Over), (ElemB, Over)]
            , mouseHoverNext = Map.fromList [(ElemA, Over)]
            }
          mouse1 = advanceMouse False False mouse0
      mouseHoverPrev mouse1 `shouldBe` Map.fromList [(ElemA, Over)]
