module Blink.LayoutSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Geometry (Alignment (..), Point (..), Rectangle (..), uniform)
import Blink.Input (ButtonState (..), KeyEvent, InputState (..))
import Blink.Layout
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

-- Test infrastructure

noInput :: InputState
noInput = InputState
  { mousePosition = Point 0 0
  , leftButton    = ButtonUp
  , keyEvents     = ([] :: [KeyEvent])
  }

emptyStyle :: Style
emptyStyle = Style
  { background   = RGBA 0 0 0 1
  , textColour   = RGBA 0 0 0 1
  , textAlign    = AlignCenter
  , margin       = uniform 0
  , padding      = uniform 0
  , borderColour = Nothing
  , borderWidth  = 0
  }

emptyStyleSet :: StyleSet
emptyStyleSet = StyleSet
  { normal   = emptyStyle
  , hovered  = emptyStyle
  , pressed  = emptyStyle
  , focused  = emptyStyle
  , disabled = emptyStyle
  }

emptyTheme :: Theme ()
emptyTheme = Theme
  { elementStyles = Map.empty
  , defaultStyle  = emptyStyleSet
  }

testColour :: Colour
testColour = RGBA 0 0 0 1

fill :: UI () () ()
fill = fillRect testColour

runLayout :: Rectangle -> UI () () () -> [Rectangle]
runLayout bounds ui =
  let ctx        = emptyUIContext bounds noInput emptyTheme
      (_, ctx')  = runUI ui ctx
  in [r | FillRect r _ <- getDrawCommands ctx']

-- hBox / vBox helpers

cfg :: BoxConfig
cfg = BoxConfig { boxSpacing = 0, boxMargin = 0, boxAlignment = TopLeft, boxFillCross = True }

runHBox :: Rectangle -> BoxConfig -> [RectConstraint] -> [Rectangle]
runHBox bounds c rcs = runLayout bounds $ hBox c [(r, fill) | r <- rcs]

runVBox :: Rectangle -> BoxConfig -> [RectConstraint] -> [Rectangle]
runVBox bounds c rcs = runLayout bounds $ vBox c [(r, fill) | r <- rcs]

rc :: Constraint -> Constraint -> Alignment -> RectConstraint
rc = RectConstraint

hBounds :: Rectangle
hBounds = Rectangle 0 0 200 100

vBounds :: Rectangle
vBounds = Rectangle 0 0 100 200

s :: Constraint -> SumConstraint
s = SumConstraint

m :: Constraint -> MaxConstraint
m = MaxConstraint

spec :: Spec
spec = describe "layout" $ do

  describe "SumConstraint" $ do
    describe "mempty" $ do
      let cases =
            [ ("Exactly", Exactly 50)
            , ("Fill",    Fill)
            , ("AtLeast", AtLeast 30)
            , ("AtMost",  AtMost 60)
            , ("Between", Between 20 70)
            ]
      forM_ cases $ \(desc, c) -> do
        it ("mempty <> " ++ desc ++ " = identity") $
          (mempty <> s c) `shouldBe` s c
        it (desc ++ " <> mempty = identity") $
          (s c <> mempty) `shouldBe` s c

    describe "(<>)" $ do
      let cases =
            [ ( "Exactly + Exactly sums fixed sizes"
              , Exactly 50,     Exactly 80,     Exactly 130 )
            , ( "Exactly + Fill produces AtLeast with the fixed minimum"
              , Exactly 50,     Fill,            AtLeast 50 )
            , ( "Exactly + AtLeast sums minimums, remains unbounded"
              , Exactly 50,     AtLeast 30,      AtLeast 80 )
            , ( "Exactly + AtMost produces a Between range"
              , Exactly 50,     AtMost 60,       Between 50 110 )
            , ( "Exactly + Between sums both bounds"
              , Exactly 50,     Between 20 70,   Between 70 120 )
            , ( "Fill + Fill stays Fill"
              , Fill,           Fill,            Fill )
            , ( "Fill + AtLeast carries the AtLeast minimum"
              , Fill,           AtLeast 30,      AtLeast 30 )
            , ( "Fill + AtMost stays Fill (no minimum)"
              , Fill,           AtMost 60,       Fill )
            , ( "Fill + Between carries the Between minimum"
              , Fill,           Between 20 70,   AtLeast 20 )
            , ( "AtLeast + AtLeast sums minimums"
              , AtLeast 30,     AtLeast 40,      AtLeast 70 )
            , ( "AtLeast + AtMost: unbounded dominates"
              , AtLeast 30,     AtMost 60,       AtLeast 30 )
            , ( "AtLeast + Between sums minimums, remains unbounded"
              , AtLeast 30,     Between 20 70,   AtLeast 50 )
            , ( "AtMost + AtMost sums maximums"
              , AtMost 60,      AtMost 40,       AtMost 100 )
            , ( "AtMost + Between: minimum from Between, maximums sum"
              , AtMost 60,      Between 20 70,   Between 20 130 )
            , ( "Between + Between sums both bounds"
              , Between 20 70,  Between 10 50,   Between 30 120 )
            ]
      forM_ cases $ \(desc, a, b, expected) ->
        it desc $
          s a <> s b `shouldBe` s expected

  describe "MaxConstraint" $ do
    describe "mempty" $ do
      let cases =
            [ ("Exactly", Exactly 50)
            , ("Fill",    Fill)
            , ("AtLeast", AtLeast 30)
            , ("AtMost",  AtMost 60)
            , ("Between", Between 20 70)
            ]
      forM_ cases $ \(desc, c) -> do
        it ("mempty <> " ++ desc ++ " = identity") $
          (mempty <> m c) `shouldBe` m c
        it (desc ++ " <> mempty = identity") $
          (m c <> mempty) `shouldBe` m c

    describe "(<>)" $ do
      let cases =
            [ ( "Exactly + Exactly takes the larger size"
              , Exactly 50,     Exactly 80,     Exactly 80 )
            , ( "Exactly + Fill produces AtLeast with the fixed minimum"
              , Exactly 50,     Fill,            AtLeast 50 )
            , ( "Exactly + AtLeast: larger minimum wins, remains unbounded"
              , Exactly 50,     AtLeast 30,      AtLeast 50 )
            , ( "Exactly + AtMost: Exactly becomes the floor"
              , Exactly 50,     AtMost 60,       Between 50 60 )
            , ( "Exactly + Between: Exactly raises the floor"
              , Exactly 50,     Between 20 70,   Between 50 70 )
            , ( "Fill + Fill stays Fill"
              , Fill,           Fill,            Fill )
            , ( "Fill + AtLeast carries the AtLeast minimum"
              , Fill,           AtLeast 30,      AtLeast 30 )
            , ( "Fill + AtMost: unbounded dominates"
              , Fill,           AtMost 60,       Fill )
            , ( "Fill + Between carries the Between minimum"
              , Fill,           Between 20 70,   AtLeast 20 )
            , ( "AtLeast + AtLeast takes the larger minimum"
              , AtLeast 30,     AtLeast 40,      AtLeast 40 )
            , ( "AtLeast + AtMost: unbounded dominates"
              , AtLeast 30,     AtMost 60,       AtLeast 30 )
            , ( "AtLeast + Between: larger minimum wins, remains unbounded"
              , AtLeast 30,     Between 20 70,   AtLeast 30 )
            , ( "AtMost + AtMost takes the larger maximum"
              , AtMost 60,      AtMost 40,       AtMost 60 )
            , ( "AtMost + Between: Between's floor and larger ceiling"
              , AtMost 60,      Between 20 70,   Between 20 70 )
            , ( "Between + Between: larger floor and larger ceiling"
              , Between 20 70,  Between 10 50,   Between 20 70 )
            ]
      forM_ cases $ \(desc, a, b, expected) ->
        it desc $
          m a <> m b `shouldBe` m expected

  describe "resolveConstraint" $ do
    it "Exactly ignores available space" $
      resolveConstraint (Exactly 50) 100 `shouldBe` 50

    it "Fill uses all available space" $
      resolveConstraint Fill 100 `shouldBe` 100

    describe "AtLeast" $ do
      it "enforces the minimum size when space is insufficient" $
        resolveConstraint (AtLeast 50) 30 `shouldBe` 50

      it "grows to fill space when it exceeds the minimum" $
        resolveConstraint (AtLeast 50) 80 `shouldBe` 80

    describe "AtMost" $ do
      it "grows to fill space when within its maximum" $
        resolveConstraint (AtMost 80) 50 `shouldBe` 50

      it "limits to its maximum when space exceeds it" $
        resolveConstraint (AtMost 80) 100 `shouldBe` 80

    describe "Between" $ do
      it "enforces the minimum size when space is insufficient" $
        resolveConstraint (Between 20 80) 10 `shouldBe` 20

      it "grows to fill space when within its range" $
        resolveConstraint (Between 20 80) 50 `shouldBe` 50

      it "limits to its maximum when space exceeds it" $
        resolveConstraint (Between 20 80) 100 `shouldBe` 80

  describe "layoutWithConstraint" $ do
    let run rct = runLayout hBounds (layoutWithConstraint rct fill)

    describe "width constraints" $ do
      it "Exactly gives the child its exact width" $
        run (rc (Exactly 80) Fill TopLeft)
          `shouldBe` [Rectangle 0 0 80 100]

      it "Fill gives the child the full available width" $
        run (rc Fill Fill TopLeft)
          `shouldBe` [Rectangle 0 0 200 100]

      it "AtLeast expands to fill available space beyond the minimum" $
        run (rc (AtLeast 50) Fill TopLeft)
          `shouldBe` [Rectangle 0 0 200 100]

      it "AtMost caps the child at its maximum" $
        run (rc (AtMost 150) Fill TopLeft)
          `shouldBe` [Rectangle 0 0 150 100]

      it "Between clamps the child between its floor and ceiling" $
        run (rc (Between 50 150) Fill TopLeft)
          `shouldBe` [Rectangle 0 0 150 100]

    describe "height constraints" $ do
      let cases =
            [ ( "Exactly gives the child its exact height"
              , Exactly 40,       Rectangle 0 0 200 40  )
            , ( "Fill gives the child the full available height"
              , Fill,             Rectangle 0 0 200 100 )
            , ( "AtLeast expands to fill available space beyond the minimum"
              , AtLeast 50,       Rectangle 0 0 200 100 )
            , ( "AtMost caps the child at its maximum"
              , AtMost 80,        Rectangle 0 0 200 80  )
            , ( "Between clamps the child between its floor and ceiling"
              , Between 50 80,    Rectangle 0 0 200 80  )
            ]
      forM_ cases $ \(desc, hc, expected) ->
        it desc $
          run (rc Fill hc TopLeft) `shouldBe` [expected]

    describe "alignment" $ do
      let cases =
            [ ( "TopLeft places the child at the top-left"
              , TopLeft,    Rectangle 0  0  80 40 )
            , ( "TopCenter centres the child horizontally at the top"
              , TopCenter,  Rectangle 60 0  80 40 )
            , ( "TopRight places the child at the top-right"
              , TopRight,   Rectangle 120 0  80 40 )
            , ( "MiddleLeft places the child at the left, vertically centred"
              , MiddleLeft, Rectangle 0  30 80 40 )
            , ( "Center centres the child in both axes"
              , Center,     Rectangle 60 30 80 40 )
            , ( "MiddleRight places the child at the right, vertically centred"
              , MiddleRight, Rectangle 120 30 80 40 )
            , ( "BottomLeft places the child at the bottom-left"
              , BottomLeft,  Rectangle 0   60 80 40 )
            , ( "BottomCenter centres the child horizontally at the bottom"
              , BottomCenter, Rectangle 60  60 80 40 )
            , ( "BottomRight places the child at the bottom-right"
              , BottomRight,  Rectangle 120 60 80 40 )
            ]
      forM_ cases $ \(desc, alignment, expected) ->
        it desc $
          run (rc (Exactly 80) (Exactly 40) alignment)
            `shouldBe` [expected]

  describe "hBox" $ do
    describe "main axis (width)" $ do
      it "a single Fill child fills the available width" $
        runHBox hBounds cfg [rc Fill Fill TopLeft]
          `shouldBe` [Rectangle 0 0 200 100]

      it "two Fill children share the available width equally" $
        runHBox hBounds cfg [rc Fill Fill TopLeft, rc Fill Fill TopLeft]
          `shouldBe` [Rectangle 0 0 100 100, Rectangle 100 0 100 100]

      it "an Exactly child gets its exact width" $
        runHBox hBounds cfg [rc (Exactly 60) Fill TopLeft]
          `shouldBe` [Rectangle 0 0 60 100]

      it "a fixed child and a Fill child share the remaining space" $
        runHBox hBounds cfg [rc (Exactly 60) Fill TopLeft, rc Fill Fill TopLeft]
          `shouldBe` [Rectangle 0 0 60 100, Rectangle 60 0 140 100]

      it "spacing separates children" $
        runHBox hBounds cfg { boxSpacing = 10 } [rc Fill Fill TopLeft, rc Fill Fill TopLeft]
          `shouldBe` [Rectangle 0 0 95 100, Rectangle 105 0 95 100]

    describe "content area" $ do
      it "margin reduces the available space on all sides" $
        runHBox hBounds cfg { boxMargin = 10 } [rc Fill Fill TopLeft, rc Fill Fill TopLeft]
          `shouldBe` [Rectangle 10 10 90 80, Rectangle 100 10 90 80]

    describe "cross axis (height)" $ do
      it "fillCross = True stretches children to the full available height" $
        runHBox hBounds cfg [rc Fill (Exactly 40) TopLeft]
          `shouldBe` [Rectangle 0 0 200 100]

      let cases =
            [ ("TopLeft aligns the child to the top",    TopLeft,    Rectangle 0 0  200 40)
            , ("Center aligns the child to the middle",  Center,     Rectangle 0 30 200 40)
            , ("BottomLeft aligns the child to the bottom", BottomLeft, Rectangle 0 60 200 40)
            ]
      forM_ cases $ \(desc, alignment, expected) ->
        it desc $
          runHBox hBounds cfg { boxFillCross = False } [rc Fill (Exactly 40) alignment]
            `shouldBe` [expected]

    describe "boxAlignment" $ do
      let threeExact = [rc (Exactly 40) Fill TopLeft, rc (Exactly 40) Fill TopLeft, rc (Exactly 40) Fill TopLeft]

      it "Center centres the content block horizontally" $
        runHBox hBounds cfg { boxAlignment = Center } threeExact
          `shouldBe` [Rectangle 40 0 40 100, Rectangle 80 0 40 100, Rectangle 120 0 40 100]

      it "MiddleRight aligns the content block to the right" $
        runHBox hBounds cfg { boxAlignment = MiddleRight } threeExact
          `shouldBe` [Rectangle 80 0 40 100, Rectangle 120 0 40 100, Rectangle 160 0 40 100]

  describe "vBox" $ do
    describe "main axis (height)" $ do
      it "a single Fill child fills the available height" $
        runVBox vBounds cfg [rc Fill Fill TopLeft]
          `shouldBe` [Rectangle 0 0 100 200]

      it "two Fill children share the available height equally" $
        runVBox vBounds cfg [rc Fill Fill TopLeft, rc Fill Fill TopLeft]
          `shouldBe` [Rectangle 0 0 100 100, Rectangle 0 100 100 100]

      it "a fixed child and a Fill child share the remaining space" $
        runVBox vBounds cfg [rc Fill (Exactly 60) TopLeft, rc Fill Fill TopLeft]
          `shouldBe` [Rectangle 0 0 100 60, Rectangle 0 60 100 140]

      it "spacing separates children" $
        runVBox vBounds cfg { boxSpacing = 10 } [rc Fill Fill TopLeft, rc Fill Fill TopLeft]
          `shouldBe` [Rectangle 0 0 100 95, Rectangle 0 105 100 95]

    describe "cross axis (width)" $ do
      it "fillCross = True stretches children to the full available width" $
        runVBox vBounds cfg [rc (Exactly 60) Fill TopLeft]
          `shouldBe` [Rectangle 0 0 100 200]

      let cases =
            [ ("TopLeft aligns the child to the left",      TopLeft,  Rectangle 0  0 60 200)
            , ("Center aligns the child to the centre",     Center,   Rectangle 20 0 60 200)
            , ("TopRight aligns the child to the right",    TopRight, Rectangle 40 0 60 200)
            ]
      forM_ cases $ \(desc, alignment, expected) ->
        it desc $
          runVBox vBounds cfg { boxFillCross = False } [rc (Exactly 60) Fill alignment]
            `shouldBe` [expected]

    describe "boxAlignment" $ do
      let threeExact = [rc Fill (Exactly 40) TopLeft, rc Fill (Exactly 40) TopLeft, rc Fill (Exactly 40) TopLeft]

      it "Center centres the content block vertically" $
        runVBox vBounds cfg { boxAlignment = Center } threeExact
          `shouldBe` [Rectangle 0 40 100 40, Rectangle 0 80 100 40, Rectangle 0 120 100 40]

      it "BottomLeft aligns the content block to the bottom" $
        runVBox vBounds cfg { boxAlignment = BottomLeft } threeExact
          `shouldBe` [Rectangle 0 80 100 40, Rectangle 0 120 100 40, Rectangle 0 160 100 40]
