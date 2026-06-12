module Blink.LayoutSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, NonNegative (..), Positive (..), choose, forAll)

import Blink.Generators ()
import Blink.Geometry (Alignment (..), Point (..), Rectangle (..), uniform)
import Blink.Input (ButtonState (..), KeyEvent, InputState (..))
import Blink.Layout
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

-- Test infrastructure

noInput :: InputState
noInput = InputState
  { inputMousePosition = Point 0 0
  , inputLeftButton    = ButtonUp
  , inputKeyEvents     = [] :: [KeyEvent]
  , inputTypedText     = []
  }

emptyStyle :: Style
emptyStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignCenter
  , styleMargin       = uniform 0
  , stylePadding      = uniform 0
  , styleBorderColour = Nothing
  , styleBorderWidth  = 0
  }

emptyStyleSet :: StyleSet
emptyStyleSet = StyleSet
  { styleSetNormal   = emptyStyle
  , styleSetHovered  = emptyStyle
  , styleSetPressed  = emptyStyle
  , styleSetFocused  = emptyStyle
  , styleSetDisabled = emptyStyle
  }

emptyTheme :: Theme ()
emptyTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  = emptyStyleSet
  }

testColour :: Colour
testColour = RGBA 0 0 0 1

fill :: UI () () ()
fill = fillRect testColour

runLayout :: Rectangle -> UI () () () -> [Rectangle]
runLayout bounds ui =
  let ctx = emptyUIContext bounds noInput emptyTheme ()
      (_, ctx') = runUI ui ctx
  in [r | FillRect r _ <- getDrawCommands ctx']

-- hBox / vBox helpers

cfg :: BoxConfig
cfg = BoxConfig { boxSpacing = 0, boxMargin = 0, boxAlignment = TopLeft, boxFillCross = True }

margin :: Gen Double
margin = fromIntegral <$> (choose (0, 49) :: Gen Int)

runHBox :: Rectangle -> BoxConfig -> [Layout] -> [Rectangle]
runHBox bounds c rcs = runLayout bounds $ hBox c [(r, fill) | r <- rcs]

runVBox :: Rectangle -> BoxConfig -> [Layout] -> [Rectangle]
runVBox bounds c rcs = runLayout bounds $ vBox c [(r, fill) | r <- rcs]

rc :: Length -> Length -> Alignment -> Layout
rc = Layout

hBounds :: Rectangle
hBounds = Rectangle 0 0 200 100

vBounds :: Rectangle
vBounds = Rectangle 0 0 100 200

spec :: Spec
spec = describe "layout" $ do

  describe "preferredSize" $ do
    it "Exactly ignores available space" $
      preferredSize (Exactly 50) 100 `shouldBe` 50

    it "Fill uses all available space" $
      preferredSize Fill 100 `shouldBe` 100

    describe "AtLeast" $ do
      it "enforces the minimum size when space is insufficient" $
        preferredSize (AtLeast 50) 30 `shouldBe` 50

      it "grows to fill space when it exceeds the minimum" $
        preferredSize (AtLeast 50) 80 `shouldBe` 80

      prop "result is never below the minimum" $ \(NonNegative n) (NonNegative x) ->
        preferredSize (AtLeast n) x >= n

    describe "AtMost" $ do
      it "grows to fill space when within its maximum" $
        preferredSize (AtMost 80) 50 `shouldBe` 50

      it "limits to its maximum when space exceeds it" $
        preferredSize (AtMost 80) 100 `shouldBe` 80

      prop "result never exceeds the maximum" $ \(NonNegative n) (NonNegative x) ->
        preferredSize (AtMost n) x <= n

    describe "Between" $ do
      it "enforces the minimum size when space is insufficient" $
        preferredSize (Between 20 80) 10 `shouldBe` 20

      it "grows to fill space when within its range" $
        preferredSize (Between 20 80) 50 `shouldBe` 50

      it "limits to its maximum when space exceeds it" $
        preferredSize (Between 20 80) 100 `shouldBe` 80

      prop "result is always within [lo, hi]" $ \(NonNegative lo) (NonNegative d) (NonNegative x) ->
        let hi = lo + d
            r  = preferredSize (Between lo hi) x
        in r >= lo && r <= hi

  describe "layoutWithConstraints" $ do
    let run rct = runLayout hBounds (layoutWithConstraints rct fill)

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
      prop "a Fill child fills the margin-inset content area" $
        forAll margin $ \m ->
          runHBox hBounds cfg { boxMargin = m } [rc Fill Fill TopLeft]
            `shouldBe` [Rectangle m m (200 - 2*m) (100 - 2*m)]

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

    prop "no slot exceeds the upper bound of its width constraint" $ \constraints ->
      let rects  = runHBox hBounds cfg constraints
          within (AtMost  w,   s) = s <= w
          within (Between _ h, s) = s <= h
          within _                = True
      in all within (zip (map layoutWidth constraints) (map rectWidth rects))

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

    describe "content area" $ do
      prop "a Fill child fills the margin-inset content area" $
        forAll margin $ \m ->
          runVBox vBounds cfg { boxMargin = m } [rc Fill Fill TopLeft]
            `shouldBe` [Rectangle m m (100 - 2*m) (200 - 2*m)]

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

  describe "defaultBoxConfig" $ do
    it "has zero spacing and margin" $ do
      boxSpacing defaultBoxConfig `shouldBe` 0
      boxMargin  defaultBoxConfig `shouldBe` 0
    it "aligns to TopLeft with cross-axis fill enabled" $ do
      boxAlignment defaultBoxConfig `shouldBe` TopLeft
      boxFillCross defaultBoxConfig `shouldBe` True

  describe "boxTotalSpacing" $ do
    it "returns 0 for zero children" $
      boxTotalSpacing cfg { boxSpacing = 10 } 0 `shouldBe` 0

    it "returns 0 for one child" $
      boxTotalSpacing cfg { boxSpacing = 10 } 1 `shouldBe` 0

    prop "returns spacing × (n-1) for n children" $ \(NonNegative spacing) (Positive n) ->
      boxTotalSpacing cfg { boxSpacing = spacing } n `shouldBe` spacing * fromIntegral (n - 1 :: Int)
