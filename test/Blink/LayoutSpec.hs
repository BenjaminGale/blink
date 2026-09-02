module Blink.LayoutSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, NonNegative (..), choose, forAll, ioProperty)

import Blink.Attribute (Attribute)
import Blink.Generators ()
import Blink.Geometry (Alignment (..), Point (..), Rectangle (..), uniform)
import Blink.Input (KeyEvent, InputState (..))
import Blink.Layout
import Blink.Layout.Constraints (capLength, minLength)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..), noBorder)
import Blink.UI
import Blink.UI.Element (elementWithLayout, runElement)

-- Test infrastructure

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 0 0
  , inputLeftButtonDown = False
  , inputKeyEvents      = [] :: [KeyEvent]
  , inputTypedText      = []
  }

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

emptyStyleSet :: StyleSet
emptyStyleSet = StyleSet { styleBase = emptyStyle, styleOverrides = Map.empty }

emptyTheme :: Theme ()
emptyTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  = (emptyMetrics, emptyStyleSet)
  }

testColour :: Colour
testColour = RGBA 0 0 0 1

paint :: UI () () ()
paint = fillRect testColour

runLayout :: Rectangle -> UI () () () -> IO [Rectangle]
runLayout bounds ui = do
  let ctx = emptyUIContext bounds noInput emptyTheme noOpTextMeasurer
  (_, ctx') <- runUI ui ctx
  pure [r | FillRect r _ <- getDrawCommands ctx']

-- hBox / vBox helpers

cfg :: [Attribute (BoxConfig () ())]
cfg = []

marginGen :: Gen Double
marginGen = fromIntegral <$> (choose (0, 49) :: Gen Int)

runHBox :: Rectangle -> [Attribute (BoxConfig () ())] -> [Layout] -> IO [Rectangle]
runHBox bounds c rcs = runLayout bounds $ runElement $ hBox (c ++ [children [elementWithLayout r paint | r <- rcs]])

runVBox :: Rectangle -> [Attribute (BoxConfig () ())] -> [Layout] -> IO [Rectangle]
runVBox bounds c rcs = runLayout bounds $ runElement $ vBox (c ++ [children [elementWithLayout r paint | r <- rcs]])

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
      preferredSize (exactly 50) 100 `shouldBe` 50

    it "Fill uses all available space" $
      preferredSize fill 100 `shouldBe` 100

    describe "AtLeast" $ do
      it "enforces the minimum size when space is insufficient" $
        preferredSize (atLeast 50) 30 `shouldBe` 50

      it "grows to fill space when it exceeds the minimum" $
        preferredSize (atLeast 50) 80 `shouldBe` 80

      prop "result is never below the minimum" $ \(NonNegative n) (NonNegative x) ->
        preferredSize (atLeast n) x >= n

    describe "AtMost" $ do
      it "grows to fill space when within its maximum" $
        preferredSize (atMost 80) 50 `shouldBe` 50

      it "limits to its maximum when space exceeds it" $
        preferredSize (atMost 80) 100 `shouldBe` 80

      prop "result never exceeds the maximum" $ \(NonNegative n) (NonNegative x) ->
        preferredSize (atMost n) x <= n

    describe "Between" $ do
      it "enforces the minimum size when space is insufficient" $
        preferredSize (between 20 80) 10 `shouldBe` 20

      it "grows to fill space when within its range" $
        preferredSize (between 20 80) 50 `shouldBe` 50

      it "limits to its maximum when space exceeds it" $
        preferredSize (between 20 80) 100 `shouldBe` 80

      prop "result is always within [lo, hi]" $ \(NonNegative lo) (NonNegative d) (NonNegative x) ->
        let hi = lo + d
            r  = preferredSize (between lo hi) x
        in r >= lo && r <= hi

  describe "layoutWithConstraints" $ do
    let run rct = runLayout hBounds (layoutWithConstraints rct paint)

    describe "width constraints" $ do
      it "Exactly gives the child its exact width" $ do
        result <- run (rc (exactly 80) fill TopLeft)
        result `shouldBe` [Rectangle 0 0 80 100]

      it "Fill gives the child the full available width" $ do
        result <- run (rc fill fill TopLeft)
        result `shouldBe` [Rectangle 0 0 200 100]

      it "AtLeast expands to fill available space beyond the minimum" $ do
        result <- run (rc (atLeast 50) fill TopLeft)
        result `shouldBe` [Rectangle 0 0 200 100]

      it "AtMost caps the child at its maximum" $ do
        result <- run (rc (atMost 150) fill TopLeft)
        result `shouldBe` [Rectangle 0 0 150 100]

      it "Between clamps the child between its floor and ceiling" $ do
        result <- run (rc (between 50 150) fill TopLeft)
        result `shouldBe` [Rectangle 0 0 150 100]

    describe "height constraints" $ do
      let cases =
            [ ( "Exactly gives the child its exact height"
              , exactly 40,       Rectangle 0 0 200 40  )
            , ( "Fill gives the child the full available height"
              , fill,             Rectangle 0 0 200 100 )
            , ( "AtLeast expands to fill available space beyond the minimum"
              , atLeast 50,       Rectangle 0 0 200 100 )
            , ( "AtMost caps the child at its maximum"
              , atMost 80,        Rectangle 0 0 200 80  )
            , ( "Between clamps the child between its floor and ceiling"
              , between 50 80,    Rectangle 0 0 200 80  )
            ]
      forM_ cases $ \(desc, hc, expected) ->
        it desc $ do
          result <- run (rc fill hc TopLeft)
          result `shouldBe` [expected]

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
      forM_ cases $ \(desc, anAlign, expected) ->
        it desc $ do
          result <- run (rc (exactly 80) (exactly 40) anAlign)
          result `shouldBe` [expected]

  describe "hBox" $ do
    it "produces no output for an empty child list" $ do
      result <- runHBox hBounds cfg []
      result `shouldBe` []

    describe "main axis (width)" $ do
      it "a single Fill child fills the available width" $ do
        result <- runHBox hBounds cfg [rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 200 100]

      it "two Fill children share the available width equally" $ do
        result <- runHBox hBounds cfg [rc fill fill TopLeft, rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 100 100, Rectangle 100 0 100 100]

      it "an Exactly child gets its exact width" $ do
        result <- runHBox hBounds cfg [rc (exactly 60) fill TopLeft]
        result `shouldBe` [Rectangle 0 0 60 100]

      it "a fixed child and a Fill child share the remaining space" $ do
        result <- runHBox hBounds cfg [rc (exactly 60) fill TopLeft, rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 60 100, Rectangle 60 0 140 100]

      it "spacing separates children" $ do
        result <- runHBox hBounds [spacing 10] [rc fill fill TopLeft, rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 95 100, Rectangle 105 0 95 100]

    describe "content area" $ do
      prop "a Fill child fills the margin-inset content area" $
        forAll marginGen $ \m ->
          ioProperty $ do
            result <- runHBox hBounds [margin m] [rc fill fill TopLeft]
            pure $ result == [Rectangle m m (200 - 2*m) (100 - 2*m)]

    describe "cross axis (height)" $ do
      it "a Fill cross request stretches the child to the full available height" $ do
        result <- runHBox hBounds cfg [rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 200 100]

      let cases =
            [ ("TopLeft aligns the child to the top",    TopLeft,    Rectangle 0 0  200 40)
            , ("Center aligns the child to the middle",  Center,     Rectangle 0 30 200 40)
            , ("BottomLeft aligns the child to the bottom", BottomLeft, Rectangle 0 60 200 40)
            ]
      forM_ cases $ \(desc, anAlign, expected) ->
        it desc $ do
          result <- runHBox hBounds cfg [rc fill (exactly 40) anAlign]
          result `shouldBe` [expected]

    describe "alignment" $ do
      let threeExact = [rc (exactly 40) fill TopLeft, rc (exactly 40) fill TopLeft, rc (exactly 40) fill TopLeft]

      it "Center centres the content block horizontally" $ do
        result <- runHBox hBounds [alignment Center] threeExact
        result `shouldBe` [Rectangle 40 0 40 100, Rectangle 80 0 40 100, Rectangle 120 0 40 100]

      it "MiddleRight aligns the content block to the right" $ do
        result <- runHBox hBounds [alignment MiddleRight] threeExact
        result `shouldBe` [Rectangle 80 0 40 100, Rectangle 120 0 40 100, Rectangle 160 0 40 100]

    prop "no slot exceeds the upper bound of its width constraint" $ \constraints ->
      ioProperty $ do
        rects <- runHBox hBounds cfg constraints
        let within (c, s) = s <= minLength c + capLength c
        pure $ all within (zip (map layoutWidth constraints) (map rectWidth rects))

  describe "vBox" $ do
    it "produces no output for an empty child list" $ do
      result <- runVBox vBounds cfg []
      result `shouldBe` []

    describe "main axis (height)" $ do
      it "a single Fill child fills the available height" $ do
        result <- runVBox vBounds cfg [rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 100 200]

      it "two Fill children share the available height equally" $ do
        result <- runVBox vBounds cfg [rc fill fill TopLeft, rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 100 100, Rectangle 0 100 100 100]

      it "a fixed child and a Fill child share the remaining space" $ do
        result <- runVBox vBounds cfg [rc fill (exactly 60) TopLeft, rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 100 60, Rectangle 0 60 100 140]

      it "spacing separates children" $ do
        result <- runVBox vBounds [spacing 10] [rc fill fill TopLeft, rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 100 95, Rectangle 0 105 100 95]

    describe "content area" $ do
      prop "a Fill child fills the margin-inset content area" $
        forAll marginGen $ \m ->
          ioProperty $ do
            result <- runVBox vBounds [margin m] [rc fill fill TopLeft]
            pure $ result == [Rectangle m m (100 - 2*m) (200 - 2*m)]

    describe "cross axis (width)" $ do
      it "a Fill cross request stretches the child to the full available width" $ do
        result <- runVBox vBounds cfg [rc fill fill TopLeft]
        result `shouldBe` [Rectangle 0 0 100 200]

      let cases =
            [ ("TopLeft aligns the child to the left",      TopLeft,  Rectangle 0  0 60 200)
            , ("Center aligns the child to the centre",     Center,   Rectangle 20 0 60 200)
            , ("TopRight aligns the child to the right",    TopRight, Rectangle 40 0 60 200)
            ]
      forM_ cases $ \(desc, anAlign, expected) ->
        it desc $ do
          result <- runVBox vBounds cfg [rc (exactly 60) fill anAlign]
          result `shouldBe` [expected]

    describe "alignment" $ do
      let threeExact = [rc fill (exactly 40) TopLeft, rc fill (exactly 40) TopLeft, rc fill (exactly 40) TopLeft]

      it "Center centres the content block vertically" $ do
        result <- runVBox vBounds [alignment Center] threeExact
        result `shouldBe` [Rectangle 0 40 100 40, Rectangle 0 80 100 40, Rectangle 0 120 100 40]

      it "BottomLeft aligns the content block to the bottom" $ do
        result <- runVBox vBounds [alignment BottomLeft] threeExact
        result `shouldBe` [Rectangle 0 80 100 40, Rectangle 0 120 100 40, Rectangle 0 160 100 40]

  describe "borderLayout" $ do
    let bounds = Rectangle 0 0 300 200

    let runBorder attrs = runLayout bounds (borderLayout attrs)

    it "produces no output when all panels are absent" $ do
      result <- runBorder []
      result `shouldBe` []

    it "top panel occupies the full width at the top" $ do
      result <- runBorder [top 30 paint]
      result `shouldBe` [Rectangle 0 0 300 30]

    it "bottom panel occupies the full width at its fixed height" $ do
      result <- runBorder [bottom 20 paint]
      result `shouldBe` [Rectangle 0 0 300 20]

    it "left panel occupies the full height on the left" $ do
      result <- runBorder [left 50 paint]
      result `shouldBe` [Rectangle 0 0 50 200]

    it "right panel occupies the full height at its fixed width" $ do
      result <- runBorder [right 40 paint]
      result `shouldBe` [Rectangle 0 0 40 200]

    it "centre panel fills all available space when alone" $ do
      result <- runBorder [centre paint]
      result `shouldBe` [Rectangle 0 0 300 200]

    it "top and bottom panels stack when no middle content is present" $ do
      result <- runBorder [top 30 paint, bottom 20 paint]
      result `shouldBe` [Rectangle 0 0 300 30, Rectangle 0 30 300 20]

    it "all five panels occupy their correct regions" $ do
      result <- runBorder
        [ top 30 paint
        , bottom 20 paint
        , left 50 paint
        , right 40 paint
        , centre paint
        ]
      result `shouldBe`
        [ Rectangle 0   0   300  30   -- top
        , Rectangle 0   30  50   150  -- left
        , Rectangle 50  30  210  150  -- centre
        , Rectangle 260 30  40   150  -- right
        , Rectangle 0   180 300  20   -- bottom
        ]
