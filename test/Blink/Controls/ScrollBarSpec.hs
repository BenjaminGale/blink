{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ScrollBarSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control
  (Attribute, FocusPolicy (..), control, defaultControlConfig, elementId, focusPolicy, resolve)
import Blink.Controls.ElementBehaviour (tagged)
import Blink.Controls.ScrollBar (ScrollBarConfig, ScrollBarPart (..), scrollBar, scrollBarOrientation, step)
import Blink.Geometry (Orientation (..), Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View
import Blink.Element (runElement)

-- | The scrollbar's own parts, plus an unrelated preceding control standing
-- in for the rest of a real form in the focus tests.
data TestElement = Part ScrollBarPart | Before deriving (Eq, Ord, Show)

tag :: ScrollBarPart -> TestElement
tag = Part

-- | The 'Blink.View.ScrollState' key 'scrollBar' reads\/writes its own
-- position under -- see its module header.
scrollEid :: TestElement
scrollEid = tag ScrollBar

testColour :: Colour
testColour = RGBA 0 0 0 1

testTheme :: Theme TestElement
testTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  = (emptyMetrics, StyleSet emptyStyle Map.empty)
  }
  where
    emptyStyle = Style
      { styleBackground   = testColour
      , styleTextColour   = testColour
      , styleTextAlign    = AlignCenter
      , styleBorderColour = Nothing
      }
    emptyMetrics = Metrics
      { metricsMargin      = uniform 0
      , metricsPadding     = uniform 0
      , metricsBorderEdges = noBorder
      }

noInput :: InputState
noInput = InputState (Point 200 200) False [] [] 0

-- | A vertical scrollbar (the default orientation) 16px wide, 100px tall:
-- the decrement arrow occupies y 0-16, the track y 16-84, the increment
-- arrow y 84-100 -- see 'Blink.Controls.ScrollBar.scrollBarThickness'
-- and 'Blink.Layout.Box.vBox's own fixed\/flexible distribution.
barBounds :: Rectangle
barBounds = Rectangle 0 0 16 100

decrementPoint, incrementPoint :: Point
decrementPoint = Point 8 8
incrementPoint = Point 8 92

-- | The track's own thumb travels 48px (its 68px length minus a 20px
-- thumb, floored at the minimum grabbable length since the default 0.2
-- visible fraction would otherwise draw a 13.6px thumb) -- centring the
-- thumb under a click 10px (half the thumb) inside either end of the track
-- lands exactly on value 0\/1.
valueZeroPoint, valueHalfPoint, valueOnePoint :: Point
valueZeroPoint = Point 8 26
valueHalfPoint = Point 8 50
valueOnePoint  = Point 8 74

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext barBounds noInput testTheme noOpTextMeasurer

type Attribute' = Attribute (ScrollBarConfig TestElement String)

render :: [Attribute'] -> View TestElement String ()
render attrs = runElement (scrollBar tag attrs)

-- | Seeds the scrollbar's position directly via 'requestScrollTo', the same
-- way 'Blink.Interaction's own module header documents for scroll\/selection
-- state that's impractical to reach via a pixel-accurate drag.
seededAt :: Double -> IO (ViewContext TestElement String)
seededAt v = resultContext <$> runInteractions barBounds seedCtx (requestScrollTo scrollEid v) [] []

-- | Scene bounds wide enough for 'Before' (0-40) followed by the bar
-- (40-56), for the Tab tests below.
sceneBounds :: Rectangle
sceneBounds = Rectangle 0 0 56 100

rectBefore :: Rectangle
rectBefore = Rectangle 0 0 40 100

rectBar :: Rectangle
rectBar = Rectangle 40 0 16 100

focusedOn :: TestElement -> InteractionResult TestElement String a -> Bool
focusedOn eid result = case contextFocusChain (resultContext result) of
  [] -> False
  xs -> last xs == eid

spec :: Spec
spec = describe "Blink.Controls.ScrollBar" $ do
  describe "defaults" $
    it "starts scrolled to the start when nothing has set a position" $ do
      result <- runInteractions barBounds seedCtx (render []) [] [Wait 1]
      contextScrollState scrollEid (resultContext result) `shouldBe` 0

  -- Every click\/drag below is preceded by a 'MoveTo' at the same point, as
  -- setup -- see 'Blink.Controls.ToggleGroupSpec' for why: without a
  -- prior frame at that point, the composite's own outer control can end up
  -- holding mouse capture instead of the part actually being interacted
  -- with.
  describe "track" $ do
    it "moves to the start when clicked at the point that centres the thumb there" $ do
      ctx <- seededAt 0.5
      result <- runInteractions barBounds ctx (render []) [MoveTo valueZeroPoint] [MouseDown valueZeroPoint]
      contextScrollState scrollEid (resultContext result) `shouldBe` 0

    it "moves to the end when clicked at the point that centres the thumb there" $ do
      ctx <- seededAt 0.5
      result <- runInteractions barBounds ctx (render []) [MoveTo valueOnePoint] [MouseDown valueOnePoint]
      contextScrollState scrollEid (resultContext result) `shouldBe` 1

    it "keeps following the pointer as a drag continues past the track" $ do
      ctx <- seededAt 0
      result <- runInteractions barBounds ctx (render [])
                  [MoveTo valueHalfPoint]
                  [MouseDown valueHalfPoint, DragTo (Point 8 500)]
      contextScrollState scrollEid (resultContext result) `shouldBe` 1

    it "does not move while disabled" $ do
      ctx <- seededAt 0
      result <- runInteractions barBounds ctx (disableWhen True (render [])) [MoveTo valueHalfPoint] [MouseDown valueHalfPoint]
      contextScrollState scrollEid (resultContext result) `shouldBe` 0

  describe "arrow buttons" $ do
    it "decreases the value by the step when the decrement arrow is pressed" $ do
      ctx <- seededAt 0.5
      result <- runInteractions barBounds ctx (render []) [MoveTo decrementPoint] [MouseDown decrementPoint]
      contextScrollState scrollEid (resultContext result) `shouldBe` 0.45

    it "increases the value by the step when the increment arrow is pressed" $ do
      ctx <- seededAt 0.5
      result <- runInteractions barBounds ctx (render []) [MoveTo incrementPoint] [MouseDown incrementPoint]
      contextScrollState scrollEid (resultContext result) `shouldBe` 0.55

    it "respects a custom step" $ do
      ctx <- seededAt 0.5
      result <- runInteractions barBounds ctx (render [step 0.25]) [MoveTo incrementPoint] [MouseDown incrementPoint]
      contextScrollState scrollEid (resultContext result) `shouldBe` 0.75

    it "clamps at the minimum instead of going below it" $ do
      ctx <- seededAt 0
      result <- runInteractions barBounds ctx (render []) [MoveTo decrementPoint] [MouseDown decrementPoint]
      contextScrollState scrollEid (resultContext result) `shouldBe` 0

    it "clamps at the maximum instead of going above it" $ do
      ctx <- seededAt 1
      result <- runInteractions barBounds ctx (render []) [MoveTo incrementPoint] [MouseDown incrementPoint]
      contextScrollState scrollEid (resultContext result) `shouldBe` 1

    -- 'RepeatButton' owns its own repeat cadence entirely (see
    -- 'Blink.Controls.RepeatButtonSpec' for the cadence itself, tested
    -- there in full); this only proves 'scrollBar' actually wires its
    -- arrows to it correctly end-to-end, since that's the one thing
    -- composing two independently-tested pieces doesn't prove on its own.
    it "keeps stepping while the decrement arrow is held across multiple frames" $ do
      ctx0 <- seededAt 0.9
      let moveInput = noInput { inputMousePosition = decrementPoint }
          downInput = noInput { inputMousePosition = decrementPoint, inputLeftButtonDown = True }
      (_, ctxMoved)   <- runView (render []) (nextFrameContext barBounds moveInput testTheme (mkAnimationState 0 0 True) ctx0)
      (_, ctxPressed) <- runView (render []) (nextFrameContext barBounds downInput testTheme (mkAnimationState 0 0 True) ctxMoved)
      -- Crosses the 0.4s initial delay plus two 0.08s intervals: the press
      -- itself plus 3 repeats, 4 decrements of 0.05 each in total.
      (_, ctxHeld) <- runView (render []) (nextFrameContext barBounds downInput testTheme (mkAnimationState 0.6 0.6 True) ctxPressed)
      let settled = settleEffects ctxHeld
      contextScrollState scrollEid settled `shouldSatisfy` (\v -> abs (v - (0.9 - 4 * 0.05)) < 1e-9)

  describe "orientation" $
    it "arranges left-to-right when set to Horizontal, with a click at the equivalent x offset behaving the same as the vertical default" $ do
      ctx <- seededAt 0.5
      let horizontalBounds = Rectangle 0 0 100 16
      result <- runInteractions horizontalBounds ctx (render [scrollBarOrientation Horizontal])
                  [MoveTo (Point 26 8)]
                  [MouseDown (Point 26 8)]
      contextScrollState scrollEid (resultContext result) `shouldBe` 0

  describe "as a control" $
    it "still raises its own raw mouse events, since it's built on `control`" $ do
      result <- runInteractions barBounds seedCtx (render tagged) [] [MoveTo valueHalfPoint]
      resultMessages result `shouldBe` ["MouseEntered"]

  describe "focus" $ do
    it "is never a tab stop itself -- Tab from before cycles straight back to it, landing on none of its parts" $ do
      let renderScene = do
            withBounds rectBefore (() <$ control (resolve defaultControlConfig [elementId Before]))
            withBounds rectBar    (render [])
      result <- runInteractions sceneBounds seedCtx renderScene [Wait 1] [Tab, Wait 1]
      contextFocusChain (resultContext result) `shouldBe` [Before]

    it "focusPolicy on the composite itself is fixed regardless of any attempt to override it" $ do
      result <- runInteractions barBounds seedCtx (render (focusPolicy Focusable : tagged)) [] [Wait 1]
      resultMessages result `shouldBe` []

    it "neither arrow button is a focus target" $ do
      result <- runInteractions barBounds seedCtx (render []) [MoveTo decrementPoint]
                  [ClickAt decrementPoint, Wait 1]
      focusedOn (tag ScrollBarDecrement) result `shouldBe` False
