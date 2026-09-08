{-# LANGUAGE OverloadedStrings #-}
module Blink.View.Controls.ScrollBarSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.View.Controls.Control
  (Attribute, FocusPolicy (..), control, defaultControlConfig, elementId, focusPolicy, resolve)
import Blink.View.Controls.ElementBehaviour (tagged)
import Blink.View.Controls.ScrollBar
  ( RepeatState (..), ScrollBarConfig, ScrollBarPart (..), decrementRepeatState, defaultScrollBarConfig
  , incrementRepeatState, initialRepeatState, onDecrementRepeatStateChanged, onIncrementRepeatStateChanged
  , onValueChanged, scrollBar, scrollBarOrientation, sbValue, step, value
  )
import Blink.Geometry (Orientation (..), Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.View.Rendering (Colour (..), TextAlign (..))
import Blink.View.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View
import Blink.View.Element (runElement)

-- | The scrollbar's own parts, plus an unrelated preceding control standing
-- in for the rest of a real form in the focus tests.
data TestElement = Part ScrollBarPart | Before deriving (Eq, Ord, Show)

tag :: ScrollBarPart -> TestElement
tag = Part

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
noInput = InputState (Point 200 200) False [] []

-- | A vertical scrollbar (the default orientation) 16px wide, 100px tall:
-- the decrement arrow occupies y 0-16, the track y 16-84, the increment
-- arrow y 84-100 -- see 'Blink.View.Controls.ScrollBar.scrollBarThickness'
-- and 'Blink.View.Layout.Box.vBox's own fixed\/flexible distribution.
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

onChanged :: Attribute'
onChanged = onValueChanged (\v -> [OutMsg ("ValueChanged:" ++ show v)])

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
spec = describe "Blink.View.Controls.ScrollBar" $ do
  describe "defaults" $
    it "starts with a value of 0" $
      sbValue (resolve defaultScrollBarConfig []) `shouldBe` 0

  -- Every click\/drag below is preceded by a 'MoveTo' at the same point, as
  -- setup -- see 'Blink.View.Controls.ToggleGroupSpec' for why: without a
  -- prior frame at that point, the composite's own outer control can end up
  -- holding mouse capture instead of the part actually being interacted
  -- with.
  describe "track" $ do
    it "reports value 0 when clicked at the point that centres the thumb at the start" $ do
      result <- runInteractions barBounds seedCtx (render [value 0.5, onChanged])
                  [MoveTo valueZeroPoint] [MouseDown valueZeroPoint]
      resultMessages result `shouldBe` ["ValueChanged:0.0"]

    it "reports value 1 when clicked at the point that centres the thumb at the end" $ do
      result <- runInteractions barBounds seedCtx (render [value 0.5, onChanged])
                  [MoveTo valueOnePoint] [MouseDown valueOnePoint]
      resultMessages result `shouldBe` ["ValueChanged:1.0"]

    it "keeps reporting the value as a drag continues past the track" $ do
      result <- runInteractions barBounds seedCtx (render [value 0, onChanged])
                  [MoveTo valueHalfPoint]
                  [MouseDown valueHalfPoint, DragTo (Point 8 500)]
      resultMessages result `shouldBe` ["ValueChanged:0.5", "ValueChanged:1.0"]

    it "does not report a value while disabled" $ do
      result <- runInteractions barBounds seedCtx (disableWhen True (render [value 0, onChanged]))
                  [MoveTo valueHalfPoint] [MouseDown valueHalfPoint]
      resultMessages result `shouldBe` []

  describe "arrow buttons" $ do
    it "decreases the value by the step when the decrement arrow is pressed" $ do
      result <- runInteractions barBounds seedCtx (render [value 0.5, onChanged])
                  [MoveTo decrementPoint] [MouseDown decrementPoint]
      resultMessages result `shouldBe` ["ValueChanged:0.45"]

    it "increases the value by the step when the increment arrow is pressed" $ do
      result <- runInteractions barBounds seedCtx (render [value 0.5, onChanged])
                  [MoveTo incrementPoint] [MouseDown incrementPoint]
      resultMessages result `shouldBe` ["ValueChanged:0.55"]

    it "respects a custom step" $ do
      result <- runInteractions barBounds seedCtx (render [value 0.5, step 0.25, onChanged])
                  [MoveTo incrementPoint] [MouseDown incrementPoint]
      resultMessages result `shouldBe` ["ValueChanged:0.75"]

    it "does not fire again once already at the minimum" $ do
      result <- runInteractions barBounds seedCtx (render [value 0, onChanged])
                  [MoveTo decrementPoint] [MouseDown decrementPoint]
      resultMessages result `shouldBe` []

    it "does not fire again once already at the maximum" $ do
      result <- runInteractions barBounds seedCtx (render [value 1, onChanged])
                  [MoveTo incrementPoint] [MouseDown incrementPoint]
      resultMessages result `shouldBe` []

    it "reports a fresh repeat state when the decrement arrow is first pressed" $ do
      let attrs = [ value 0.5
                  , onDecrementRepeatStateChanged (\s -> [OutMsg ("Repeat:" ++ show (rsFiredCount s))])
                  ]
      result <- runInteractions barBounds seedCtx (render attrs) [MoveTo decrementPoint] [MouseDown decrementPoint]
      resultMessages result `shouldBe` ["Repeat:0"]

    it "reports a fresh repeat state when the increment arrow is first pressed" $ do
      let attrs = [ value 0.5
                  , onIncrementRepeatStateChanged (\s -> [OutMsg ("Repeat:" ++ show (rsFiredCount s))])
                  ]
      result <- runInteractions barBounds seedCtx (render attrs) [MoveTo incrementPoint] [MouseDown incrementPoint]
      resultMessages result `shouldBe` ["Repeat:0"]

    it "feeds a stored repeat state back in without error" $ do
      let attrs = [value 0.5, decrementRepeatState (RepeatState (Just 0) 2), incrementRepeatState initialRepeatState]
      result <- runInteractions barBounds seedCtx (render attrs) [] [Wait 1]
      resultMessages result `shouldBe` []

  describe "orientation" $
    it "arranges left-to-right when set to Horizontal, with a click at the equivalent x offset behaving the same as the vertical default" $ do
      let horizontalBounds = Rectangle 0 0 100 16
          attrs = [scrollBarOrientation Horizontal, value 0.5, onChanged]
      result <- runInteractions horizontalBounds
                  (emptyViewContext horizontalBounds noInput testTheme noOpTextMeasurer)
                  (render attrs)
                  [MoveTo (Point 26 8)]
                  [MouseDown (Point 26 8)]
      resultMessages result `shouldBe` ["ValueChanged:0.0"]

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
