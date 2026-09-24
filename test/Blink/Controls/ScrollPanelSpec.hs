{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ScrollPanelSpec (spec) where

import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.ControlBehaviour (ControlBehaviourConfig (..), controlBehaviourSpec)
import Blink.Controls.Fixtures
  (hitRectFor, mkTestTheme, noInput, plainStyle, plainStyleSet, standardMetrics, testColour, zeroMetrics)
import Blink.Controls.ScrollBar (ScrollBarPart (..))
import Blink.Controls.ScrollPanel
import Blink.Element (Element (..), runElement)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (DrawCommand (..))
import Blink.Style (Theme)
import Blink.View
import Blink.View.Drawing (fillRect)

data TestElem = Part ScrollPanelPart | FocusHolder deriving (Eq, Ord, Show)

tag :: ScrollPanelPart -> TestElem
tag = Part

hScrollEid, vScrollEid :: TestElem
hScrollEid = tag (ScrollPanelHBar ScrollBar)
vScrollEid = tag (ScrollPanelVBar ScrollBar)

testTheme :: Theme TestElem
testTheme = mkTestTheme zeroMetrics (plainStyleSet (plainStyle testColour))

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 60

seedCtx :: ViewContext TestElem String
seedCtx = emptyViewContext testBounds noInput testTheme

-- | A child with a fixed natural size of @w x h@ regardless of the space
-- it's offered, that fills whatever bounds it's actually run at and draws
-- a marker fill there -- so a test can read the offset\/clip 'scrollPanel'
-- applied straight off the drawn rectangle.
fixedChild :: Double -> Double -> Element TestElem String
fixedChild w h = Element
  { elLayout  = Layout fill fill TopLeft
  , elMeasure = const (pure (Size w h))
  , elRun     = fillRect testColour
  }

type Attribute' = Attribute (ScrollPanelConfig TestElem String)

render :: [Attribute'] -> View TestElem String ()
render attrs = runElement (scrollPanel tag attrs)

contractTheme :: Theme TestElem
contractTheme = mkTestTheme standardMetrics (plainStyleSet (plainStyle testColour))

contractCtx :: ViewContext TestElem String
contractCtx = emptyViewContext testBounds noInput contractTheme

contractHitRect :: Rectangle
contractHitRect = hitRectFor testBounds

seededAt :: TestElem -> Double -> IO (ViewContext TestElem String)
seededAt eid v = resultContext <$> runInteractions testBounds seedCtx (requestScrollTo eid v) [] []

spec :: Spec
spec = describe "Blink.Controls.ScrollPanel" $ do
  describe "content that fits" $
    it "draws the child unclipped and unoffset, with no scrollbar taking any space" $ do
      result <- runInteractions testBounds seedCtx (render [content (fixedChild 50 40)]) [] [Wait 1]
      resultDraws result `shouldContain` [FillRect (Rectangle 0 0 100 60) testColour]

  describe "vertical overflow" $ do
    it "offsets the content by the scroll fraction, reserving width for the vertical bar" $ do
      ctx <- seededAt vScrollEid 0.5
      result <- runInteractions testBounds ctx (render [content (fixedChild 50 200)]) [] [Wait 1]
      -- viewport: 100x60 minus a 16px-wide vertical bar = 84x60; content is
      -- 200px tall, so 140px of scrollable range -- half of that is 70px.
      resultDraws result `shouldContain` [FillRect (Rectangle 0 (-70) 84 200) testColour]

    it "scrolls with the mouse wheel while the pointer is over the panel" $ do
      result <- runInteractions testBounds seedCtx (render [content (fixedChild 50 200)])
                  [MoveTo (Point 50 10)]
                  [Wheel 1]
      -- One 48px notch against 140px of scrollable range.
      contextScrollState vScrollEid (resultContext result) `shouldSatisfy` (\v -> abs (v - 48 / 140) < 1e-9)

    it "does not scroll when the wheel moves while the pointer is elsewhere" $ do
      result <- runInteractions testBounds seedCtx (render [content (fixedChild 50 200)])
                  [MoveTo (Point 500 500)]
                  [Wheel 1]
      contextScrollState vScrollEid (resultContext result) `shouldBe` 0

  describe "horizontal overflow" $
    it "offsets the content by the scroll fraction, reserving height for the horizontal bar" $ do
      ctx <- seededAt hScrollEid 0.5
      result <- runInteractions testBounds ctx (render [content (fixedChild 200 40)]) [] [Wait 1]
      -- viewport: 100x60 minus a 16px-tall horizontal bar = 100x44; content
      -- is 200px wide, so 100px of scrollable range -- half of that is 50px.
      resultDraws result `shouldContain` [FillRect (Rectangle (-50) 0 200 44) testColour]

  describe "both axes overflowing" $
    it "shows both bars and offsets both axes, each against its own reduced viewport" $ do
      ctxV <- seededAt vScrollEid 1
      ctx  <- resultContext <$> runInteractions testBounds ctxV (requestScrollTo hScrollEid 1) [] []
      result <- runInteractions testBounds ctx (render [content (fixedChild 200 200)]) [] [Wait 1]
      -- viewport: 100x60 minus both 16px bars = 84x44; content is 200x200,
      -- so 116px of horizontal range and 156px of vertical range.
      resultDraws result `shouldContain` [FillRect (Rectangle (-116) (-156) 200 200) testColour]

  controlBehaviourSpec (ControlBehaviourConfig { cbcAutoClaims = False, cbcClickFocuses = False })
    testBounds contractCtx (tag ScrollPanel) FocusHolder (Point 5 5) contractHitRect (Point 200 200) render
