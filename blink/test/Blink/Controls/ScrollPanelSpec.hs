{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ScrollPanelSpec (spec) where

import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.ControlBehaviour (ControlBehaviourConfig (..), controlBehaviourSpec)
import Blink.Controls.Fixtures
  (hitRectFor, mkTestTheme, noInput, plainStyle, plainStyleSet, standardMetrics, testColour, zeroMetrics)
import Blink.Controls.ScrollPanel
import Blink.Element (Element (..), emptyElement, preserveState, runElement)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (DrawCommand (..))
import Blink.Style (Theme)
import Blink.View hiding (ControlId (..))
import qualified Blink.View as V (ControlId (..))
import Blink.Testing
import Blink.View.Drawing (fillRect)

data TestElem = Panel | Pages | FocusHolder deriving (Eq, Ord, Show)

-- | Where the panel's viewport keeps its vertical scroll bar's position.
vScrollEid :: V.ControlId TestElem
vScrollEid = V.Part Panel "Viewport/VerticalBar"

-- | Seeds the horizontal position, which has no request function of its
-- own, by running 'requestScrollTo' inside the parts the bar lives in.
seedHorizontal :: Double -> View TestElem String ()
seedHorizontal v = withPart Panel "Viewport" (withPart Panel "HorizontalBar" (requestScrollTo Panel v))

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
render attrs = runElement (scrollPanel Panel attrs)

contractTheme :: Theme TestElem
contractTheme = mkTestTheme standardMetrics (plainStyleSet (plainStyle testColour))

contractCtx :: ViewContext TestElem String
contractCtx = emptyViewContext testBounds noInput contractTheme

contractHitRect :: Rectangle
contractHitRect = hitRectFor testBounds

seededAt :: View TestElem String () -> IO (ViewContext TestElem String)
seededAt seed = resultContext <$> runInteractions testBounds seedCtx seed [] []

spec :: Spec
spec = describe "Blink.Controls.ScrollPanel" $ do
  describe "content that fits" $
    it "draws the child unclipped and unoffset, with no scrollbar taking any space" $ do
      result <- runInteractions testBounds seedCtx (render [content (fixedChild 50 40)]) [] [Wait 1]
      resultDraws result `shouldContain` [FillRect (Rectangle 0 0 100 60) testColour]

  describe "vertical overflow" $ do
    it "offsets the content by the scroll fraction, reserving width for the vertical bar" $ do
      ctx <- seededAt (scrollPanelTo Panel 0.5)
      result <- runInteractions testBounds ctx (render [content (fixedChild 50 200)]) [] [Wait 1]
      -- viewport: 100x60 minus a 16px-wide vertical bar = 84x60; content is
      -- 200px tall, so 140px of scrollable range -- half of that is 70px.
      resultDraws result `shouldContain` [FillRect (Rectangle 0 (-70) 84 200) testColour]

    it "keeps showing the same content when more is added below it" $ do
      ctx   <- seededAt (scrollPanelTo Panel 0.5)
      short <- runInteractions testBounds ctx (render [content (fixedChild 50 200)]) [] [Wait 1]
      grown <- runInteractions testBounds (resultContext short) (render [content (fixedChild 50 400)]) [] [Wait 1]
      -- Scrolled 70px down before the content doubled; still 70px after.
      resultDraws grown `shouldContain` [FillRect (Rectangle 0 (-70) 84 400) testColour]

    it "scrolls back to the new end when the content shrinks below the scrolled offset" $ do
      ctx   <- seededAt (scrollPanelTo Panel 1)
      long  <- runInteractions testBounds ctx (render [content (fixedChild 50 400)]) [] [Wait 1]
      short <- runInteractions testBounds (resultContext long) (render [content (fixedChild 50 200)]) [] [Wait 1]
      -- 340px scrolled before; 200px of content leaves 140px of range.
      resultDraws short `shouldContain` [FillRect (Rectangle 0 (-140) 84 200) testColour]

    it "scrolls with the mouse wheel while the pointer is over the panel" $ do
      result <- runInteractions testBounds seedCtx (render [content (fixedChild 50 200)])
                  [MoveTo (Point 50 10)]
                  [Wheel 1]
      -- One 48px notch, well within the 140px of scrollable range.
      contextScrollState vScrollEid (resultContext result) `shouldBe` 48

    it "does not scroll when the wheel moves while the pointer is elsewhere" $ do
      result <- runInteractions testBounds seedCtx (render [content (fixedChild 50 200)])
                  [MoveTo (Point 500 500)]
                  [Wheel 1]
      contextScrollState vScrollEid (resultContext result) `shouldBe` 0

  describe "horizontal overflow" $
    it "offsets the content by the scroll fraction, reserving height for the horizontal bar" $ do
      ctx <- seededAt (seedHorizontal 0.5)
      result <- runInteractions testBounds ctx (render [content (fixedChild 200 40)]) [] [Wait 1]
      -- viewport: 100x60 minus a 16px-tall horizontal bar = 100x44; content
      -- is 200px wide, so 100px of scrollable range -- half of that is 50px.
      resultDraws result `shouldContain` [FillRect (Rectangle (-50) 0 200 44) testColour]

  describe "kept state" $ do
    let panel      = scrollPanel Panel [content (fixedChild 50 200)]
        scrolled   = FillRect (Rectangle 0 (-70) 84 200) testColour
        unscrolled = FillRect (Rectangle 0 0 84 200) testColour
        frame ctx v = runInteractions testBounds ctx v [] [Wait 1]

    it "forgets its scroll position after a frame in which it isn't drawn" $ do
      ctx    <- seededAt (scrollPanelTo Panel 0.5)
      shown  <- frame ctx (runElement panel)
      hidden <- frame (resultContext shown) (pure ())
      again  <- frame (resultContext hidden) (runElement panel)
      resultDraws again `shouldContain` [unscrolled]

    it "keeps its scroll position while hidden inside preserveState" $ do
      ctx    <- seededAt (scrollPanelTo Panel 0.5)
      shown  <- frame ctx (runElement (preserveState Pages panel))
      hidden <- frame (resultContext shown) (runElement (preserveState Pages emptyElement))
      again  <- frame (resultContext hidden) (runElement (preserveState Pages panel))
      resultDraws again `shouldContain` [scrolled]

    it "forgets its scroll position once the preserveState around it stops being drawn too" $ do
      ctx    <- seededAt (scrollPanelTo Panel 0.5)
      shown  <- frame ctx (runElement (preserveState Pages panel))
      hidden <- frame (resultContext shown) (pure ())
      again  <- frame (resultContext hidden) (runElement (preserveState Pages panel))
      resultDraws again `shouldContain` [unscrolled]

    it "keeps a scroll position requested before it was first drawn" $ do
      ctx    <- seededAt (scrollPanelTo Panel 0.5)
      idle   <- frame ctx (pure ())
      shown  <- frame (resultContext idle) (runElement panel)
      resultDraws shown `shouldContain` [scrolled]

  describe "both axes overflowing" $
    it "shows both bars and offsets both axes, each against its own reduced viewport" $ do
      ctxV <- seededAt (scrollPanelTo Panel 1)
      ctx  <- resultContext <$> runInteractions testBounds ctxV (seedHorizontal 1) [] []
      result <- runInteractions testBounds ctx (render [content (fixedChild 200 200)]) [] [Wait 1]
      -- viewport: 100x60 minus both 16px bars = 84x44; content is 200x200,
      -- so 116px of horizontal range and 156px of vertical range.
      resultDraws result `shouldContain` [FillRect (Rectangle (-116) (-156) 200 200) testColour]

  controlBehaviourSpec (ControlBehaviourConfig { cbcAutoClaims = False, cbcClickFocuses = False, cbcFocusableById = True })
    testBounds contractCtx Panel FocusHolder (Point 5 5) contractHitRect (Point 200 200) render
