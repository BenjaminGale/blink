{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.RepeatButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, choose)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Blink.Controls.Button (onActivated)
import Blink.Controls.ButtonBehaviour (ButtonBehaviourConfig (..), buttonBehaviourSpec, defaultButtonBehaviourConfig)
import Blink.Controls.Control (Attribute, elementId, post)
import Blink.Controls.RepeatButton (RepeatButtonConfig, initialDelay, repeatButton, repeatInterval)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View
import Blink.Element (Element (..), runElement)

data TestElement = Ok deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignCenter
  , styleBorderColour = Nothing
  }

testMetrics :: Metrics
testMetrics = Metrics
  { metricsMargin      = uniform 10
  , metricsPadding     = uniform 5
  , metricsBorderEdges = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet { styleBase = testStyle, styleOverrides = Map.empty }

testTheme :: Theme TestElement
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = (testMetrics, testStyleSet) }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  , inputWheelDelta     = 0
  }

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

insidePoint :: Point
insidePoint = Point 50 50

type Attribute' = Attribute (RepeatButtonConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme noOpTextMeasurer

-- | The behaviour contracts below are about interaction, not sizing --
-- they're written against a control that fills its given bounds entirely,
-- the same way 'Blink.Controls.ButtonSpec.fullSize' forces 'button' full
-- for the same reason. 'repeatButton' defaults to fitting its own caption
-- height, same as 'Blink.Controls.Button.button'.
renderWithId :: [Attribute'] -> View TestElement String ()
renderWithId attrs = runElement (repeatButton Ok attrs) { elLayout = Layout fill fill TopLeft }

taggedActivated :: [Attribute']
taggedActivated = [onActivated (post "Activated")]

action :: View TestElement String ()
action = renderWithId taggedActivated

-- | 'action', but with a custom initial delay and its repeat interval
-- pushed far out of range -- for tests that only care about the first
-- repeat, isolated from ever landing on a second one.
actionWith :: Double -> View TestElement String ()
actionWith delay = renderWithId (taggedActivated ++ [initialDelay delay, repeatInterval 1000000])

-- | Drives @act@ through a fixed sequence of frames, each with its own
-- explicit 'AnimationState' -- unlike 'runInteractions', which always
-- carries the seed context's animation clock forward unchanged, this lets
-- a test advance elapsed time across frames to exercise the repeat
-- cadence. Accumulates every frame's messages, in order, same as
-- 'runInteractions'. No caller-side handoff needs stating between frames
-- here -- 'repeatButton' owns its own repeat-press state.
runAnimatedFrames
  :: View TestElement String ()
  -> ViewContext TestElement String
  -> [(Float, Float, InputState)]
  -> IO ([String], ViewContext TestElement String)
runAnimatedFrames act = go []
  where
    go acc ctx [] = pure (acc, ctx)
    go acc ctx ((delta, elapsed, input) : rest) = do
      let ctx' = nextFrameContext testBounds input testTheme (mkAnimationState delta elapsed True) ctx
      (_, ctxAfter) <- runView act ctx'
      go (acc ++ getMessages ctxAfter) ctxAfter rest

mouseDownInside :: InputState
mouseDownInside = noInput { inputMousePosition = insidePoint, inputLeftButtonDown = True }

-- | Runs the actual press frame (elapsed 0, discarding its own "Activated"
-- message so cadence tests below only see repeats) and returns the
-- resulting context -- the base every 'runAnimatedFrames' cadence test
-- continues from, so its own first frame is a held-to-held transition
-- rather than a fresh down edge.
pressed :: IO (ViewContext TestElement String)
pressed = snd <$> runView action (nextFrameContext testBounds mouseDownInside testTheme (mkAnimationState 0 0 True) seedCtx)

spec :: Spec
spec = describe "Blink.Controls.RepeatButton" $ do
  buttonBehaviourSpec (defaultButtonBehaviourConfig { bbcRepeatsOnHeldEnter = True })
    testBounds seedCtx Ok (Point 5 5) hitRect (Point 200 200) (\attrs -> renderWithId (elementId Ok : attrs))

  describe "mouse activation" $ do
    it "fires onActivated immediately on press, not on release" $ do
      result <- runInteractions testBounds seedCtx (renderWithId taggedActivated) [] [MouseDown insidePoint]
      resultMessages result `shouldBe` ["Activated"]

    it "does not fire onActivated again on release, unlike a plain button" $ do
      result <- runInteractions testBounds seedCtx (renderWithId taggedActivated) [MouseDown insidePoint] [MouseUp insidePoint]
      resultMessages result `shouldBe` []

    it "fires exactly once for a click shorter than the initial delay" $ do
      result <- runInteractions testBounds seedCtx (renderWithId taggedActivated) [] [MouseDown insidePoint, MouseUp insidePoint]
      length (filter (== "Activated") (resultMessages result)) `shouldBe` 1

  describe "repeat cadence" $ do
    -- Anchored at elapsed 0 (the press frame), with the default 0.4s
    -- initial delay and 0.08s repeat interval -- see 'pressed'.
    it "does not repeat before the initial delay has elapsed" $ do
      ctx0 <- pressed
      (msgs, _) <- runAnimatedFrames action ctx0
        [ (0.1, 0.1, mouseDownInside)
        , (0.1, 0.2, mouseDownInside)
        , (0.1, 0.3, mouseDownInside)
        ]
      msgs `shouldBe` []

    it "fires its first repeat exactly at the initial delay" $ do
      ctx0 <- pressed
      (msgs, _) <- runAnimatedFrames action ctx0
        [ (0.3, 0.3, mouseDownInside)
        , (0.1, 0.4, mouseDownInside)
        ]
      msgs `shouldBe` ["Activated"]

    prop "waits the normal delay before repeating, regardless of how long the app has already been running" $
      forAll ((,) <$> choose (0, 10000 :: Double) <*> choose (0.1, 2 :: Double)) $ \(pressAt, delay) -> monadicIO $ do
        -- A margin comfortably clear of both the exact boundary and of
        -- 'Float' rounding at the scale 'pressAt' can reach.
        let margin = delay * 0.25 :: Double
            pressAt' = realToFrac pressAt
            delay'   = realToFrac delay
            margin'  = realToFrac margin
        (tooSoon, justPast) <- run $ do
          (_, ctx0) <- runView (actionWith delay)
            (nextFrameContext testBounds mouseDownInside testTheme (mkAnimationState 0 pressAt' True) seedCtx)
          (msgsBefore, ctx1) <- runAnimatedFrames (actionWith delay) ctx0
            [(delay' - margin', pressAt' + delay' - margin', mouseDownInside)]
          (msgsAt, _)        <- runAnimatedFrames (actionWith delay) ctx1
            [(2 * margin', pressAt' + delay' + margin', mouseDownInside)]
          pure (msgsBefore, msgsAt)
        assert (tooSoon == [])
        assert (justPast == ["Activated"])

    it "fires again every interval thereafter" $ do
      -- 0.49\/0.57, not the exactly-on-a-boundary 0.48\/0.56, for the same
      -- 'Float'-rounding reason as the catch-up test below.
      ctx0 <- pressed
      (msgs, _) <- runAnimatedFrames action ctx0
        [ (0.4, 0.4, mouseDownInside)   -- crosses the initial delay: 1st repeat
        , (0.09, 0.49, mouseDownInside) -- one interval later: 2nd repeat
        , (0.08, 0.57, mouseDownInside) -- 3rd repeat
        ]
      msgs `shouldBe` ["Activated", "Activated", "Activated"]

    it "catches up on multiple interval crossings spanned by one long frame" $ do
      -- A single frame jumping from just past the initial delay to three
      -- intervals further should fire three times in that one frame, not
      -- silently drop the ones it stepped over.
      -- 0.65, not the exactly-on-a-boundary 0.64, so the check isn't at the
      -- mercy of 'Float' rounding landing a hair either side of a boundary
      -- -- real held-time from a wall clock is never exactly on one anyway.
      ctx0 <- pressed
      (msgs, _) <- runAnimatedFrames action ctx0
        [ (0.4, 0.4, mouseDownInside)   -- 1st repeat, at the delay
        , (0.25, 0.65, mouseDownInside) -- jumps past 3 more interval boundaries
        ]
      msgs `shouldBe` ["Activated", "Activated", "Activated", "Activated"]

    it "stops repeating once the button is released" $ do
      ctx0 <- pressed
      (msgs, _) <- runAnimatedFrames action ctx0
        [ (0.4, 0.4, mouseDownInside)
        , (0.0, 0.4, noInput { inputMousePosition = insidePoint, inputLeftButtonDown = False })
        , (0.08, 0.48, noInput { inputMousePosition = insidePoint, inputLeftButtonDown = False })
        ]
      msgs `shouldBe` ["Activated"]

    it "fires no extra repeats on a fresh press immediately after releasing" $ do
      ctx0 <- pressed
      (_, ctx1) <- runAnimatedFrames action ctx0
        [ (0.4, 0.4, mouseDownInside) -- 1st repeat, at the delay
        , (0.0, 0.4, noInput { inputMousePosition = insidePoint, inputLeftButtonDown = False })
        ]
      -- If the old press's anchor and fired count had leaked through
      -- instead of being cleared on release, this fresh press would already
      -- read as ~10s held and fire a burst of repeats immediately.
      (msgs, _) <- runAnimatedFrames action ctx1 [(0, 10, mouseDownInside)]
      msgs `shouldBe` ["Activated"]

    it "starts a fresh cadence on a new press after releasing" $ do
      ctx0 <- pressed
      (_, ctx1) <- runAnimatedFrames action ctx0
        [ (0.4, 0.4, mouseDownInside) -- 1st repeat, at the delay
        , (0.0, 0.4, noInput { inputMousePosition = insidePoint, inputLeftButtonDown = False })
        ]
      (_, ctx2) <- runAnimatedFrames action ctx1 [(0, 10, mouseDownInside)]
      -- 10.45, not the exactly-on-a-boundary 10.4, for the same
      -- 'Float'-rounding reason as the cadence tests above.
      (msgs, _) <- runAnimatedFrames action ctx2 [(0.45, 10.45, mouseDownInside)]
      msgs `shouldBe` ["Activated"]

  describe "animation ticker" $ do
    it "requires animation while held" $ do
      result <- runInteractions testBounds seedCtx (renderWithId []) [] [MouseDown insidePoint]
      contextRequiresAnimation (resultContext result) `shouldBe` True

    it "does not require animation while idle" $ do
      result <- runInteractions testBounds seedCtx (renderWithId []) [] [Wait 1]
      contextRequiresAnimation (resultContext result) `shouldBe` False
