{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.RepeatButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Button (onActivated)
import Blink.Controls.ButtonBehaviour (ButtonBehaviourConfig (..), buttonBehaviourSpec, defaultButtonBehaviourConfig)
import Blink.Controls.Control (elementId)
import Blink.Controls.Element (Attribute)
import Blink.Controls.RepeatButton
  (RepeatButtonConfig, firedCount, onPressEnded, onPressStarted, pressStartedAt, repeatButton)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.UI
import Blink.UI.Element (Element (..), runElement)

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
  }

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

insidePoint :: Point
insidePoint = Point 50 50

type Attribute' = Attribute (RepeatButtonConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

-- | The behaviour contracts below are about interaction, not sizing --
-- they're written against a control that fills its given bounds entirely,
-- the same way 'Blink.Controls.ButtonSpec.fullSize' forces 'button' full
-- for the same reason. 'repeatButton' defaults to fitting its own caption
-- height, same as 'Blink.Controls.Button.button'.
renderWithId :: [Attribute'] -> UI TestElement String ()
renderWithId attrs = runElement (repeatButton Ok attrs) { elLayout = Layout fill fill TopLeft }

taggedActivated :: [Attribute']
taggedActivated = [onActivated (const [OutMsg "Activated"])]

-- | Drives @mkAction@ (given the fired count the caller would have stored
-- from the previous frame's 'Blink.Controls.RepeatButton.onFiredCountChanged',
-- 0 for the first frame) through a fixed sequence of frames, each with its
-- own explicit 'AnimationState' -- unlike 'runInteractions', which always
-- carries the seed context's animation clock forward unchanged, this lets
-- a test advance elapsed time across frames to exercise the repeat
-- cadence. Accumulates every frame's messages, in order, same as
-- 'runInteractions'. The fired count for each frame is supplied explicitly
-- rather than threaded automatically, so a test states the same handoff a
-- real caller performs, in full, rather than hiding it in the harness.
runAnimatedFrames
  :: (Int -> UI TestElement String ())
  -> UIContext TestElement String
  -> [(Float, Float, Int, InputState)]
  -> IO ([String], UIContext TestElement String)
runAnimatedFrames mkAction = go []
  where
    go acc ctx [] = pure (acc, ctx)
    go acc ctx ((delta, elapsed, fc, input) : rest) = do
      let ctx' = nextFrameContext testBounds input testTheme (mkAnimationState delta elapsed True) ctx
      (_, ctxAfter) <- runUI (mkAction fc) ctx'
      go (acc ++ getMessages ctxAfter) ctxAfter rest

mouseDownInside :: InputState
mouseDownInside = noInput { inputMousePosition = insidePoint, inputLeftButtonDown = True }

-- | Runs the actual press frame (elapsed 0, discarding its own "Activated"
-- message so cadence tests below only see repeats) and returns the
-- resulting context -- the base every 'runAnimatedFrames' cadence test
-- continues from, so its own first frame is a held-to-held transition
-- rather than a fresh down edge.
pressed :: UI TestElement String () -> IO (UIContext TestElement String)
pressed action = snd <$> runUI action (nextFrameContext testBounds mouseDownInside testTheme (mkAnimationState 0 0 True) seedCtx)

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

  describe "press anchor handoff" $ do
    it "reports onPressStarted with the animation clock's elapsed time at the moment of the press" $ do
      let elapsedCtx = nextFrameContext testBounds noInput testTheme (mkAnimationState 0 5 False) seedCtx
          attrs = [onPressStarted (\t -> [OutMsg ("PressStarted:" ++ show t)])]
      result <- runInteractions testBounds elapsedCtx (renderWithId attrs) [] [MouseDown insidePoint]
      resultMessages result `shouldBe` ["PressStarted:5.0"]

    it "reports onPressEnded once when the press releases" $ do
      let attrs = [pressStartedAt (Just 0), onPressEnded [OutMsg "PressEnded"]]
      result <- runInteractions testBounds seedCtx (renderWithId attrs) [MouseDown insidePoint] [MouseUp insidePoint]
      resultMessages result `shouldBe` ["PressEnded"]

    it "reports no onPressEnded while nothing has ever been pressed" $ do
      let attrs = [onPressEnded [OutMsg "PressEnded"]]
      result <- runInteractions testBounds seedCtx (renderWithId attrs) [] [Wait 1]
      resultMessages result `shouldBe` []

  describe "repeat cadence" $ do
    -- Anchored at elapsed 0 (the press frame), with the default 0.4s
    -- initial delay and 0.08s repeat interval. Each frame below states the
    -- fired count explicitly, exactly as a real caller would after storing
    -- the previous frame's 'Blink.Controls.RepeatButton.onFiredCountChanged'
    -- -- see 'runAnimatedFrames'.
    let mkAction fc = renderWithId (taggedActivated ++ [pressStartedAt (Just 0), firedCount fc])
        pressAction = renderWithId (taggedActivated ++ [pressStartedAt (Just 0), firedCount 0])

    it "does not repeat before the initial delay has elapsed" $ do
      ctx0 <- pressed pressAction
      (msgs, _) <- runAnimatedFrames mkAction ctx0
        [ (0.1, 0.1, 0, mouseDownInside)
        , (0.1, 0.2, 0, mouseDownInside)
        , (0.1, 0.3, 0, mouseDownInside)
        ]
      msgs `shouldBe` []

    it "fires its first repeat exactly at the initial delay" $ do
      ctx0 <- pressed pressAction
      (msgs, _) <- runAnimatedFrames mkAction ctx0
        [ (0.3, 0.3, 0, mouseDownInside)
        , (0.1, 0.4, 0, mouseDownInside)
        ]
      msgs `shouldBe` ["Activated"]

    it "fires again every interval thereafter" $ do
      -- 0.49\/0.57, not the exactly-on-a-boundary 0.48\/0.56, for the same
      -- 'Float'-rounding reason as the catch-up test below.
      ctx0 <- pressed pressAction
      (msgs, _) <- runAnimatedFrames mkAction ctx0
        [ (0.4, 0.4, 0, mouseDownInside)   -- crosses the initial delay: 1st repeat
        , (0.09, 0.49, 1, mouseDownInside) -- one interval later: 2nd repeat
        , (0.08, 0.57, 2, mouseDownInside) -- 3rd repeat
        ]
      msgs `shouldBe` ["Activated", "Activated", "Activated"]

    it "catches up on multiple interval crossings spanned by one long frame" $ do
      -- A single frame jumping from just past the initial delay to three
      -- intervals further should fire three times in that one frame, not
      -- silently drop the ones it stepped over.
      -- 0.65, not the exactly-on-a-boundary 0.64, so the check isn't at the
      -- mercy of 'Float' rounding landing a hair either side of a boundary
      -- -- real held-time from a wall clock is never exactly on one anyway.
      ctx0 <- pressed pressAction
      (msgs, _) <- runAnimatedFrames mkAction ctx0
        [ (0.4, 0.4, 0, mouseDownInside)   -- 1st repeat, at the delay
        , (0.25, 0.65, 1, mouseDownInside) -- jumps past 3 more interval boundaries
        ]
      msgs `shouldBe` ["Activated", "Activated", "Activated", "Activated"]

    it "stops repeating once the button is released" $ do
      ctx0 <- pressed pressAction
      (msgs, _) <- runAnimatedFrames mkAction ctx0
        [ (0.4, 0.4, 0, mouseDownInside)
        , (0.0, 0.4, 1, noInput { inputMousePosition = insidePoint, inputLeftButtonDown = False })
        , (0.08, 0.48, 1, noInput { inputMousePosition = insidePoint, inputLeftButtonDown = False })
        ]
      msgs `shouldBe` ["Activated"]

  describe "animation ticker" $ do
    it "requires animation while held" $ do
      result <- runInteractions testBounds seedCtx (renderWithId []) [] [MouseDown insidePoint]
      contextRequiresAnimation (resultContext result) `shouldBe` True

    it "does not require animation while idle" $ do
      result <- runInteractions testBounds seedCtx (renderWithId []) [] [Wait 1]
      contextRequiresAnimation (resultContext result) `shouldBe` False
