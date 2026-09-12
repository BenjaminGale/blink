{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ControlSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control
  ( Attribute, ControlConfig (..), FocusPolicy (..)
  , control, defaultControlConfig, elementId, focusTargetOnClick, isEnabled, focusPolicy
  , onClicked, onFocusGained, onFocusLost, onKeyPressed
  , onMouseDown, onMouseEntered, onMouseExited, onMouseUp, post, postWith, resolve
  )
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..), VisualState (CommonPressed))
import Blink.View

data TestElement
  = ElemA | ElemB | ElemC
  deriving (Eq, Ord, Show)

rectA, rectB, rectC :: Rectangle
rectA = Rectangle 0 0 50 100
rectB = Rectangle 50 0 50 100
rectC = Rectangle 100 0 50 100

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

pressedColour :: Colour
pressedColour = RGBA 1 1 1 1

-- | Like 'testTheme', but with a 'CommonPressed' override with a
-- background distinct from every other state, so a test can tell whether
-- the pressed style was actually the one drawn.
pressedTestTheme :: Theme TestElement
pressedTestTheme = testTheme
  { themeDefaultStyle =
      ( testMetrics
      , testStyleSet { styleOverrides = Map.singleton CommonPressed (\s -> s { styleBackground = pressedColour }) }
      )
  }

onA, onB :: Point
onA = Point 10 50
onB = Point 60 50

type Attribute' = Attribute (ControlConfig TestElement String)

-- | Renders a single control at whatever bounds are current, with its
-- attrs (plus 'elementId' @eid@) resolved against 'defaultControlConfig'.
renderAt :: TestElement -> [Attribute'] -> View TestElement String ()
renderAt eid attrs = () <$ control (resolve defaultControlConfig (elementId eid : attrs))

-- | Renders 'ElemA' at 'rectA' and 'ElemB' at 'rectB' with the given attrs.
both :: [Attribute'] -> [Attribute'] -> View TestElement String ()
both attrsA attrsB = do
  withBounds rectA (renderAt ElemA attrsA)
  withBounds rectB (renderAt ElemB attrsB)

-- | Renders @fromId@ non-focusable, redirecting its own click onto @toId@
-- via 'focusTargetOnClick' -- the same shape 'Blink.Controls.Label.label'
-- builds on top of 'control', exercised here directly against the
-- low-level primitive rather than through a label.
renderRedirect :: TestElement -> [Attribute'] -> TestElement -> [Attribute'] -> View TestElement String ()
renderRedirect fromId attrsFrom toId attrsTo = do
  withBounds rectA $ do
    scope <- getCurrentScope
    ci    <- control (resolve defaultControlConfig (elementId fromId : focusPolicy NotFocusable : attrsFrom))
    focusTargetOnClick scope toId ci
  withBounds rectB (renderAt toId attrsTo)

-- | Renders a single 'ElemA' at 'testBounds' with the given attrs -- the
-- same way every real widget built on 'control' does.
renderControl :: [Attribute'] -> View TestElement String ()
renderControl = renderAt ElemA

three :: [Attribute'] -> [Attribute'] -> [Attribute'] -> View TestElement String ()
three attrsA attrsB attrsC = do
  withBounds rectA (renderAt ElemA attrsA)
  withBounds rectB (renderAt ElemB attrsB)
  withBounds rectC (renderAt ElemC attrsC)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme noOpTextMeasurer
  where
    noInput = InputState (Point 200 200) False [] [] 0

pressedSeedCtx :: ViewContext TestElement String
pressedSeedCtx = emptyViewContext testBounds noInput pressedTestTheme noOpTextMeasurer
  where
    noInput = InputState (Point 200 200) False [] [] 0

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

spec :: Spec
spec = describe "Blink.Controls.Control.control" $ do
  controlBehaviourSpec defaultControlBehaviourConfig testBounds seedCtx ElemA (Point 5 5) hitRect (Point 200 200) renderControl

  describe "chrome" $ do
    it "draws background via renderStyled, inset by margin" $ do
      ctx <- snd <$> runView (renderControl []) seedCtx
      getDrawCommands ctx `shouldContain` [FillRect (insetRect (uniform 10) testBounds) testColour]

    it "draws in its pressed style while the mouse is held down over it" $ do
      result <- runInteractions testBounds pressedSeedCtx (renderControl []) [] [MouseDown (Point 50 50)]
      getDrawCommands (resultContext result) `shouldContain` [FillRect (insetRect (uniform 10) testBounds) pressedColour]

    describe "with a nested child control" $ do
      let rectChild = Rectangle 20 20 40 40
          containerWithChild =
            () <$ control (resolve
              (defaultControlConfig { ccContent = const (withBounds rectChild (renderAt ElemB [])) })
              [elementId ElemA])

      it "draws the child in its pressed style when the mouse is held down over it" $ do
        result <- runInteractions testBounds pressedSeedCtx containerWithChild [MoveTo (Point 40 40)] [MouseDown (Point 40 40)]
        getDrawCommands (resultContext result) `shouldContain` [FillRect (insetRect (uniform 10) rectChild) pressedColour]

      it "does not draw the container in its pressed style when the mouse is held down over the child instead" $ do
        result <- runInteractions testBounds pressedSeedCtx containerWithChild [MoveTo (Point 40 40)] [MouseDown (Point 40 40)]
        getDrawCommands (resultContext result) `shouldNotContain` [FillRect (insetRect (uniform 10) testBounds) pressedColour]

      it "still draws the container in its pressed style when the mouse is held down over empty space inside it, away from the child" $ do
        result <- runInteractions testBounds pressedSeedCtx containerWithChild [MoveTo (Point 70 70)] [MouseDown (Point 70 70)]
        getDrawCommands (resultContext result) `shouldContain` [FillRect (insetRect (uniform 10) testBounds) pressedColour]

    describe "with isEnabled False on the container and a nested child control" $ do
      let rectChild = Rectangle 20 20 40 40
          childClickPoint = Point 40 40
          containerDisabledWithChild attrsChild =
            () <$ control (resolve
              (defaultControlConfig { ccContent = const (withBounds rectChild (renderAt ElemB attrsChild)) })
              [elementId ElemA, isEnabled False])

      it "raises no click event from the child, even though only the container has isEnabled False" $ do
        result <- runInteractions testBounds seedCtx
          (containerDisabledWithChild [onClicked (post ("B clicked" :: String))])
          [MoveTo childClickPoint] [MouseDown childClickPoint, MouseUp childClickPoint]
        resultMessages result `shouldBe` []

      it "does not draw the child in its pressed style while the mouse is held over it" $ do
        result <- runInteractions testBounds pressedSeedCtx (containerDisabledWithChild []) [MoveTo childClickPoint] [MouseDown childClickPoint]
        getDrawCommands (resultContext result) `shouldNotContain` [FillRect (insetRect (uniform 10) rectChild) pressedColour]

    describe "with a control nested two levels deep" $ do
      -- rectMid's hit area (30,30)-(50,50) contains rectInner's hit area
      -- (38,38)-(42,42), which contains the shared click point (40,40) --
      -- so all three of outer/mid/inner are hit at once, only inner should
      -- ever show pressed.
      let rectMid   = Rectangle 20 20 40 40
          rectInner = Rectangle 28 28 24 24
          clickPoint = Point 40 40
          nested =
            () <$ control (resolve
              (defaultControlConfig { ccContent = const (withBounds rectMid innerControl) })
              [elementId ElemA])
          innerControl =
            () <$ control (resolve
              (defaultControlConfig { ccContent = const (withBounds rectInner (renderAt ElemC [])) })
              [elementId ElemB])

      it "draws the innermost control in its pressed style" $ do
        result <- runInteractions testBounds pressedSeedCtx nested [MoveTo clickPoint] [MouseDown clickPoint]
        getDrawCommands (resultContext result) `shouldContain` [FillRect (insetRect (uniform 10) rectInner) pressedColour]

      it "does not draw the middle control in its pressed style" $ do
        result <- runInteractions testBounds pressedSeedCtx nested [MoveTo clickPoint] [MouseDown clickPoint]
        getDrawCommands (resultContext result) `shouldNotContain` [FillRect (insetRect (uniform 10) rectMid) pressedColour]

      it "does not draw the outermost control in its pressed style" $ do
        result <- runInteractions testBounds pressedSeedCtx nested [MoveTo clickPoint] [MouseDown clickPoint]
        getDrawCommands (resultContext result) `shouldNotContain` [FillRect (insetRect (uniform 10) testBounds) pressedColour]

    describe "with two overlapping sibling controls (not nested)" $ do
      -- Distinct rects, but overlapping around the shared click point --
      -- 'rectBack' is simply rendered first and 'rectFront' second, in the
      -- same view, with no parent/child relationship, the same way a later,
      -- visually-on-top sibling (e.g. a floating panel) would be in real
      -- layout.
      let rectBack   = Rectangle 0 0 80 80
          rectFront  = Rectangle 20 20 80 80
          clickPoint = Point 50 50 -- within both hit areas: (10,10)-(70,70) and (30,30)-(90,90)
          overlapping = do
            withBounds rectBack  (renderAt ElemA [])
            withBounds rectFront (renderAt ElemB [])

      it "draws the later (topmost) sibling in its pressed style" $ do
        result <- runInteractions testBounds pressedSeedCtx overlapping [MoveTo clickPoint] [MouseDown clickPoint]
        getDrawCommands (resultContext result) `shouldContain` [FillRect (insetRect (uniform 10) rectFront) pressedColour]

      it "does not draw the earlier (bottommost) sibling in its pressed style" $ do
        result <- runInteractions testBounds pressedSeedCtx overlapping [MoveTo clickPoint] [MouseDown clickPoint]
        getDrawCommands (resultContext result) `shouldNotContain` [FillRect (insetRect (uniform 10) rectBack) pressedColour]

  describe "no id" $ do
    -- No 'elementId' at all, unlike 'renderAt'/'renderControl'.
    let renderNoId attrs = () <$ control (resolve defaultControlConfig attrs)

    it "still draws chrome via renderStyled, inset by margin" $ do
      ctx <- snd <$> runView (renderNoId []) seedCtx
      getDrawCommands ctx `shouldContain` [FillRect (insetRect (uniform 10) testBounds) testColour]

    it "never draws its pressed style, even with the mouse held down over it" $ do
      result <- runInteractions testBounds pressedSeedCtx (renderNoId []) [] [MouseDown (Point 50 50)]
      getDrawCommands (resultContext result) `shouldNotContain` [FillRect (insetRect (uniform 10) testBounds) pressedColour]

    it "raises no focus gained event by rendering first, even though nothing else is focused" $ do
      let attrs = [onFocusGained (post ("gained" :: String))]
      result <- runInteractions testBounds seedCtx (renderNoId attrs) [] []
      resultMessages result `shouldBe` []

    it "raises no click event for a press and release over it" $ do
      let attrs = [onClicked (post ("clicked" :: String))]
      result <- runInteractions testBounds seedCtx (renderNoId attrs) [] [ClickAt (Point 50 50)]
      resultMessages result `shouldBe` []

  describe "auto-claim" $
    it "raises a focus gained event for only the first of several simultaneously-eligible controls" $ do
      let attrsA = [onFocusGained (post ("A gained" :: String))]
          attrsB = [onFocusGained (post ("B gained" :: String))]
      result <- runInteractions testBounds seedCtx (both attrsA attrsB) [] []
      resultMessages result `shouldBe` ["A gained"]

  describe "cross-element interaction" $ do
    it "reports MouseDown for the element the press started on, and MouseUp for whichever element the release happens over" $ do
      -- Mouse goes down over ElemA, is dragged (still held) onto ElemB, and
      -- released there. ElemA should only ever see MouseDown (it's not hit
      -- by the time the button comes up); ElemB should only ever see
      -- MouseUp (it wasn't hit when the button went down) -- confirming a
      -- drag begun on one element and released over another doesn't count
      -- as a click for the second.
      let attrsA =
            [ focusPolicy NotFocusable
            , onMouseEntered (post ("A entered" :: String))
            , onMouseExited  (post "A exited")
            , onMouseDown    (post "A down")
            ]
          attrsB =
            [ focusPolicy NotFocusable
            , onMouseEntered (post ("B entered" :: String))
            , onMouseUp      (post "B up")
            ]
      result <- runInteractions testBounds seedCtx (both attrsA attrsB) []
        [MouseDown onA, DragTo onB, MouseUp onB]
      resultMessages result `shouldBe` ["A entered", "A down", "A exited", "B up"]

  describe "capture suppresses hover elsewhere" $ do
    let attrsB = [focusPolicy NotFocusable, onMouseEntered (post ("B entered" :: String))]

    it "does not fire a sibling's mouse-entered event while another element holds capture" $ do
      result <- runInteractions testBounds seedCtx (both [] attrsB) []
        [MouseDown onA, DragTo onB, MouseUp onB]
      resultMessages result `shouldBe` []

    it "fires the sibling's mouse-entered event normally once capture is released" $ do
      result <- runInteractions testBounds seedCtx (both [] attrsB)
        [MouseDown onA, DragTo onB, MouseUp onB, Wait 1, MoveTo (Point 200 200)]
        [MoveTo onB]
      resultMessages result `shouldBe` ["B entered"]

  describe "click-to-focus" $ do
    let attrsA = [onFocusLost   (post ("A lost"   :: String))]
        attrsB = [onFocusGained (post ("B gained" :: String))]
        render = both attrsA attrsB

    it "does not take effect on the mouse-down's own frame" $ do
      result <- runInteractions testBounds seedCtx render [] [MouseDown onB]
      resultMessages result `shouldBe` []

    it "takes effect one frame after mouse-down, firing FocusLost/FocusGained for the right elements, without waiting for release" $ do
      result <- runInteractions testBounds seedCtx render [] [MouseDown onB, Wait 1]
      resultMessages result `shouldBe` ["A lost", "B gained"]

  describe "focusTargetOnClick" $ do
    it "does not redirect on the mouse-down's own frame" $ do
      let taggedB = [onFocusGained (post ("B gained" :: String))]
      result <- runInteractions testBounds seedCtx (renderRedirect ElemA [] ElemB taggedB) [] [MouseDown onA]
      resultMessages result `shouldBe` []

    it "does not redirect on mouse-down alone, even a frame later -- only a full click" $ do
      let taggedB = [onFocusGained (post ("B gained" :: String))]
      result <- runInteractions testBounds seedCtx (renderRedirect ElemA [] ElemB taggedB) [] [MouseDown onA, Wait 1]
      resultMessages result `shouldBe` []

    it "redirects focus to the named element one frame after a full click" $ do
      let taggedB = [onFocusGained (post ("B gained" :: String))]
      result <- runInteractions testBounds seedCtx (renderRedirect ElemA [] ElemB taggedB) [] [ClickAt onA, Wait 1]
      resultMessages result `shouldBe` ["B gained"]

    it "does not redirect focus onto a disabled element" $ do
      let taggedB = [isEnabled False, onFocusGained (post ("B gained" :: String))]
      result <- runInteractions testBounds seedCtx (renderRedirect ElemA [] ElemB taggedB) [] [ClickAt onA, Wait 1]
      resultMessages result `shouldBe` []
      contextFocus (resultContext result) `shouldBe` Nothing

  describe "keyboard navigation" $ do
    let attrsA = [onFocusLost   (post ("A lost"   :: String))]
        attrsB = [onFocusGained (post ("B gained" :: String))]
        render = both attrsA attrsB

    it "Tab gives up focus immediately, letting the next control auto-claim in the same frame" $ do
      result <- runInteractions testBounds seedCtx render [Wait 1] [Tab]
      resultMessages result `shouldBe` ["A lost", "B gained"]

    it "does not hand focus to the previous tab stop on the Shift-Tab frame itself" $ do
      result <- runInteractions testBounds seedCtx render [Wait 1] [ShiftTab]
      resultMessages result `shouldBe` []

    it "hands focus to the previous tab stop one frame after Shift-Tab" $ do
      result <- runInteractions testBounds seedCtx render [Wait 1] [ShiftTab, Wait 1]
      resultMessages result `shouldBe` ["A lost", "B gained"]

    it "does not report Tab as a key event to the control it moves focus away from" $ do
      let keyAttrs = [onKeyPressed (postWith (\k -> (show k)))]
      result <- runInteractions testBounds seedCtx (both keyAttrs []) [Wait 1] [Tab]
      resultMessages result `shouldBe` []

    it "does not report Shift-Tab as a key event to the control it moves focus away from" $ do
      let keyAttrs = [onKeyPressed (postWith (\k -> (show k)))]
      result <- runInteractions testBounds seedCtx (both keyAttrs []) [Wait 1] [ShiftTab]
      resultMessages result `shouldBe` []

    it "reports an ordinary key press with its triggering KeyEvent" $ do
      let keyAttrs = [onKeyPressed (postWith (\k -> (show k)))]
      result <- runInteractions testBounds seedCtx (renderControl keyAttrs) [Wait 1] [PressKey KeyReturn []]
      resultMessages result `shouldBe` [show (KeyEvent KeyReturn [] False)]

    describe "Shift-Tab past a disabled control" $ do
      let tagged e = [onFocusGained (post (show e ++ " gained")), onFocusLost (post (show e ++ " lost"))]
          renderWithDisabledMiddle = three (tagged ElemA) (isEnabled False : tagged ElemB) (tagged ElemC)

      it "Tab from the first control skips the disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithDisabledMiddle [Wait 1] [Tab, Wait 1]
        resultMessages result `shouldBe` ["ElemA lost", "ElemC gained"]

      it "Shift-Tab back from the last control also skips the disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithDisabledMiddle [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        resultMessages result `shouldBe` ["ElemA gained", "ElemC lost"]

    describe "Shift-Tab past a control disabled via an ambient disableWhen" $ do
      let tagged e = [onFocusGained (post (show e ++ " gained")), onFocusLost (post (show e ++ " lost"))]
          renderWithAmbientlyDisabledMiddle = do
            withBounds rectA (renderAt ElemA (tagged ElemA))
            disableWhen True (withBounds rectB (renderAt ElemB (tagged ElemB)))
            withBounds rectC (renderAt ElemC (tagged ElemC))

      it "Tab from the first control skips the ambiently-disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithAmbientlyDisabledMiddle [Wait 1] [Tab, Wait 1]
        resultMessages result `shouldBe` ["ElemA lost", "ElemC gained"]

      it "Shift-Tab back from the last control also skips the ambiently-disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithAmbientlyDisabledMiddle [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        resultMessages result `shouldBe` ["ElemA gained", "ElemC lost"]
