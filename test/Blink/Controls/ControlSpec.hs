{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ControlSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control
  ( Attribute, ControlConfig (..), FocusOptions (..), FocusPolicy (..)
  , control, defaultControlConfig, defaultFocusOptions, elementId, focusTargetOnClick, isEnabled, focusPolicy
  , onClicked, onFocusGained, onFocusLost, onKeyPressed
  , onMouseDown, onMouseEntered, onMouseExited, onMouseUp, post, postWith, resolve
  )
import Blink.Controls.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Controls.Fixtures (mkTestTheme, plainStyle, plainStyleSet, standardMetrics, testColour)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, uniform)
import Blink.Input (HitRect (..), InputState (..), Key (..), KeyEvent (..), Mouse (..), emptyInputState, emptyMouse)
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), DrawCommand (..))
import Blink.Style (StyleSet (..), Theme (..), VisualState (CommonPressed), styleBackground)
import Blink.View
import Blink.View.Context (ViewContext (ctxInput, ctxMouse))

data TestElement
  = ElemA | ElemB | ElemC
  deriving (Eq, Ord, Show)

rectA, rectB, rectC :: Rectangle
rectA = Rectangle 0 0 50 100
rectB = Rectangle 50 0 50 100
rectC = Rectangle 100 0 50 100

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testStyleSet :: StyleSet
testStyleSet = plainStyleSet (plainStyle testColour)

testTheme :: Theme TestElement
testTheme = mkTestTheme standardMetrics testStyleSet

pressedColour :: Colour
pressedColour = RGBA 1 1 1 1

-- | Like 'testTheme', but with a 'CommonPressed' override with a
-- background distinct from every other state, so a test can tell whether
-- the pressed style was actually the one drawn.
pressedTestTheme :: Theme TestElement
pressedTestTheme = testTheme
  { themeDefaultStyle =
      ( standardMetrics
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
seedCtx = emptyViewContext testBounds noInput testTheme
  where
    noInput = emptyInputState { inputMousePosition = Point 200 200 }

pressedSeedCtx :: ViewContext TestElement String
pressedSeedCtx = emptyViewContext testBounds noInput pressedTestTheme
  where
    noInput = emptyInputState { inputMousePosition = Point 200 200 }

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

-- | The draw command for a control's chrome fill, inset by the 10px margin
-- every test style here uses.
chromeFill :: Colour -> Rectangle -> DrawCommand
chromeFill colour rect = FillRect (insetRect (uniform 10) rect) colour

-- | Renders 'ElemA' and 'ElemB' via 'both', wired so A posts "A lost" when
-- it loses focus and B posts "B gained" when it gains focus -- for tracking
-- a focus handoff between them.
focusHandoffRender :: View TestElement String ()
focusHandoffRender = both [onFocusLost (post ("A lost" :: String))] [onFocusGained (post ("B gained" :: String))]

-- | Attrs that post "<e> gained"/"<e> lost" via 'onFocusGained'/'onFocusLost',
-- for tracking which element focus moves through.
tagged :: TestElement -> [Attribute']
tagged e = [onFocusGained (post (show e ++ " gained")), onFocusLost (post (show e ++ " lost"))]

spec :: Spec
spec = describe "Blink.Controls.Control.control" $ do
  controlBehaviourSpec defaultControlBehaviourConfig testBounds seedCtx ElemA (Point 5 5) hitRect (Point 200 200) renderControl

  describe "chrome" $ do
    it "draws background via renderStyled, inset by margin" $ do
      ctx <- snd <$> runView (renderControl []) seedCtx
      getDrawCommands ctx `shouldContain` [chromeFill testColour testBounds]

    it "draws in its pressed style while the mouse is held down over it" $ do
      result <- runInteractions testBounds pressedSeedCtx (renderControl []) [] [MouseDown (Point 50 50)]
      getDrawCommands (resultContext result) `shouldContain` [chromeFill pressedColour testBounds]

    describe "with a nested child control" $ do
      let rectChild = Rectangle 20 20 40 40
          containerWithChild =
            () <$ control (resolve
              (defaultControlConfig { ccContent = const (withBounds rectChild (renderAt ElemB [])) })
              [elementId ElemA])

      it "draws the child in its pressed style when the mouse is held down over it" $ do
        result <- runInteractions testBounds pressedSeedCtx containerWithChild [MoveTo (Point 40 40)] [MouseDown (Point 40 40)]
        getDrawCommands (resultContext result) `shouldContain` [chromeFill pressedColour rectChild]

      it "does not draw the container in its pressed style when the mouse is held down over the child instead" $ do
        result <- runInteractions testBounds pressedSeedCtx containerWithChild [MoveTo (Point 40 40)] [MouseDown (Point 40 40)]
        getDrawCommands (resultContext result) `shouldNotContain` [chromeFill pressedColour testBounds]

      it "still draws the container in its pressed style when the mouse is held down over empty space inside it, away from the child" $ do
        result <- runInteractions testBounds pressedSeedCtx containerWithChild [MoveTo (Point 70 70)] [MouseDown (Point 70 70)]
        getDrawCommands (resultContext result) `shouldContain` [chromeFill pressedColour testBounds]

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
        getDrawCommands (resultContext result) `shouldNotContain` [chromeFill pressedColour rectChild]

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
        getDrawCommands (resultContext result) `shouldContain` [chromeFill pressedColour rectInner]

      it "does not draw the middle control in its pressed style" $ do
        result <- runInteractions testBounds pressedSeedCtx nested [MoveTo clickPoint] [MouseDown clickPoint]
        getDrawCommands (resultContext result) `shouldNotContain` [chromeFill pressedColour rectMid]

      it "does not draw the outermost control in its pressed style" $ do
        result <- runInteractions testBounds pressedSeedCtx nested [MoveTo clickPoint] [MouseDown clickPoint]
        getDrawCommands (resultContext result) `shouldNotContain` [chromeFill pressedColour testBounds]

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
        getDrawCommands (resultContext result) `shouldContain` [chromeFill pressedColour rectFront]

      it "does not draw the earlier (bottommost) sibling in its pressed style" $ do
        result <- runInteractions testBounds pressedSeedCtx overlapping [MoveTo clickPoint] [MouseDown clickPoint]
        getDrawCommands (resultContext result) `shouldNotContain` [chromeFill pressedColour rectBack]

  describe "no id" $ do
    -- No 'elementId' at all, unlike 'renderAt'/'renderControl'.
    let renderNoId attrs = () <$ control (resolve defaultControlConfig attrs)

    it "still draws chrome via renderStyled, inset by margin" $ do
      ctx <- snd <$> runView (renderNoId []) seedCtx
      getDrawCommands ctx `shouldContain` [chromeFill testColour testBounds]

    it "never draws its pressed style, even with the mouse held down over it" $ do
      result <- runInteractions testBounds pressedSeedCtx (renderNoId []) [] [MouseDown (Point 50 50)]
      getDrawCommands (resultContext result) `shouldNotContain` [chromeFill pressedColour testBounds]

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

  describe "occluded by a popup" $ do
    -- Stands in for a popup's own registered hit-rect covering the point
    -- under test -- as "Blink.App"'s drain step would leave it in
    -- 'mouseHitRectsPrev' after a real frame, without needing a real popup
    -- or a real second frame to produce that state.
    let mousePos  = Point 50 50 -- inside ElemA's margin-inset hit area
        popupRect = Rectangle 0 0 100 100
        withMouseAt = (\ctx -> ctx { ctxInput = emptyInputState { inputMousePosition = mousePos } })
        withLastFrameRect idx floorIdx ctx = ctx
          { ctxMouse = emptyMouse
              { mouseHitRectsPrev = Map.singleton ElemB (HitRect popupRect idx)
              , mousePopupFloor   = floorIdx
              }
          }
        attrsA = [focusPolicy NotFocusable, onMouseEntered (post ("A entered" :: String))]
        enteredMessages ctx = do
          (_, ctx') <- runView (renderAt ElemA attrsA) ctx
          pure (getMessages ctx')

    it "does not fire mouse-entered when a popup covered this point last frame" $ do
      msgs <- enteredMessages (withLastFrameRect 5 5 (withMouseAt seedCtx))
      msgs `shouldBe` []

    it "fires mouse-entered normally when nothing covered this point last frame" $ do
      msgs <- enteredMessages (withMouseAt seedCtx)
      msgs `shouldBe` ["A entered"]

    it "still fires mouse-entered when the covering rect predates the popup floor (an ordinary overlapping control, not a popup)" $ do
      msgs <- enteredMessages (withLastFrameRect 1 5 (withMouseAt seedCtx))
      msgs `shouldBe` ["A entered"]

  describe "click-to-focus" $ do
    it "does not take effect on the mouse-down's own frame" $ do
      result <- runInteractions testBounds seedCtx focusHandoffRender [] [MouseDown onB]
      resultMessages result `shouldBe` []

    it "takes effect one frame after mouse-down, firing FocusLost/FocusGained for the right elements, without waiting for release" $ do
      result <- runInteractions testBounds seedCtx focusHandoffRender [] [MouseDown onB, Wait 1]
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

  describe "FocusOptions" $ do
    let notATabStop     = Focusable defaultFocusOptions { focusIsTabStop = False }
        notClickToFocus = Focusable defaultFocusOptions { focusIsClickToFocus = False }

    describe "focusIsTabStop False" $ do
      it "never auto-claims focus by rendering first, even with nothing else focused" $ do
        result <- runInteractions testBounds seedCtx
          (renderAt ElemA (focusPolicy notATabStop : tagged ElemA)) [] []
        resultMessages result `shouldBe` []

      it "is skipped by Tab, leaving it for a click-only-focusable sibling instead" $ do
        let render = do
              withBounds rectA (renderAt ElemA (focusPolicy notATabStop : tagged ElemA))
              withBounds rectB (renderAt ElemB (tagged ElemB))
        -- Setup clicks A into focus (click-to-focus is untouched) and
        -- releases the mouse, so B's auto-claim isn't contested by A still
        -- holding capture; the single Tab in the test phase then gives up
        -- A's focus and B auto-claims, immediately, in that same frame.
        result <- runInteractions testBounds seedCtx render [MouseDown onA, Wait 1, MouseUp onA, Wait 1] [Tab]
        resultMessages result `shouldBe` ["ElemA lost", "ElemB gained"]

      it "is never recorded as the previous tab stop, so Shift-Tab skips over it entirely" $ do
        let render = three (tagged ElemA) (focusPolicy notATabStop : tagged ElemB) (tagged ElemC)
        result <- runInteractions testBounds seedCtx render [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        resultMessages result `shouldBe` ["ElemA gained", "ElemC lost"]

      it "still takes focus via click, since focusIsClickToFocus is untouched" $ do
        result <- runInteractions testBounds seedCtx
          (renderAt ElemA (focusPolicy notATabStop : tagged ElemA)) [] [MouseDown onA, Wait 1]
        resultMessages result `shouldBe` ["ElemA gained"]

    describe "focusIsClickToFocus False" $ do
      it "does not take focus on mouse-down" $ do
        let render = both (tagged ElemA) (focusPolicy notClickToFocus : tagged ElemB)
        -- A auto-claims focus during setup; clicking B in the test phase
        -- must not move it, since B has opted out of click-to-focus.
        result <- runInteractions testBounds seedCtx render [Wait 1] [MouseDown onB, Wait 1]
        resultMessages result `shouldBe` []

      it "still auto-claims focus by rendering first, since focusIsTabStop is untouched" $ do
        result <- runInteractions testBounds seedCtx
          (renderAt ElemA (focusPolicy notClickToFocus : tagged ElemA)) [] []
        resultMessages result `shouldBe` ["ElemA gained"]

    it "with both flags False, is never a Tab stop or click target, but is still reachable via setFocus" $ do
      let bothFalse = Focusable defaultFocusOptions { focusIsTabStop = False, focusIsClickToFocus = False }
      result <- runInteractions testBounds seedCtx
        (renderAt ElemA (focusPolicy bothFalse : tagged ElemA)) [] [MouseDown onA, Wait 1, Tab, Wait 1]
      resultMessages result `shouldBe` []
      contextFocus (resultContext result) `shouldBe` Nothing

  describe "keyboard navigation" $ do
    it "Tab gives up focus immediately, letting the next control auto-claim in the same frame" $ do
      result <- runInteractions testBounds seedCtx focusHandoffRender [Wait 1] [Tab]
      resultMessages result `shouldBe` ["A lost", "B gained"]

    it "does not hand focus to the previous tab stop on the Shift-Tab frame itself" $ do
      result <- runInteractions testBounds seedCtx focusHandoffRender [Wait 1] [ShiftTab]
      resultMessages result `shouldBe` []

    it "hands focus to the previous tab stop one frame after Shift-Tab" $ do
      result <- runInteractions testBounds seedCtx focusHandoffRender [Wait 1] [ShiftTab, Wait 1]
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
      let renderWithDisabledMiddle = three (tagged ElemA) (isEnabled False : tagged ElemB) (tagged ElemC)

      it "Tab from the first control skips the disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithDisabledMiddle [Wait 1] [Tab, Wait 1]
        resultMessages result `shouldBe` ["ElemA lost", "ElemC gained"]

      it "Shift-Tab back from the last control also skips the disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithDisabledMiddle [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        resultMessages result `shouldBe` ["ElemA gained", "ElemC lost"]

    describe "Shift-Tab past a control disabled via an ambient disableWhen" $ do
      let renderWithAmbientlyDisabledMiddle = do
            withBounds rectA (renderAt ElemA (tagged ElemA))
            disableWhen True (withBounds rectB (renderAt ElemB (tagged ElemB)))
            withBounds rectC (renderAt ElemC (tagged ElemC))

      it "Tab from the first control skips the ambiently-disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithAmbientlyDisabledMiddle [Wait 1] [Tab, Wait 1]
        resultMessages result `shouldBe` ["ElemA lost", "ElemC gained"]

      it "Shift-Tab back from the last control also skips the ambiently-disabled middle one" $ do
        result <- runInteractions testBounds seedCtx renderWithAmbientlyDisabledMiddle [Wait 1, Tab, Wait 1] [ShiftTab, Wait 1]
        resultMessages result `shouldBe` ["ElemA gained", "ElemC lost"]
