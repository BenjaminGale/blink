{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls2Spec (spec) where

import Control.Monad (forM_)
import Test.Hspec

import Blink.Controls2
  ( Attr
  , ControlEvent (..)
  , FocusOnClick (..)
  , HasControlEvent (..)
  , activatable
  , applyFocus
  , applyMouseOver
  , configAny
  , configure
  , control
  , fire
  , focusOnClick
  , isActivatedBy
  , isKeyPressed
  , isMouseOver
  , isPressed
  , getStyle
  , text
  , label
  , measureChrome
  , ProgressValue (..)
  , progressBar
  , bandSpeed
  , progress
  , onEvent
  , post
  , postWith
  , perform
  , performWith
  , forward
  , translate
  , translateWith
  , onFocusGained
  , onFocusLost
  , onMouseEnter
  , onMouseExit
  , renderChrome
  , tabStop
  , whenFocused
  , button
  , onClick
  , CheckboxPart (..)
  , onToggle
  , checked
  , renderCheckboxGlyph
  , checkbox
  , onInput
  , onSubmit
  , inputFilter
  , displayFilter
  , textInputControl
  , SliderPart (..)
  , onChange
  , arrowStep
  , thumbRatio
  , orientation
  , value
  , slider
  , ScrollBarPart (..)
  , scrollBar
  , ViewportPart (..)
  , contentSize
  , viewport
  , virtualContent
  , onSelect
  , items
  , selected
  , selector
  , radioGroup
  , ListBoxPart (..)
  , listBox
  )
import Blink.ControlsTestSupport
  ( TestElement (..)
  , bgRect
  , drawnTexts
  , contentRect
  , controlRect
  , dispatchCount
  , getFocused
  , insidePoints
  , mouseAt
  , noInput
  , outsidePoints
  , settle
  , testBorderColour
  , testColour
  , testStyle
  , testTheme
  , testThemeWithBorder
  , withButtonReleased
  , withFocus
  )
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Blink.Geometry (Orientation (..), Point (..), Rectangle (..), Size (..), noBorder, uniform, uniformBorder)
import Blink.Input (InputState (..), Key (..), KeyEvent (..), Modifier (..))
import Blink.Layout (Length (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI hiding (getStyle, isPressed)

-- | 'Blink.ControlsTestSupport.mkCtx' fixes @msg ~ ()@; several suites below
-- (anything using 'fire' to observe emitted values) need other message
-- types, so this stays polymorphic.
mkCtxFor :: InputState -> UIContext TestElement msg
mkCtxFor input = emptyUIContext controlRect input testTheme noOpTextMeasurer

-- | Spies on the shared 'ControlEvent's a primitive fires, by emitting the
-- event itself as the message.
newtype Probe = Probe ControlEvent deriving (Eq, Show)

instance HasControlEvent Probe where
  liftControl = Probe
  matchControl (Probe ce) = Just ce

captureAttrs :: Attr e Probe Probe cfg
captureAttrs = onEvent (\ev -> [OutMsg ev])

noProbeAttrs :: [Attr e Probe Probe ()]
noProbeAttrs = []

isStrokeRect :: DrawCommand -> Bool
isStrokeRect (StrokeBorder {}) = True
isStrokeRect _                 = False

data DummyEvent = Ping | Pong deriving (Eq, Show)

data DummyConfig = DummyConfig { dummyFlag :: Bool, dummyCount :: Int }
  deriving (Eq, Show)

defaultDummyConfig :: DummyConfig
defaultDummyConfig = DummyConfig { dummyFlag = False, dummyCount = 0 }

setFlag :: Bool -> Attr e ev msg DummyConfig
setFlag b = configAny $ \cfg -> cfg { dummyFlag = b }

addCount :: Int -> Attr e ev msg DummyConfig
addCount n = configAny $ \cfg -> cfg { dummyCount = dummyCount cfg + n }

onPing :: msg -> Attr e DummyEvent msg cfg
onPing msg = onEvent $ \ev -> case ev of
  Ping -> [OutMsg msg]
  Pong -> []

runFire :: Ord e => [Attr e ev msg cfg] -> [ev] -> UIContext e msg -> IO (UIContext e msg)
runFire attrs evs ctx = snd <$> runUI (fire attrs evs) ctx

dummyLabel :: DummyEvent -> String
dummyLabel Ping = "Ping"
dummyLabel Pong = "Pong"

-- checkbox takes a tagging function rather than a single element ID, so its
-- tests use CheckboxPart as the element type directly and 'id' as the
-- tagging function — the same convention the old suite used for scrollBar.
-- Zero margin/padding throughout so the whole checkboxRect is hittable, and
-- the composite's own outer bounds (not just the glyph) span the full row.
checkboxRect :: Rectangle
checkboxRect = Rectangle 0 0 100 20

checkboxStyle :: Style
checkboxStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignCenter
  , styleMargin       = uniform 0
  , stylePadding      = uniform 0
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

checkboxStyleSet :: StyleSet
checkboxStyleSet = StyleSet
  { styleSetNormal   = checkboxStyle
  , styleSetHovered  = checkboxStyle
  , styleSetPressed  = checkboxStyle
  , styleSetFocused  = checkboxStyle
  , styleSetDisabled = checkboxStyle
  }

checkboxTheme :: Theme CheckboxPart
checkboxTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = checkboxStyleSet }

mkCheckboxCtx :: InputState -> UIContext CheckboxPart Bool
mkCheckboxCtx input = emptyUIContext checkboxRect input checkboxTheme noOpTextMeasurer

runCheckbox :: Bool -> UIContext CheckboxPart Bool -> IO (UIContext CheckboxPart Bool)
runCheckbox isChecked ctx = fmap (settle . snd) $ runUI (checkbox id [text "Notify me", checked isChecked, onToggle (postWith id)]) ctx

glyphPoint :: Point
glyphPoint = Point 10 10

labelPoint :: Point
labelPoint = Point 50 10

mkTextCtx :: Text -> InputState -> UIContext TestElement Text
mkTextCtx _value input = emptyUIContext controlRect input testTheme noOpTextMeasurer

-- A measurer with a fixed 20px advance per character, for scroll tests
-- where noOpTextMeasurer's all-zero offsets can't produce a cursor position
-- past the viewport edge.
fixedCharWidth :: TextMeasurer
fixedCharWidth = TextMeasurer
  { tmCharOffset   = \_ n -> pure (fromIntegral n * 20)
  , tmCharAtOffset = \_ x -> pure (round (x / 20))
  , tmTextSize     = \_ -> pure (Size 0 0)
  }

mkTextCtxWith :: TextMeasurer -> Text -> InputState -> UIContext TestElement Text
mkTextCtxWith measurer _value input = emptyUIContext controlRect input testTheme measurer

runTextField :: Text -> UIContext TestElement Text -> IO (UIContext TestElement Text)
runTextField v ctx = fmap (settle . snd) $ runUI (textInputControl TestControl [text v, onInput (postWith id)]) ctx

runNumberField :: Text -> UIContext TestElement Text -> IO (UIContext TestElement Text)
runNumberField v ctx = fmap (settle . snd) $ runUI (textInputControl TestControl [text v, inputFilter (T.filter isDigit), onInput (postWith id)]) ctx

runPasswordField :: Text -> UIContext TestElement Text -> IO (UIContext TestElement Text)
runPasswordField v ctx = fmap (settle . snd) $ runUI (textInputControl TestControl [text v, displayFilter (T.map (const '•')), onInput (postWith id)]) ctx

-- slider takes a tagging function, so its tests use SliderPart as the
-- element type directly and 'id' as the tagging function (matching the
-- checkbox/scrollBar convention). App state IS the slider's value.
-- Rect is 200x30; with zero margin/padding the thumb is 30x30, giving a
-- travel range of 170px. mouseToTrackPos centres the thumb on the cursor:
--   value = clamp 0 1 ((mouseX - 15) / 170)
-- Key positions: mouseX=15 -> 0.0, mouseX=100 -> 0.5, mouseX=185 -> 1.0.
sliderStyleSet :: StyleSet
sliderStyleSet = StyleSet
  { styleSetNormal   = checkboxStyle
  , styleSetHovered  = checkboxStyle
  , styleSetPressed  = checkboxStyle
  , styleSetFocused  = checkboxStyle
  , styleSetDisabled = checkboxStyle
  }

sliderTheme :: Theme SliderPart
sliderTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = sliderStyleSet }

sliderRect :: Rectangle
sliderRect = Rectangle 0 0 200 30

runSlider :: Orientation -> Double -> InputState -> IO (UIContext SliderPart Double)
runSlider ori val input =
  fmap (settle . snd) $ runUI (slider id [orientation ori, value val, onChange (postWith id)])
    (emptyUIContext sliderRect input sliderTheme noOpTextMeasurer)

-- 20x200 vertical scrollbar with a 0.25 thumb ratio: buttons at y 0-20 and
-- 180-200, track at y 20-180 (zero margin/padding throughout, same as
-- checkboxStyle, so those pixel boundaries are exact).
scrollBarTheme :: Theme ScrollBarPart
scrollBarTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = checkboxStyleSet }

scrollBarRect :: Rectangle
scrollBarRect = Rectangle 0 0 20 200

mkScrollBarCtx :: Double -> InputState -> UIContext ScrollBarPart msg
mkScrollBarCtx pos input =
  let base = emptyUIContext scrollBarRect input scrollBarTheme noOpTextMeasurer
  in base { ctxElements = (ctxElements base) { elmScrollStates = Map.singleton ScrollTrack (ScrollState pos) } }

runScrollBar :: UIContext ScrollBarPart () -> IO (UIContext ScrollBarPart ())
runScrollBar = fmap (settle . snd) . runUI (scrollBar id [orientation Vertical, thumbRatio 0.25])

scrollBarPos :: UIContext ScrollBarPart () -> Double
scrollBarPos = scrollPosition . Map.findWithDefault (ScrollState 0) ScrollTrack . elmScrollStates . ctxElements

-- viewport tests use a 200x100 outer rect throughout, so the same
-- content-size scenarios (fits / H-only / V-only / both) produce the same
-- scrollbar geometry across every describe block below:
--   fits:    Size 100 50  -> no scrollbars,               vp 200x100
--   H-only:  Size 300 50  -> hBar only,                   vp 200x84
--   V-only:  Size 50  200 -> vBar only,                   vp 184x100
--   both:    Size 300 200 -> hBar (0,84,184,16) + vBar (184,0,16,84), vp 184x84
data ViewportElem = VPPart ViewportPart | VPChild
  deriving (Eq, Ord, Show)

vpTheme :: Theme ViewportElem
vpTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = checkboxStyleSet }

vpOuterRect :: Rectangle
vpOuterRect = Rectangle 0 0 200 100

runViewportDraw :: Size -> UIContext ViewportElem () -> IO (UIContext ViewportElem ())
runViewportDraw sz ctx = fmap (settle . snd) $ runUI (viewport VPPart [contentSize sz] (fillRect testColour)) ctx

vpCtx :: UIContext ViewportElem ()
vpCtx = emptyUIContext vpOuterRect noInput vpTheme noOpTextMeasurer

runViewportChild :: Size -> [Attr ViewportElem Probe Probe ()] -> Point -> IO (UIContext ViewportElem Probe)
runViewportChild sz childAttrs mousePos =
  let input = noInput { inputMousePosition = mousePos }
      ctx   = emptyUIContext vpOuterRect input vpTheme noOpTextMeasurer
  in fmap snd $ runUI (viewport VPPart [contentSize sz] (control VPChild childAttrs (pure ()))) ctx

-- selector/radioGroup tests use element type Int directly (mkId = id), the
-- same convention as checkbox/scrollBar/slider.
radioGroupTheme :: Theme Int
radioGroupTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = checkboxStyleSet }

radioGroupRect :: Rectangle
radioGroupRect = Rectangle 0 0 100 90

radioItems :: [(String, Text)]
radioItems = [("a", "Alpha"), ("b", "Beta"), ("c", "Gamma")]

mkRadioGroupCtx :: InputState -> UIContext Int String
mkRadioGroupCtx input = emptyUIContext radioGroupRect input radioGroupTheme noOpTextMeasurer

runRadioGroup :: String -> UIContext Int String -> IO (UIContext Int String)
runRadioGroup sel = fmap (settle . snd) . runUI (radioGroup id [items radioItems, selected sel, onSelect (postWith id)])

-- listBox setup: 100x60 viewport, 20px items -> 3 fully visible at a time,
-- 6 items total -> content is twice the viewport height, so scrolling is
-- exercised. mkId = id, so element IDs are ListBoxPart values directly.
listBoxTheme :: Theme ListBoxPart
listBoxTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = checkboxStyleSet }

listBoxRect :: Rectangle
listBoxRect = Rectangle 0 0 100 60

listBoxItemHeight :: Double
listBoxItemHeight = 20

listBoxItems :: [(Int, Text)]
listBoxItems =
  [ (0, "Item0"), (1, "Item1"), (2, "Item2")
  , (3, "Item3"), (4, "Item4"), (5, "Item5")
  ]

listBoxRenderItem :: ListBoxPart -> Bool -> (Int, Text) -> UI ListBoxPart Int ()
listBoxRenderItem _eid isSelected (_val, lbl) =
  drawText testColour AlignLeft ((if isSelected then "SEL:" else "UNSEL:") <> lbl)

mkListBoxCtx :: InputState -> UIContext ListBoxPart Int
mkListBoxCtx input = emptyUIContext listBoxRect input listBoxTheme noOpTextMeasurer

runListBox :: Int -> UIContext ListBoxPart Int -> IO (UIContext ListBoxPart Int)
runListBox sel = fmap (settle . snd) . runUI (listBox id listBoxItemHeight [items listBoxItems, selected sel, onSelect (postWith id)] listBoxRenderItem)

withListBoxScroll :: Double -> UIContext ListBoxPart Int -> UIContext ListBoxPart Int
withListBoxScroll frac ctx = ctx { ctxElements = (ctxElements ctx) { elmScrollStates = Map.singleton (ListBoxScroll ScrollTrack) (ScrollState frac) } }

listBoxScrollFrac :: UIContext ListBoxPart Int -> Double
listBoxScrollFrac ctx = scrollPosition (Map.findWithDefault (ScrollState 0) (ListBoxScroll ScrollTrack) (elmScrollStates (ctxElements ctx)))

spec :: Spec
spec = describe "Blink.Controls2" $ do
  describe "configure" $ do
    it "returns the base config unchanged when there are no attrs" $
      configure defaultDummyConfig ([] :: [Attr TestElement DummyEvent () DummyConfig])
        `shouldBe` defaultDummyConfig

    it "applies a single Config attr" $
      dummyFlag (configure defaultDummyConfig [setFlag True]) `shouldBe` True

    it "folds multiple Config attrs left to right" $
      dummyCount (configure defaultDummyConfig [addCount 3, addCount 4]) `shouldBe` 7

    it "a later Config attr overrides an earlier one for the same field" $
      dummyFlag (configure defaultDummyConfig [setFlag True, setFlag False]) `shouldBe` False

    it "ignores On attrs entirely" $
      dummyFlag (configure defaultDummyConfig [onPing (1 :: Int), setFlag True]) `shouldBe` True

  describe "fire" $ do
    it "emits a message when a handler matches the event" $ do
      ctx <- runFire [onPing (1 :: Int)] [Ping] (mkCtxFor noInput)
      getMessages ctx `shouldBe` [1]

    it "emits nothing when no handler matches the event" $ do
      ctx <- runFire [onPing (1 :: Int)] [Pong] (mkCtxFor noInput)
      getMessages ctx `shouldBe` []

    it "emits nothing when the attribute list is empty" $ do
      ctx <- runFire ([] :: [Attr TestElement DummyEvent Int DummyConfig]) [Ping] (mkCtxFor noInput)
      getMessages ctx `shouldBe` []

    it "runs in event order, then handler-list order within an event" $ do
      let attrs =
            [ onEvent (\ev -> [OutMsg (dummyLabel ev <> "-a")])
            , onEvent (\ev -> [OutMsg (dummyLabel ev <> "-b")])
            ]
      ctx <- runFire attrs [Ping, Pong] (mkCtxFor noInput)
      getMessages ctx `shouldBe` ["Ping-a", "Ping-b", "Pong-a", "Pong-b"]

    it "dispatches OutUi effects via emitUi rather than as messages" $ do
      let attrs = [onEvent (const [OutUi (ScrollTo TestControl 0.5)])]
      ctx <- runFire attrs [Ping] (mkCtxFor noInput :: UIContext TestElement Int)
      getUiEffects ctx `shouldBe` [ScrollTo TestControl 0.5]
      getMessages ctx `shouldBe` []

    it "a single handler can fan out to both a message and a UiEffect" $ do
      let attrs = [onEvent (const [OutMsg (1 :: Int), OutUi (ScrollTo TestControl 0.5)])]
      ctx <- runFire attrs [Ping] (mkCtxFor noInput)
      getMessages ctx `shouldBe` [1]
      getUiEffects ctx `shouldBe` [ScrollTo TestControl 0.5]

  describe "onEvent" $
    it "builds a handler with full access to Out, usable like any other On attr" $ do
      ctx <- runFire [onPing (1 :: Int)] [Ping] (mkCtxFor noInput)
      getMessages ctx `shouldBe` [1]

  describe "post" $
    it "emits the given message, ignoring the triggering event's data" $
      (post (1 :: Int) Ping :: [Out TestElement Int]) `shouldBe` [OutMsg 1]

  describe "postWith" $
    it "emits a message derived from the triggering event's data" $
      (postWith dummyLabel Ping :: [Out TestElement String]) `shouldBe` [OutMsg "Ping"]

  describe "perform" $
    it "queues the given UiEffect, ignoring the triggering event's data" $
      (perform (ScrollTo TestControl 0.5) Ping :: [Out TestElement Int]) `shouldBe` [OutUi (ScrollTo TestControl 0.5)]

  describe "performWith" $
    it "queues a UiEffect derived from the triggering event's data" $
      (performWith (ScrollTo TestControl) (0.5 :: Double) :: [Out TestElement Int]) `shouldBe` [OutUi (ScrollTo TestControl 0.5)]

  describe "forward" $ do
    it "calls the matching handler in the given attrs list" $
      forward ([onFocusGained (post "gained")] :: [Attr TestElement Probe String ()]) FocusGained
        `shouldBe` [OutMsg "gained"]

    it "produces nothing when no handler in the attrs list matches" $
      forward ([onFocusGained (post "gained")] :: [Attr TestElement Probe String ()]) FocusLost
        `shouldBe` []

  describe "translate" $ do
    it "raises the given event against another attrs list, ignoring the triggering data" $
      (translate [onPing (1 :: Int)] Ping () :: [Out TestElement Int]) `shouldBe` [OutMsg 1]

    it "produces nothing when no handler in the attrs list matches the given event" $
      (translate [onPing (1 :: Int)] Pong () :: [Out TestElement Int]) `shouldBe` []

  describe "translateWith" $ do
    let toDummy :: Int -> DummyEvent
        toDummy n = if n > 0 then Ping else Pong

    it "maps the triggering data through f and raises the result against another attrs list" $
      (translateWith toDummy [onPing (1 :: Int)] 5 :: [Out TestElement Int]) `shouldBe` [OutMsg 1]

    it "produces nothing when the mapped event has no matching handler" $
      (translateWith toDummy [onPing (1 :: Int)] (-1) :: [Out TestElement Int]) `shouldBe` []

  describe "configAny" $
    it "builds a config attr usable like any other, for a control author's own cfg type" $
      dummyCount (configure defaultDummyConfig [configAny (\cfg -> cfg { dummyCount = 5 })])
        `shouldBe` 5

  describe "isMouseOver" $ do
    forM_ insidePoints $ \(desc, pt) ->
      it ("is True " <> desc) $ do
        (result, _) <- runUI (isMouseOver TestControl) (mkCtxFor (mouseAt pt False []) :: UIContext TestElement ())
        result `shouldBe` True

    forM_ outsidePoints $ \(desc, pt) ->
      it ("is False " <> desc) $ do
        (result, _) <- runUI (isMouseOver TestControl) (mkCtxFor (mouseAt pt False []) :: UIContext TestElement ())
        result `shouldBe` False

    it "several elements can each independently be over at once (geometric, not last-writer-wins)" $ do
      let ctx = mkCtxFor (mouseAt (Point 50 50) False []) :: UIContext TestElement ()
      (a, _) <- runUI (isMouseOver TestControl) ctx
      (b, _) <- runUI (isMouseOver OtherControl) ctx
      (a, b) `shouldBe` (True, True)

  describe "hover/pressed styling" $ do
    -- Regression coverage: getStyle/isPressed must resolve hover/pressed
    -- priority from geometric isMouseOver, not the legacy single-owner
    -- hover field this module never writes to (setHovered is never called
    -- by applyMouseOver). Every other test theme in this file uses the same
    -- style value for all five variants, which makes this bug invisible to
    -- them; this theme uses a distinct colour per variant specifically so
    -- the wrong-variant case is observable.
    let distinctStyle c = testStyle { styleBackground = c }
        distinctStyleSet = StyleSet
          { styleSetNormal   = distinctStyle (RGBA 0 0 0 1)
          , styleSetHovered  = distinctStyle (RGBA 1 0 0 1)
          , styleSetPressed  = distinctStyle (RGBA 0 1 0 1)
          , styleSetFocused  = distinctStyle (RGBA 0 0 1 1)
          , styleSetDisabled = distinctStyle (RGBA 1 1 1 1)
          }
        distinctTheme = testTheme { themeElementStyles = Map.fromList [(TestControl, distinctStyleSet), (OtherControl, distinctStyleSet)] }
        ctxWith input = emptyUIContext controlRect input distinctTheme noOpTextMeasurer :: UIContext TestElement ()

    it "getStyle resolves the Hovered variant when the mouse is over the control" $ do
      (s, _) <- runUI (getStyle TestControl) (ctxWith (mouseAt (Point 50 50) False []))
      styleBackground s `shouldBe` RGBA 1 0 0 1

    it "getStyle resolves the Normal variant when the mouse is not over the control" $ do
      (s, _) <- runUI (getStyle TestControl) (ctxWith (mouseAt (Point 200 200) False []))
      styleBackground s `shouldBe` RGBA 0 0 0 1

    it "isPressed is True when the mouse is over the control and the button is held" $ do
      (result, _) <- runUI (isPressed TestControl) (ctxWith (mouseAt (Point 50 50) True []))
      result `shouldBe` True

    it "isPressed is False when the mouse is over the control but the button is not held" $ do
      (result, _) <- runUI (isPressed TestControl) (ctxWith (mouseAt (Point 50 50) False []))
      result `shouldBe` False

  describe "applyMouseOver" $ do
    it "fires MouseEntered the first frame the mouse is over" $ do
      (_, ctx) <- runUI (applyMouseOver TestControl [captureAttrs]) (mkCtxFor (mouseAt (Point 50 50) False []))
      getMessages ctx `shouldBe` [Probe MouseEntered]

    it "does not fire MouseEntered again on a later frame while still over" $ do
      (_, ctx1) <- runUI (applyMouseOver TestControl [captureAttrs]) (mkCtxFor (mouseAt (Point 50 50) False []))
      let ctx2 = nextFrameContext controlRect (mouseAt (Point 50 50) False []) ctx1
      (_, ctx3) <- runUI (applyMouseOver TestControl [captureAttrs]) ctx2
      getMessages ctx3 `shouldBe` []

    it "fires MouseExited on the frame the mouse leaves after being over" $ do
      (_, ctx1) <- runUI (applyMouseOver TestControl [captureAttrs]) (mkCtxFor (mouseAt (Point 50 50) False []))
      let ctx2 = nextFrameContext controlRect (mouseAt (Point 200 200) False []) ctx1
      (_, ctx3) <- runUI (applyMouseOver TestControl [captureAttrs]) ctx2
      getMessages ctx3 `shouldBe` [Probe MouseExited]

    it "fires nothing when the mouse was never over" $ do
      (_, ctx) <- runUI (applyMouseOver TestControl [captureAttrs]) (mkCtxFor (mouseAt (Point 200 200) False []))
      getMessages ctx `shouldBe` []

    it "acquires hot capture when hit and the button is down" $ do
      (_, ctx) <- runUI (applyMouseOver TestControl noProbeAttrs) (mkCtxFor (mouseAt (Point 50 50) True []))
      ixnCaptured (ctxInteraction ctx) `shouldBe` Just TestControl

    it "does not register mouse-over, or fire enter, when disabled" $ do
      let ctx0 = mkCtxFor (mouseAt (Point 50 50) False []) :: UIContext TestElement Probe
      (_, ctx) <- runUI (applyMouseOver TestControl [captureAttrs]) (ctx0 { ctxDisabled = True })
      getMessages ctx `shouldBe` []

    it "fires via onMouseEnter when the mouse enters" $ do
      let attrs = [onMouseEnter (post "entered")] :: [Attr TestElement Probe String ()]
      (_, ctx) <- runUI (applyMouseOver TestControl attrs) (mkCtxFor (mouseAt (Point 50 50) False []))
      getMessages ctx `shouldBe` ["entered"]

    it "fires via onMouseExit when the mouse leaves after being over" $ do
      let enterAttrs = [] :: [Attr TestElement Probe String ()]
          exitAttrs  = [onMouseExit (post "exited")] :: [Attr TestElement Probe String ()]
      (_, ctx1) <- runUI (applyMouseOver TestControl enterAttrs) (mkCtxFor (mouseAt (Point 50 50) False []))
      let ctx2 = nextFrameContext controlRect (mouseAt (Point 200 200) False []) ctx1
      (_, ctx3) <- runUI (applyMouseOver TestControl exitAttrs) ctx2
      getMessages ctx3 `shouldBe` ["exited"]

    it "several elements can each register mouse-over in the same frame" $ do
      let ctx0 = mkCtxFor (mouseAt (Point 50 50) False []) :: UIContext TestElement Probe
      (_, ctx1) <- runUI (applyMouseOver TestControl noProbeAttrs) ctx0
      (_, ctx2) <- runUI (applyMouseOver OtherControl noProbeAttrs) ctx1
      let ctx3 = nextFrameContext controlRect noInput ctx2
      (a, _) <- runUI (wasMouseOverLastFrame TestControl) ctx3
      (b, _) <- runUI (wasMouseOverLastFrame OtherControl) ctx3
      (a, b) `shouldBe` (True, True)

  describe "applyFocus" $ do
    describe "default (FocusSelf)" $ do
      let run attrs ctx = snd <$> runUI (applyFocus TestControl attrs) ctx
          noAttrs = [] :: [Attr TestElement Probe Probe ()]

      it "receives focus when nothing else is focused" $ do
        ctx' <- run noAttrs (mkCtxFor noInput)
        getFocused ctx' `shouldBe` Just TestControl

      it "does not take focus from another element" $ do
        ctx' <- run noAttrs (withFocus (Just OtherControl) (mkCtxFor noInput))
        getFocused ctx' `shouldBe` Just OtherControl

      it "receives focus when clicked" $ do
        ctx' <- run noAttrs (withFocus (Just OtherControl) (withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False []))))
        getFocused ctx' `shouldBe` Just TestControl

      it "does not steal focus when the mouse is released on it after dragging from another element" $ do
        let base = withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False []))
            ctx  = base { ctxInteraction = (ctxInteraction base) { ixnCaptured = Just OtherControl } }
        ctx' <- run noAttrs ctx
        getFocused ctx' `shouldBe` Nothing

      it "retains focus on the previously focused element when a drag releases elsewhere" $ do
        let base = withFocus (Just OtherControl) (withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])))
            ctx  = base { ctxInteraction = (ctxInteraction base) { ixnCaptured = Just OtherControl } }
        ctx' <- snd <$> runUI (applyFocus OtherControl noAttrs) ctx
        getFocused ctx' `shouldBe` Just OtherControl

      it "fires FocusGained via onFocusGained when focus is gained" $ do
        ctx' <- run ([onFocusGained (post "gained")] :: [Attr TestElement Probe String ()]) (mkCtxFor noInput)
        getMessages ctx' `shouldBe` ["gained"]

      it "fires FocusLost via onFocusLost when focus is lost to a Tab press" $ do
        -- This is the reason focus and tab navigation are one primitive: a
        -- Tab-driven loss is only visible to a bracket spanning both, since
        -- applyFocus alone would return before the loss happens, and by the
        -- time anything ran again this same element would have already
        -- auto-reclaimed focus, masking the transition.
        let base = withFocus (Just TestControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyTab []] })
        ctx' <- run ([onFocusLost (post "lost")] :: [Attr TestElement Probe String ()]) base
        getMessages ctx' `shouldBe` ["lost"]

      it "fires nothing when focus is retained" $ do
        ctx' <- run [captureAttrs] (withFocus (Just TestControl) (mkCtxFor noInput))
        getMessages ctx' `shouldBe` []

      it "fires nothing when it stays unfocused" $ do
        ctx' <- run [captureAttrs] (withFocus (Just OtherControl) (mkCtxFor noInput))
        getMessages ctx' `shouldBe` []

    describe "tab navigation" $ do
      let run attrs ctx = snd <$> runUI (applyFocus TestControl attrs) ctx
          noAttrs = [] :: [Attr TestElement Probe Probe ()]

      it "clears focus when Tab is pressed while focused" $ do
        ctx' <- run noAttrs (withFocus (Just TestControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyTab []] }))
        getFocused ctx' `shouldBe` Nothing

      it "passes focus to the previous tab stop when Shift+Tab is pressed" $ do
        let base = withFocus (Just TestControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyTab [Shift]] })
            ctx  = base { ctxInteraction = (ctxInteraction base) { ixnPrevTabStop = Just OtherControl } }
        ctx' <- run noAttrs ctx
        getFocused ctx' `shouldBe` Just OtherControl

      it "registers itself as the previous tab stop by default" $ do
        ctx' <- run noAttrs (mkCtxFor noInput)
        ixnPrevTabStop (ctxInteraction ctx') `shouldBe` Just TestControl

      it "does not register itself as the previous tab stop when tabStop is False" $ do
        ctx' <- run ([tabStop False] :: [Attr TestElement Probe Probe ()]) (mkCtxFor noInput)
        ixnPrevTabStop (ctxInteraction ctx') `shouldBe` Nothing

      it "a tabStop-False control leaves the previous tab-stop record unchanged" $ do
        ctx0 <- run noAttrs (mkCtxFor noInput)
        ctx1 <- snd <$> runUI (applyFocus OtherControl ([tabStop False] :: [Attr TestElement Probe Probe ()])) ctx0
        ixnPrevTabStop (ctxInteraction ctx1) `shouldBe` Just TestControl

      it "does not consume Tab or lose focus when disabled while focused" $ do
        let disabledCtx = (withFocus (Just TestControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyTab []] })) { ctxDisabled = True }
        ctx' <- run noAttrs disabledCtx
        getFocused ctx' `shouldBe` Just TestControl

    describe "focusOnClick (FocusTarget)" $ do
      let attrs = [focusOnClick (FocusTarget OtherControl)] :: [Attr TestElement Probe Probe ()]

      it "does not auto-claim focus when nothing is focused" $ do
        ctx' <- snd <$> runUI (applyFocus TestControl attrs) (mkCtxFor noInput)
        getFocused ctx' `shouldBe` Nothing

      it "gives focus to the target when clicked, not to itself" $ do
        ctx' <- snd <$> runUI (applyFocus TestControl attrs) (withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])))
        getFocused ctx' `shouldBe` Just OtherControl

    describe "focusOnClick (NoFocus)" $ do
      let attrs = [focusOnClick NoFocus] :: [Attr TestElement Probe Probe ()]

      it "does not auto-claim focus when nothing is focused" $ do
        ctx' <- snd <$> runUI (applyFocus TestControl attrs) (mkCtxFor noInput)
        getFocused ctx' `shouldBe` Nothing

      it "does not take focus when clicked" $ do
        ctx' <- snd <$> runUI (applyFocus TestControl attrs) (withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])))
        getFocused ctx' `shouldBe` Nothing

      it "still retains focus if it already holds it" $ do
        ctx' <- snd <$> runUI (applyFocus TestControl attrs) (withFocus (Just TestControl) (mkCtxFor noInput))
        getFocused ctx' `shouldBe` Just TestControl

  describe "measureChrome" $ do
    it "sums margin, border, and padding on each axis" $ do
      ((Exactly dw, Exactly dh), _) <- runUI (measureChrome TestControl) (mkCtxFor noInput :: UIContext TestElement ())
      (dw, dh) `shouldBe` (30, 30)

    it "includes border width when a border is set" $ do
      let ctx = (mkCtxFor noInput :: UIContext TestElement ()) { ctxTheme = testThemeWithBorder }
      ((Exactly dw, Exactly dh), _) <- runUI (measureChrome TestControl) ctx
      (dw, dh) `shouldBe` (32, 32)

  describe "renderChrome" $ do
    let run ctx = snd <$> runUI (renderChrome TestControl (pure ())) ctx

    it "does not draw a background in the margin area" $ do
      ctx' <- run (mkCtxFor noInput :: UIContext TestElement ())
      getDrawCommands ctx' `shouldNotContain` [FillRect controlRect testColour]

    it "fills its background area" $ do
      ctx' <- run (mkCtxFor noInput :: UIContext TestElement ())
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    it "clips content to its padding area" $ do
      ctx' <- run (mkCtxFor noInput :: UIContext TestElement ())
      getDrawCommands ctx' `shouldContain` [PushClip contentRect]

    it "does not draw a border when borderColour is Nothing" $ do
      ctx' <- run (mkCtxFor noInput :: UIContext TestElement ())
      filter isStrokeRect (getDrawCommands ctx') `shouldBe` []

    it "draws a border when borderColour is set" $ do
      let ctx = (mkCtxFor noInput :: UIContext TestElement ()) { ctxTheme = testThemeWithBorder }
      ctx' <- run ctx
      getDrawCommands ctx' `shouldContain` [StrokeBorder bgRect testBorderColour (uniformBorder 1)]

  describe "control" $ do
    it "composes mouse-over, focus, tab navigation, and chrome for a single interactive element" $ do
      let ctx = withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False []))
      ctx' <- snd <$> runUI (control TestControl ([] :: [Attr TestElement Probe Probe ()]) (pure ())) ctx
      getFocused ctx' `shouldBe` Just TestControl
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    it "threads focusOnClick through to the underlying applyFocus" $ do
      let attrs = [focusOnClick (FocusTarget OtherControl)] :: [Attr TestElement Probe Probe ()]
          ctx   = withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False []))
      ctx' <- snd <$> runUI (control TestControl attrs (pure ())) ctx
      getFocused ctx' `shouldBe` Just OtherControl

    it "runs content within the padded content rectangle" $ do
      ctx' <- snd <$> runUI (control TestControl ([] :: [Attr TestElement Probe Probe ()]) (fillRect testColour)) (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect contentRect testColour]

  describe "isKeyPressed" $ do
    it "is True when focused and the key is present this frame" $ do
      let ctx = withFocus (Just TestControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) :: UIContext TestElement ()
      (result, _) <- runUI (isKeyPressed TestControl KeyReturn) ctx
      result `shouldBe` True

    it "is False when not focused, even if the key is present" $ do
      let ctx = withFocus (Just OtherControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) :: UIContext TestElement ()
      (result, _) <- runUI (isKeyPressed TestControl KeyReturn) ctx
      result `shouldBe` False

    it "is False when focused but the key is absent" $ do
      let ctx = withFocus (Just TestControl) (mkCtxFor noInput) :: UIContext TestElement ()
      (result, _) <- runUI (isKeyPressed TestControl KeyReturn) ctx
      result `shouldBe` False

  describe "whenFocused" $ do
    it "runs the action when the element holds focus" $ do
      let ctx = withFocus (Just TestControl) (mkCtxFor noInput)
      ctx' <- snd <$> runUI (whenFocused TestControl (emit (1 :: Int))) ctx
      getMessages ctx' `shouldBe` [1]

    it "skips the action when the element does not hold focus" $ do
      let ctx = withFocus (Just OtherControl) (mkCtxFor noInput)
      ctx' <- snd <$> runUI (whenFocused TestControl (emit (1 :: Int))) ctx
      getMessages ctx' `shouldBe` []

  describe "isActivatedBy" $ do
    it "is True when clicked" $ do
      let ctx = withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])) :: UIContext TestElement ()
      (result, _) <- runUI (isActivatedBy TestControl [KeyReturn]) ctx
      result `shouldBe` True

    it "is False when the click misses" $ do
      let ctx = withButtonReleased (mkCtxFor (mouseAt (Point 200 200) False [])) :: UIContext TestElement ()
      (result, _) <- runUI (isActivatedBy TestControl [KeyReturn]) ctx
      result `shouldBe` False

    it "is True when a listed key is pressed while focused" $ do
      let ctx = withFocus (Just TestControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) :: UIContext TestElement ()
      (result, _) <- runUI (isActivatedBy TestControl [KeyReturn]) ctx
      result `shouldBe` True

    it "is False when neither clicked nor a listed key is pressed" $ do
      let ctx = mkCtxFor noInput :: UIContext TestElement ()
      (result, _) <- runUI (isActivatedBy TestControl [KeyReturn]) ctx
      result `shouldBe` False

    it "is False when disabled, even if clicked" $ do
      let ctx = (withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])) :: UIContext TestElement ()) { ctxDisabled = True }
      (result, _) <- runUI (isActivatedBy TestControl [KeyReturn]) ctx
      result `shouldBe` False

    it "is False on a click released while a different element holds drag capture" $ do
      -- Regression coverage for the fix this primitive needed: it must not
      -- reuse the legacy isHovered/isClicked (built on the single-owner
      -- ixnHovered field this module never writes to), and it must apply
      -- the same drag-exclusion gating applyMouseOver does.
      let base = withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])) :: UIContext TestElement ()
          ctx  = base { ctxInteraction = (ctxInteraction base) { ixnCaptured = Just OtherControl } }
      (result, _) <- runUI (isActivatedBy TestControl [KeyReturn]) ctx
      result `shouldBe` False

    it "is True on a click released while this same element holds drag capture" $ do
      let base = withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])) :: UIContext TestElement ()
          ctx  = base { ctxInteraction = (ctxInteraction base) { ixnCaptured = Just TestControl } }
      (result, _) <- runUI (isActivatedBy TestControl [KeyReturn]) ctx
      result `shouldBe` True

  describe "activatable" $ do
    it "returns True and renders chrome when activated by a click" $ do
      let ctx = withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False []))
      (activated, ctx') <- runUI (activatable TestControl ([] :: [Attr TestElement Probe Probe ()]) [KeyReturn] (pure ())) ctx
      activated `shouldBe` True
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    it "returns False when not activated" $ do
      let ctx = mkCtxFor noInput :: UIContext TestElement Probe
      (activated, _) <- runUI (activatable TestControl ([] :: [Attr TestElement Probe Probe ()]) [KeyReturn] (pure ())) ctx
      activated `shouldBe` False

    it "still takes focus via the underlying control even when not activated by a key" $ do
      let ctx = withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False []))
      (_, ctx') <- runUI (activatable TestControl ([] :: [Attr TestElement Probe Probe ()]) [KeyReturn] (pure ())) ctx
      getFocused ctx' `shouldBe` Just TestControl

  describe "label" $ do
    it "draws the given text in the resolved style's colour and alignment" $ do
      ctx' <- snd <$> runUI (label TestControl [text "Hello"]) (mkCtxFor noInput :: UIContext TestElement ())
      getDrawCommands ctx' `shouldContain` [DrawText contentRect "Hello" testColour AlignCenter]

    it "renders chrome like any other control" $ do
      ctx' <- snd <$> runUI (label TestControl [text "Hello"]) (mkCtxFor noInput :: UIContext TestElement ())
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    it "does not take focus by default (tabStop/focusOnClick still default like any control)" $ do
      ctx' <- snd <$> runUI (label TestControl [text "Hello"]) (mkCtxFor noInput :: UIContext TestElement ())
      getFocused ctx' `shouldBe` Just TestControl

    it "honours tabStop False, so it is skipped by Shift-Tab from what comes after it" $ do
      ctx' <- snd <$> runUI (label TestControl [text "Hello", tabStop False]) (mkCtxFor noInput :: UIContext TestElement ())
      ixnPrevTabStop (ctxInteraction ctx') `shouldBe` Nothing

    it "honours focusOnClick (FocusTarget), redirecting a click onto another element" $ do
      let attrs = [text "Caption", focusOnClick (FocusTarget OtherControl)]
          ctx   = withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False []) :: UIContext TestElement ())
      ctx' <- snd <$> runUI (label TestControl attrs) ctx
      getFocused ctx' `shouldBe` Just OtherControl

  describe "progressBar" $ do
    let run v ctx = snd <$> runUI (progressBar TestControl [progress (Progress v)]) ctx

    it "fills the correct proportion of the content area at 0.5" $ do
      ctx' <- run 0.5 (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 15 15 35 70) testColour]

    it "fills the full content area at 1.0" $ do
      ctx' <- run 1.0 (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect contentRect testColour]

    it "fills zero width at 0.0" $ do
      ctx' <- run 0.0 (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 15 15 0 70) testColour]

    it "clamps values above 1.0 to full width" $ do
      ctx' <- run 1.5 (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect contentRect testColour]

    it "clamps values below 0.0 to zero width" $ do
      ctx' <- run (-0.5) (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 15 15 0 70) testColour]

    it "renders chrome like any other control" $ do
      ctx' <- run 0.5 (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    describe "Indeterminate" $ do
      let baseCtx    = mkCtxFor noInput :: UIContext TestElement ()
          elapsedCtx = baseCtx { ctxAnimation = (ctxAnimation baseCtx) { animElapsed = 1 } }

      it "sweeps the band using the default band speed (0.5)" $ do
        ctx' <- snd <$> runUI (progressBar TestControl [progress Indeterminate]) elapsedCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 39.5 15 21 70) testColour]

      it "uses bandSpeed instead of the default when given" $ do
        ctx' <- snd <$> runUI (progressBar TestControl [progress Indeterminate, bandSpeed 1.0]) elapsedCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle (-6) 15 21 70) testColour]

      it "keeps the animation ticker alive" $ do
        ctx' <- snd <$> runUI (progressBar TestControl [progress Indeterminate]) elapsedCtx
        outRequiresAnimation (ctxOutputs ctx') `shouldBe` True

      it "a determinate bar does not request animation" $ do
        ctx' <- run 0.5 elapsedCtx
        outRequiresAnimation (ctxOutputs ctx') `shouldBe` False

  describe "button" $ do
    it "draws the label" $ do
      ctx' <- snd <$> runUI (button TestControl [text "label"]) (mkCtxFor noInput)
      drawnTexts ctx' `shouldContain` ["label"]

    it "renders chrome like any other control" $ do
      ctx' <- snd <$> runUI (button TestControl [text "label"]) (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    forM_ insidePoints $ \(desc, pt) ->
      it ("is clicked when the mouse is released " <> desc) $ do
        (_, ctx') <- runUI (button TestControl [text "label", onClick (post ())]) (withButtonReleased (mkCtxFor (mouseAt pt False [])))
        getMessages ctx' `shouldBe` [()]

    forM_ outsidePoints $ \(desc, pt) ->
      it ("is not clicked when the mouse is released " <> desc) $ do
        (_, ctx') <- runUI (button TestControl [text "label", onClick (post ())]) (withButtonReleased (mkCtxFor (mouseAt pt False [])))
        getMessages ctx' `shouldBe` []

    it "is clicked when Enter is pressed and the button has focus" $ do
      (_, ctx') <- runUI (button TestControl [text "label", onClick (post ())]) (withFocus (Just TestControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
      getMessages ctx' `shouldBe` [()]

    it "is not clicked when Enter is pressed and the button does not have focus" $ do
      (_, ctx') <- runUI (button TestControl [text "label", onClick (post ())]) (withFocus (Just OtherControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
      getMessages ctx' `shouldBe` []

    it "is not clicked when Tab and Enter are pressed simultaneously" $ do
      (_, ctx') <- runUI (button TestControl [text "label", onClick (post ())])
        (withFocus (Just TestControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyTab [], KeyEvent KeyReturn []] }))
      getMessages ctx' `shouldBe` []

    it "is not activated by a click when disabled" $ do
      (_, ctx') <- runUI (disableWhen True (button TestControl [text "label", onClick (post ())])) (withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])))
      getMessages ctx' `shouldBe` []

    it "is not activated by Enter when disabled" $ do
      (_, ctx') <- runUI (disableWhen True (button TestControl [text "label", onClick (post ())])) (withFocus (Just TestControl) (mkCtxFor noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
      getMessages ctx' `shouldBe` []

    it "onClick (perform eff) queues the given UiEffect when clicked, instead of emitting a message" $ do
      (_, ctx') <- runUI (button TestControl [text "label", onClick (perform (SetSelectionAt TestControl (cursor 0)))])
        (withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])) :: UIContext TestElement ())
      getUiEffects ctx' `shouldContain` [SetSelectionAt TestControl (cursor 0)]
      getMessages ctx' `shouldBe` []

    it "onClick (post msg <> perform eff) fans out to both a message and a UiEffect" $ do
      let attrs = [text "label", onClick (post (1 :: Int) <> perform (SetSelectionAt TestControl (cursor 0)))]
      (_, ctx') <- runUI (button TestControl attrs) (withButtonReleased (mkCtxFor (mouseAt (Point 50 50) False [])))
      getMessages ctx' `shouldBe` [1]
      getUiEffects ctx' `shouldContain` [SetSelectionAt TestControl (cursor 0)]

  describe "renderCheckboxGlyph" $ do
    it "draws a checkmark when checked" $ do
      (_, ctx') <- runUI (renderCheckboxGlyph CheckboxGlyph True) (mkCheckboxCtx noInput)
      drawnTexts ctx' `shouldContain` ["✓"]

    it "draws nothing when unchecked" $ do
      (_, ctx') <- runUI (renderCheckboxGlyph CheckboxGlyph False) (mkCheckboxCtx noInput)
      drawnTexts ctx' `shouldBe` []

  describe "checkbox" $ do
    describe "toggle behaviour" $ do
      it "dispatches True when clicked while unchecked" $ do
        ctx' <- runCheckbox False (withButtonReleased (mkCheckboxCtx (mouseAt labelPoint False [])))
        getMessages ctx' `shouldBe` [True]

      it "dispatches False when clicked while checked" $ do
        ctx' <- runCheckbox True (withButtonReleased (mkCheckboxCtx (mouseAt labelPoint False [])))
        getMessages ctx' `shouldBe` [False]

      it "dispatches when clicked directly on the glyph, not just the label" $ do
        ctx' <- runCheckbox False (withButtonReleased (mkCheckboxCtx (mouseAt glyphPoint False [])))
        getMessages ctx' `shouldBe` [True]

      it "dispatches toggle when Enter is pressed while focused" $ do
        ctx' <- runCheckbox False (withFocus (Just CheckboxBox) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        getMessages ctx' `shouldBe` [True]

      it "dispatches toggle when Space is pressed while focused" $ do
        ctx' <- runCheckbox False (withFocus (Just CheckboxBox) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeySpace []] }))
        getMessages ctx' `shouldBe` [True]

      it "does not dispatch when clicked outside the checkbox" $ do
        ctx' <- runCheckbox False (withButtonReleased (mkCheckboxCtx (mouseAt (Point 200 200) False [])))
        getMessages ctx' `shouldBe` []

      it "does not dispatch when Enter is pressed while unfocused" $ do
        ctx' <- runCheckbox False (withFocus (Just CheckboxGlyph) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        getMessages ctx' `shouldBe` []

    describe "disabled" $ do
      it "does not dispatch when clicked while disabled" $ do
        (_, ctx') <- runUI (disableWhen True (checkbox id [text "Notify me", checked False, onToggle (postWith id)]))
          (withButtonReleased (mkCheckboxCtx (mouseAt labelPoint False [])))
        getMessages ctx' `shouldBe` []

      it "does not dispatch when Enter is pressed while disabled" $ do
        (_, ctx') <- runUI (disableWhen True (checkbox id [text "Notify me", checked False, onToggle (postWith id)]))
          (withFocus (Just CheckboxBox) (mkCheckboxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        getMessages ctx' `shouldBe` []

    describe "rendering" $ do
      it "draws the checkmark when checked" $ do
        ctx' <- runCheckbox True (mkCheckboxCtx noInput)
        drawnTexts ctx' `shouldContain` ["✓"]

      it "does not draw the checkmark when unchecked" $ do
        ctx' <- runCheckbox False (mkCheckboxCtx noInput)
        drawnTexts ctx' `shouldNotContain` ["✓"]

      it "draws the label text" $ do
        ctx' <- runCheckbox False (mkCheckboxCtx noInput)
        drawnTexts ctx' `shouldContain` ["Notify me"]

      it "fills its own background across the whole composite" $ do
        ctx' <- runCheckbox False (mkCheckboxCtx noInput)
        getDrawCommands ctx' `shouldContain` [FillRect checkboxRect testColour]

    describe "focus" $ do
      it "gives focus to the checkbox itself (not the glyph or label) when the label is clicked" $ do
        ctx' <- runCheckbox False (withButtonReleased (mkCheckboxCtx (mouseAt labelPoint False [])))
        getFocused ctx' `shouldBe` Just CheckboxBox

      it "gives focus to the checkbox itself when the glyph is clicked" $ do
        ctx' <- runCheckbox False (withButtonReleased (mkCheckboxCtx (mouseAt glyphPoint False [])))
        getFocused ctx' `shouldBe` Just CheckboxBox

  describe "textInputControl" $ do
    it "renders chrome like any other control" $ do
      ctx' <- runTextField "hello" (mkTextCtx "hello" noInput)
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    describe "rendering" $ do
      it "displays the value without a cursor when unfocused" $ do
        ctx' <- runTextField "hello" (withFocus (Just OtherControl) (mkTextCtx "hello" noInput))
        drawnTexts ctx' `shouldContain` ["hello"]

      it "displays the value with a cursor when focused" $ do
        ctx' <- runTextField "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput))
        drawnTexts ctx' `shouldContain` ["hello"]
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 15 15 1 70) testColour]

    describe "text editing" $ do
      it "appends typed characters to the value" $ do
        ctx' <- runTextField "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["!"] }))
        getMessages ctx' `shouldBe` ["hello!"]

      it "removes the last character on backspace" $ do
        ctx' <- runTextField "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyBackspace []] }))
        getMessages ctx' `shouldBe` ["hell"]

      it "does not dispatch when backspace is pressed on an empty value" $ do
        ctx' <- runTextField "" (withFocus (Just TestControl) (mkTextCtx "" noInput { inputKeyEvents = [KeyEvent KeyBackspace []] }))
        dispatchCount ctx' `shouldBe` 0

      it "does not dispatch when there is no input" $ do
        ctx' <- runTextField "hello" (withFocus (Just TestControl) (mkTextCtx "hello" noInput))
        dispatchCount ctx' `shouldBe` 0

      it "does not process input when unfocused" $ do
        ctx' <- runTextField "hello" (withFocus (Just OtherControl) (mkTextCtx "hello" noInput { inputTypedText = ["!"], inputKeyEvents = [KeyEvent KeyBackspace []] }))
        dispatchCount ctx' `shouldBe` 0

    describe "disabled" $ do
      it "does not process input when disabled" $ do
        (_, ctx') <- runUI (disableWhen True (textInputControl TestControl [text "hello", onInput (postWith id)])) (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["!"] }))
        dispatchCount (settle ctx') `shouldBe` 0

      it "does not show a cursor when focused and disabled" $ do
        (_, ctx') <- runUI (disableWhen True (textInputControl TestControl [text "hello", onInput (postWith id)])) (withFocus (Just TestControl) (mkTextCtx "hello" noInput))
        getDrawCommands (settle ctx') `shouldNotContain` [FillRect (Rectangle 15 15 1 70) testColour]

    describe "cursor placement" $ do
      it "sets the cursor to the clicked position on mouse press" $ do
        -- noOpTextMeasurer maps every offset to 0, so any click -> position 0
        ctx' <- runTextField "hello" (withFocus (Just TestControl) (mkTextCtx "hello" (mouseAt (Point 50 50) True [])))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 0 0]

      it "extends the active end on drag while keeping anchor" $ do
        -- First frame: click starts drag; second frame: drag extends selection.
        frame1 <- runTextField "hello" (withFocus (Just TestControl) (mkTextCtx "hello" (mouseAt (Point 50 50) True [])))
        frame2 <- fmap (settle . snd) $ runUI (textInputControl TestControl [text "hello", onInput (postWith id)])
                    (nextFrameContext controlRect (mouseAt (Point 70 50) True []) frame1)
        case Map.lookup TestControl (elmSelections (ctxElements frame2)) of
          Just [Selection a _] -> a `shouldBe` 0
          other                -> expectationFailure $ "expected Just [Selection a _], got: " <> show other

    describe "arrow navigation" $ do
      let withSel a v ctx = ctx { ctxElements = (ctxElements ctx) { elmSelections = Map.singleton TestControl [Selection a v] } }

      it "moves cursor left with Left" $ do
        ctx' <- runTextField "hello" (withSel 3 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyLeft []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 2 2]

      it "moves cursor right with Right" $ do
        ctx' <- runTextField "hello" (withSel 2 2 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyRight []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 3]

      it "collapses selection to low end on plain Left" $ do
        ctx' <- runTextField "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyLeft []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 1 1]

      it "collapses selection to high end on plain Right" $ do
        ctx' <- runTextField "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyRight []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 3]

      it "extends selection left with Shift+Left" $ do
        ctx' <- runTextField "hello" (withSel 3 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyLeft [Shift]] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 2]

      it "extends selection right with Shift+Right" $ do
        ctx' <- runTextField "hello" (withSel 3 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyRight [Shift]] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 4]

      it "does not move cursor past the beginning" $ do
        ctx' <- runTextField "hello" (withSel 0 0 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyLeft []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 0 0]

      it "does not move cursor past the end" $ do
        ctx' <- runTextField "hello" (withSel 5 5 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyRight []] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 5 5]

    describe "selection editing" $ do
      let withSel a v ctx = ctx { ctxElements = (ctxElements ctx) { elmSelections = Map.singleton TestControl [Selection a v] } }

      it "deletes the selected range on backspace" $ do
        ctx' <- runTextField "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyBackspace []] })))
        getMessages ctx' `shouldBe` ["hlo"]

      it "replaces the selected range with typed text" $ do
        ctx' <- runTextField "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["X"] })))
        getMessages ctx' `shouldBe` ["hXlo"]

      it "collapses cursor to insertion point after replacing selection" $ do
        ctx' <- runTextField "hello" (withSel 1 3 (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["XY"] })))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 3 3]

    describe "focus persistence" $ do
      let withSel a v ctx = ctx { ctxElements = (ctxElements ctx) { elmSelections = Map.singleton TestControl [Selection a v] } }

      it "leaves the selection unchanged on a frame where the control is not focused" $ do
        ctx' <- runTextField "hello" (withSel 1 3 (withFocus (Just OtherControl) (mkTextCtx "hello" noInput)))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 1 3]

      it "restores the previous selection when focus returns without a click" $ do
        -- No withFocus: focus starts at Nothing, so TestControl auto-focuses
        -- this frame via the same path as gaining focus by Tab, not by click.
        ctx' <- runTextField "hello" (withSel 2 4 (mkTextCtx "hello" noInput))
        Map.lookup TestControl (elmSelections (ctxElements ctx')) `shouldBe` Just [Selection 2 4]

    describe "scrolling" $ do
      let withSel a v ctx = ctx { ctxElements = (ctxElements ctx) { elmSelections = Map.singleton TestControl [Selection a v] } }
          withScrollFrac x ctx = ctx { ctxElements = (ctxElements ctx) { elmScrollStates = Map.singleton TestControl (ScrollState x) } }

      it "scrolls right to keep the cursor visible when it moves past the right edge" $ do
        -- Content width is 100px (5 chars * fixedCharWidth's 20px); viewport
        -- is 70px, so the cursor at index 5 (position 100) needs a 31px
        -- scroll to stay just inside the right edge.
        let base = withSel 5 5 (withFocus (Just TestControl) (mkTextCtxWith fixedCharWidth "hello" noInput))
        ctx' <- runTextField "hello" base
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 84 15 1 70) testColour]

      it "stores the scroll position as a bounded [0, 1] fraction, not an unbounded pixel value" $ do
        -- The 31px target above exceeds the content's actual 30px max
        -- scroll (100px content - 70px viewport), so the stored fraction
        -- clamps to 1.0 rather than persisting an out-of-range pixel count
        -- the way the old single pixel-offset convention did.
        let base = withSel 5 5 (withFocus (Just TestControl) (mkTextCtxWith fixedCharWidth "hello" noInput))
        ctx' <- runTextField "hello" base
        Map.lookup TestControl (elmScrollStates (ctxElements ctx')) `shouldBe` Just (ScrollState 1.0)

      it "scrolls left to keep the cursor visible when it moves before the left edge" $ do
        let base = withScrollFrac 1.0 (withSel 0 0 (withFocus (Just TestControl) (mkTextCtxWith fixedCharWidth "hello" noInput)))
        ctx' <- runTextField "hello" base
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 15 15 1 70) testColour]

    describe "digits-only input (inputFilter)" $ do
      describe "input filter" $ do
        it "inserts digits typed alongside non-digits, dropping the non-digits" $ do
          ctx' <- runNumberField "12" (withFocus (Just TestControl) (mkTextCtx "12" noInput { inputTypedText = ["a3b"] }))
          getMessages ctx' `shouldBe` ["123"]

        it "does not dispatch when the only typed characters are non-digits" $ do
          ctx' <- runNumberField "12" (withFocus (Just TestControl) (mkTextCtx "12" noInput { inputTypedText = ["!"] }))
          dispatchCount ctx' `shouldBe` 0

        it "still allows backspace to remove digits" $ do
          ctx' <- runNumberField "12" (withFocus (Just TestControl) (mkTextCtx "12" noInput { inputKeyEvents = [KeyEvent KeyBackspace []] }))
          getMessages ctx' `shouldBe` ["1"]

      describe "rendering" $ do
        it "displays the value unmasked" $ do
          ctx' <- runNumberField "42" (withFocus (Just TestControl) (mkTextCtx "42" noInput))
          drawnTexts ctx' `shouldContain` ["42"]

    describe "password masking (displayFilter)" $ do
      describe "rendering" $ do
        it "displays a mask character per character of the value instead of the value itself" $ do
          ctx' <- runPasswordField "hunter2" (withFocus (Just TestControl) (mkTextCtx "hunter2" noInput))
          drawnTexts ctx' `shouldContain` ["•••••••"]
          drawnTexts ctx' `shouldNotContain` ["hunter2"]

      describe "editing" $ do
        it "appends typed characters to the real (unmasked) value" $ do
          ctx' <- runPasswordField "hunter2" (withFocus (Just TestControl) (mkTextCtx "hunter2" noInput { inputTypedText = ["!"] }))
          getMessages ctx' `shouldBe` ["hunter2!"]

      describe "cursor placement" $ do
        it "places the cursor using offsets measured against the masked text, not the real value" $ do
          let base = withFocus (Just TestControl) (mkTextCtxWith fixedCharWidth "hunter2" (mouseAt (Point 35 50) True []))
          ctx' <- runPasswordField "hunter2" base
          case Map.lookup TestControl (elmSelections (ctxElements ctx')) of
            Just [Selection a v] -> (a, v) `shouldBe` (1, 1)
            other                 -> expectationFailure $ "expected Just [Selection 1 1], got: " <> show other

    describe "custom filters" $ do
      it "lets a custom input filter reject keystrokes entirely" $ do
        (_, ctx') <- runUI (textInputControl TestControl [text "hello", inputFilter (const T.empty), onInput (postWith id)])
          (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputTypedText = ["x"] }))
        dispatchCount (settle ctx') `shouldBe` 0

      it "lets a custom display filter change what is rendered without changing the value" $ do
        (_, ctx') <- runUI (textInputControl TestControl [text "hello", displayFilter T.toUpper, onInput (postWith id)])
          (withFocus (Just TestControl) (mkTextCtx "hello" noInput))
        drawnTexts (settle ctx') `shouldContain` ["HELLO"]

    describe "onSubmit" $ do
      it "fires Submitted when Enter is pressed while focused" $ do
        (_, ctx') <- runUI (textInputControl TestControl [text "hello", onSubmit (post "submitted")])
          (withFocus (Just TestControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        getMessages (settle ctx') `shouldBe` ["submitted"]

      it "does not fire Submitted when a different element is focused" $ do
        (_, ctx') <- runUI (textInputControl TestControl [text "hello", onSubmit (post "submitted")])
          (withFocus (Just OtherControl) (mkTextCtx "hello" noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        getMessages (settle ctx') `shouldBe` []

  describe "slider" $ do
    it "renders chrome like any other control" $ do
      ctx' <- snd <$> runUI (slider id [orientation Horizontal, value 0.5, onChange (post ())])
        (emptyUIContext sliderRect noInput sliderTheme noOpTextMeasurer)
      getDrawCommands ctx' `shouldContain` [FillRect sliderRect testColour]

    describe "thumbRatio" $ do
      -- sliderRect is 200x30; the default square-thumb calculation gives a
      -- 30x30 thumb (side = cross-axis). An explicit thumbRatio of 0.5
      -- gives a 100-wide thumb instead — distinct enough from the default
      -- to prove the override actually took effect, not just that the
      -- track's own (same-coloured) background is present regardless.
      it "defaults to a square thumb when not given" $ do
        ctx' <- snd <$> runUI (slider id [orientation Horizontal, value 0, onChange (post ())])
          (emptyUIContext sliderRect noInput sliderTheme noOpTextMeasurer)
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 0 30 30) testColour]

      it "overrides the default square-thumb sizing when given" $ do
        ctx' <- snd <$> runUI (slider id [orientation Horizontal, value 0, onChange (post ()), thumbRatio 0.5])
          (emptyUIContext sliderRect noInput sliderTheme noOpTextMeasurer)
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 0 100 30) testColour]

    describe "drag interaction" $ do
      it "does not dispatch when the mouse is over the track but the button is not held" $ do
        ctx' <- runSlider Horizontal 0.5 (mouseAt (Point 100 15) False [])
        dispatchCount ctx' `shouldBe` 0

      it "does not dispatch when not dragging even if the button is held elsewhere" $ do
        ctx' <- runSlider Horizontal 0.5 (mouseAt (Point 300 300) True [])
        dispatchCount ctx' `shouldBe` 0

      it "sets value to 0.5 when dragged to the midpoint" $ do
        ctx' <- runSlider Horizontal 0 (mouseAt (Point 100 15) True [])
        getMessages ctx' `shouldBe` [0.5]

      it "sets value to 0 when dragged to the far left" $ do
        ctx' <- runSlider Horizontal 0.5 (mouseAt (Point 15 15) True [])
        getMessages ctx' `shouldBe` [0.0]

      it "sets value to 1 when dragged to the far right" $ do
        ctx' <- runSlider Horizontal 0.5 (mouseAt (Point 185 15) True [])
        getMessages ctx' `shouldBe` [1.0]

      it "continues tracking when the mouse moves outside the track while button held" $ do
        frame1 <- runSlider Horizontal 0 (mouseAt (Point 100 15) True [])
        let val1 = head (getMessages frame1)
        frame2 <- fmap (settle . snd) $ runUI (slider id [orientation Horizontal, value val1, onChange (postWith id)])
                                   (nextFrameContext sliderRect (mouseAt (Point 300 15) True []) frame1)
        getMessages frame2 `shouldBe` [1.0]

      it "stops tracking when the button is released" $ do
        frame1 <- runSlider Horizontal 0 (mouseAt (Point 100 15) True [])
        let val1 = head (getMessages frame1)
        frame2 <- fmap (settle . snd) $ runUI (slider id [orientation Horizontal, value val1, onChange (postWith id)])
                                   (nextFrameContext sliderRect (mouseAt (Point 300 15) False []) frame1)
        dispatchCount frame2 `shouldBe` 0

      -- Releasing the mouse while it is still over the track (as opposed to
      -- having dragged off it) must not dispatch a further change.
      it "does not dispatch on the release frame when the mouse is still over the track" $ do
        frame1 <- runSlider Horizontal 0 (mouseAt (Point 100 15) True [])
        let val1 = head (getMessages frame1)
        frame2 <- fmap (settle . snd) $ runUI (slider id [orientation Horizontal, value val1, onChange (postWith id)])
                                   (nextFrameContext sliderRect (mouseAt (Point 100 15) False []) frame1)
        dispatchCount frame2 `shouldBe` 0

    describe "keyboard nudging" $ do
      it "increases value by 0.05 when Right is pressed (Horizontal)" $ do
        ctx' <- runSlider Horizontal 0.5 noInput { inputKeyEvents = [KeyEvent KeyRight []] }
        getMessages ctx' `shouldBe` [0.55]

      it "decreases value by 0.05 when Left is pressed (Horizontal)" $ do
        ctx' <- runSlider Horizontal 0.5 noInput { inputKeyEvents = [KeyEvent KeyLeft []] }
        getMessages ctx' `shouldBe` [0.45]

      it "increases value by 0.05 when Down is pressed (Vertical)" $ do
        ctx' <- runSlider Vertical 0.5 noInput { inputKeyEvents = [KeyEvent KeyDown []] }
        getMessages ctx' `shouldBe` [0.55]

      it "decreases value by 0.05 when Up is pressed (Vertical)" $ do
        ctx' <- runSlider Vertical 0.5 noInput { inputKeyEvents = [KeyEvent KeyUp []] }
        getMessages ctx' `shouldBe` [0.45]

      it "clamps to 1 when nudging at the maximum" $ do
        ctx' <- runSlider Horizontal 1.0 noInput { inputKeyEvents = [KeyEvent KeyRight []] }
        getMessages ctx' `shouldBe` [1.0]

      it "clamps to 0 when nudging at the minimum" $ do
        ctx' <- runSlider Horizontal 0.0 noInput { inputKeyEvents = [KeyEvent KeyLeft []] }
        getMessages ctx' `shouldBe` [0.0]

      it "uses arrowStep instead of the 0.05 default when given" $ do
        ctx' <- fmap (settle . snd) $ runUI (slider id [orientation Horizontal, value 0.5, onChange (postWith id), arrowStep 0.2])
          (emptyUIContext sliderRect noInput { inputKeyEvents = [KeyEvent KeyRight []] } sliderTheme noOpTextMeasurer)
        getMessages ctx' `shouldBe` [0.7]

      it "does not nudge when another element has focus" $ do
        ctx' <- fmap (settle . snd) $ runUI (slider id [orientation Horizontal, value 0.5, onChange (postWith id)])
          (withFocus (Just SliderThumb) (emptyUIContext sliderRect noInput { inputKeyEvents = [KeyEvent KeyRight []] } sliderTheme noOpTextMeasurer))
        getMessages ctx' `shouldBe` []

      it "does not nudge when disabled" $ do
        ctx' <- fmap (settle . snd) $ runUI (disableWhen True (slider id [orientation Horizontal, value 0.5, onChange (postWith id)]))
          (withFocus (Just SliderTrack) (emptyUIContext sliderRect noInput { inputKeyEvents = [KeyEvent KeyRight []] } sliderTheme noOpTextMeasurer))
        dispatchCount ctx' `shouldBe` 0

    describe "without interaction" $ do
      it "does not dispatch when there is no input" $ do
        ctx' <- runSlider Horizontal 0.5 noInput
        dispatchCount ctx' `shouldBe` 0

  describe "scrollBar" $ do
    it "renders chrome for the track like any other control" $ do
      -- Track occupies the middle 160px between the two 20px buttons.
      ctx' <- runScrollBar (mkScrollBarCtx 0 noInput)
      getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 20 20 160) testColour]

    describe "defaults" $ do
      it "defaults to Horizontal orientation, drawing horizontal arrow glyphs" $ do
        ctx' <- snd <$> runUI (scrollBar id []) (mkScrollBarCtx 0 noInput)
        drawnTexts ctx' `shouldContain` ["◀"]
        drawnTexts ctx' `shouldContain` ["▶"]

      it "defaults to a full-track thumb ratio (nothing to step by)" $ do
        ctx' <- fmap (settle . snd) $ runUI (scrollBar id [orientation Vertical])
          (withButtonReleased (mkScrollBarCtx 0.5 (mouseAt (Point 10 190) False [])))
        scrollBarPos ctx' `shouldBe` 1.0

    describe "button stepping" $ do
      it "steps forward by the thumb ratio when the increment button is clicked" $ do
        ctx' <- runScrollBar (withButtonReleased (mkScrollBarCtx 0.5 (mouseAt (Point 10 190) False [])))
        scrollBarPos ctx' `shouldBe` 0.75

      it "steps back by the thumb ratio when the decrement button is clicked" $ do
        ctx' <- runScrollBar (withButtonReleased (mkScrollBarCtx 0.5 (mouseAt (Point 10 10) False [])))
        scrollBarPos ctx' `shouldBe` 0.25

      it "clamps to 1 when stepping forward near the end" $ do
        ctx' <- runScrollBar (withButtonReleased (mkScrollBarCtx 0.9 (mouseAt (Point 10 190) False [])))
        scrollBarPos ctx' `shouldBe` 1

      it "clamps to 0 when stepping back near the start" $ do
        ctx' <- runScrollBar (withButtonReleased (mkScrollBarCtx 0.1 (mouseAt (Point 10 10) False [])))
        scrollBarPos ctx' `shouldBe` 0

    describe "track dragging" $ do
      it "centres the thumb on the cursor while the track is pressed" $ do
        ctx' <- runScrollBar (mkScrollBarCtx 0 (mouseAt (Point 10 100) True []))
        scrollBarPos ctx' `shouldBe` 0.5

      it "continues tracking when the mouse moves off the track while the button is held" $ do
        frame1 <- runScrollBar (mkScrollBarCtx 0 (mouseAt (Point 10 100) True []))
        frame2 <- fmap (settle . snd) $ runUI (scrollBar id [orientation Vertical, thumbRatio 0.25])
                                   (nextFrameContext scrollBarRect (mouseAt (Point 200 40) True []) frame1)
        scrollBarPos frame2 `shouldBe` 0.0

      it "stops tracking when the button is released after dragging off the track" $ do
        frame1 <- runScrollBar (mkScrollBarCtx 0 (mouseAt (Point 10 100) True []))
        frame2 <- fmap (settle . snd) $ runUI (scrollBar id [orientation Vertical, thumbRatio 0.25])
                                   (nextFrameContext scrollBarRect (mouseAt (Point 200 40) False []) frame1)
        scrollBarPos frame2 `shouldBe` 0.5

    describe "keyboard nudging (inherited from the underlying slider)" $ do
      it "nudges the position when an arrow key is pressed while the track is focused" $ do
        ctx' <- fmap (settle . snd) $ runUI (scrollBar id [orientation Vertical, thumbRatio 0.25])
          (withFocus (Just ScrollTrack) (mkScrollBarCtx 0.5 noInput { inputKeyEvents = [KeyEvent KeyDown []] }))
        scrollBarPos ctx' `shouldBe` 0.55

    describe "without interaction" $ do
      it "leaves the position unchanged" $ do
        ctx' <- runScrollBar (mkScrollBarCtx 0.5 noInput)
        scrollBarPos ctx' `shouldBe` 0.5

    describe "lifecycle events" $ do
      it "bridges the track's FocusGained into onFocusGained" $ do
        -- Starts with a different sub-part focused so the buttons' own
        -- default auto-claim-when-nothing-focused rule doesn't race the
        -- track for focus first; the click is what moves it explicitly.
        let attrs = [orientation Vertical, thumbRatio 0.25, onFocusGained (post "gained")]
            ctx0  = withFocus (Just ScrollIncrBtn) (withButtonReleased (mkScrollBarCtx 0 (mouseAt (Point 10 100) False []))) :: UIContext ScrollBarPart String
        ctx' <- snd <$> runUI (scrollBar id attrs) ctx0
        getMessages ctx' `shouldBe` ["gained"]

      it "bridges the track's MouseEntered into onMouseEnter" $ do
        let attrs = [orientation Vertical, thumbRatio 0.25, onMouseEnter (post "entered")]
            ctx0  = mkScrollBarCtx 0 (mouseAt (Point 10 100) False []) :: UIContext ScrollBarPart String
        ctx' <- snd <$> runUI (scrollBar id attrs) ctx0
        getMessages ctx' `shouldBe` ["entered"]

  describe "viewport" $ do
    describe "no scrollbars needed" $ do
      it "renders content at its own size, unclipped and untranslated, when it fits the viewport" $ do
        ctx' <- runViewportDraw (Size 100 50) vpCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 0 100 50) testColour]

      it "does not draw either scrollbar" $ do
        ctx' <- runViewportDraw (Size 100 50) vpCtx
        getDrawCommands ctx' `shouldNotContain` [FillRect (Rectangle 0 84 16 16) testColour]
        getDrawCommands ctx' `shouldNotContain` [FillRect (Rectangle 184 0 16 16) testColour]

      it "defaults to Size 0 0 (nothing to scroll) when contentSize is not given" $ do
        ctx' <- fmap (settle . snd) $ runUI (viewport VPPart [] (fillRect testColour)) vpCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 0 0 0) testColour]

    describe "horizontal scrollbar" $ do
      it "appears when content is wider than the viewport but not taller once reduced" $ do
        ctx' <- runViewportDraw (Size 300 50) vpCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 84 16 16) testColour]

      it "does not appear on the vertical axis" $ do
        ctx' <- runViewportDraw (Size 300 50) vpCtx
        getDrawCommands ctx' `shouldNotContain` [FillRect (Rectangle 184 0 16 16) testColour]

      it "reduces the viewport height by the scrollbar strip size" $ do
        ctx' <- runViewportDraw (Size 300 50) vpCtx
        getDrawCommands ctx' `shouldContain` [PushClip (Rectangle 0 0 200 84)]

    describe "vertical scrollbar" $ do
      it "appears when content is taller than the viewport but not wider once reduced" $ do
        ctx' <- runViewportDraw (Size 50 200) vpCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 184 0 16 16) testColour]

      it "does not appear on the horizontal axis" $ do
        ctx' <- runViewportDraw (Size 50 200) vpCtx
        getDrawCommands ctx' `shouldNotContain` [FillRect (Rectangle 0 84 16 16) testColour]

      it "reduces the viewport width by the scrollbar strip size" $ do
        ctx' <- runViewportDraw (Size 50 200) vpCtx
        getDrawCommands ctx' `shouldContain` [PushClip (Rectangle 0 0 184 100)]

    describe "both scrollbars" $
      it "appear on both axes, two-pass, when content exceeds either dimension after the other's strip is reserved" $ do
        ctx' <- runViewportDraw (Size 300 200) vpCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 84 16 16) testColour]
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 184 0 16 16) testColour]

    describe "scroll offset" $ do
      let withHScroll x ctx = ctx { ctxElements = (ctxElements ctx) { elmScrollStates = Map.singleton (VPPart (ViewportH ScrollTrack)) (ScrollState x) } }
          withVScroll y ctx = ctx { ctxElements = (ctxElements ctx) { elmScrollStates = Map.singleton (VPPart (ViewportV ScrollTrack)) (ScrollState y) } }

      it "translates content left by the stored horizontal scroll fraction" $ do
        -- H-only (Size 300 50): vpW 200, max scroll = 300 - 200 = 100px.
        ctx' <- runViewportDraw (Size 300 50) (withHScroll 1.0 vpCtx)
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle (-100) 0 300 50) testColour]

      it "translates content up by the stored vertical scroll fraction" $ do
        -- V-only (Size 50 200): vpH 100, max scroll = 200 - 100 = 100px.
        ctx' <- runViewportDraw (Size 50 200) (withVScroll 1.0 vpCtx)
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 (-100) 50 200) testColour]

    describe "interaction clipping" $ do
      it "does not fire MouseEntered for a child under the horizontal scrollbar strip" $ do
        -- Both scrollbars (Size 400 100): vpRect 184x84, hBar at y 84-100.
        ctx' <- runViewportChild (Size 400 100) [captureAttrs] (Point 100 92)
        getMessages ctx' `shouldNotContain` [Probe MouseEntered]

      it "fires MouseEntered for a child within the visible viewport" $ do
        ctx' <- runViewportChild (Size 400 100) [captureAttrs] (Point 100 42)
        getMessages ctx' `shouldContain` [Probe MouseEntered]

  describe "virtualContent" $ do
    -- Viewport is controlRect: 100x100. Each item marks itself with a
    -- FillRect whose colour encodes its index, so a test can assert both
    -- which indices were rendered and at what rectangle.
    let marker :: Int -> Rectangle -> DrawCommand
        marker i r = FillRect r (RGBA (fromIntegral i) 0 0 1)
        runVirtualContent pos itemH count =
          fmap (settle . snd) $ runUI (virtualContent pos itemH count (\i -> fillRect (RGBA (fromIntegral i) 0 0 1))) (mkCtxFor noInput :: UIContext TestElement ())

    it "renders items starting from the top when unscrolled" $ do
      -- itemHeight 20, viewport 100 tall -> exactly 5 full items fit.
      ctx' <- runVirtualContent 0 20 10
      getDrawCommands ctx' `shouldContain` [marker 0 (Rectangle 0 0 100 20)]
      getDrawCommands ctx' `shouldContain` [marker 4 (Rectangle 0 80 100 20)]
      getDrawCommands ctx' `shouldNotContain` [marker 5 (Rectangle 0 100 100 20)]

    it "clips the first item by the fractional scroll offset" $ do
      -- scrollPos 25 with itemHeight 20 -> first visible item is index 1,
      -- pushed up 5px above the viewport top.
      ctx' <- runVirtualContent 25 20 10
      getDrawCommands ctx' `shouldContain` [marker 1 (Rectangle 0 (-5) 100 20)]
      getDrawCommands ctx' `shouldNotContain` [marker 0 (Rectangle 0 0 100 20)]

    it "renders one extra item to cover the clipped final row" $ do
      ctx' <- runVirtualContent 25 20 10
      -- 6 items are needed to cover a 100px viewport once offset by 5px.
      getDrawCommands ctx' `shouldContain` [marker 6 (Rectangle 0 95 100 20)]

    it "does not render past the last item" $ do
      ctx' <- runVirtualContent 0 20 3
      getDrawCommands ctx' `shouldContain` [marker 2 (Rectangle 0 40 100 20)]
      getDrawCommands ctx' `shouldNotContain` [marker 3 (Rectangle 0 60 100 20)]

    it "renders nothing when there are no items" $ do
      ctx' <- runVirtualContent 0 20 0
      [c | c@(FillRect _ _) <- getDrawCommands ctx'] `shouldBe` []

    it "clips content to the current bounds" $ do
      ctx' <- runVirtualContent 0 20 10
      getDrawCommands ctx' `shouldContain` [PushClip controlRect]

  describe "selector" $ do
    let renderItem :: Int -> Bool -> (String, Text) -> UI Int String ()
        renderItem _eid isSelected (_val, lbl) =
          drawText testColour AlignLeft ((if isSelected then "SEL:" else "UNSEL:") <> lbl)

        runSelector :: String -> UIContext Int String -> IO (UIContext Int String)
        runSelector sel = fmap (settle . snd) . runUI (selector id [items radioItems, selected sel, onSelect (postWith id)] renderItem)

    describe "selection" $ do
      it "dispatches the value of a clicked item" $ do
        ctx' <- runSelector "a" (withButtonReleased (mkRadioGroupCtx (mouseAt (Point 50 45) False [])))
        getMessages ctx' `shouldBe` ["b"]

      it "dispatches the value when Enter is pressed while an item is focused" $ do
        ctx' <- runSelector "a" (withFocus (Just 1) (mkRadioGroupCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        getMessages ctx' `shouldBe` ["b"]

      it "does not dispatch when clicked while disabled" $ do
        (_, ctx') <- runUI (disableWhen True (selector id [items radioItems, selected "a", onSelect (postWith id)] renderItem))
          (withButtonReleased (mkRadioGroupCtx (mouseAt (Point 50 45) False [])))
        dispatchCount (settle ctx') `shouldBe` 0

      it "does not dispatch when there is no interaction" $ do
        ctx' <- runSelector "b" (mkRadioGroupCtx noInput)
        dispatchCount ctx' `shouldBe` 0

    describe "keyboard navigation" $ do
      let nav focusIdx k = do
            ctx' <- runSelector "a" (withFocus (Just focusIdx) (mkRadioGroupCtx noInput { inputKeyEvents = [KeyEvent k []] }))
            pure $ getFocused ctx'

      it "moves focus to the next item when Down is pressed" $ do
        result <- nav 0 KeyDown
        result `shouldBe` Just 1

      it "moves focus to the previous item when Up is pressed" $ do
        result <- nav 1 KeyUp
        result `shouldBe` Just 0

      it "does not move focus when disabled" $ do
        (_, ctx') <- runUI (disableWhen True (selector id [items radioItems, selected "a", onSelect (postWith id)] renderItem))
          (withFocus (Just 0) (mkRadioGroupCtx noInput { inputKeyEvents = [KeyEvent KeyDown []] }))
        getFocused (settle ctx') `shouldBe` Just 0

    describe "rendering" $ do
      it "passes isSelected=True for the selected item" $ do
        ctx' <- runSelector "b" (mkRadioGroupCtx noInput)
        drawnTexts ctx' `shouldContain` ["SEL:Beta"]

      it "passes isSelected=False for other items" $ do
        ctx' <- runSelector "b" (mkRadioGroupCtx noInput)
        drawnTexts ctx' `shouldContain` ["UNSEL:Alpha"]
        drawnTexts ctx' `shouldContain` ["UNSEL:Gamma"]

    describe "defaults" $ do
      it "defaults to no items when items is not given" $ do
        ctx' <- fmap (settle . snd) $ runUI (selector id [onSelect (postWith id)] renderItem) (mkRadioGroupCtx noInput)
        drawnTexts ctx' `shouldBe` []

      it "defaults to nothing selected when selected is not given" $ do
        ctx' <- fmap (settle . snd) $ runUI (selector id [items radioItems, onSelect (postWith id)] renderItem) (mkRadioGroupCtx noInput)
        drawnTexts ctx' `shouldNotContain` ["SEL:Alpha"]
        drawnTexts ctx' `shouldNotContain` ["SEL:Beta"]
        drawnTexts ctx' `shouldNotContain` ["SEL:Gamma"]

    describe "lifecycle events" $
      it "fires FocusGained via onFocusGained for whichever item gains focus" $ do
        let attrs = [items radioItems, selected "a", onFocusGained (post "gained")]
        ctx' <- fmap (settle . snd) $ runUI (selector id attrs renderItem) (mkRadioGroupCtx noInput)
        getMessages ctx' `shouldBe` ["gained"]

  describe "radioGroup" $ do
    describe "selection" $ do
      it "dispatches the value of a clicked item" $ do
        ctx' <- runRadioGroup "a" (withButtonReleased (mkRadioGroupCtx (mouseAt (Point 50 45) False [])))
        getMessages ctx' `shouldBe` ["b"]

      it "dispatches the correct value when the last item is clicked" $ do
        ctx' <- runRadioGroup "a" (withButtonReleased (mkRadioGroupCtx (mouseAt (Point 50 75) False [])))
        getMessages ctx' `shouldBe` ["c"]

      it "dispatches the value when Enter is pressed while an item is focused" $ do
        ctx' <- runRadioGroup "a" (withFocus (Just 1) (mkRadioGroupCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        getMessages ctx' `shouldBe` ["b"]

      it "dispatches the value when Space is pressed while an item is focused" $ do
        ctx' <- runRadioGroup "a" (withFocus (Just 2) (mkRadioGroupCtx noInput { inputKeyEvents = [KeyEvent KeySpace []] }))
        getMessages ctx' `shouldBe` ["c"]

      it "does not dispatch when no item is focused and a key is pressed" $ do
        ctx' <- runRadioGroup "a" (withFocus (Just 99) (mkRadioGroupCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        dispatchCount ctx' `shouldBe` 0

      it "does not dispatch when there is no interaction" $ do
        ctx' <- runRadioGroup "b" (mkRadioGroupCtx noInput)
        dispatchCount ctx' `shouldBe` 0

    describe "keyboard navigation" $ do
      let nav focusIdx k = do
            ctx' <- runRadioGroup "a" (withFocus (Just focusIdx) (mkRadioGroupCtx noInput { inputKeyEvents = [KeyEvent k []] }))
            pure $ getFocused ctx'

      it "moves focus to the next item when Down is pressed" $ do
        result <- nav 0 KeyDown
        result `shouldBe` Just 1

      it "moves focus to the previous item when Up is pressed" $ do
        result <- nav 1 KeyUp
        result `shouldBe` Just 0

      it "stays on the last item when Down is pressed at the end" $ do
        result <- nav 2 KeyDown
        result `shouldBe` Just 2

      it "stays on the first item when Up is pressed at the beginning" $ do
        result <- nav 0 KeyUp
        result `shouldBe` Just 0

      it "does not move focus when disabled" $ do
        (_, ctx') <- runUI (disableWhen True (radioGroup id [items radioItems, selected "a", onSelect (postWith id)]))
          (withFocus (Just 0) (mkRadioGroupCtx noInput { inputKeyEvents = [KeyEvent KeyDown []] }))
        getFocused (settle ctx') `shouldBe` Just 0

      it "handles arrow keys on the frame focus is gained by click" $ do
        -- Click on item 0 (centre Point 50 15) and press Down in the same
        -- frame with no prior focus. The newly focused item should handle
        -- the key.
        ctx' <- runRadioGroup "a" (withButtonReleased (mkRadioGroupCtx noInput { inputMousePosition = Point 50 15, inputKeyEvents = [KeyEvent KeyDown []] }))
        getFocused ctx' `shouldBe` Just 1

    describe "rendering" $ do
      it "shows the selected mark on the selected item" $ do
        ctx' <- runRadioGroup "b" (mkRadioGroupCtx noInput)
        drawnTexts ctx' `shouldContain` ["● Beta"]

      it "shows the unselected mark on other items" $ do
        ctx' <- runRadioGroup "b" (mkRadioGroupCtx noInput)
        drawnTexts ctx' `shouldContain` ["○ Alpha"]
        drawnTexts ctx' `shouldContain` ["○ Gamma"]

      it "displays all labels regardless of selection" $ do
        ctx' <- runRadioGroup "a" (mkRadioGroupCtx noInput)
        length (drawnTexts ctx') `shouldBe` 3

  describe "listBox" $ do
    describe "selection" $ do
      it "dispatches the value of a clicked item" $ do
        ctx' <- runListBox 0 (withButtonReleased (mkListBoxCtx (mouseAt (Point 50 30) False [])))
        getMessages ctx' `shouldBe` [1]

      it "dispatches the value when Enter is pressed while an item is focused" $ do
        ctx' <- runListBox 0 (withFocus (Just (ListBoxItem 1)) (mkListBoxCtx noInput { inputKeyEvents = [KeyEvent KeyReturn []] }))
        getMessages ctx' `shouldBe` [1]

      it "does not dispatch when clicked while disabled" $ do
        (_, ctx') <- runUI (disableWhen True (listBox id listBoxItemHeight [items listBoxItems, selected 0, onSelect (postWith id)] listBoxRenderItem))
          (withButtonReleased (mkListBoxCtx (mouseAt (Point 50 30) False [])))
        dispatchCount (settle ctx') `shouldBe` 0

      it "does not dispatch when there is no interaction" $ do
        ctx' <- runListBox 0 (mkListBoxCtx noInput)
        dispatchCount ctx' `shouldBe` 0

    describe "keyboard navigation" $ do
      it "moves focus to the next item when Down is pressed" $ do
        ctx' <- runListBox 0 (withFocus (Just (ListBoxItem 0)) (mkListBoxCtx noInput { inputKeyEvents = [KeyEvent KeyDown []] }))
        getFocused ctx' `shouldBe` Just (ListBoxItem 1)

      it "moves focus to the previous item when Up is pressed" $ do
        ctx' <- runListBox 0 (withFocus (Just (ListBoxItem 1)) (mkListBoxCtx noInput { inputKeyEvents = [KeyEvent KeyUp []] }))
        getFocused ctx' `shouldBe` Just (ListBoxItem 0)

      it "does not move focus when disabled" $ do
        (_, ctx') <- runUI (disableWhen True (listBox id listBoxItemHeight [items listBoxItems, selected 0, onSelect (postWith id)] listBoxRenderItem))
          (withFocus (Just (ListBoxItem 0)) (mkListBoxCtx noInput { inputKeyEvents = [KeyEvent KeyDown []] }))
        getFocused (settle ctx') `shouldBe` Just (ListBoxItem 0)

    describe "scroll-to-current" $ do
      it "scrolls down when the current item moves past the bottom of the window" $ do
        -- Item 2 (y 40-60) is the last visible row; moving to item 3 (y
        -- 60-80) requires scrolling so its bottom (80) reaches the viewport
        -- bottom: newScroll = 80 - 60 = 20px, as a fraction of the 60px of
        -- scrollable range (120px content - 60px viewport) = 1/3.
        ctx' <- runListBox 0 (withFocus (Just (ListBoxItem 2)) (mkListBoxCtx noInput { inputKeyEvents = [KeyEvent KeyDown []] }))
        getFocused ctx' `shouldBe` Just (ListBoxItem 3)
        listBoxScrollFrac ctx' `shouldBe` 20 / 60

      it "scrolls up when the current item moves above the top of the window" $ do
        -- Scrolled so item 1 (y 20-40) is the first visible row; moving to
        -- item 0 (y 0-20) requires scrolling back to the top.
        ctx' <- runListBox 0 (withListBoxScroll (20 / 60) (withFocus (Just (ListBoxItem 1)) (mkListBoxCtx noInput { inputKeyEvents = [KeyEvent KeyUp []] })))
        getFocused ctx' `shouldBe` Just (ListBoxItem 0)
        listBoxScrollFrac ctx' `shouldBe` 0

      it "does not change scroll when the new current item is already visible" $ do
        ctx' <- runListBox 0 (withFocus (Just (ListBoxItem 0)) (mkListBoxCtx noInput { inputKeyEvents = [KeyEvent KeyDown []] }))
        listBoxScrollFrac ctx' `shouldBe` 0

    describe "rendering" $ do
      it "passes isSelected=True for the selected item" $ do
        ctx' <- runListBox 1 (mkListBoxCtx noInput)
        drawnTexts ctx' `shouldContain` ["SEL:Item1"]

      it "passes isSelected=False for other visible items" $ do
        ctx' <- runListBox 1 (mkListBoxCtx noInput)
        drawnTexts ctx' `shouldContain` ["UNSEL:Item0"]
        drawnTexts ctx' `shouldContain` ["UNSEL:Item2"]

      it "only renders the items within the visible window" $ do
        ctx' <- runListBox 0 (mkListBoxCtx noInput)
        drawnTexts ctx' `shouldContain` ["SEL:Item0"]
        drawnTexts ctx' `shouldContain` ["UNSEL:Item1"]
        drawnTexts ctx' `shouldContain` ["UNSEL:Item2"]
        drawnTexts ctx' `shouldNotContain` ["UNSEL:Item3"]
        drawnTexts ctx' `shouldNotContain` ["UNSEL:Item4"]
        drawnTexts ctx' `shouldNotContain` ["UNSEL:Item5"]

      it "renders items scrolled into view instead of the top of the list" $ do
        ctx' <- runListBox 0 (withListBoxScroll (20 / 60) (mkListBoxCtx noInput))
        drawnTexts ctx' `shouldContain` ["UNSEL:Item3"]
        drawnTexts ctx' `shouldNotContain` ["UNSEL:Item0"]

    describe "defaults" $ do
      it "renders no items when items is not given" $ do
        -- The scrollbar (its decr/incr buttons draw "▲"/"▼") is unaffected
        -- by the item list being empty; only item markers are checked here.
        ctx' <- fmap (settle . snd) $ runUI (listBox id listBoxItemHeight [onSelect (postWith id)] listBoxRenderItem) (mkListBoxCtx noInput)
        [t | t <- drawnTexts ctx', t /= "▲", t /= "▼"] `shouldBe` []

      it "defaults to nothing selected when selected is not given" $ do
        ctx' <- fmap (settle . snd) $ runUI (listBox id listBoxItemHeight [items listBoxItems, onSelect (postWith id)] listBoxRenderItem) (mkListBoxCtx noInput)
        drawnTexts ctx' `shouldNotContain` ["SEL:Item0"]
