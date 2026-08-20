{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Blink.ControlsSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec

import Blink.Controls
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
  , styledElement
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
  , SelectionState (..)
  , SelectionEvent (..)
  , itemContainer
  , itemTemplate
  , items
  , itemsLayout
  , itemsPanel
  , CompositeControlConfig
  , compositeControl
  , onSelect
  , selected
  , selectedIndex
  , selectionControl
  , RadioPart (..)
  , onPick
  , picked
  , radioButton
  , RadioGroupPart (..)
  , RadioGroupConfig
  , itemLabel
  , radioGroup
  )
import Blink.ControlsTestSupport
  ( TestElement (..)
  , bgRect
  , drawnTexts
  , contentRect
  , controlRect
  , dispatchCount
  , insidePoints
  , minimalControl
  , mouseAt
  , noInput
  , outsidePoints
  , settle
  , testBorderColour
  , testColour
  , testStyle
  , testTheme
  , testThemeWithBorder
  )
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Blink.Geometry (Alignment (..), Orientation (..), Point (..), Rectangle (..), Size (..), noBorder, uniform, uniformBorder)
import Blink.Input (InputState (..), Key (..), KeyEvent (..), Modifier (..))
import Blink.Layout (BoxConfig (..), Layout (..), Length (..), defaultBoxConfig)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

-- | Off-screen fixed location for a real "other" control, standing in for
-- "focus\/capture belongs to some other, unrelated element" wherever a test
-- needs that as a precondition. It never overlaps any control under test
-- (all of which use small positive-coordinate rects), so it can be reused
-- everywhere without per-test geometry.
otherRect :: Rectangle
otherRect = Rectangle (-1000) (-1000) 100 100

otherHitPoint :: Point
otherHitPoint = Point (-950) (-950)

-- | Composes @action@ with a real off-screen control (rendered first) that
-- can legitimately hold focus\/capture\/tab-stop precedence instead of it —
-- replaces poking a synthetic "other element" id directly into the context.
withOther :: Ord e => e -> UI e s () -> UI e s ()
withOther otherId action = withBounds otherRect (minimalControl otherId) >> action

-- | 'Blink.ControlsTestSupport.mkCtx' fixes @msg ~ ()@; several suites below
-- (anything using 'fire' to observe emitted values) need other message
-- types, so this stays polymorphic.
mkCtxFor :: InputState -> UIContext TestElement msg
mkCtxFor input = emptyUIContext controlRect input testTheme noOpTextMeasurer

-- | Advances @ctx@ to the next frame with @input@, carrying its theme and
-- animation state forward unchanged.
advance :: Ord e => Rectangle -> InputState -> UIContext e msg -> UIContext e msg
advance bounds input ctx = nextFrameContext bounds input (contextTheme ctx) (contextAnimation ctx) ctx

-- | Seeds an exact scroll\/selection precondition through the public
-- 'emitUi'\/'UiEffect' API — reproducing it via a pixel-accurate drag would
-- be fragile, and the effect-queue mechanism is already the sanctioned way
-- to set this state, applied for real by 'runInteractions's auto-settle.
seedEffect :: Ord e => Rectangle -> UIContext e msg -> UiEffect e -> IO (UIContext e msg)
seedEffect bounds ctx0 eff = resultContext <$> runInteractions bounds ctx0 (emitUi eff) [] []

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

-- radioButton fixtures ------------------------------------------------------

-- radioButton shares checkbox's row layout exactly (20px glyph column then
-- label), so it reuses 'checkboxRect'\/'checkboxStyleSet'\/'glyphPoint'\/
-- 'labelPoint' -- only the element type and theme differ.
radioTheme :: Theme RadioPart
radioTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = checkboxStyleSet }

mkRadioCtx :: InputState -> UIContext RadioPart ()
mkRadioCtx input = emptyUIContext checkboxRect input radioTheme noOpTextMeasurer

runRadioButton :: Bool -> UIContext RadioPart () -> IO (UIContext RadioPart ())
runRadioButton isPicked ctx = fmap (settle . snd) $ runUI (radioButton id [text "Ship to home", picked isPicked, onPick (post ())]) ctx

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


-- viewport tests use a 200x100 outer rect throughout, so the same
-- content-size scenarios (fits / H-only / V-only / both) produce the same
-- scrollbar geometry across every describe block below:
--   fits:    Size 100 50  -> no scrollbars,               vp 200x100
--   H-only:  Size 300 50  -> hBar only,                   vp 200x84
--   V-only:  Size 50  200 -> vBar only,                   vp 184x100
--   both:    Size 300 200 -> hBar (0,84,184,16) + vBar (184,0,16,84), vp 184x84
data ViewportElem = VPPart ViewportPart | VPChild
  deriving (Eq, Ord, Show)

data TIElem = TIButton1 | TIButton2 | TICheckbox CheckboxPart | TIDisabledButton | TIDisabledGroup RadioGroupPart
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

-- | The generic focus\/tab\/hover\/disabled behaviour every control gets for
-- free from 'control'\/'activatable', re-verified through each control's
-- own real function rather than through those shared primitives directly —
-- catching a control that mis-wires its own id or attrs into them, which a
-- primitive-only test can't. Mirrors the pre-migration suite's
-- @controlBehaviourSpec@, generalised over element type and app state so
-- it's reusable across controls that don't all share one harness.
controlBehaviourSpec
  :: (Ord e, Show e)
  => Rectangle                     -- ^ the harness's outer bounds
  -> (InputState -> UIContext e s) -- ^ builds a fresh context
  -> e                             -- ^ the element under test
  -> e                             -- ^ a distinct other element
  -> Point                         -- ^ a point that hits the element under test
  -> UI e s ()                     -- ^ the widget under test
  -> Spec
controlBehaviourSpec bounds mkCtx this other hitPoint action = do
  let composed = withOther other action

  describe "focus" $ do
    it "receives focus when nothing else is focused" $ do
      result <- runInteractions bounds (mkCtx noInput) action [] []
      contextFocus (resultContext result) `shouldBe` Just this

    it "does not take focus from another element" $ do
      -- `other` renders first and auto-claims focus since nothing is
      -- focused yet — no click needed.
      result <- runInteractions bounds (mkCtx noInput) composed [] []
      contextFocus (resultContext result) `shouldBe` Just other

    it "receives focus when clicked" $ do
      result <- runInteractions bounds (mkCtx noInput) composed [] [ClickAt hitPoint]
      contextFocus (resultContext result) `shouldBe` Just this

    it "does not steal focus when the mouse is released on it after dragging from another element" $ do
      -- `other` auto-claims focus for real just by rendering first (nothing
      -- is focused yet), no explicit click needed — this exercises the
      -- auto-claim-then-drag path, distinct from the explicit-click path
      -- the next test covers. Both converge on the same "isDragRelease
      -- blocks stealing" check, so both end up focused on `other`, not
      -- Nothing: with `this` defaulting to FocusSelf, nothing ever reaches
      -- a genuinely unfocused steady state once any real interaction runs,
      -- so a synthetic "capture held, nothing focused" precondition was
      -- never a state a real interaction sequence could produce.
      result <- runInteractions bounds (mkCtx noInput) composed []
        [MouseDown otherHitPoint, DragTo hitPoint, MouseUp hitPoint]
      contextFocus (resultContext result) `shouldBe` Just other

    it "retains focus on the previously focused element when a drag releases elsewhere" $ do
      result <- runInteractions bounds (mkCtx noInput) composed
        [ClickAt otherHitPoint]
        [MouseDown otherHitPoint, DragTo hitPoint, MouseUp hitPoint]
      contextFocus (resultContext result) `shouldBe` Just other

  describe "tab navigation" $ do
    it "passes focus to the next control when Tab is pressed" $ do
      result <- runInteractions bounds (mkCtx noInput) composed [ClickAt hitPoint] [Tab]
      contextFocus (resultContext result) `shouldBe` Nothing

    it "passes focus to the previous control when Shift+Tab is pressed" $ do
      -- `other` renders before `this` every frame, so it legitimately
      -- becomes the previous tab stop just by being in the tree.
      result <- runInteractions bounds (mkCtx noInput) composed [ClickAt hitPoint] [ShiftTab]
      contextFocus (resultContext result) `shouldBe` Just other

  describe "hover detection" $ do
    it "registers mouse-over on the frame after the mouse is inside" $ do
      result <- runInteractions bounds (mkCtx (mouseAt hitPoint False []))
        (action >> wasMouseOverLastFrame this) [] [Wait 2]
      resultValue result `shouldBe` True

    it "does not register mouse-over when the mouse is outside" $ do
      result <- runInteractions bounds (mkCtx (mouseAt (Point (-500) (-500)) False []))
        (action >> wasMouseOverLastFrame this) [] [Wait 2]
      resultValue result `shouldBe` False

  describe "when disabled" $ do
    let disabledAction      = disableWhen True action
        disabledComposed    = withOther other (disableWhen True action)

    it "does not take auto-focus" $ do
      result <- runInteractions bounds (mkCtx noInput) disabledAction [] []
      contextFocus (resultContext result) `shouldBe` Nothing

    it "does not steal focus when clicked" $ do
      result <- runInteractions bounds (mkCtx noInput) disabledComposed [ClickAt otherHitPoint] [ClickAt hitPoint]
      contextFocus (resultContext result) `shouldBe` Just other

    it "does not register mouse-over when the mouse is inside" $ do
      result <- runInteractions bounds (mkCtx (mouseAt hitPoint False []))
        (disabledAction >> wasMouseOverLastFrame this) [] [Wait 2]
      resultValue result `shouldBe` False

    it "is not recorded as the previous tab stop" $ do
      result <- runInteractions bounds (mkCtx noInput) disabledAction [] []
      contextPrevTabStop (resultContext result) `shouldBe` Nothing

    it "does not consume Tab or lose focus when disabled while focused" $ do
      focused <- runInteractions bounds (mkCtx noInput) action [] [ClickAt hitPoint]
      result <- runInteractions bounds (resultContext focused) disabledAction [] [Tab]
      contextFocus (resultContext result) `shouldBe` Just this

    it "does not hand focus to the previous tab stop on Shift+Tab when disabled while focused" $ do
      focused <- runInteractions bounds (mkCtx noInput) composed [] [ClickAt hitPoint]
      result <- runInteractions bounds (resultContext focused) disabledComposed [] [ShiftTab]
      contextFocus (resultContext result) `shouldBe` Just this

-- | Background\/border chrome rendering, re-verified through each control's
-- own function. Only applicable to controls that render their own chrome
-- directly across a single rectangle sized from margin\/padding\/border (not
-- composites like 'checkbox'\/'slider' whose test harnesses use zero-margin
-- styles for drag-position precision instead). Mirrors the pre-migration
-- suite's @backgroundAndBorderSpec@.
backgroundAndBorderSpec :: Rectangle -> UI TestElement s () -> Spec
backgroundAndBorderSpec bounds action = do
  let seed         = emptyUIContext bounds noInput testTheme noOpTextMeasurer
      borderedSeed = emptyUIContext bounds noInput testThemeWithBorder noOpTextMeasurer

  it "does not draw a background in the margin area" $ do
    result <- runInteractions bounds seed action [] []
    resultDraws result `shouldNotContain` [FillRect controlRect testColour]

  it "fills its background area" $ do
    result <- runInteractions bounds seed action [] []
    resultDraws result `shouldContain` [FillRect bgRect testColour]

  it "clips content to its padding area" $ do
    result <- runInteractions bounds seed action [] []
    resultDraws result `shouldContain` [PushClip contentRect]

  it "does not draw a border when borderColour is Nothing" $ do
    result <- runInteractions bounds seed action [] []
    resultDraws result `shouldNotContain` [StrokeBorder bgRect testBorderColour (uniformBorder 1)]

  it "draws a border when borderColour is set" $ do
    result <- runInteractions bounds borderedSeed action [] []
    resultDraws result `shouldContain` [StrokeBorder bgRect testBorderColour (uniformBorder 1)]

-- itemsLayout/selectionControl fixtures -----------------------------------

itemLabels :: [Text]
itemLabels = ["Alpha", "Beta", "Gamma"]

-- Zero margin/padding so the whole 100x90 rect is evenly split three ways,
-- so click points are easy to reason about.
zeroChromeStyle :: Style
zeroChromeStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignLeft
  , styleMargin       = uniform 0
  , stylePadding      = uniform 0
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

zeroChromeStyleSet :: StyleSet
zeroChromeStyleSet = StyleSet
  { styleSetNormal   = zeroChromeStyle
  , styleSetHovered  = zeroChromeStyle
  , styleSetPressed  = zeroChromeStyle
  , styleSetFocused  = zeroChromeStyle
  , styleSetDisabled = zeroChromeStyle
  }

listRect :: Rectangle
listRect = Rectangle 0 0 100 90

-- 'itemsLayout' has no element id, so it never looks anything up in the
-- theme; 'Int' is just a convenient, concrete element type for
-- 'selectionControl's per-item ids.
listTheme :: Theme Int
listTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = zeroChromeStyleSet }

mkListCtx :: InputState -> UIContext Int msg
mkListCtx input = emptyUIContext listRect input listTheme noOpTextMeasurer

isFillRect :: DrawCommand -> Bool
isFillRect (FillRect {}) = True
isFillRect _             = False

-- | Draws only the item's label -- used to check ordering/content.
itemsRenderItem :: Int -> Text -> (Layout, UI Int String ())
itemsRenderItem _ lbl = (Layout Fill Fill TopLeft, drawText testColour AlignLeft lbl)

-- | Draws its own bounds as @(x, y)@ -- used to check panel arrangement.
boundsRenderItem :: Int -> Text -> (Layout, UI Int String ())
boundsRenderItem _ _ =
  ( Layout Fill Fill TopLeft
  , do
      r <- getBounds
      drawText testColour AlignLeft (T.pack (show (rectX r, rectY r)))
  )

selectionRenderItem :: Int -> SelectionState -> Text -> (Layout, UI Int (Int, Text) ())
selectionRenderItem _ st lbl =
  (Layout Fill Fill TopLeft, drawText testColour AlignLeft ((if st == Selected then "SEL:" else "UNSEL:") <> lbl))

dispatchActivated :: Int -> Text -> [Out Int (Int, Text)]
dispatchActivated idx val = [OutMsg (idx, val)]

-- | Shared behaviour: renders 'items' via the template, in order.
itemOrderingSpec :: (UIContext e msg -> IO (UIContext e msg)) -> UIContext e msg -> [Text] -> Spec
itemOrderingSpec run baseCtx expectedTexts =
  it "renders each item's content, in order" $ do
    ctx' <- run baseCtx
    drawnTexts ctx' `shouldBe` expectedTexts

-- | Shared behaviour: renders nothing when 'items' is left at its default
-- (empty).
emptyItemsSpec :: (UIContext e msg -> IO (UIContext e msg)) -> UIContext e msg -> Spec
emptyItemsSpec run baseCtx =
  it "renders nothing when items is empty" $ do
    ctx' <- run baseCtx
    drawnTexts ctx' `shouldBe` []

-- | Shared behaviour: neither control is a chrome-drawing control -- see
-- the "Items and selection" section of the "Blink.Controls" module header.
noChromeSpec :: (UIContext e msg -> IO (UIContext e msg)) -> UIContext e msg -> Spec
noChromeSpec run baseCtx =
  it "draws no background or border of its own" $ do
    ctx' <- run baseCtx
    filter isFillRect (getDrawCommands ctx') `shouldBe` []

-- radioGroup fixtures --------------------------------------------------------

-- Reuses 'itemLabels'\/'listRect'\/'zeroChromeStyleSet' from the
-- 'selectionControl' fixtures above -- same three-row layout, just under
-- 'RadioGroupPart' instead of bare 'Int' since 'radioGroup' has its own
-- group-level id ('RadioGroup') alongside each item's own 'RadioPart's
-- ('RadioItem' idx part).
radioGroupTheme :: Theme RadioGroupPart
radioGroupTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = zeroChromeStyleSet }

mkRadioGroupCtx :: InputState -> UIContext RadioGroupPart msg
mkRadioGroupCtx input = emptyUIContext listRect input radioGroupTheme noOpTextMeasurer

dispatchRGActivated :: Int -> Text -> [Out RadioGroupPart (Int, Text)]
dispatchRGActivated idx val = [OutMsg (idx, val)]

runRadioGroup :: [Attr RadioGroupPart (SelectionEvent Text) (Int, Text) (RadioGroupConfig Text)]
              -> UIContext RadioGroupPart (Int, Text) -> IO (UIContext RadioGroupPart (Int, Text))
runRadioGroup attrs = fmap (settle . snd) . runUI (radioGroup id attrs)

-- withFocusScope fixtures --------------------------------------------------

-- | A composite ('ListElem'\/'GroupElem'), its direct children
-- ('RowElem'\/'SubRowElem'), and two plain, unrelated elements used to prove
-- a composite's focus window doesn't leak past its own boundary.
data CompElem = ListElem | RowElem Int | GroupElem | SubRowElem Int | AfterElem | SiblingElem
  deriving (Eq, Ord, Show)

compTheme :: Theme CompElem
compTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = zeroChromeStyleSet }

mkCompCtx :: InputState -> UIContext CompElem msg
mkCompCtx input = emptyUIContext controlRect input compTheme noOpTextMeasurer

-- | A bare focusable leaf, standing in for "some unmodified control" inside
-- a composite -- the point of 'withFocusScope' is that this needs no
-- changes of its own to compose correctly.
leaf :: CompElem -> UI CompElem msg ()
leaf eid = control eid ([] :: [Attr CompElem Probe msg ()]) (pure ())

-- tabTraversalSpec fixtures ---------------------------------------------

-- | A slot in a full-UI traversal scenario: either a plain 'button' or a
-- real 'radioGroup', addressed by position. Composite slots use
-- 'radioGroup' directly (not a test-only shim), so they get its actual
-- atomic-tab-stop behaviour -- the group itself is the one Tab target,
-- never one of its items.
data MultiElem = MultiButton Int | MultiGroup Int RadioGroupPart
  deriving (Eq, Ord, Show)

multiTheme :: Theme MultiElem
multiTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = checkboxStyleSet }

multiButton :: Int -> UI MultiElem msg ()
multiButton n = button (MultiButton n) [text (T.pack ("Button" <> show n))]

multiGroup :: Int -> UI MultiElem msg ()
multiGroup n = radioGroup (MultiGroup n) [items (["X", "Y"] :: [Text]), itemLabel id]

-- | Drives @key@ (Tab or ShiftTab) @n@ times from @ctx0@, collecting the
-- focused element after each press. Each press is modelled as the key
-- event followed by one idle frame, mirroring 'Blink.App.doStepEventDriven'
-- -- which runs a real event-driven frame twice, key then settle -- so a
-- hand-off that only completes on that second pass (e.g. forward wrap,
-- which has no direct previous-tab-stop-style jump the way Shift-Tab does)
-- still resolves within what one physical key press actually produces.
driveFocusSequence
  :: Ord e => Rectangle -> UI e msg () -> Interaction -> UIContext e msg -> Int -> IO [Maybe e]
driveFocusSequence bounds render interaction ctx0 n = go ctx0 n
  where
    go _ 0 = pure []
    go ctx k = do
      result <- runInteractions bounds ctx render [] [interaction, Wait 1]
      rest <- go (resultContext result) (k - 1)
      pure (contextFocus (resultContext result) : rest)

-- | The generic Tab\/Shift-Tab cycling behaviour any full UI should have,
-- regardless of what mix of plain and composite controls it's built from.
-- @expectedOrder@ is the enabled, focusable elements in the order Tab
-- should visit them -- its length doubles as the number of key presses to
-- drive, and its own contents double as the expected landing spot at each
-- step, so the same three checks scale to any scenario fixture without
-- this function needing to know its shape in advance.
tabTraversalSpec :: (Ord e, Show e) => Rectangle -> Theme e -> [e] -> UI e msg () -> Spec
tabTraversalSpec bounds theme expectedOrder render = do
  let ctx0 = emptyUIContext bounds noInput theme noOpTextMeasurer

  it "auto-claims the first element when nothing is focused" $ do
    result <- runInteractions bounds ctx0 render [] []
    contextFocus (resultContext result) `shouldBe` Just (head expectedOrder)

  it "Tab visits every expected element in order, then wraps" $ do
    settled <- runInteractions bounds ctx0 render [] []
    let steps = drop 1 expectedOrder ++ [head expectedOrder]
    focuses <- driveFocusSequence bounds render Tab (resultContext settled) (length steps)
    focuses `shouldBe` map Just steps

  it "Shift-Tab visits the same elements in reverse" $ do
    settled <- runInteractions bounds ctx0 render [] []
    focuses <- driveFocusSequence bounds render ShiftTab (resultContext settled) (length expectedOrder)
    focuses `shouldBe` map Just (reverse expectedOrder)

spec :: Spec
spec = describe "Blink.Controls" $ do
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
      let ctx2 = advance controlRect (mouseAt (Point 50 50) False []) ctx1
      (_, ctx3) <- runUI (applyMouseOver TestControl [captureAttrs]) ctx2
      getMessages ctx3 `shouldBe` []

    it "fires MouseExited on the frame the mouse leaves after being over" $ do
      (_, ctx1) <- runUI (applyMouseOver TestControl [captureAttrs]) (mkCtxFor (mouseAt (Point 50 50) False []))
      let ctx2 = advance controlRect (mouseAt (Point 200 200) False []) ctx1
      (_, ctx3) <- runUI (applyMouseOver TestControl [captureAttrs]) ctx2
      getMessages ctx3 `shouldBe` [Probe MouseExited]

    it "fires nothing when the mouse was never over" $ do
      (_, ctx) <- runUI (applyMouseOver TestControl [captureAttrs]) (mkCtxFor (mouseAt (Point 200 200) False []))
      getMessages ctx `shouldBe` []

    it "acquires hot capture when hit and the button is down" $ do
      (_, ctx) <- runUI (applyMouseOver TestControl noProbeAttrs) (mkCtxFor (mouseAt (Point 50 50) True []))
      contextCaptured ctx `shouldBe` MouseCapturedBy TestControl

    it "does not register mouse-over, or fire enter, when disabled" $ do
      (_, ctx) <- runUI (disableWhen True (applyMouseOver TestControl [captureAttrs]))
        (mkCtxFor (mouseAt (Point 50 50) False []) :: UIContext TestElement Probe)
      getMessages ctx `shouldBe` []

    it "notifies the caller when the mouse enters" $ do
      let attrs = [onMouseEnter (post "entered")] :: [Attr TestElement Probe String ()]
      (_, ctx) <- runUI (applyMouseOver TestControl attrs) (mkCtxFor (mouseAt (Point 50 50) False []))
      getMessages ctx `shouldBe` ["entered"]

    it "notifies the caller when the mouse leaves after being over" $ do
      let enterAttrs = [] :: [Attr TestElement Probe String ()]
          exitAttrs  = [onMouseExit (post "exited")] :: [Attr TestElement Probe String ()]
      (_, ctx1) <- runUI (applyMouseOver TestControl enterAttrs) (mkCtxFor (mouseAt (Point 50 50) False []))
      let ctx2 = advance controlRect (mouseAt (Point 200 200) False []) ctx1
      (_, ctx3) <- runUI (applyMouseOver TestControl exitAttrs) ctx2
      getMessages ctx3 `shouldBe` ["exited"]

    it "several elements can each register mouse-over in the same frame" $ do
      let ctx0 = mkCtxFor (mouseAt (Point 50 50) False []) :: UIContext TestElement Probe
      (_, ctx1) <- runUI (applyMouseOver TestControl noProbeAttrs) ctx0
      (_, ctx2) <- runUI (applyMouseOver OtherControl noProbeAttrs) ctx1
      let ctx3 = advance controlRect noInput ctx2
      (a, _) <- runUI (wasMouseOverLastFrame TestControl) ctx3
      (b, _) <- runUI (wasMouseOverLastFrame OtherControl) ctx3
      (a, b) `shouldBe` (True, True)

  describe "applyFocus" $ do
    describe "default (FocusSelf)" $ do
      let action attrs = applyFocus TestControl attrs
          otherThenThis :: forall ev msg cfg. HasControlEvent ev => [Attr TestElement ev msg cfg] -> UI TestElement msg ()
          otherThenThis attrs = applyFocus OtherControl ([] :: [Attr TestElement ev msg cfg]) >> applyFocus TestControl attrs
          noAttrs = [] :: [Attr TestElement Probe Probe ()]
          pt = Point 50 50

      it "receives focus when nothing else is focused" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (action noAttrs) [] []
        contextFocus (resultContext result) `shouldBe` Just TestControl

      it "does not take focus from another element" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (otherThenThis noAttrs) [] []
        contextFocus (resultContext result) `shouldBe` Just OtherControl

      it "receives focus when clicked" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (otherThenThis noAttrs) [] [ClickAt pt]
        contextFocus (resultContext result) `shouldBe` Just TestControl

      it "does not steal focus when the mouse is released on it after dragging from another element" $ do
        -- 'applyFocus' alone never acquires capture (that's 'acquireCapture'/
        -- 'applyMouseOver's job at the 'control' layer) — composed here so
        -- 'isDragRelease' has a real captured element to read. OtherControl
        -- auto-claims focus for real just by rendering first (nothing else
        -- is focused), no explicit click needed.
        let composedOther = acquireCapture OtherControl >> applyFocus OtherControl noAttrs >> action noAttrs
        result <- runInteractions controlRect (mkCtxFor noInput) composedOther []
          [MouseDown pt, MouseUp pt]
        contextFocus (resultContext result) `shouldBe` Just OtherControl

      it "retains focus on the previously focused element when a drag releases elsewhere" $ do
        let composedOther = acquireCapture OtherControl >> applyFocus OtherControl noAttrs >> action noAttrs
        result <- runInteractions controlRect (mkCtxFor noInput) composedOther
          [ClickAt pt] [MouseDown pt, MouseUp pt]
        contextFocus (resultContext result) `shouldBe` Just OtherControl

      it "notifies the caller when focus is gained" $ do
        result <- runInteractions controlRect (mkCtxFor noInput)
          (action ([onFocusGained (post "gained")] :: [Attr TestElement Probe String ()])) [] []
        resultMessages result `shouldBe` ["gained"]

      it "notifies the caller when focus is lost to a Tab press" $ do
        -- This is the reason focus and tab navigation are one primitive: a
        -- Tab-driven loss is only visible to a bracket spanning both, since
        -- applyFocus alone would return before the loss happens, and by the
        -- time anything ran again this same element would have already
        -- auto-reclaimed focus, masking the transition.
        result <- runInteractions controlRect (mkCtxFor noInput)
          (action ([onFocusLost (post "lost")] :: [Attr TestElement Probe String ()])) [ClickAt pt] [Tab]
        resultMessages result `shouldBe` ["lost"]

      it "fires nothing when focus is retained" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (action [captureAttrs]) [ClickAt pt] []
        resultMessages result `shouldBe` []

      it "fires nothing when it stays unfocused" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (otherThenThis [captureAttrs]) [] []
        resultMessages result `shouldBe` []

    describe "tab navigation" $ do
      let action attrs = applyFocus TestControl attrs
          noAttrs = [] :: [Attr TestElement Probe Probe ()]
          pt = Point 50 50

      it "clears focus when Tab is pressed while focused" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (action noAttrs) [ClickAt pt] [Tab]
        contextFocus (resultContext result) `shouldBe` Nothing

      it "passes focus to the previous tab stop when Shift+Tab is pressed" $ do
        let composed = applyFocus OtherControl ([] :: [Attr TestElement Probe Probe ()]) >> action noAttrs
        result <- runInteractions controlRect (mkCtxFor noInput) composed [ClickAt pt] [ShiftTab]
        contextFocus (resultContext result) `shouldBe` Just OtherControl

      it "registers itself as the previous tab stop by default" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (action noAttrs) [] []
        contextPrevTabStop (resultContext result) `shouldBe` Just TestControl

      it "can be excluded from Shift-Tab's target list" $ do
        result <- runInteractions controlRect (mkCtxFor noInput)
          (action ([tabStop False] :: [Attr TestElement Probe Probe ()])) [] []
        contextPrevTabStop (resultContext result) `shouldBe` Nothing

      it "an excluded control leaves the previous tab-stop record unchanged" $ do
        let composed = action noAttrs >> applyFocus OtherControl ([tabStop False] :: [Attr TestElement Probe Probe ()])
        result <- runInteractions controlRect (mkCtxFor noInput) composed [] []
        contextPrevTabStop (resultContext result) `shouldBe` Just TestControl

      it "does not auto-claim focus when excluded from tab order, even with nothing else focused" $ do
        result <- runInteractions controlRect (mkCtxFor noInput)
          (action ([tabStop False] :: [Attr TestElement Probe Probe ()])) [] []
        contextFocus (resultContext result) `shouldBe` Nothing

      it "keeps focus auto-claimed this frame instead of immediately clearing it on the same Tab press" $ do
        -- Regression: wraparound after Tab runs past the last control clears
        -- focus with nothing left this frame to auto-claim it. The *next*
        -- frame's Tab press should let the first control auto-claim (nothing
        -- is focused) and keep it — not immediately lose it again just
        -- because Tab is the very key that's pressed this same frame.
        result <- runInteractions controlRect (mkCtxFor noInput) (action noAttrs) [] [Tab]
        contextFocus (resultContext result) `shouldBe` Just TestControl

      it "does not consume Tab or lose focus when disabled while focused" $ do
        focused <- runInteractions controlRect (mkCtxFor noInput) (action noAttrs) [] [ClickAt pt]
        result <- runInteractions controlRect (resultContext focused) (disableWhen True (action noAttrs)) [] [Tab]
        contextFocus (resultContext result) `shouldBe` Just TestControl

    describe "focusOnClick (FocusTarget)" $ do
      let attrs = [focusOnClick (FocusTarget OtherControl)] :: [Attr TestElement Probe Probe ()]
          pt = Point 50 50

      it "does not auto-claim focus when nothing is focused" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (applyFocus TestControl attrs) [] []
        contextFocus (resultContext result) `shouldBe` Nothing

      it "gives focus to the target when clicked, not to itself" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (applyFocus TestControl attrs) [] [ClickAt pt]
        contextFocus (resultContext result) `shouldBe` Just OtherControl

    describe "focusOnClick (NoFocus)" $ do
      let attrs = [focusOnClick NoFocus] :: [Attr TestElement Probe Probe ()]
          pt = Point 50 50

      it "does not auto-claim focus when nothing is focused" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (applyFocus TestControl attrs) [] []
        contextFocus (resultContext result) `shouldBe` Nothing

      it "does not take focus when clicked" $ do
        result <- runInteractions controlRect (mkCtxFor noInput) (applyFocus TestControl attrs) [] [ClickAt pt]
        contextFocus (resultContext result) `shouldBe` Nothing

      it "still retains focus if it already holds it" $ do
        focused <- runInteractions controlRect (mkCtxFor noInput) (applyFocus TestControl ([] :: [Attr TestElement Probe Probe ()])) [] [ClickAt pt]
        result <- runInteractions controlRect (resultContext focused) (applyFocus TestControl attrs) [] []
        contextFocus (resultContext result) `shouldBe` Just TestControl

  describe "measureChrome" $ do
    it "sums margin, border, and padding on each axis" $ do
      ((Exactly dw, Exactly dh), _) <- runUI (measureChrome TestControl) (mkCtxFor noInput :: UIContext TestElement ())
      (dw, dh) `shouldBe` (30, 30)

    it "includes border width when a border is set" $ do
      let ctx = emptyUIContext controlRect noInput testThemeWithBorder noOpTextMeasurer :: UIContext TestElement ()
      ((Exactly dw, Exactly dh), _) <- runUI (measureChrome TestControl) ctx
      (dw, dh) `shouldBe` (32, 32)

  describe "styledElement" $ do
    let run ctx = snd <$> runUI (styledElement TestControl (pure ())) ctx

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
      let ctx = emptyUIContext controlRect noInput testThemeWithBorder noOpTextMeasurer :: UIContext TestElement ()
      ctx' <- run ctx
      getDrawCommands ctx' `shouldContain` [StrokeBorder bgRect testBorderColour (uniformBorder 1)]

  describe "control" $ do
    let pt = Point 50 50

    it "composes mouse-over, focus, tab navigation, and chrome for a single interactive element" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (control TestControl ([] :: [Attr TestElement Probe Probe ()]) (pure ())) [] [ClickAt pt]
      contextFocus (resultContext result) `shouldBe` Just TestControl
      resultDraws result `shouldContain` [FillRect bgRect testColour]

    it "clicking moves focus onto a different element instead of itself" $ do
      let attrs = [focusOnClick (FocusTarget OtherControl)] :: [Attr TestElement Probe Probe ()]
      result <- runInteractions controlRect (mkCtxFor noInput)
        (control TestControl attrs (pure ())) [] [ClickAt pt]
      contextFocus (resultContext result) `shouldBe` Just OtherControl

    it "runs content within the padded content rectangle" $ do
      ctx' <- snd <$> runUI (control TestControl ([] :: [Attr TestElement Probe Probe ()]) (fillRect testColour)) (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect contentRect testColour]

  describe "withFocusScope" $ do
    -- Standalone usage has no outer focus claim of its own to give up
    -- (that's what 'Blink.Controls.compositeControl' adds via
    -- @blockFreshClaim@), so it always passes 'False'.
    let wc eid inner = withFocusScope eid False inner

    it "a composite is considered focused when its first child auto-claims focus" $ do
      let render = wc ListElem (leaf (RowElem 0) >> leaf (RowElem 1))
      result <- runInteractions controlRect (mkCompCtx noInput) render [] []
      contextFocusChain (resultContext result) `shouldBe` [ListElem, RowElem 0]

    it "a composite retains its focused child across renders" $ do
      let render = wc ListElem (leaf (RowElem 0) >> leaf (RowElem 1))
      -- Frame 1: nothing focused, RowElem 0 auto-claims. Frame 2 (Tab):
      -- RowElem 0 gives up focus and RowElem 1 -- rendered second -- takes
      -- it. RowElem 0 renders first on every subsequent frame, so retaining
      -- RowElem 1 instead of reverting to it is the actual thing under test.
      result <- runInteractions controlRect (mkCompCtx noInput) render [Wait 1, Tab] []
      contextFocusChain (resultContext result) `shouldBe` [ListElem, RowElem 1]

    forM_ [1, 2, 3] $ \n ->
      it ("an empty composite keeps holding focus after " <> show n <> " idle frame(s)") $ do
        let render = wc ListElem (pure ())
        result <- runInteractions controlRect (mkCompCtx noInput) render [] (replicate n (Wait 1))
        contextFocusChain (resultContext result) `shouldBe` [ListElem]

    it "an empty composite claims focus" $ do
      -- Even an empty composite claims the "nothing is focused" opportunity
      -- purely by rendering first, the same way an ordinary focusable
      -- control would -- so a plain control positioned after it must not
      -- treat that as an invitation to auto-claim in its place.
      let render = wc ListElem (pure ()) >> leaf AfterElem
      result <- runInteractions controlRect (mkCompCtx noInput) render [] []
      contextFocusChain (resultContext result) `shouldBe` [ListElem]

    it "a composite is considered focused whenever a nested composite inside it is focused" $ do
      let render = wc ListElem $ do
            wc GroupElem (leaf (SubRowElem 0) >> leaf (SubRowElem 1))
            leaf (RowElem 1)
      result <- runInteractions controlRect (mkCompCtx noInput) render [] []
      let chain = contextFocusChain (resultContext result)
      chain `shouldBe` [ListElem, GroupElem, SubRowElem 0]
      ListElem `elem` chain `shouldBe` True
      GroupElem `elem` chain `shouldBe` True
      SubRowElem 0 `elem` chain `shouldBe` True
      RowElem 1 `elem` chain `shouldBe` False
      SubRowElem 1 `elem` chain `shouldBe` False

    it "a composite does not steal focus already held by an unrelated element" $ do
      let render = leaf SiblingElem >> wc ListElem (leaf (RowElem 0))
      result <- runInteractions controlRect (mkCompCtx noInput) render [] [Wait 1, Wait 1]
      contextFocus (resultContext result) `shouldBe` Just SiblingElem

    it "Shift-Tab from a composite's first child wraps to its last child" $ do
      let render = wc ListElem (leaf (RowElem 0) >> leaf (RowElem 1))
      result <- runInteractions controlRect (mkCompCtx noInput) render [] [Wait 1, ShiftTab]
      contextFocusChain (resultContext result) `shouldBe` [ListElem, RowElem 1]

    it "a child correctly sees itself as no longer focused on the frame its composite releases it" $ do
      let focusedStyle = testStyle { styleBackground = RGBA 0 0 1 1 }
          rowTheme = compTheme
            { themeElementStyles = Map.fromList
                [(RowElem 0, zeroChromeStyleSet { styleSetFocused = focusedStyle })] }
          rowCtx = emptyUIContext controlRect noInput rowTheme noOpTextMeasurer :: UIContext CompElem ()
          styledLeaf eid = control eid ([] :: [Attr CompElem Probe () ()]) (fillRect =<< (styleBackground <$> getStyle eid))
          render = wc ListElem (styledLeaf (RowElem 0))
      result <- runInteractions controlRect rowCtx render [] [Wait 1, Tab]
      resultDraws result `shouldNotContain` [FillRect controlRect (RGBA 0 0 1 1)]

    describe "when disabled" $ do
      -- A disabled composite must never appear to hold focus, not even in
      -- the vacuous "composite focused, no child chosen" shape -- see the
      -- invariant documented directly on 'withFocusScope'.
      it "does not claim focus when nothing else is focused" $ do
        let render = disableWhen True (wc ListElem (leaf (RowElem 0)))
        result <- runInteractions controlRect (mkCompCtx noInput) render [] []
        contextFocus (resultContext result) `shouldNotBe` Just ListElem
        contextFocus (resultContext result) `shouldBe` Nothing

      it "does not block a sibling from claiming focus" $ do
        let render = disableWhen True (wc ListElem (leaf (RowElem 0))) >> leaf AfterElem
        result <- runInteractions controlRect (mkCompCtx noInput) render [] []
        contextFocus (resultContext result) `shouldBe` Just AfterElem

      it "does not steal focus already held by an unrelated element" $ do
        let render = leaf SiblingElem >> disableWhen True (wc ListElem (leaf (RowElem 0)))
        result <- runInteractions controlRect (mkCompCtx noInput) render [] [Wait 1, Wait 1]
        contextFocus (resultContext result) `shouldBe` Just SiblingElem

  describe "isKeyPressed" $ do
    -- isKeyPressed is a bare query primitive with no click-handling of its
    -- own, so a click alone can't establish focus for it — 'setFocus'
    -- (composed into the same re-run-every-frame action) is the real,
    -- direct way to hold focus here.
    it "is True when focused and the key is present this frame" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (setFocus TestControl >> isKeyPressed TestControl KeyReturn) [] [PressKey KeyReturn []]
      resultValue result `shouldBe` True

    it "is False when not focused, even if the key is present" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (setFocus OtherControl >> isKeyPressed TestControl KeyReturn) [] [PressKey KeyReturn []]
      resultValue result `shouldBe` False

    it "is False when focused but the key is absent" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (setFocus TestControl >> isKeyPressed TestControl KeyReturn) [] []
      resultValue result `shouldBe` False

  describe "whenFocused" $ do
    it "runs the action when the element holds focus" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (setFocus TestControl >> whenFocused TestControl (emit (1 :: Int))) [] []
      resultMessages result `shouldBe` [1]

    it "skips the action when the element does not hold focus" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (setFocus OtherControl >> whenFocused TestControl (emit (1 :: Int))) [] []
      resultMessages result `shouldBe` []

  describe "isActivatedBy" $ do
    let pt = Point 50 50

    it "is True when clicked" $ do
      result <- runInteractions controlRect (mkCtxFor noInput) (isActivatedBy TestControl [KeyReturn])
        [] [ClickAt pt]
      resultValue result `shouldBe` True

    it "is False when the click misses" $ do
      result <- runInteractions controlRect (mkCtxFor noInput) (isActivatedBy TestControl [KeyReturn])
        [] [ClickAt (Point 200 200)]
      resultValue result `shouldBe` False

    it "is True when a listed key is pressed while focused" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (applyFocus TestControl ([] :: [Attr TestElement Probe Probe ()]) >> isActivatedBy TestControl [KeyReturn])
        [ClickAt pt] [PressKey KeyReturn []]
      resultValue result `shouldBe` True

    it "is False when neither clicked nor a listed key is pressed" $ do
      result <- runInteractions controlRect (mkCtxFor noInput) (isActivatedBy TestControl [KeyReturn]) [] []
      resultValue result `shouldBe` False

    it "is False when disabled, even if clicked" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (disableWhen True (isActivatedBy TestControl [KeyReturn])) [] [ClickAt pt]
      resultValue result `shouldBe` False

    it "is False on a click released while a different element holds drag capture" $ do
      -- Regression coverage for the fix this primitive needed: it must not
      -- reuse the legacy isHovered/isClicked (built on the single-owner
      -- ixnHovered field this module never writes to), and it must apply
      -- the same drag-exclusion gating applyMouseOver does.
      let composed = applyMouseOver OtherControl noProbeAttrs >> isActivatedBy TestControl [KeyReturn]
      result <- runInteractions controlRect (mkCtxFor noInput) composed []
        [MouseDown pt, MouseUp pt]
      resultValue result `shouldBe` False

    it "is True on a click released while this same element holds drag capture" $ do
      let composed = applyMouseOver TestControl noProbeAttrs >> isActivatedBy TestControl [KeyReturn]
      result <- runInteractions controlRect (mkCtxFor noInput) composed []
        [MouseDown pt, MouseUp pt]
      resultValue result `shouldBe` True

  describe "activatable" $ do
    let pt = Point 50 50

    it "returns True and renders chrome when activated by a click" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (activatable TestControl ([] :: [Attr TestElement Probe Probe ()]) [KeyReturn] (pure ())) [] [ClickAt pt]
      resultValue result `shouldBe` True
      resultDraws result `shouldContain` [FillRect bgRect testColour]

    it "returns False when not activated" $ do
      result <- runInteractions controlRect (mkCtxFor noInput :: UIContext TestElement Probe)
        (activatable TestControl ([] :: [Attr TestElement Probe Probe ()]) [KeyReturn] (pure ())) [] []
      resultValue result `shouldBe` False

    it "still takes focus via the underlying control even when not activated by a key" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (activatable TestControl ([] :: [Attr TestElement Probe Probe ()]) [KeyReturn] (pure ())) [] [ClickAt pt]
      contextFocus (resultContext result) `shouldBe` Just TestControl

  describe "label" $ do
    it "draws the given text in the resolved style's colour and alignment" $ do
      ctx' <- snd <$> runUI (label TestControl [text "Hello"]) (mkCtxFor noInput :: UIContext TestElement ())
      getDrawCommands ctx' `shouldContain` [DrawText contentRect "Hello" testColour AlignCenter]

    it "renders chrome like any other control" $ do
      ctx' <- snd <$> runUI (label TestControl [text "Hello"]) (mkCtxFor noInput :: UIContext TestElement ())
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    it "does not take focus by default, unlike every other control here" $ do
      ctx' <- snd <$> runUI (label TestControl [text "Hello"]) (mkCtxFor noInput :: UIContext TestElement ())
      contextFocus ctx' `shouldBe` Nothing

    it "does not register itself as the previous tab stop by default" $ do
      ctx' <- snd <$> runUI (label TestControl [text "Hello"]) (mkCtxFor noInput :: UIContext TestElement ())
      contextPrevTabStop ctx' `shouldBe` Nothing

    it "clicking moves focus onto a different element instead of itself" $ do
      let attrs = [text "Caption", focusOnClick (FocusTarget OtherControl)]
      result <- runInteractions controlRect (mkCtxFor noInput) (label TestControl attrs) [] [ClickAt (Point 50 50)]
      contextFocus (resultContext result) `shouldBe` Just OtherControl

    it "can still be made reachable by Tab when explicitly requested" $ do
      ctx' <- snd <$> runUI (label TestControl [text "Hello", tabStop True]) (mkCtxFor noInput :: UIContext TestElement ())
      contextPrevTabStop ctx' `shouldBe` Just TestControl

    it "can still be made focusable by a direct click when explicitly requested" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (label TestControl [text "Hello", focusOnClick FocusSelf]) [] [ClickAt (Point 50 50)]
      contextFocus (resultContext result) `shouldBe` Just TestControl

  describe "progressBar" $ do
    let run v ctx = snd <$> runUI (progressBar TestControl [progress (Progress v)]) ctx

    describe "background and border" $ backgroundAndBorderSpec controlRect (progressBar TestControl [progress (Progress 0.5)])

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
      let elapsedCtx = nextFrameContext controlRect noInput testTheme
                         (mkAnimationState 0 1 False)
                         (mkCtxFor noInput :: UIContext TestElement ())

      it "sweeps the band using the default band speed (0.5)" $ do
        ctx' <- snd <$> runUI (progressBar TestControl [progress Indeterminate]) elapsedCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 39.5 15 21 70) testColour]

      it "sweeps faster when a custom speed is given" $ do
        ctx' <- snd <$> runUI (progressBar TestControl [progress Indeterminate, bandSpeed 1.0]) elapsedCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle (-6) 15 21 70) testColour]

      it "keeps the animation ticker alive" $ do
        ctx' <- snd <$> runUI (progressBar TestControl [progress Indeterminate]) elapsedCtx
        contextRequiresAnimation ctx' `shouldBe` True

      it "a determinate bar does not request animation" $ do
        ctx' <- run 0.5 elapsedCtx
        contextRequiresAnimation ctx' `shouldBe` False

  describe "button" $ do
    let pt = Point 50 50

    controlBehaviourSpec controlRect mkCtxFor TestControl OtherControl pt (button TestControl [text "label"])
    describe "background and border" $ backgroundAndBorderSpec controlRect (button TestControl [text "label"])

    it "draws the label" $ do
      ctx' <- snd <$> runUI (button TestControl [text "label"]) (mkCtxFor noInput)
      drawnTexts ctx' `shouldContain` ["label"]

    it "renders chrome like any other control" $ do
      ctx' <- snd <$> runUI (button TestControl [text "label"]) (mkCtxFor noInput)
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    forM_ insidePoints $ \(desc, hitPt) ->
      it ("is clicked when the mouse is released " <> desc) $ do
        result <- runInteractions controlRect (mkCtxFor noInput)
          (button TestControl [text "label", onClick (post ())]) [] [ClickAt hitPt]
        resultMessages result `shouldBe` [()]

    forM_ outsidePoints $ \(desc, missPt) ->
      it ("is not clicked when the mouse is released " <> desc) $ do
        result <- runInteractions controlRect (mkCtxFor noInput)
          (button TestControl [text "label", onClick (post ())]) [] [ClickAt missPt]
        resultMessages result `shouldBe` []

    it "is clicked when Enter is pressed and the button has focus" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (button TestControl [text "label", onClick (post ())]) [ClickAt pt] [PressKey KeyReturn []]
      resultMessages result `shouldBe` [()]

    it "is not clicked when Enter is pressed and the button does not have focus" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (withOther OtherControl (button TestControl [text "label", onClick (post ())])) [] [PressKey KeyReturn []]
      resultMessages result `shouldBe` []

    it "is not clicked when Tab and Enter are pressed simultaneously" $ do
      -- Both key events land in the same frame, which the Interaction DSL's
      -- one-key-per-frame vocabulary can't express — driven directly instead.
      focused <- runInteractions controlRect (mkCtxFor noInput) (button TestControl [text "label", onClick (post ())]) [ClickAt pt] []
      let action = button TestControl [text "label", onClick (post ())]
          frame  = noInput { inputKeyEvents = [KeyEvent KeyTab [], KeyEvent KeyReturn []] }
      (_, ctx') <- runUI action (advance controlRect frame (resultContext focused))
      getMessages ctx' `shouldBe` []

    it "is not activated by a click when disabled" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (disableWhen True (button TestControl [text "label", onClick (post ())])) [] [ClickAt pt]
      resultMessages result `shouldBe` []

    it "is not activated by Enter when disabled" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (disableWhen True (button TestControl [text "label", onClick (post ())])) [ClickAt pt] [PressKey KeyReturn []]
      resultMessages result `shouldBe` []

    it "can queue an effect instead of emitting a message when clicked" $ do
      result <- runInteractions controlRect (mkCtxFor noInput)
        (button TestControl [text "label", onClick (perform (SetSelectionAt TestControl (cursor 0)))]) [] [ClickAt pt]
      contextSelections TestControl (resultContext result) `shouldBe` [cursor 0]
      resultMessages (result :: InteractionResult TestElement () ()) `shouldBe` []

    it "can emit a message and queue an effect together from the same click" $ do
      let attrs = [text "label", onClick (post (1 :: Int) <> perform (SetSelectionAt TestControl (cursor 0)))]
      result <- runInteractions controlRect (mkCtxFor noInput) (button TestControl attrs) [] [ClickAt pt]
      resultMessages result `shouldBe` [1]
      contextSelections TestControl (resultContext result) `shouldBe` [cursor 0]

  describe "renderCheckboxGlyph" $ do
    it "draws a checkmark when checked" $ do
      (_, ctx') <- runUI (renderCheckboxGlyph CheckboxGlyph True) (mkCheckboxCtx noInput)
      drawnTexts ctx' `shouldContain` ["✓"]

    it "draws nothing when unchecked" $ do
      (_, ctx') <- runUI (renderCheckboxGlyph CheckboxGlyph False) (mkCheckboxCtx noInput)
      drawnTexts ctx' `shouldBe` []

  describe "checkbox" $ do
    let checkboxAction isChecked = checkbox id [text "Notify me", checked isChecked, onToggle (postWith id)]

    controlBehaviourSpec checkboxRect mkCheckboxCtx CheckboxBox CheckboxGlyph labelPoint (checkboxAction False)

    describe "toggle behaviour" $ do
      it "dispatches True when clicked while unchecked" $ do
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput) (checkboxAction False) [] [ClickAt labelPoint]
        resultMessages result `shouldBe` [True]

      it "dispatches False when clicked while checked" $ do
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput) (checkboxAction True) [] [ClickAt labelPoint]
        resultMessages result `shouldBe` [False]

      it "dispatches when clicked directly on the glyph, not just the label" $ do
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput) (checkboxAction False) [] [ClickAt glyphPoint]
        resultMessages result `shouldBe` [True]

      it "dispatches toggle when Enter is pressed while focused" $ do
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput) (checkboxAction False)
          [ClickAt labelPoint] [PressKey KeyReturn []]
        resultMessages result `shouldBe` [True]

      it "dispatches toggle when Space is pressed while focused" $ do
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput) (checkboxAction False)
          [ClickAt labelPoint] [PressKey KeySpace []]
        resultMessages result `shouldBe` [True]

      it "does not dispatch when clicked outside the checkbox" $ do
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput) (checkboxAction False) [] [ClickAt (Point 200 200)]
        resultMessages result `shouldBe` []

      it "does not dispatch when Enter is pressed while unfocused" $ do
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput)
          (withOther CheckboxGlyph (checkboxAction False)) [] [PressKey KeyReturn []]
        resultMessages result `shouldBe` []

    describe "disabled" $ do
      it "does not dispatch when clicked while disabled" $ do
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput)
          (disableWhen True (checkboxAction False)) [] [ClickAt labelPoint]
        resultMessages result `shouldBe` []

      it "does not dispatch when Enter is pressed while disabled" $ do
        focused <- runInteractions checkboxRect (mkCheckboxCtx noInput) (checkboxAction False) [] [ClickAt labelPoint]
        result <- runInteractions checkboxRect (resultContext focused)
          (disableWhen True (checkboxAction False)) [] [PressKey KeyReturn []]
        resultMessages result `shouldBe` []

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
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput) (checkboxAction False) [] [ClickAt labelPoint]
        contextFocus (resultContext result) `shouldBe` Just CheckboxBox

      it "gives focus to the checkbox itself when the glyph is clicked" $ do
        result <- runInteractions checkboxRect (mkCheckboxCtx noInput) (checkboxAction False) [] [ClickAt glyphPoint]
        contextFocus (resultContext result) `shouldBe` Just CheckboxBox

  describe "radioButton" $ do
    let radioAction isPicked = radioButton id [text "Ship to home", picked isPicked, onPick (post ())]

    controlBehaviourSpec checkboxRect mkRadioCtx RadioBox RadioGlyph labelPoint (radioAction False)

    describe "pick behaviour" $ do
      it "dispatches when clicked while unpicked" $ do
        result <- runInteractions checkboxRect (mkRadioCtx noInput) (radioAction False) [] [ClickAt labelPoint]
        resultMessages result `shouldBe` [()]

      it "still dispatches when clicked while already picked -- a radio button never un-picks itself" $ do
        result <- runInteractions checkboxRect (mkRadioCtx noInput) (radioAction True) [] [ClickAt labelPoint]
        resultMessages result `shouldBe` [()]

      it "dispatches when clicked directly on the glyph, not just the label" $ do
        result <- runInteractions checkboxRect (mkRadioCtx noInput) (radioAction False) [] [ClickAt glyphPoint]
        resultMessages result `shouldBe` [()]

      it "dispatches when Enter is pressed while focused" $ do
        result <- runInteractions checkboxRect (mkRadioCtx noInput) (radioAction False)
          [ClickAt labelPoint] [PressKey KeyReturn []]
        resultMessages result `shouldBe` [()]

      it "dispatches when Space is pressed while focused" $ do
        result <- runInteractions checkboxRect (mkRadioCtx noInput) (radioAction False)
          [ClickAt labelPoint] [PressKey KeySpace []]
        resultMessages result `shouldBe` [()]

      it "does not dispatch when clicked outside the radio button" $ do
        result <- runInteractions checkboxRect (mkRadioCtx noInput) (radioAction False) [] [ClickAt (Point 200 200)]
        resultMessages result `shouldBe` []

    describe "disabled" $ do
      it "does not dispatch when clicked while disabled" $ do
        result <- runInteractions checkboxRect (mkRadioCtx noInput)
          (disableWhen True (radioAction False)) [] [ClickAt labelPoint]
        resultMessages result `shouldBe` []

    describe "rendering" $ do
      it "draws a filled mark when picked" $ do
        ctx' <- runRadioButton True (mkRadioCtx noInput)
        drawnTexts ctx' `shouldContain` ["●"]

      it "draws an unfilled mark when unpicked" $ do
        ctx' <- runRadioButton False (mkRadioCtx noInput)
        drawnTexts ctx' `shouldContain` ["○"]

      it "draws the label text" $ do
        ctx' <- runRadioButton False (mkRadioCtx noInput)
        drawnTexts ctx' `shouldContain` ["Ship to home"]

    describe "focus" $ do
      it "gives focus to the radio button itself (not the glyph or label) when the label is clicked" $ do
        result <- runInteractions checkboxRect (mkRadioCtx noInput) (radioAction False) [] [ClickAt labelPoint]
        contextFocus (resultContext result) `shouldBe` Just RadioBox

      it "gives focus to the radio button itself when the glyph is clicked" $ do
        result <- runInteractions checkboxRect (mkRadioCtx noInput) (radioAction False) [] [ClickAt glyphPoint]
        contextFocus (resultContext result) `shouldBe` Just RadioBox

  describe "textInputControl" $ do
    let textAction v = textInputControl TestControl [text v, onInput (postWith id)]
        focusPt = Point 50 50

        -- Establishes real focus via a click (which also sets the cursor to
        -- the click position), then overwrites the selection to the exact
        -- precondition each test wants via the public emitUi/UiEffect API.
        focusedWithSelection :: Text -> Int -> Int -> IO (UIContext TestElement Text)
        focusedWithSelection v a v' = do
          focused <- runInteractions controlRect (mkTextCtx v noInput) (textAction v) [] [ClickAt focusPt]
          seedEffect controlRect (resultContext focused) (SetSelectionAt TestControl (Selection a v'))

        focusedWithSelectionWith :: TextMeasurer -> Text -> Int -> Int -> IO (UIContext TestElement Text)
        focusedWithSelectionWith measurer v a v' = do
          focused <- runInteractions controlRect (mkTextCtxWith measurer v noInput) (textAction v) [] [ClickAt focusPt]
          seedEffect controlRect (resultContext focused) (SetSelectionAt TestControl (Selection a v'))

    controlBehaviourSpec controlRect (mkTextCtx "hello") TestControl OtherControl focusPt (textAction "hello")
    describe "background and border" $ backgroundAndBorderSpec controlRect (textAction "hello")

    it "renders chrome like any other control" $ do
      ctx' <- runTextField "hello" (mkTextCtx "hello" noInput)
      getDrawCommands ctx' `shouldContain` [FillRect bgRect testColour]

    describe "rendering" $ do
      it "displays the value without a cursor when unfocused" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput) (withOther OtherControl (textAction "hello")) [] []
        drawnTexts (resultContext result) `shouldContain` ["hello"]

      it "displays the value with a cursor when focused" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput) (textAction "hello") [] [ClickAt focusPt]
        drawnTexts (resultContext result) `shouldContain` ["hello"]
        resultDraws result `shouldContain` [FillRect (Rectangle 15 15 1 70) testColour]

    describe "text editing" $ do
      it "appends typed characters to the value" $ do
        -- No click: a click would set the cursor to position 0 (per "sets
        -- the cursor to the clicked position" below); relying on plain
        -- auto-claim keeps no selection recorded, which appends at the end.
        result <- runInteractions controlRect (mkTextCtx "hello" noInput) (textAction "hello") [] [TypeText "!"]
        resultMessages result `shouldBe` ["hello!"]

      it "removes the last character on backspace" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput) (textAction "hello") [] [PressKey KeyBackspace []]
        resultMessages result `shouldBe` ["hell"]

      it "does not dispatch when backspace is pressed on an empty value" $ do
        result <- runInteractions controlRect (mkTextCtx "" noInput) (textAction "") [ClickAt focusPt] [PressKey KeyBackspace []]
        length (resultMessages result) `shouldBe` 0

      it "does not dispatch when there is no input" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput) (textAction "hello") [ClickAt focusPt] []
        length (resultMessages result) `shouldBe` 0

      it "does not process input when unfocused" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput)
          (withOther OtherControl (textAction "hello")) [] [PressKey KeyBackspace []]
        length (resultMessages result) `shouldBe` 0

    describe "disabled" $ do
      it "does not process input when disabled" $ do
        focused <- runInteractions controlRect (mkTextCtx "hello" noInput) (textAction "hello") [] [ClickAt focusPt]
        result <- runInteractions controlRect (resultContext focused) (disableWhen True (textAction "hello")) [] [TypeText "!"]
        length (resultMessages result) `shouldBe` 0

      it "does not show a cursor when focused and disabled" $ do
        focused <- runInteractions controlRect (mkTextCtx "hello" noInput) (textAction "hello") [] [ClickAt focusPt]
        result <- runInteractions controlRect (resultContext focused) (disableWhen True (textAction "hello")) [] []
        resultDraws result `shouldNotContain` [FillRect (Rectangle 15 15 1 70) testColour]

    describe "cursor placement" $ do
      it "sets the cursor to the clicked position on mouse press" $ do
        -- noOpTextMeasurer maps every offset to 0, so any click -> position 0
        result <- runInteractions controlRect (mkTextCtx "hello" noInput) (textAction "hello") [] [ClickAt focusPt]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 0 0]

      it "extends the active end on drag while keeping anchor" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput) (textAction "hello") []
          [MouseDown focusPt, DragTo (Point 70 50)]
        case contextSelections TestControl (resultContext result) of
          [Selection a _] -> a `shouldBe` 0
          other            -> expectationFailure $ "expected [Selection a _], got: " <> show other

    describe "arrow navigation" $ do
      it "moves cursor left with Left" $ do
        seeded <- focusedWithSelection "hello" 3 3
        result <- runInteractions controlRect seeded (textAction "hello") [] [PressKey KeyLeft []]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 2 2]

      it "moves cursor right with Right" $ do
        seeded <- focusedWithSelection "hello" 2 2
        result <- runInteractions controlRect seeded (textAction "hello") [] [PressKey KeyRight []]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 3 3]

      it "collapses selection to low end on plain Left" $ do
        seeded <- focusedWithSelection "hello" 1 3
        result <- runInteractions controlRect seeded (textAction "hello") [] [PressKey KeyLeft []]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 1 1]

      it "collapses selection to high end on plain Right" $ do
        seeded <- focusedWithSelection "hello" 1 3
        result <- runInteractions controlRect seeded (textAction "hello") [] [PressKey KeyRight []]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 3 3]

      it "extends selection left with Shift+Left" $ do
        seeded <- focusedWithSelection "hello" 3 3
        result <- runInteractions controlRect seeded (textAction "hello") [] [PressKey KeyLeft [Shift]]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 3 2]

      it "extends selection right with Shift+Right" $ do
        seeded <- focusedWithSelection "hello" 3 3
        result <- runInteractions controlRect seeded (textAction "hello") [] [PressKey KeyRight [Shift]]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 3 4]

      it "does not move cursor past the beginning" $ do
        seeded <- focusedWithSelection "hello" 0 0
        result <- runInteractions controlRect seeded (textAction "hello") [] [PressKey KeyLeft []]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 0 0]

      it "does not move cursor past the end" $ do
        seeded <- focusedWithSelection "hello" 5 5
        result <- runInteractions controlRect seeded (textAction "hello") [] [PressKey KeyRight []]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 5 5]

    describe "selection editing" $ do
      it "deletes the selected range on backspace" $ do
        seeded <- focusedWithSelection "hello" 1 3
        result <- runInteractions controlRect seeded (textAction "hello") [] [PressKey KeyBackspace []]
        resultMessages result `shouldBe` ["hlo"]

      it "replaces the selected range with typed text" $ do
        seeded <- focusedWithSelection "hello" 1 3
        result <- runInteractions controlRect seeded (textAction "hello") [] [TypeText "X"]
        resultMessages result `shouldBe` ["hXlo"]

      it "collapses cursor to insertion point after replacing selection" $ do
        seeded <- focusedWithSelection "hello" 1 3
        result <- runInteractions controlRect seeded (textAction "hello") [] [TypeText "XY"]
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 3 3]

    describe "focus persistence" $ do
      it "leaves the selection unchanged on a frame where the control is not focused" $ do
        seeded <- seedEffect controlRect (mkTextCtx "hello" noInput) (SetSelectionAt TestControl (Selection 1 3))
        result <- runInteractions controlRect seeded (withOther OtherControl (textAction "hello")) [] []
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 1 3]

      it "restores the previous selection when focus returns without a click" $ do
        -- No click: focus starts at Nothing, so TestControl auto-focuses
        -- this frame via the same path as gaining focus by Tab, not by click.
        seeded <- seedEffect controlRect (mkTextCtx "hello" noInput) (SetSelectionAt TestControl (Selection 2 4))
        result <- runInteractions controlRect seeded (textAction "hello") [] []
        contextSelections TestControl (resultContext result) `shouldBe` [Selection 2 4]

    describe "scrolling" $ do
      it "scrolls right to keep the cursor visible when it moves past the right edge" $ do
        -- Content width is 100px (5 chars * fixedCharWidth's 20px); viewport
        -- is 70px, so the cursor at index 5 (position 100) needs a 31px
        -- scroll to stay just inside the right edge.
        seeded <- focusedWithSelectionWith fixedCharWidth "hello" 5 5
        result <- runInteractions controlRect seeded (textAction "hello") [] []
        resultDraws result `shouldContain` [FillRect (Rectangle 84 15 1 70) testColour]

      it "stores the scroll position as a bounded [0, 1] fraction, not an unbounded pixel value" $ do
        -- The 31px target above exceeds the content's actual 30px max
        -- scroll (100px content - 70px viewport), so the stored fraction
        -- clamps to 1.0 rather than persisting an out-of-range pixel count
        -- the way the old single pixel-offset convention did.
        seeded <- focusedWithSelectionWith fixedCharWidth "hello" 5 5
        result <- runInteractions controlRect seeded (textAction "hello") [] []
        contextScrollPosition TestControl (resultContext result) `shouldBe` 1.0

      it "scrolls left to keep the cursor visible when it moves before the left edge" $ do
        withSel <- focusedWithSelectionWith fixedCharWidth "hello" 0 0
        base <- seedEffect controlRect withSel (ScrollTo TestControl 1.0)
        result <- runInteractions controlRect base (textAction "hello") [] []
        resultDraws result `shouldContain` [FillRect (Rectangle 15 15 1 70) testColour]

    describe "digits-only input (inputFilter)" $ do
      let numberAction v = textInputControl TestControl [text v, inputFilter (T.filter isDigit), onInput (postWith id)]

      describe "input filter" $ do
        it "inserts digits typed alongside non-digits, dropping the non-digits" $ do
          result <- runInteractions controlRect (mkTextCtx "12" noInput) (numberAction "12") [] [TypeText "a3b"]
          resultMessages result `shouldBe` ["123"]

        it "does not dispatch when the only typed characters are non-digits" $ do
          result <- runInteractions controlRect (mkTextCtx "12" noInput) (numberAction "12") [] [TypeText "!"]
          length (resultMessages result) `shouldBe` 0

        it "still allows backspace to remove digits" $ do
          result <- runInteractions controlRect (mkTextCtx "12" noInput) (numberAction "12") [] [PressKey KeyBackspace []]
          resultMessages result `shouldBe` ["1"]

      describe "rendering" $ do
        it "displays the value unmasked" $ do
          result <- runInteractions controlRect (mkTextCtx "42" noInput) (numberAction "42") [ClickAt focusPt] []
          drawnTexts (resultContext result) `shouldContain` ["42"]

    describe "password masking (displayFilter)" $ do
      let passwordAction v = textInputControl TestControl [text v, displayFilter (T.map (const '•')), onInput (postWith id)]

      describe "rendering" $ do
        it "displays a mask character per character of the value instead of the value itself" $ do
          result <- runInteractions controlRect (mkTextCtx "hunter2" noInput) (passwordAction "hunter2") [ClickAt focusPt] []
          drawnTexts (resultContext result) `shouldContain` ["•••••••"]
          drawnTexts (resultContext result) `shouldNotContain` ["hunter2"]

      describe "editing" $ do
        it "appends typed characters to the real (unmasked) value" $ do
          result <- runInteractions controlRect (mkTextCtx "hunter2" noInput) (passwordAction "hunter2") [] [TypeText "!"]
          resultMessages result `shouldBe` ["hunter2!"]

      describe "cursor placement" $ do
        it "places the cursor using offsets measured against the masked text, not the real value" $ do
          result <- runInteractions controlRect (mkTextCtxWith fixedCharWidth "hunter2" noInput) (passwordAction "hunter2")
            [] [ClickAt (Point 35 50)]
          case contextSelections TestControl (resultContext result) of
            [Selection a v] -> (a, v) `shouldBe` (1, 1)
            other            -> expectationFailure $ "expected [Selection 1 1], got: " <> show other

    describe "custom filters" $ do
      it "lets a custom input filter reject keystrokes entirely" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput)
          (textInputControl TestControl [text "hello", inputFilter (const T.empty), onInput (postWith id)])
          [ClickAt focusPt] [TypeText "x"]
        length (resultMessages result) `shouldBe` 0

      it "lets a custom display filter change what is rendered without changing the value" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput)
          (textInputControl TestControl [text "hello", displayFilter T.toUpper, onInput (postWith id)])
          [ClickAt focusPt] []
        drawnTexts (resultContext result) `shouldContain` ["HELLO"]

    describe "onSubmit" $ do
      it "fires Submitted when Enter is pressed while focused" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput)
          (textInputControl TestControl [text "hello", onSubmit (post "submitted")])
          [ClickAt focusPt] [PressKey KeyReturn []]
        resultMessages result `shouldBe` ["submitted"]

      it "does not fire Submitted when a different element is focused" $ do
        result <- runInteractions controlRect (mkTextCtx "hello" noInput)
          (withOther OtherControl (textInputControl TestControl [text "hello", onSubmit (post "submitted")]))
          [] [PressKey KeyReturn []]
        resultMessages result `shouldBe` []

  describe "slider" $ do
    let mkSliderCtx input = emptyUIContext sliderRect input sliderTheme noOpTextMeasurer
        sliderWidget = slider id [orientation Horizontal, value 0.5, onChange (post ())]

    controlBehaviourSpec sliderRect mkSliderCtx SliderTrack SliderThumb (Point 100 15) sliderWidget

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

      it "clamps a ratio above 1 to a full-width thumb" $ do
        ctx' <- snd <$> runUI (slider id [orientation Horizontal, value 0, onChange (post ()), thumbRatio 2])
          (emptyUIContext sliderRect noInput sliderTheme noOpTextMeasurer)
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 0 200 30) testColour]

      it "clamps a negative ratio to a zero-width thumb" $ do
        ctx' <- snd <$> runUI (slider id [orientation Horizontal, value 0, onChange (post ()), thumbRatio (-1)])
          (emptyUIContext sliderRect noInput sliderTheme noOpTextMeasurer)
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 0 0 30) testColour]

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
        -- resultMessages accumulates across both frames (the initial press
        -- at x=100 and the drag to x=300), not just the last.
        result <- runInteractions sliderRect (mkSliderCtx noInput)
          (slider id [orientation Horizontal, value 0, onChange (postWith id)]) []
          [MouseDown (Point 100 15), DragTo (Point 300 15)]
        resultMessages result `shouldBe` [0.5, 1.0]

      it "stops tracking when the button is released" $ do
        result <- runInteractions sliderRect (mkSliderCtx noInput)
          (slider id [orientation Horizontal, value 0, onChange (postWith id)]) []
          [MouseDown (Point 100 15), DragTo (Point 300 15), MouseUp (Point 300 15)]
        resultMessages result `shouldBe` [0.5, 1.0]

      -- Releasing the mouse while it is still over the track (as opposed to
      -- having dragged off it) must not dispatch a further change.
      it "does not dispatch on the release frame when the mouse is still over the track" $ do
        result <- runInteractions sliderRect (mkSliderCtx noInput)
          (slider id [orientation Horizontal, value 0, onChange (postWith id)]) []
          [MouseDown (Point 100 15), MouseUp (Point 100 15)]
        resultMessages result `shouldBe` [0.5]

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

      it "nudges by a custom step size when given" $ do
        ctx' <- fmap (settle . snd) $ runUI (slider id [orientation Horizontal, value 0.5, onChange (postWith id), arrowStep 0.2])
          (emptyUIContext sliderRect noInput { inputKeyEvents = [KeyEvent KeyRight []] } sliderTheme noOpTextMeasurer)
        getMessages ctx' `shouldBe` [0.7]

      it "does not nudge when another element has focus" $ do
        result <- runInteractions sliderRect (mkSliderCtx noInput)
          (withOther SliderThumb (slider id [orientation Horizontal, value 0.5, onChange (postWith id)]))
          [] [PressKey KeyRight []]
        resultMessages result `shouldBe` []

      it "does not nudge when disabled" $ do
        focused <- runInteractions sliderRect (mkSliderCtx noInput)
          (slider id [orientation Horizontal, value 0.5, onChange (postWith id)]) [] [ClickAt (Point 100 15)]
        result <- runInteractions sliderRect (resultContext focused)
          (disableWhen True (slider id [orientation Horizontal, value 0.5, onChange (postWith id)])) [] [PressKey KeyRight []]
        length (resultMessages result) `shouldBe` 0

    describe "without interaction" $ do
      it "does not dispatch when there is no input" $ do
        ctx' <- runSlider Horizontal 0.5 noInput
        dispatchCount ctx' `shouldBe` 0

  describe "scrollBar" $ do
    let scrollBarAction = scrollBar id [orientation Vertical, thumbRatio 0.25]
        freshScrollBarCtx :: UIContext ScrollBarPart msg
        freshScrollBarCtx = emptyUIContext scrollBarRect noInput scrollBarTheme noOpTextMeasurer
        -- Sets an exact scroll position by queuing and applying a real 'ScrollTo' effect.
        seedScrollBarCtx :: Double -> IO (UIContext ScrollBarPart msg)
        seedScrollBarCtx pos = seedEffect scrollBarRect
          (emptyUIContext scrollBarRect noInput scrollBarTheme noOpTextMeasurer) (ScrollTo ScrollTrack pos)

    it "renders chrome for the track like any other control" $ do
      -- Track occupies the middle 160px between the two 20px buttons.
      seeded <- seedScrollBarCtx 0
      result <- runInteractions scrollBarRect seeded scrollBarAction [] []
      resultDraws result `shouldContain` [FillRect (Rectangle 0 20 20 160) testColour]

    describe "defaults" $ do
      it "defaults to Horizontal orientation, drawing horizontal arrow glyphs" $ do
        ctx' <- snd <$> runUI (scrollBar id []) freshScrollBarCtx
        drawnTexts ctx' `shouldContain` ["◀"]
        drawnTexts ctx' `shouldContain` ["▶"]

      it "defaults to a full-track thumb ratio (nothing to step by)" $ do
        seeded <- seedScrollBarCtx 0.5
        result <- runInteractions scrollBarRect seeded (scrollBar id [orientation Vertical]) [] [ClickAt (Point 10 190)]
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 1.0

    describe "button stepping" $ do
      it "steps forward by the thumb ratio when the increment button is clicked" $ do
        seeded <- seedScrollBarCtx 0.5
        result <- runInteractions scrollBarRect seeded scrollBarAction [] [ClickAt (Point 10 190)]
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 0.75

      it "steps back by the thumb ratio when the decrement button is clicked" $ do
        seeded <- seedScrollBarCtx 0.5
        result <- runInteractions scrollBarRect seeded scrollBarAction [] [ClickAt (Point 10 10)]
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 0.25

      it "clamps to 1 when stepping forward near the end" $ do
        seeded <- seedScrollBarCtx 0.9
        result <- runInteractions scrollBarRect seeded scrollBarAction [] [ClickAt (Point 10 190)]
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 1

      it "clamps to 0 when stepping back near the start" $ do
        seeded <- seedScrollBarCtx 0.1
        result <- runInteractions scrollBarRect seeded scrollBarAction [] [ClickAt (Point 10 10)]
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 0

    describe "track dragging" $ do
      it "centres the thumb on the cursor while the track is pressed" $ do
        seeded <- seedScrollBarCtx 0
        result <- runInteractions scrollBarRect seeded scrollBarAction [] [MouseDown (Point 10 100)]
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 0.5

      it "continues tracking when the mouse moves off the track while the button is held" $ do
        seeded <- seedScrollBarCtx 0
        result <- runInteractions scrollBarRect seeded scrollBarAction [] [MouseDown (Point 10 100), DragTo (Point 200 40)]
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 0.0

      it "stops tracking when the button is released after dragging off the track" $ do
        seeded <- seedScrollBarCtx 0
        result <- runInteractions scrollBarRect seeded scrollBarAction [] [MouseDown (Point 10 100), MouseUp (Point 200 40)]
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 0.5

    describe "keyboard nudging (inherited from the underlying slider)" $ do
      it "nudges the position when an arrow key is pressed while the track is focused" $ do
        -- Establish real focus via a click, then re-seed the scroll
        -- position afterward — composed with the real widget action (not a
        -- bare emitUi) so focus is re-affirmed the same frame the seed is
        -- queued, since focus is only carried forward to the next frame if
        -- some control visited it this frame.
        focused <- runInteractions scrollBarRect freshScrollBarCtx scrollBarAction [] [ClickAt (Point 10 100)]
        seeded <- runInteractions scrollBarRect (resultContext focused)
          (scrollBarAction >> emitUi (ScrollTo ScrollTrack 0.5)) [] []
        result <- runInteractions scrollBarRect (resultContext seeded) scrollBarAction [] [PressKey KeyDown []]
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 0.55

    describe "without interaction" $ do
      it "leaves the position unchanged" $ do
        seeded <- seedScrollBarCtx 0.5
        result <- runInteractions scrollBarRect seeded scrollBarAction [] []
        contextScrollPosition ScrollTrack (resultContext result) `shouldBe` 0.5

    describe "lifecycle events" $ do
      it "bridges the track's FocusGained into onFocusGained" $ do
        -- Starts with a different sub-part focused (via a real click on the
        -- increment button) so the auto-claim-when-nothing-focused rule
        -- doesn't race the track for focus first; the second click is what
        -- moves it explicitly.
        let attrs = [orientation Vertical, thumbRatio 0.25, onFocusGained (post ("gained" :: String))]
        focused <- runInteractions scrollBarRect freshScrollBarCtx (scrollBar id attrs) [] [ClickAt (Point 10 190)]
        result <- runInteractions scrollBarRect (resultContext focused) (scrollBar id attrs) [] [ClickAt (Point 10 100)]
        resultMessages result `shouldBe` ["gained"]

      it "bridges the track's MouseEntered into onMouseEnter" $ do
        let attrs = [orientation Vertical, thumbRatio 0.25, onMouseEnter (post ("entered" :: String))]
        result <- runInteractions scrollBarRect freshScrollBarCtx (scrollBar id attrs) [] [MoveTo (Point 10 100)]
        resultMessages result `shouldBe` ["entered"]

  describe "viewport" $ do
    describe "no scrollbars needed" $ do
      it "renders content at its own size, unclipped and untranslated, when it fits the viewport" $ do
        ctx' <- runViewportDraw (Size 100 50) vpCtx
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle 0 0 100 50) testColour]

      it "does not draw either scrollbar" $ do
        ctx' <- runViewportDraw (Size 100 50) vpCtx
        getDrawCommands ctx' `shouldNotContain` [FillRect (Rectangle 0 84 16 16) testColour]
        getDrawCommands ctx' `shouldNotContain` [FillRect (Rectangle 184 0 16 16) testColour]

      it "defaults to nothing to scroll when the content size isn't given" $ do
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
      it "translates content left by the stored horizontal scroll fraction" $ do
        -- H-only (Size 300 50): vpW 200, max scroll = 300 - 200 = 100px.
        seeded <- seedEffect vpOuterRect vpCtx (ScrollTo (VPPart (ViewportH ScrollTrack)) 1.0)
        ctx' <- runViewportDraw (Size 300 50) seeded
        getDrawCommands ctx' `shouldContain` [FillRect (Rectangle (-100) 0 300 50) testColour]

      it "translates content up by the stored vertical scroll fraction" $ do
        -- V-only (Size 50 200): vpH 100, max scroll = 200 - 100 = 100px.
        seeded <- seedEffect vpOuterRect vpCtx (ScrollTo (VPPart (ViewportV ScrollTrack)) 1.0)
        ctx' <- runViewportDraw (Size 50 200) seeded
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

    it "clamps a zero item height instead of hanging" $ do
      ctx' <- runVirtualContent 0 0 10
      -- rowHeight clamped to 1 -> 100px viewport fits 100 rows, capped at itemCount.
      [c | c@(FillRect _ _) <- getDrawCommands ctx'] `shouldSatisfy` ((<= 10) . length)

    it "clamps a negative item height instead of hanging" $ do
      ctx' <- runVirtualContent 0 (-20) 10
      [c | c@(FillRect _ _) <- getDrawCommands ctx'] `shouldSatisfy` ((<= 10) . length)

  let runItemsLayout attrs = fmap (settle . snd) . runUI (itemsLayout attrs)
      runSelectionControl attrs = fmap (settle . snd) . runUI (selectionControl id attrs)

  describe "itemsLayout" $ do
    let run      = runItemsLayout [items itemLabels, itemTemplate itemsRenderItem]
        runEmpty = runItemsLayout [itemTemplate itemsRenderItem]

    describe "items" $ do
      itemOrderingSpec run (mkListCtx noInput) itemLabels
      emptyItemsSpec runEmpty (mkListCtx noInput)

    noChromeSpec run (mkListCtx noInput)

    describe "orientation / itemsPanel" $ do
      it "arranges items in a vertical stack by default" $ do
        ctx' <- runItemsLayout [items itemLabels, itemTemplate boundsRenderItem] (mkListCtx noInput)
        let coords = map (read . T.unpack) (drawnTexts ctx') :: [(Double, Double)]
        map fst coords `shouldSatisfy` \xs -> all (== head xs) xs
        map snd coords `shouldSatisfy` \ys -> and (zipWith (<) ys (tail ys))

      it "arranges items in a horizontal row with orientation Horizontal" $ do
        ctx' <- runItemsLayout
          [items itemLabels, orientation Horizontal, itemTemplate boundsRenderItem]
          (mkListCtx noInput)
        let coords = map (read . T.unpack) (drawnTexts ctx') :: [(Double, Double)]
        map snd coords `shouldSatisfy` \ys -> all (== head ys) ys
        map fst coords `shouldSatisfy` \xs -> and (zipWith (<) xs (tail xs))

      it "still stacks vertically when itemsPanel changes spacing" $ do
        ctx' <- runItemsLayout
          [items itemLabels, itemsPanel (defaultBoxConfig { boxSpacing = 2 }), itemTemplate boundsRenderItem]
          (mkListCtx noInput)
        let coords = map (read . T.unpack) (drawnTexts ctx') :: [(Double, Double)]
        map fst coords `shouldSatisfy` \xs -> all (== head xs) xs
        map snd coords `shouldSatisfy` \ys -> and (zipWith (<) ys (tail ys))

  describe "compositeControl" $ do
    -- Built directly on 'withFocusScope' -- see that suite for the
    -- underlying focus mechanics. These tests only check that
    -- 'compositeControl' actually wires it (plus mouse-over and chrome) in,
    -- not that it works, which 'withFocusScope' already covers.
    let compAttrs :: [Attr CompElem Probe msg (CompositeControlConfig CompElem msg Int)]
        compAttrs = [items ([0, 1] :: [Int]), itemTemplate (\idx _ -> (Layout Fill Fill TopLeft, leaf (RowElem idx)))]
        render    = compositeControl ListElem compAttrs

    describe "background and border" $
      backgroundAndBorderSpec controlRect (compositeControl TestControl ([] :: [Attr TestElement Probe () (CompositeControlConfig TestElement () Int)]))

    it "a composite claims focus for itself, atomically -- not into a specific child" $ do
      result <- runInteractions controlRect (mkCompCtx noInput) render [] []
      contextFocus (resultContext result) `shouldBe` Just ListElem

    it "with tabStop off, the composite never claims focus itself, and a child does instead" $ do
      let render' = compositeControl ListElem (tabStop False : compAttrs)
      result <- runInteractions controlRect (mkCompCtx noInput) render' [] []
      contextFocus (resultContext result) `shouldBe` Just (RowElem 0)

    it "Tab from a focused composite moves past it entirely, not into its own children" $ do
      let withAfter = render >> leaf AfterElem
      result <- runInteractions controlRect (mkCompCtx noInput) withAfter [Wait 1, Tab] []
      contextFocus (resultContext result) `shouldBe` Just AfterElem

    it "a composite does not steal focus already held by an unrelated element" $ do
      let withSibling = leaf SiblingElem >> render
      result <- runInteractions controlRect (mkCompCtx noInput) withSibling [] [Wait 1, Wait 1]
      contextFocus (resultContext result) `shouldBe` Just SiblingElem

    it "disabling a composite disables the items inside it too" $ do
      result <- runInteractions controlRect (mkCompCtx noInput) (disableWhen True render) [] [ClickAt (Point 50 45)]
      RowElem 0 `elem` contextFocusChain (resultContext result) `shouldBe` False

    describe "an idle composite (no eligible children)" $ do
      let emptyAttrs :: [Attr CompElem Probe msg (CompositeControlConfig CompElem msg Int)]
          emptyAttrs = [itemTemplate (\idx _ -> (Layout Fill Fill TopLeft, leaf (RowElem idx)))]
          renderEmpty = compositeControl ListElem emptyAttrs

      it "an empty composite claims focus" $ do
        result <- runInteractions controlRect (mkCompCtx noInput) renderEmpty [] []
        contextFocus (resultContext result) `shouldBe` Just ListElem

      it "Tab from an idle composite gives up focus entirely" $ do
        result <- runInteractions controlRect (mkCompCtx noInput) renderEmpty [] [Wait 1, Tab]
        contextFocus (resultContext result) `shouldBe` Nothing

      it "Shift-Tab from an idle composite returns to whatever was focused before it" $ do
        let withSibling = leaf SiblingElem >> renderEmpty
        result <- runInteractions controlRect (mkCompCtx noInput) withSibling [] [Wait 1, Tab, Wait 1, ShiftTab]
        contextFocus (resultContext result) `shouldBe` Just SiblingElem

  describe "selectionControl" $ do
    let run      = runSelectionControl [items itemLabels, itemContainer selectionRenderItem, onSelect dispatchActivated]
        runEmpty = runSelectionControl [itemContainer selectionRenderItem, onSelect dispatchActivated]

    describe "items" $ do
      itemOrderingSpec run (mkListCtx noInput) (map ("UNSEL:" <>) itemLabels)
      emptyItemsSpec runEmpty (mkListCtx noInput)

    noChromeSpec run (mkListCtx noInput)

    describe "selection" $ do
      it "defaults to nothing selected" $ do
        ctx' <- run (mkListCtx noInput)
        drawnTexts ctx' `shouldBe` map ("UNSEL:" <>) itemLabels

      it "marks the item matching 'selected' as Selected" $ do
        ctx' <- runSelectionControl
          [items itemLabels, selected "Beta", itemContainer selectionRenderItem, onSelect dispatchActivated]
          (mkListCtx noInput)
        drawnTexts ctx' `shouldBe` ["UNSEL:Alpha", "SEL:Beta", "UNSEL:Gamma"]

      it "marks the item at 'selectedIndex' as Selected regardless of value" $ do
        ctx' <- runSelectionControl
          [items itemLabels, selectedIndex 2, itemContainer selectionRenderItem, onSelect dispatchActivated]
          (mkListCtx noInput)
        drawnTexts ctx' `shouldBe` ["UNSEL:Alpha", "UNSEL:Beta", "SEL:Gamma"]

    describe "click detection" $ do
      it "fires Activated with the clicked item's index and value" $ do
        result <- runInteractions listRect (mkListCtx noInput)
          (selectionControl id [items itemLabels, itemContainer selectionRenderItem, onSelect dispatchActivated])
          [] [ClickAt (Point 50 45)]
        resultMessages result `shouldBe` [(1, "Beta")]

      it "does not dispatch when there is no interaction" $ do
        ctx' <- run (mkListCtx noInput)
        dispatchCount ctx' `shouldBe` 0

      it "does not dispatch when clicked while disabled" $ do
        result <- runInteractions listRect (mkListCtx noInput)
          (disableWhen True (selectionControl id [items itemLabels, itemContainer selectionRenderItem, onSelect dispatchActivated]))
          [] [ClickAt (Point 50 45)]
        resultMessages result `shouldBe` []

  describe "radioGroup" $ do
    let baseAttrs = [items itemLabels, itemLabel id, onSelect dispatchRGActivated]
        render    = radioGroup id baseAttrs
        run       = runRadioGroup baseAttrs
        marks     = filter (`elem` ["●", "○"]) . drawnTexts

    describe "items" $
      it "renders each item's label, in order" $ do
        ctx' <- run (mkRadioGroupCtx noInput)
        filter (`notElem` ["●", "○"]) (drawnTexts ctx') `shouldBe` itemLabels

    describe "selection" $ do
      it "defaults to nothing selected" $ do
        ctx' <- run (mkRadioGroupCtx noInput)
        marks ctx' `shouldBe` ["○", "○", "○"]

      it "marks the item matching 'selected' as picked" $ do
        ctx' <- runRadioGroup (selected "Beta" : baseAttrs) (mkRadioGroupCtx noInput)
        marks ctx' `shouldBe` ["○", "●", "○"]

      it "marks the item at 'selectedIndex' as picked regardless of value" $ do
        ctx' <- runRadioGroup (selectedIndex 2 : baseAttrs) (mkRadioGroupCtx noInput)
        marks ctx' `shouldBe` ["○", "○", "●"]

    describe "click detection" $ do
      it "fires Activated with the clicked item's index and value" $ do
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render [] [ClickAt (Point 50 45)]
        resultMessages result `shouldBe` [(1, "Beta")]

      it "does not dispatch when clicked while disabled" $ do
        result <- runInteractions listRect (mkRadioGroupCtx noInput) (disableWhen True render) [] [ClickAt (Point 50 45)]
        resultMessages result `shouldBe` []

    describe "focus" $ do
      it "claims focus for the group as a whole, atomically -- not into a specific item" $ do
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render [] []
        contextFocus (resultContext result) `shouldBe` Just RadioGroup

      it "Tab from a focused group moves past it entirely, not into its items" $ do
        let withAfter = render >> withBounds otherRect (minimalControl (RadioItem 99 RadioBox))
        result <- runInteractions listRect (mkRadioGroupCtx noInput) withAfter [Wait 1, Tab] []
        contextFocus (resultContext result) `shouldBe` Just (RadioItem 99 RadioBox)

      it "with tabStop off, items become individually Tab-reachable instead" $ do
        let render' = radioGroup id (tabStop False : baseAttrs)
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render' [] []
        contextFocus (resultContext result) `shouldBe` Just (RadioItem 0 RadioBox)

      it "with tabStop off, a disabled group claims no focus at all" $ do
        let render' = disableWhen True (radioGroup id (tabStop False : baseAttrs))
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render' [] []
        contextFocus (resultContext result) `shouldBe` Nothing

      it "Shift-Tab after clicking an item returns to the element before the group, not the group itself" $ do
        let withSibling = withOther (RadioItem 99 RadioBox) render
        clicked <- runInteractions listRect (mkRadioGroupCtx noInput) withSibling [] [ClickAt (Point 50 45)]
        result <- runInteractions listRect (resultContext clicked) withSibling [] [ShiftTab]
        contextFocus (resultContext result) `shouldBe` Just (RadioItem 99 RadioBox)

      it "clicking an item leaves only the group focused, not the item's own box too" $ do
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render [] [ClickAt (Point 50 45)]
        let resultChain = contextFocusChain (resultContext result)
        RadioGroup `elem` resultChain `shouldBe` True
        RadioItem 1 RadioBox `elem` resultChain `shouldBe` False

    describe "keyboard navigation" $ do
      it "Down selects the first item when nothing is selected yet" $ do
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render [] [PressKey KeyDown []]
        resultMessages result `shouldBe` [(0, "Alpha")]

      it "Up also selects the first item when nothing is selected yet" $ do
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render [] [PressKey KeyUp []]
        resultMessages result `shouldBe` [(0, "Alpha")]

      it "Down moves the selection to the next item" $ do
        let render' = radioGroup id (selectedIndex 0 : baseAttrs)
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render' [] [PressKey KeyDown []]
        resultMessages result `shouldBe` [(1, "Beta")]

      it "Up moves the selection to the previous item" $ do
        let render' = radioGroup id (selectedIndex 2 : baseAttrs)
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render' [] [PressKey KeyUp []]
        resultMessages result `shouldBe` [(1, "Beta")]

      it "Down clamps at the last item instead of wrapping" $ do
        let render' = radioGroup id (selectedIndex 2 : baseAttrs)
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render' [] [PressKey KeyDown []]
        resultMessages result `shouldBe` [(2, "Gamma")]

      it "Up clamps at the first item instead of wrapping" $ do
        let render' = radioGroup id (selectedIndex 0 : baseAttrs)
        result <- runInteractions listRect (mkRadioGroupCtx noInput) render' [] [PressKey KeyUp []]
        resultMessages result `shouldBe` [(0, "Alpha")]

      it "does not respond to arrow keys while unfocused" $ do
        result <- runInteractions listRect (mkRadioGroupCtx noInput) (withOther (RadioItem 99 RadioBox) render) [] [PressKey KeyDown []]
        resultMessages result `shouldBe` []

      it "does not respond to arrow keys while disabled" $ do
        result <- runInteractions listRect (mkRadioGroupCtx noInput) (disableWhen True render) [] [PressKey KeyDown []]
        resultMessages result `shouldBe` []

  describe "tab order across a real composite and a disabled sibling (integration)" $ do
    -- Mirrors the app's actual structure that regressed: two plain buttons,
    -- then a real checkbox (three genuine sub-controls: box, glyph, label,
    -- each running their own applyFocus), then a permanently-disabled
    -- button that must never be reachable. One discrete Tab press per
    -- frame, exactly like a real key press-and-release, not a held-key
    -- repeat.
    let tiTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = checkboxStyleSet } :: Theme TIElem

        render :: UI TIElem () ()
        render = do
          button TIButton1 [text "One"]
          button TIButton2 [text "Two"]
          checkbox TICheckbox [text "Check", checked False]
          disableWhen True (button TIDisabledButton [text "Disabled"])

        initialCtx = emptyUIContext controlRect noInput tiTheme noOpTextMeasurer :: UIContext TIElem ()

    it "auto-claims the first focusable element with nothing focused and no Tab pressed" $ do
      result <- runInteractions controlRect initialCtx render [] []
      contextFocus (resultContext result) `shouldBe` Just TIButton1

    it "advances button -> button -> checkbox box -> (disabled skipped) -> wraps to the first button" $ do
      -- One discrete Tab press per 'runInteractions' call, exactly like a
      -- real key press-and-release, not a held-key repeat.
      r0 <- runInteractions controlRect initialCtx render [] []
      r1 <- runInteractions controlRect (resultContext r0) render [] [Tab]
      contextFocus (resultContext r1) `shouldBe` Just TIButton2
      r2 <- runInteractions controlRect (resultContext r1) render [] [Tab]
      contextFocus (resultContext r2) `shouldBe` Just (TICheckbox CheckboxBox)
      r3 <- runInteractions controlRect (resultContext r2) render [] [Tab]
      contextFocus (resultContext r3) `shouldBe` Nothing
      r4 <- runInteractions controlRect (resultContext r3) render [] [Tab]
      contextFocus (resultContext r4) `shouldBe` Just TIButton1

  describe "tab order across a real composite and a disabled composite sibling (integration)" $ do
    -- Mirrors the app's actual structure that regressed: two plain buttons,
    -- a real checkbox, then a disabled radioGroup (a composite control, not
    -- a plain control) as the last element with nothing enabled after it.
    let tiTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = checkboxStyleSet } :: Theme TIElem

        render :: UI TIElem (Int, Text) ()
        render = do
          button TIButton1 [text "One"]
          button TIButton2 [text "Two"]
          checkbox TICheckbox [text "Check", checked False]
          disableWhen True $
            radioGroup TIDisabledGroup
              [items (["Small", "Medium", "Large"] :: [Text]), itemLabel id]

        initialCtx = emptyUIContext controlRect noInput tiTheme noOpTextMeasurer :: UIContext TIElem (Int, Text)

    it "Tab moves past a disabled composite without losing or misplacing focus" $ do
      r0 <- runInteractions controlRect initialCtx render [] []
      r1 <- runInteractions controlRect (resultContext r0) render [] [Tab]
      r2 <- runInteractions controlRect (resultContext r1) render [] [Tab]
      contextFocus (resultContext r2) `shouldBe` Just (TICheckbox CheckboxBox)
      r3 <- runInteractions controlRect (resultContext r2) render [] [Tab]
      -- The disabled composite must never end up holding focus, even in
      -- the vacuous "composite focused, no child chosen" shape.
      contextFocus (resultContext r3) `shouldNotBe` Just (TIDisabledGroup RadioGroup)
      contextFocus (resultContext r3) `shouldBe` Nothing
      -- And a later idle frame must be able to reclaim it -- proving focus
      -- wasn't silently trapped on the disabled composite.
      r4 <- runInteractions controlRect (resultContext r3) render [] [Wait 1]
      contextFocus (resultContext r4) `shouldBe` Just TIButton1

  describe "tab traversal across a full UI (multiple controls)" $ do
    describe "single controls only" $
      tabTraversalSpec controlRect multiTheme
        [MultiButton 0, MultiButton 1, MultiButton 2]
        (multiButton 0 >> multiButton 1 >> multiButton 2)

    describe "composite only" $
      tabTraversalSpec controlRect multiTheme
        [MultiGroup 0 RadioGroup]
        (multiGroup 0)

    describe "mixed, starting with composite" $
      tabTraversalSpec controlRect multiTheme
        [MultiGroup 0 RadioGroup, MultiButton 0, MultiButton 1]
        (multiGroup 0 >> multiButton 0 >> multiButton 1)

    describe "mixed, ending with composite" $
      tabTraversalSpec controlRect multiTheme
        [MultiButton 0, MultiButton 1, MultiGroup 0 RadioGroup]
        (multiButton 0 >> multiButton 1 >> multiGroup 0)

    describe "mixed, composite in the middle" $
      tabTraversalSpec controlRect multiTheme
        [MultiButton 0, MultiGroup 0 RadioGroup, MultiButton 1]
        (multiButton 0 >> multiGroup 0 >> multiButton 1)

    describe "multiple composites, back to back" $
      tabTraversalSpec controlRect multiTheme
        [MultiGroup 0 RadioGroup, MultiGroup 1 RadioGroup]
        (multiGroup 0 >> multiGroup 1)

    describe "a disabled slot inside an otherwise-mixed tree" $
      tabTraversalSpec controlRect multiTheme
        [MultiButton 0, MultiButton 1]
        (multiButton 0 >> disableWhen True (multiGroup 0) >> multiButton 1)
