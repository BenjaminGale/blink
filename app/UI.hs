{-# LANGUAGE OverloadedStrings #-}
module UI (ControlId, AppState (..), demoApp) where

import Blink.App hiding (Continue)
import Blink.View.Attribute (Attribute)
import Blink.View.Controls
import Blink.View.Controls.Button (onActivated)
import Blink.View.Controls.Control
  (ChildNavigation (..), ContainedNavigation (..), ControlConfig (..), EntryPolicy (..), FocusPolicy (..)
  , WrapPolicy (..), control, defaultControlConfig, elementId, focusPolicy, isEnabled, measureChrome, post
  , postWith, resolve
  )
import Blink.View.Controls.Divider (orientation)
import Blink.View.Controls.Label (LabelConfig, target, text)
import Blink.View.Controls.ProgressBar (ProgressValue (..), progress)
import Blink.View.Controls.RepeatButton
  (firedCount, onFiredCountChanged, onPressEnded, onPressStarted, pressStartedAt)
import Blink.View.Controls.Slider (onValueChanged)
import qualified Blink.View.Controls.Slider as Slider (value)
import Blink.View.Controls.TextInput (displayFilter, onInput, value)
import Blink.View.Controls.Toggle (isSelected, onSelectedChanged)
import Blink.Geometry
import Blink.Input
import Blink.View.Layout
import Blink.View.Rendering
import Blink.View
import Blink.View.Element (Element (..), elementWithLayout, runElement)
import Blink.Update
import Theme (ControlId (..), Page (..), lightTheme, darkTheme)
import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T

-- Application state

data AppState = AppState
  { darkMode       :: Bool
  , editingEnabled :: Bool
  , clickCount     :: Int
  , toggleOn       :: Bool
  , radioChoice    :: Maybe Text
  , inputText      :: Text
  , passwordText   :: Text
  , animating      :: Bool
  , holdPressedAt  :: Maybe Double
  , holdFiredCount :: Int
  , sliderValue    :: Double
  , isHovering     :: Bool
  , lastInput      :: Text
  , lastInputCount :: Int
  , currentPage    :: Page
  , lastContainedActivation :: Text
  , containedWrap     :: WrapPolicy
  , containedRemember :: Bool
  , continueSearchText :: Text
  }

data Msg
  = SetDarkMode Bool
  | SetEditingEnabled Bool
  | AddClick
  | ResetClicks
  | SetToggle Bool
  | PickRadio Text
  | SetInputText Text
  | SetPasswordText Text
  | SetAnimating Bool
  | HoldPressStarted Double
  | HoldFiredCountChanged Int
  | HoldPressEnded
  | SetSlider Double
  | FrameObserved Bool Text  -- ^ mouse-is-hovering, this frame's raw key/typed-text label
  | SetPage Page
  | ContainedActivated Text
  | SetContainedWrap WrapPolicy
  | SetContainedRemember Bool
  | SetContinueSearch Text
  | ClearContinueSearch

demoApp :: App ControlId Msg AppState
demoApp = App
  { startUp = pure AppState
      { darkMode       = False
      , editingEnabled = True
      , clickCount     = 0
      , toggleOn       = False
      , radioChoice    = Nothing
      , inputText      = ""
      , passwordText   = ""
      , animating      = False
      , holdPressedAt  = Nothing
      , holdFiredCount = 0
      , sliderValue    = 0.5
      , isHovering     = False
      , lastInput      = ""
      , lastInputCount = 0
      , currentPage    = ControlsPage
      , lastContainedActivation = ""
      , containedWrap     = WrapCycle
      , containedRemember = False
      , continueSearchText = ""
      }
  , theme   = \s -> if darkMode s then darkTheme else lightTheme
  , view    = demoView
  , update  = updateApp
  }

updateApp :: Msg -> Update AppState ()
updateApp msg = case msg of
  SetDarkMode v       -> modify $ \s -> s { darkMode = v }
  SetEditingEnabled v -> modify $ \s -> s { editingEnabled = v }
  AddClick             -> modify $ \s -> s { clickCount = min 50 (clickCount s + 1) }
  ResetClicks           -> modify $ \s -> s { clickCount = 0 }
  SetToggle v          -> modify $ \s -> s { toggleOn = v }
  PickRadio v          -> modify $ \s -> s { radioChoice = Just v }
  SetInputText t       -> modify $ \s -> s { inputText = t }
  SetPasswordText t    -> modify $ \s -> s { passwordText = t }
  SetAnimating v       -> modify $ \s -> s { animating = v }
  HoldPressStarted t      -> modify $ \s -> s { holdPressedAt = Just t, holdFiredCount = 0 }
  HoldFiredCountChanged n -> modify $ \s -> s { holdFiredCount = n }
  HoldPressEnded          -> modify $ \s -> s { holdPressedAt = Nothing, holdFiredCount = 0 }
  SetSlider v          -> modify $ \s -> s { sliderValue = v }
  FrameObserved hov keyLabel -> modify $ \s -> s
    { isHovering     = hov
    , lastInput      = if T.null keyLabel then lastInput s else keyLabel
    , lastInputCount = if T.null keyLabel then lastInputCount s
                       else if keyLabel == lastInput s then lastInputCount s + 1
                       else 1
    }
  SetPage p            -> modify $ \s -> s { currentPage = p }
  ContainedActivated t -> modify $ \s -> s { lastContainedActivation = t }
  SetContainedWrap w      -> modify $ \s -> s { containedWrap = w }
  SetContainedRemember v  -> modify $ \s -> s { containedRemember = v }
  SetContinueSearch t     -> modify $ \s -> s { continueSearchText = t }
  ClearContinueSearch     -> modify $ \s -> s { continueSearchText = "" }

type DemoUI = View ControlId Msg

-- Shell

-- | Plain, non-interactive text under the shared 'Label' element ID.
caption :: Text -> [Attribute (LabelConfig ControlId Msg)] -> Element ControlId Msg
caption t attrs = label Label (text t : attrs)

-- | A row pairing a caption with the control it describes: fixed-width
-- label on the left (redirecting clicks to @targetId@ via 'target'),
-- control filling the rest. Takes 'rowLayout' (or another layout) so the
-- row's own width\/height fit alongside its siblings. The label shares
-- @enabled@ with @control@, so a disabled field's label stops redirecting
-- clicks to it too. Identified by its own 'FieldLabel' id rather than
-- 'caption's shared one -- a redirecting label needs its click tracked
-- against its own row, not conflated with every other label on screen.
field :: [Attribute (BoxConfig ControlId Msg)] -> Bool -> ControlId -> Text -> Element ControlId Msg -> Element ControlId Msg
field layoutAttrs enabled targetId labelText fieldControl =
  hBox
    ( layoutAttrs ++
      [ spacing 8, alignment Center
      , children
          [ label (FieldLabel targetId)
              [text labelText, target targetId, isEnabled enabled, width (exactly 120), height fill, align MiddleLeft]
          , fieldControl
          ]
      ]
    )

rowHeight :: Length
rowHeight = exactly 40

-- | Layout attributes shared by every direct child of 'mainList': the full
-- row width, the shared row height, top-left within that slot.
rowLayout :: HasLayoutConfig cfg => [Attribute cfg]
rowLayout = [width fill, height rowHeight, align TopLeft]

-- Control rows, top to bottom

rowDarkMode :: AppState -> Element ControlId Msg
rowDarkMode s =
  checkbox DarkModeCheckbox (rowLayout ++ [text "Dark mode", isSelected (darkMode s), onSelectedChanged (postWith SetDarkMode)])

rowEditing :: AppState -> Element ControlId Msg
rowEditing s =
  checkbox EditingCheckbox
    (rowLayout ++ [text "Enable editing", isSelected (editingEnabled s), onSelectedChanged (postWith SetEditingEnabled)])

-- | A plain full-width separator between the settings checkboxes above and
-- the interactive controls below -- 'divider's own default orientation and
-- thickness, and (since nothing here reacts to it) no id either.
rowDivider :: Element ControlId Msg
rowDivider = divider []

rowButtons :: AppState -> Element ControlId Msg
rowButtons s =
  hBox
    ( rowLayout ++
      [ spacing 8
      , children
          [ button ClickButton [text "Click me", onActivated (post AddClick), isEnabled (editingEnabled s), width (exactly 100), height fill]
          , repeatButton HoldButton
              [ text "Hold me", onActivated (post AddClick), isEnabled (editingEnabled s), width (exactly 100), height fill
              , pressStartedAt (holdPressedAt s), onPressStarted (postWith HoldPressStarted)
              , firedCount (holdFiredCount s), onFiredCountChanged (postWith HoldFiredCountChanged)
              , onPressEnded [OutMsg HoldPressEnded]
              ]
          , button ResetButton [text "Reset", onActivated (post ResetClicks), isEnabled (editingEnabled s), width (exactly 100), height fill]
          , divider [orientation Vertical, height fill]
          , caption ("Clicks: " <> T.pack (show (clickCount s))) [width fill, height fill, align MiddleLeft]
          ]
      ]
    )

rowToggle :: AppState -> Element ControlId Msg
rowToggle s =
  hBox
    ( rowLayout ++
      [ spacing 8
      , children
          [ toggleButton ToggleCtl
              [ text "Toggle me", isSelected (toggleOn s), onSelectedChanged (postWith SetToggle)
              , isEnabled (editingEnabled s), width (exactly 160), height fill
              ]
          , caption (if toggleOn s then "On" else "Off") [width fill, height fill, align MiddleLeft]
          ]
      ]
    )

radioOptions :: [Text]
radioOptions = ["Small", "Medium", "Large"]

rowRadio :: AppState -> Element ControlId Msg
rowRadio s =
  hBox
    ( rowLayout ++
      [ spacing 16
      , children
          [ radioOption i opt
          | (i, opt) <- zip [0 ..] radioOptions
          ]
      ]
    )
  where
    radioOption i opt =
      radioButton (RadioCtl i)
        [ text opt, isSelected (radioChoice s == Just opt), onSelectedChanged (\_ -> [OutMsg (PickRadio opt)])
        , isEnabled (editingEnabled s), width (exactly 100), height fill, align MiddleLeft
        ]

rowTextInput :: AppState -> Element ControlId Msg
rowTextInput s =
  field rowLayout (editingEnabled s) TextInputCtl "Text input"
    (textInput TextInputCtl [value (inputText s), onInput (postWith SetInputText), isEnabled (editingEnabled s), height fill])

rowPasswordInput :: AppState -> Element ControlId Msg
rowPasswordInput s =
  field rowLayout (editingEnabled s) PasswordInputCtl "Password input"
    (textInput PasswordInputCtl
        [ value (passwordText s), displayFilter (T.map (const '\8226')), onInput (postWith SetPasswordText)
        , isEnabled (editingEnabled s), height fill
        ])

rowAnimate :: AppState -> Element ControlId Msg
rowAnimate s =
  checkbox AnimateCheckbox
    (rowLayout ++ [text "Animate progress bar", isSelected (animating s), onSelectedChanged (postWith SetAnimating), isEnabled (editingEnabled s)])

rowProgress :: AppState -> Element ControlId Msg
rowProgress s =
  if animating s
    then progressBar (rowLayout ++ [progress Indeterminate])
    else progressBar (rowLayout ++ [progress (Progress (fromIntegral (clickCount s) / 50))])

rowSlider :: AppState -> Element ControlId Msg
rowSlider s =
  field rowLayout (editingEnabled s) SliderCtl "Slider"
    ( hBox
        [ spacing 8, alignment Center
        , children
            [ slider SliderCtl [Slider.value (sliderValue s), onValueChanged (postWith SetSlider), isEnabled (editingEnabled s), height fill]
            , caption (T.pack (show (round (sliderValue s * 100) :: Int)) <> "%") [width (exactly 60), height fill, align MiddleLeft]
            ]
        ]
    )

-- Footer

footer :: AppState -> DemoUI ()
footer s = do
  pos    <- getMousePos
  input  <- getInput
  win    <- getWindowSize
  let winW      = round (rectWidth win) :: Int
      winH      = round (rectHeight win) :: Int
      winText    = "Window: " <> T.pack (show winW) <> " x " <> T.pack (show winH)
      mx         = T.pack (show (round (pointX pos) :: Int))
      my         = T.pack (show (round (pointY pos) :: Int))
      mouseText  = "Mouse: " <> mx <> ", " <> my
      buttonText = "Button: " <> T.pack (show (inputLeftButtonDown input))
      hoverText  = "Hover: " <> if isHovering s then "Yes" else "No"
      countSuffix = if lastInputCount s > 1
                      then " (" <> T.pack (show (lastInputCount s)) <> ")"
                      else ""
      keyText    = "Last Key Press: "
                <> if T.null (lastInput s) then "none" else lastInput s <> countSuffix
  runElement $ hBox
    [ spacing 24, margin 4, alignment Center
    , children
        [ caption winText    [width (exactly 160), height fill, align MiddleLeft]
        , caption mouseText  [width (exactly 160), height fill, align MiddleLeft]
        , caption buttonText [width (exactly 160), height fill, align MiddleLeft]
        , caption hoverText  [width (exactly 100), height fill, align MiddleLeft]
        , caption keyText    [width fill,          height fill, align MiddleLeft]
        ]
    ]

-- Sidebar & pages
--
-- The screen has grown enough controls that it needed a way to switch
-- between them: a fixed sidebar of page buttons on the left, the selected
-- page's content filling the rest. 'ContainedPage' is here specifically to
-- show off 'Blink.View.Controls.Control.FocusScope'\'s
-- 'Blink.View.Controls.Control.Contained' navigation -- a group of options
-- that Tab treats as a single stop, with Up\/Down moving the selection
-- between them, and its own radio\/checkbox controls to reconfigure the
-- group's 'WrapPolicy'\/'EntryPolicy' live. 'Continue' isn't demonstrated
-- separately since it isn't visibly different from how every other
-- composite on the controls page already behaves.

sidebarWidth :: Double
sidebarWidth = 170

pages :: [(Page, Text)]
pages =
  [ (ControlsPage,  "Controls")
  , (ContinuePage,  "Continue")
  , (ContainedPage, "Contained")
  ]

-- | One button per 'Page', disabled for whichever page is already showing
-- -- both a lightweight "you are here" indicator and (since a disabled
-- control ignores clicks) preventing a pointless re-select.
sidebar :: AppState -> DemoUI ()
sidebar s =
  runElement $ vBox
    [ width fill, height fill, spacing 4, margin 12
    , children
        ( caption "Pages" [width fill, height (exactly 20), align TopLeft]
        : [ sidebarButton page label' | (page, label') <- pages ]
        )
    ]
  where
    sidebarButton page label' =
      button (SidebarPageButton page)
        [ text label', onActivated (post (SetPage page)), isEnabled (currentPage s /= page)
        , width fill, height (exactly 32)
        ]

pageContent :: AppState -> DemoUI ()
pageContent s = case currentPage s of
  ControlsPage  -> mainList s
  ContinuePage  -> continuePage s
  ContainedPage -> containedPage s

-- | 'continueGroup's own natural height (its own margin plus one row of
-- content, at 'rowHeight') -- same reasoning as 'containedGroupHeight'.
continueGroupHeight :: Double
continueGroupHeight = containedMargin * 2 + 40

continuePage :: AppState -> DemoUI ()
continuePage s =
  runElement $ vBox
    [ spacing 12, margin 12
    , children
        [ caption "Continue navigation" [width fill, height (exactly 24), align TopLeft]
        , caption description [width fill, height (exactly 40), align TopLeft]
        , button ContinueBefore [text "Before", width fill, height (exactly 32)]
        , continueGroup s
        , button ContinueAfter [text "After", width fill, height (exactly 32)]
        ]
    ]
  where
    description =
      "Tab moves through the search field and Clear button as if this \
      \container weren't here at all -- but the container's own border \
      \still shows it as focused whenever either one does."

-- | A 'Blink.View.Controls.Control.FocusScope' composite wrapping an
-- ordinary search field: a text input and a Clear button, Tab-reachable
-- individually exactly as if this container didn't exist, with no
-- traversal logic of its own -- 'FocusScope' 'Continue' needs none. The
-- container's own chrome (see its border once either child is focused)
-- is the one thing 'Continue' actually adds: reading as focused via
-- focus-within, for styling a "genuine container" this way rather than a
-- plain, invisible grouping.
continueGroup :: AppState -> Element ControlId Msg
continueGroup s = Element
  { elLayout  = Layout fill fitContent TopLeft
  , elMeasure = measureChrome (ccStyleKey cfg) (fixedSize 0 continueGroupHeight)
  , elRun     = void (control cfg)
  }
  where
    cfg = (resolve defaultControlConfig
            [ elementId ContinueGroup
            , focusPolicy (FocusScope Continue)
            ])
            { ccContent = const (runElement innerBox) }
    innerBox = hBox
      [ spacing 8, margin containedMargin
      , children
          [ textInput ContinueSearchInput
              [value (continueSearchText s), onInput (postWith SetContinueSearch), width fill, height (exactly 40)]
          , button ContinueClearButton
              [text "Clear", onActivated (post ClearContinueSearch), width (exactly 80), height (exactly 40)]
          ]
      ]

containedOptions :: [Text]
containedOptions = ["Alpha", "Bravo", "Charlie", "Delta"]

containedRowHeight, containedSpacing, containedMargin :: Double
containedRowHeight = 32
containedSpacing   = 4
containedMargin    = 6

-- | A measurement-only element reporting a fixed natural size -- content
-- is rendered separately (via @ccContent@), never through this; it exists
-- purely for 'measureChrome' to inflate by the wrapping control's own
-- chrome, the same way e.g. 'Blink.View.Controls.Toggle.glyphCaptionElement'
-- stands in for a checkbox's actual content when measuring 'toggleBase'.
fixedSize :: Double -> Double -> Element e msg
fixedSize w h = Element
  { elLayout  = Layout fill fitContent TopLeft
  , elMeasure = const (pure (Size w h))
  , elRun     = pure ()
  }

-- | The option list's own natural height -- it never changes, so this is
-- a plain calculation rather than genuine measurement. Fed through
-- 'measureChrome' (see 'containedGroup') to additionally account for the
-- wrapping composite's own chrome, on top of this.
containedGroupHeight :: Double
containedGroupHeight =
  containedMargin * 2 + containedRowHeight * n + containedSpacing * (n - 1)
  where
    n = fromIntegral (length containedOptions)

-- | The wrap policies offered by 'rowContainedWrap', paired with their
-- radio-row captions.
wrapOptions :: [(WrapPolicy, Text)]
wrapOptions =
  [ (WrapCycle, "Cycle (wraps past either end)")
  , (WrapStop,  "Stop (clamps at either end)")
  ]

-- | A radio row choosing 'containedWrap', and a checkbox choosing
-- 'containedRemember' -- reconfiguring the live group below on the
-- 'ContainedPage' without needing a page each per combination.
rowContainedWrap :: AppState -> Element ControlId Msg
rowContainedWrap s =
  hBox
    ( rowLayout ++
      [ spacing 16
      , children
          [ radioOption i wrap opt
          | (i, (wrap, opt)) <- zip [0 ..] wrapOptions
          ]
      ]
    )
  where
    radioOption i wrap opt =
      radioButton (ContainedWrapRadio i)
        [ text opt, isSelected (containedWrap s == wrap), onSelectedChanged (\_ -> [OutMsg (SetContainedWrap wrap)])
        , width (exactly 220), height fill, align MiddleLeft
        ]

rowContainedRemember :: AppState -> Element ControlId Msg
rowContainedRemember s =
  checkbox ContainedRememberCheckbox
    ( rowLayout ++
      [ text "Remember last selection when re-entering"
      , isSelected (containedRemember s), onSelectedChanged (postWith SetContainedRemember)
      ]
    )

containedPage :: AppState -> DemoUI ()
containedPage s =
  runElement $ vBox
    [ spacing 12, margin 12
    , children
        [ caption "Contained navigation" [width fill, height (exactly 24), align TopLeft]
        , caption description [width fill, height (exactly 40), align TopLeft]
        , rowContainedWrap s
        , rowContainedRemember s
        , button ContainedBefore [text "Before", width fill, height (exactly 32)]
        , containedGroup s
        , button ContainedAfter [text "After", width fill, height (exactly 32)]
        , caption ("Last activated: " <> lastText) [width fill, height (exactly 24), align TopLeft]
        ]
    ]
  where
    lastText = if T.null (lastContainedActivation s) then "none" else lastContainedActivation s
    description =
      "Tab moves between Before, the group, and After as three ordinary stops. \
      \Up/Down move the selection between options, per the wrap mode below."

-- | A 'Blink.View.Controls.Control.FocusScope' composite: a single Tab stop
-- from outside, with 'containedOptions' as its own distinctly-identified
-- children navigated by Up\/Down instead, reconfigured live from
-- 'rowContainedWrap'\/'rowContainedRemember'.
containedGroup :: AppState -> Element ControlId Msg
containedGroup s = Element
  { elLayout  = Layout fill fitContent TopLeft
  , elMeasure = measureChrome (ccStyleKey cfg) (fixedSize 0 containedGroupHeight)
  , elRun     = void (control cfg)
  }
  where
    nav = ContainedNavigation
      { navForward  = (KeyDown, [])
      , navBackward = (KeyUp, [])
      , navWrap     = containedWrap s
      , navEntry    = if containedRemember s then EnterRemembered else EnterFirst
      }
    cfg = (resolve defaultControlConfig
            [ elementId ContainedGroup
            , focusPolicy (FocusScope (Contained nav))
            ])
            { ccContent = const (runElement optionsBox) }
    optionsBox = vBox [spacing containedSpacing, margin containedMargin, children optionElements]
    optionElements =
      [ button (ContainedOption i)
          [ text opt, onActivated (post (ContainedActivated opt)), width fill, height (exactly containedRowHeight) ]
      | (i, opt) <- zip [0 :: Int ..] containedOptions
      ]

-- Top-level view

demoView :: AppState -> Element ControlId Msg
demoView s = elementWithLayout (Layout fill fill TopLeft) $ do
  input <- getInput
  when (darkMode s) $ fillRect (RGBA 0.082 0.102 0.129 1)
  borderLayout [left sidebarWidth (sidebar s), centre (pageContent s), bottom 36 (footer s)]
  anyHov <- isAnyMouseOver
  let typed   = T.concat (inputTypedText input)
      keyName = case inputKeyEvents input of
                  []      -> ""
                  (e : _) -> T.pack (show (key e))
      newInput = if not (T.null typed)
                   then if typed == " " then "Space" else "Character " <> typed
                   else keyName
  emit (FrameObserved anyHov newInput)

mainList :: AppState -> DemoUI ()
mainList s =
  runElement $ vBox
    [ spacing 8, margin 12
    , children
        [ caption "Blink controls demo" [width fill, height (exactly 24), align TopLeft]
        , rowDarkMode s
        , rowEditing s
        , rowDivider
        , rowButtons s
        , rowToggle s
        , rowRadio s
        , rowTextInput s
        , rowPasswordInput s
        , rowAnimate s
        , rowProgress s
        , rowSlider s
        ]
    ]
