{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The shared base for every control that tracks a selected\/unselected
-- state instead of only ever being momentarily pressed: 'toggleButton'
-- flips every time it's clicked; a checkbox
-- (see "Blink.Controls.Checkbox") does too, drawing a checkmark glyph
-- beside its caption; a radio button (see "Blink.Controls.RadioButton")
-- can only become selected, never unselected, by being clicked -- it gives
-- up selection when another radio button in the same group is selected
-- instead.
--
-- @
-- control --> buttonBase --> button                    (see "Blink.Controls.Button")
--                             --> toggleBase --> toggleButton --> toggleButtonGroup (see "Blink.Controls.ToggleGroup")
--                                             --> iconToggle --> checkbox     (see "Blink.Controls.Checkbox")
--                                                            --> radioButton  (see "Blink.Controls.RadioButton") --> radioButtonGroup
-- @
module Blink.Controls.ToggleButton
  ( ToggleConfig (..)
  , ToggleInteraction (..)
  , defaultToggleButtonConfig
  , defaultGlyphToggleConfig
  , toggleButtonStyleKey
  , toggleStyleGroup
  , toggleChecked
  , toggleUnchecked
  , toggleBase
  , toggleButton
  , IconToggleConfig (..)
  , iconToggle
  , isSelected
  , onSelectedChanged
    -- * Style
  , defaultStyleEntries
  ) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Blink.Controls.Button
  (ButtonConfig (..), ButtonInteraction (..), HasButtonConfig (..), buttonBase, captionedButton, defaultButtonConfig, withCaptionContent)
import Blink.Controls.Control
import Blink.Controls.Label
  (HasLabelledConfig (..), LabelledConfig (..), renderLabelledContent)
import Blink.Geometry (Alignment (TopLeft), Rectangle (..), Size (..))
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.View (Effect, View, getBounds, getStyleSet, isRegionHit, measureText, withBounds)
import Blink.View.Drawing (drawImage)
import Blink.Element (Element (..), HasLayoutConfig (..))
import Blink.Rendering (Colour, ImagePath, TextAlign (..))
import Blink.Style
import Blink.Controls.Style (buttonStyle, controlMetrics, iconStyleKey)

-- | Whether the control is currently selected.
isSelected :: Bool -> Attribute (ToggleConfig e msg)
isSelected b = Attribute (\cfg -> cfg { tgcSelected = b })

-- | Reacts when activating the control (a click or Enter while focused)
-- moves its selected state to a new value, with the value it changed to.
-- It's up to the reaction to actually store the new value and pass it back
-- in via 'isSelected' next frame.
onSelectedChanged :: (Bool -> [Effect e msg]) -> Attribute (ToggleConfig e msg)
onSelectedChanged = appendTo tgcOnSelectedChanged (\cfg hs -> cfg { tgcOnSelectedChanged = hs })

-- | Every capability 'toggleButton' (and any checkbox\/radio button) shares:
-- the wrapped 'ButtonConfig', how activating the control changes its
-- selected state (fixed by the concrete widget below, never
-- attr-settable -- @not@ for 'toggleButton'\/checkbox, @const True@ for a
-- radio button), whether it was selected (per 'isSelected'), and its
-- 'onSelectedChanged' reactions.
data ToggleConfig e msg = ToggleConfig
  { tgcButton            :: ButtonConfig e msg
  , tgcNext              :: Bool -> Bool
  , tgcSelected          :: Bool
  , tgcOnSelectedChanged :: [Bool -> [Effect e msg]]
  }

-- | 'defaultButtonConfig' (styled via 'toggleButtonStyleKey'), @not@ (a
-- placeholder -- every concrete widget fixes this itself), not selected,
-- and no 'onSelectedChanged' reactions.
defaultToggleButtonConfig :: ToggleConfig e msg
defaultToggleButtonConfig = ToggleConfig
  { tgcButton            = defaultButtonConfig { bcControl = (bcControl defaultButtonConfig) { ccStyleKey = toggleButtonStyleKey } }
  , tgcNext              = not
  , tgcSelected          = False
  , tgcOnSelectedChanged = []
  }

instance HasControlConfig e msg (ToggleConfig e msg) where
  overControl = nested tgcButton (\tc x -> tc { tgcButton = x }) . overControl

instance HasEventHandlers (ToggleConfig e msg)

instance HasLabelledConfig e msg (ToggleConfig e msg) where
  overLabelled = nested tgcButton (\tc x -> tc { tgcButton = x }) . overLabelled

instance HasButtonConfig e msg (ToggleConfig e msg) where
  overButton = nested tgcButton (\tc x -> tc { tgcButton = x })

instance HasLayoutConfig (ToggleConfig e msg) where
  overLayout = nested tgcButton (\tc x -> tc { tgcButton = x }) . overLayout

-- | What 'toggleBase' reports back: the wrapped button's own
-- 'ButtonInteraction', and the selected state after this frame's
-- activation, if any (see 'toggleBase').
data ToggleInteraction e msg = ToggleInteraction
  { tgiButton   :: ButtonInteraction e msg
  , tgiSelected :: Bool
  }

-- | Runs @cfg@ as 'Blink.Controls.Button.buttonBase', and additionally fires
-- every 'onSelectedChanged' reaction (only) when activating the control
-- would move its selected state (per 'tgcSelected') to a different value
-- than 'tgcNext' computes from it -- e.g. a radio button that's already
-- selected stays selected when clicked again, so it fires nothing. The
-- shape 'toggleButton' and any checkbox\/radio button share.
toggleBase :: Ord e => e -> ToggleConfig e msg -> View e msg (ToggleInteraction e msg)
toggleBase eid cfg = do
  r <- buttonBase eid (tgcButton cfg')
  let wasSelected = tgcSelected cfg
      newValue    = tgcNext cfg wasSelected
      changed     = biActivated r && newValue /= wasSelected
  when changed $ runHandlers (tgcOnSelectedChanged cfg) newValue
  pure (ToggleInteraction r (if biActivated r then newValue else wasSelected))
  where
    btn  = tgcButton cfg
    ctrl = bcControl btn
    pseudoState = if tgcSelected cfg then toggleChecked else toggleUnchecked
    cfg' = cfg { tgcButton = btn { bcControl = ctrl { ccActiveStates = Set.singleton pseudoState } } }

-- | A button labelled via 'Blink.Controls.Label.text' that tracks an external selected\/unselected
-- state (see 'isSelected') instead of only ever being momentarily pressed,
-- flipping every time it's activated. Drawn with 'toggleChecked' active
-- while selected -- see "Blink.Style" for how a theme gives that a
-- distinct look. Activated the same way as 'Blink.Controls.Button.button';
-- see 'onSelectedChanged' for reacting to it. Defaults to filling the width
-- it's given and sizing its height to its own chrome-wrapped caption, the
-- same as 'Blink.Controls.Button.button'; override with 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
toggleButton :: Ord e => e -> [Attribute (ToggleConfig e msg)] -> Element e msg
toggleButton eid attrs = captionedButton btn (void (toggleBase eid cfg { tgcNext = not, tgcButton = withCaptionContent btn }))
  where
    cfg = resolve defaultToggleButtonConfig attrs
    btn = tgcButton cfg

-- | 'defaultToggleButtonConfig' styled via @styleKey@ and sized to fit its
-- own content on both axes -- the shared default for a leaf toggle control
-- that pairs a fixed-width glyph with a caption beside it (a checkbox or
-- radio button), rather than filling its row the way 'toggleButton' does.
defaultGlyphToggleConfig :: StyleKey e -> ToggleConfig e msg
defaultGlyphToggleConfig styleKey = defaultToggleButtonConfig
  { tgcButton = (tgcButton defaultToggleButtonConfig)
      { bcControl = (bcControl (tgcButton defaultToggleButtonConfig)) { ccStyleKey = styleKey }
      , bcLayout  = Layout fitContent fitContent TopLeft
      }
  }

-- | Every capability 'iconToggle' resolves: the wrapped 'ToggleConfig',
-- and the icon's fixed look.
data IconToggleConfig e msg = IconToggleConfig
  { itToggle         :: ToggleConfig e msg
  , itIconWidth      :: Double
    -- ^ Width of the icon column, left of the caption.
  , itGap            :: Double
    -- ^ Gap between the icon column and the caption.
  , itIconInset      :: Double
    -- ^ Margin between the icon column's edges and the drawn icon.
  , itTrailingSpace  :: Double
    -- ^ Space measured after the caption.
  , itSelectedIcon   :: ImagePath
    -- ^ Drawn while selected.
  , itUnselectedIcon :: ImagePath
    -- ^ Drawn while not selected.
  }

-- | A toggle drawn as an icon beside its caption, running @cfg@'s
-- 'itToggle' as 'toggleBase' --
-- clicking either the icon or the caption activates it. The icon is
-- centred in its column and tinted from 'Blink.Controls.Style.iconStyleKey':
-- a separate 'StyleKey' from the control's own, resolved against whether
-- the pointer is over the /icon's own rectangle specifically/, so hovering
-- the caption beside it, still within the control's larger hit area,
-- leaves the icon (and the caption's own colour, which never reads from
-- 'Blink.Controls.Style.iconStyleKey') alone. The shape a checkbox and a
-- radio button share.
iconToggle :: Ord e => e -> IconToggleConfig e msg -> Element e msg
iconToggle eid iconCfg =
  chromeElement (bcLayout btn) (ccStyleKey (bcControl btn)) content (void (toggleBase eid cfg { tgcButton = btn { bcControl = ctrl } }))
  where
    cfg     = itToggle iconCfg
    btn     = tgcButton cfg
    iconW   = itIconWidth iconCfg
    content = withTrailingSpace (glyphCaptionElement iconW (itGap iconCfg) (lcText (bcLabelled btn)))
    withTrailingSpace el =
      el { elMeasure = fmap (\sz -> sz { sizeWidth = sizeWidth sz + itTrailingSpace iconCfg }) . elMeasure el }
    ctrl = (bcControl btn) { ccContent = \ci -> glyphCaptionContent iconW (itGap iconCfg) (drawIcon ci) (bcLabelled btn) }

    drawIcon ci = do
      (_, iconStyleSet) <- getStyleSet iconStyleKey
      bounds            <- getBounds
      let iconSize = max 0 (min iconW (rectHeight bounds) - itIconInset iconCfg)
          iconRect = Rectangle
            { rectX      = rectX bounds + (iconW - iconSize) / 2
            , rectY      = rectY bounds + (rectHeight bounds - iconSize) / 2
            , rectWidth  = iconSize
            , rectHeight = iconSize
            }
          icon = if tgcSelected cfg then itSelectedIcon iconCfg else itUnselectedIcon iconCfg
      withBounds iconRect $ do
        hovered <- isRegionHit
        let iconState
              | ciDisabled ci = CommonDisabled
              | hovered       = CommonMouseOver
              | otherwise     = CommonNormal
            colour = styleTextColour (resolveStyle iconStyleSet (Set.singleton iconState))
        drawImage colour icon

-- | The glyph-plus-caption content's own preferred size: the glyph's fixed
-- width plus the gap between it and the caption plus the caption's
-- unwrapped single-line width; the taller of the glyph's width (drawn as a
-- square) and the caption's line height. Shared measure for a checkbox\/
-- radio button; see 'glyphCaptionContent' for the matching render shape.
glyphCaptionElement :: Double -> Double -> Text -> Element e msg
glyphCaptionElement glyphWidth gap t = Element
  { elLayout  = Layout fill fitContent TopLeft
  , elMeasure = const $ do
      capSize <- measureText t
      pure (Size (glyphWidth + gap + sizeWidth capSize) (max glyphWidth (sizeHeight capSize)))
  , elRun     = pure ()
  }

-- | Renders a fixed-width glyph column, drawn by @drawGlyph@ into just that
-- column's own bounds, beside a caption filling the remaining space. The
-- shared render shape behind a checkbox\/radio button; see
-- 'glyphCaptionElement' for the matching measure.
glyphCaptionContent :: Double -> Double -> View e msg () -> LabelledConfig e msg -> View e msg ()
glyphCaptionContent glyphWidth gap drawGlyph labelled = do
  bounds <- getBounds
  let glyphRect = bounds { rectWidth = glyphWidth }
      textRect  = bounds
        { rectX     = rectX bounds + glyphWidth + gap
        , rectWidth = max 0 (rectWidth bounds - glyphWidth - gap)
        }
  withBounds glyphRect drawGlyph
  withBounds textRect (renderLabelledContent labelled)

-- * Style

-- | The 'StyleKey' 'Blink.Controls.ToggleButton.toggleButton' resolves
-- its style from unless overridden via 'Blink.Controls.Control.style'.
toggleButtonStyleKey :: StyleKey e
toggleButtonStyleKey = Class "toggleButton"

-- | The 'VisualState' group name shared by 'toggleChecked'\/
-- 'toggleUnchecked' -- see "Blink.Style"'s module header for why a
-- control defines its own pseudo-states as opaque exported constants
-- rather than letting callers build 'Custom' values themselves.
toggleStyleGroup :: Text
toggleStyleGroup = "Toggle"

-- | The pseudo-state 'Blink.Controls.ToggleButton.toggleBase' puts in
-- 'Blink.Controls.Control.ccActiveStates' while the control is
-- selected (see 'Blink.Controls.ToggleButton.isSelected') -- a theme
-- registers an override for this on its own 'StyleSet' (keyed to whatever
-- 'StyleKey' the control actually resolves to, e.g. 'toggleButtonStyleKey')
-- to give it a distinct "selected" look, composed with whatever
-- common\/focus state is also active.
toggleChecked :: VisualState
toggleChecked = Custom toggleStyleGroup "Checked"

-- | The pseudo-state 'Blink.Controls.ToggleButton.toggleBase' puts in
-- 'Blink.Controls.Control.ccActiveStates' while the control is
-- unselected. Themes typically register no override for this -- the plain
-- base look already reads as "unchecked".
toggleUnchecked :: VisualState
toggleUnchecked = Custom toggleStyleGroup "Unchecked"

-- | 'buttonStyle' with an added bold accent fill while 'toggleChecked' --
-- the same fill 'buttonStyle' already uses for 'CommonPressed'.
checkedStyle :: Colour -> Colour -> StyleSet -> StyleSet
checkedStyle accent onAccent s = s
  { styleOverrides = Map.insert toggleChecked
      (\st -> st { styleBackground = accent, styleTextColour = onAccent, styleBorder = withBorderColour accent (styleBorder st) })
      (styleOverrides s)
  }

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ ( toggleButtonStyleKey
    , (controlMetrics, checkedStyle (paletteAccent p) (paletteTextOnAccent p) (buttonStyle AlignCenter p))
    )
  ]
