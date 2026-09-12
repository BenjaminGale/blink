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
--                                             --> checkbox     (see "Blink.Controls.Checkbox")
--                                             --> radioButton  (see "Blink.Controls.RadioButton") --> radioButtonGroup
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
  , glyphCaptionElement
  , glyphCaptionContent
  , isSelected
  , onSelectedChanged
  ) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Set as Set

import Blink.Controls.Button
  (ButtonConfig (..), ButtonInteraction (..), HasButtonConfig (..), buttonBase, defaultButtonConfig)
import Blink.Controls.Control
import Blink.Controls.Label
  (HasLabelledConfig (..), LabelledConfig (..), captionElement, lcText, renderLabelledContent)
import Blink.Controls.ToggleButton.Style (toggleButtonStyleKey, toggleChecked, toggleStyleGroup, toggleUnchecked)
import Blink.Geometry (Alignment (TopLeft), Rectangle (..), Size (..))
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.View (Effect, View, getBounds, measureText, withBounds)
import Blink.Element (Element (..), HasLayoutConfig (..))

-- | Whether the control is currently selected.
isSelected :: Bool -> Attribute (ToggleConfig e msg)
isSelected b = Attribute (\cfg -> cfg { tgcSelected = b })

-- | Reacts when activating the control (a click or Enter while focused)
-- moves its selected state to a new value, with the value it changed to.
-- It's up to the reaction to actually store the new value and pass it back
-- in via 'isSelected' next frame.
onSelectedChanged :: (Bool -> [Effect e msg]) -> Attribute (ToggleConfig e msg)
onSelectedChanged f = Attribute (\cfg -> cfg { tgcOnSelectedChanged = tgcOnSelectedChanged cfg ++ [f] })

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
  overControl attr = Attribute (\tc -> tc { tgcButton = runAttribute (overControl attr) (tgcButton tc) })

instance HasLabelledConfig e msg (ToggleConfig e msg) where
  overLabelled attr = Attribute (\tc -> tc { tgcButton = runAttribute (overLabelled attr) (tgcButton tc) })

instance HasButtonConfig e msg (ToggleConfig e msg) where
  overButton attr = Attribute (\tc -> tc { tgcButton = runAttribute attr (tgcButton tc) })

instance HasLayoutConfig (ToggleConfig e msg) where
  overLayout attr = Attribute (\tc -> tc { tgcButton = runAttribute (overLayout attr) (tgcButton tc) })

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
toggleButton eid attrs = Element
  { elLayout  = bcLayout btn
  , elMeasure = measureChrome (ccStyleKey (bcControl btn)) (captionElement (lcText (bcLabelled btn)))
  , elRun     = void (toggleBase eid cfg { tgcNext = not, tgcButton = btn { bcControl = ctrl } })
  }
  where
    cfg  = resolve defaultToggleButtonConfig attrs
    btn  = tgcButton cfg
    ctrl = (bcControl btn) { ccContent = const (renderLabelledContent (bcLabelled btn)) }

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
