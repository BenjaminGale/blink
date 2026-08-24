{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Text and\/or a glyph, drawn in the resolved style -- see 'HasLabelConfig'.
--
-- 'label' is the plain, non-focusable display control. 'labelledControl' is
-- the lower-level, generic base for a control that shows a label as (part
-- of) its own content -- see 'HasLabelledContent' for how a caller built on
-- it places that label within whatever else it draws. The two don't derive
-- from one another; they just share 'HasLabelConfig'.
module Blink.Label
  ( -- * Labels
    LabelAttributes
  , LabelConfig
  , label
  , labelStyleKey
  , target

    -- * Labelled controls
  , LabelledControlAttrs
  , labelledControl
  , HasLabelledContent (..)
  , content

    -- * Configuring a label
  , HasLabelConfig (..)
  , LabelProperties
  , DisplayMode (..)
  , text
  , glyph
  , displayMode
  , isEnabled
  , style
  , StyleKey (..)

    -- * Events
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost
  ) where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Blink.Control hiding (content, text)
import qualified Blink.Control as Control
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI (UI, currentStyle, drawText, getBounds, withBounds)

-- | How a labelled thing's glyph and text combine -- set via 'displayMode'.
-- Defaults to 'TextOnly'.
data DisplayMode
  = TextAndGlyph
    -- ^ The glyph in a fixed-width column, followed by the text in the rest.
  | TextOnly
    -- ^ Just the text, filling the whole space.
  | GlyphOnly
    -- ^ Just the glyph, filling the whole space.
  deriving (Eq, Show)

-- | One of the three capabilities shared by anything with 'HasLabelConfig'
-- -- the single payload its attrs type carries one of, the same way
-- 'Blink.Control.ControlProperties' works for 'Blink.Control.HasControlConfig'.
data LabelProperties
  = LabelPropText Text
  | LabelPropGlyph Text
  | LabelPropDisplayMode DisplayMode

-- | Implemented by an attrs type that lets a caller set 'text'\/'glyph'\/
-- 'displayMode' -- shared by 'LabelAttributes' and 'LabelledControlAttrs'
-- alike, though neither is built out of the other.
class HasLabelConfig cfg where
  configureLabelCapability :: LabelProperties -> cfg
  extractLabelCapability :: cfg -> Maybe LabelProperties

-- | Sets the text a label displays. Defaults to @\"\"@ when not given.
text :: HasLabelConfig cfg => Text -> cfg
text = configureLabelCapability . LabelPropText

-- | Sets the glyph a label displays. Defaults to @\"\"@ when not given.
glyph :: HasLabelConfig cfg => Text -> cfg
glyph = configureLabelCapability . LabelPropGlyph

-- | Sets how 'text' and 'glyph' combine -- see 'DisplayMode'.
displayMode :: HasLabelConfig cfg => DisplayMode -> cfg
displayMode = configureLabelCapability . LabelPropDisplayMode

-- | Every 'HasLabelConfig' capability, resolved once from a list of attrs.
data LabelConfig = LabelConfig
  { lcfgText        :: Text
  , lcfgGlyph       :: Text
  , lcfgDisplayMode :: DisplayMode
  }

defaultLabelConfig :: LabelConfig
defaultLabelConfig = LabelConfig { lcfgText = "", lcfgGlyph = "", lcfgDisplayMode = TextOnly }

applyLabelProperty :: LabelConfig -> LabelProperties -> LabelConfig
applyLabelProperty cfg (LabelPropText t)        = cfg { lcfgText = t }
applyLabelProperty cfg (LabelPropGlyph g)       = cfg { lcfgGlyph = g }
applyLabelProperty cfg (LabelPropDisplayMode m) = cfg { lcfgDisplayMode = m }

-- | The fixed width reserved for the glyph, on the left of the text, when
-- 'displayMode' is 'TextAndGlyph'.
glyphColumnWidth :: Double
glyphColumnWidth = 20

-- | Draws @cfg@'s glyph and\/or text (per its 'DisplayMode') into the
-- current bounds, in the resolved style's text colour -- the rendering
-- 'label' and 'labelledControl' both build their content from.
renderLabelContent :: LabelConfig -> UI e msg ()
renderLabelContent cfg = do
  s <- currentStyle
  case lcfgDisplayMode cfg of
    GlyphOnly    -> drawText (styleTextColour s) AlignCenter (lcfgGlyph cfg)
    TextOnly     -> drawText (styleTextColour s) (styleTextAlign s) (lcfgText cfg)
    TextAndGlyph -> do
      bounds <- getBounds
      let glyphRect = bounds { rectWidth = glyphColumnWidth }
          textRect  = bounds { rectX = rectX bounds + glyphColumnWidth, rectWidth = max 0 (rectWidth bounds - glyphColumnWidth) }
      withBounds glyphRect $ drawText (styleTextColour s) AlignCenter (lcfgGlyph cfg)
      withBounds textRect  $ drawText (styleTextColour s) (styleTextAlign s) (lcfgText cfg)

-- | 'Blink.Label.label'\'s own closed attrs type: the common capabilities
-- every control has, plus 'HasLabelConfig' and 'target'. A label never
-- takes keyboard focus itself, whether by Tab or by being clicked; this is
-- fixed behaviour, not a default, so 'label' always appends its own
-- 'isFocusable' 'False' last, overriding whatever a caller passes --
-- 'Blink.Control.isFocusable' still typechecks against 'LabelAttributes'
-- (the 'HasControlConfig' instance is public, like every other widget's),
-- it just never has any effect.
data LabelAttributes e msg
  = LabelCommon (ControlProperties e)
  | LabelEvent (ElementEvents e msg)
  | LabelProp LabelProperties
  | LabelTarget e

instance HasControlConfig e (LabelAttributes e msg) where
  configureControlCapability = LabelCommon
  extractControlCapability (LabelCommon c) = Just c
  extractControlCapability _ = Nothing

instance HasElementEvents e msg (LabelAttributes e msg) where
  configureElementEvent = LabelEvent
  extractElementEvent (LabelEvent c) = Just c
  extractElementEvent _ = Nothing

instance HasLabelConfig (LabelAttributes e msg) where
  configureLabelCapability = LabelProp
  extractLabelCapability (LabelProp p) = Just p
  extractLabelCapability _ = Nothing

-- | Names the element a click on the label should focus instead of the
-- label itself -- e.g. a caption redirecting a click onto the input beside
-- it. Unset by default, in which case clicking the label does nothing to
-- focus.
target :: e -> LabelAttributes e msg
target = LabelTarget

-- | The 'StyleKey' 'label' resolves its style from unless overridden via
-- 'style'.
labelStyleKey :: StyleKey e
labelStyleKey = Class "label"

resolveLabelConfig :: [LabelAttributes e msg] -> (LabelConfig, Maybe e)
resolveLabelConfig = foldl' apply (defaultLabelConfig, Nothing)
  where
    apply (cfg, tgt) (LabelProp p)   = (applyLabelProperty cfg p, tgt)
    apply (cfg, _)   (LabelTarget t) = (cfg, Just t)
    apply acc        _               = acc

toLabelControlAttr :: LabelAttributes e msg -> Maybe (ControlAttrs e msg)
toLabelControlAttr (LabelProp _)   = Nothing
toLabelControlAttr (LabelTarget _) = Nothing
toLabelControlAttr a               = translateCommon a

-- | Displays text and\/or a glyph (see 'HasLabelConfig'). Unlike every other
-- control built on 'control', a label never takes keyboard focus itself,
-- whether by Tab or by being clicked: this is fixed behaviour, not a
-- default -- 'label' always appends its own 'isFocusable' 'False' last, so
-- it wins regardless of what a caller passes. The only way a click on a
-- label affects focus at all is 'target', which redirects it to a
-- different, named element.
label :: Ord e => e -> [LabelAttributes e msg] -> UI e msg ()
label eid attrs = control eid (style labelStyleKey : mapMaybe toLabelControlAttr attrs ++ [isFocusable False, focusOnClick focusTarget, Control.content (renderLabelContent cfg)])
  where
    (cfg, tgt) = resolveLabelConfig attrs
    focusTarget = maybe NoFocus FocusTarget tgt

-- | Implemented by an attrs type that carries how a labelled control places
-- its rendered label within its own content -- see 'content'.
class HasLabelledContent e msg cfg | cfg -> e msg where
  configureLabelledContent :: (UI e msg () -> UI e msg ()) -> cfg

-- | Says how this control places its rendered label (built from
-- 'HasLabelConfig') within its own content: the function is handed that
-- rendered label as a ready-made @UI e msg ()@ and returns the control's
-- whole content -- the caller's only job is to decide /where/ to run it
-- (e.g. alongside a selection glyph of the control's own), not to build it.
-- Defaults to running the label over the whole content area unchanged.
content :: HasLabelledContent e msg cfg => (UI e msg () -> UI e msg ()) -> cfg
content = configureLabelledContent

-- | 'labelledControl'\'s own closed attrs type: the common capabilities
-- every control has, plus 'HasLabelConfig' and 'HasLabelledContent'.
data LabelledControlAttrs e msg
  = LabelledControlCommon (ControlProperties e)
  | LabelledControlEvent (ElementEvents e msg)
  | LabelledControlFocusOnClick (FocusOnClick e)
  | LabelledControlProp LabelProperties
  | LabelledControlContent (UI e msg () -> UI e msg ())

instance HasControlConfig e (LabelledControlAttrs e msg) where
  configureControlCapability = LabelledControlCommon
  extractControlCapability (LabelledControlCommon c) = Just c
  extractControlCapability _ = Nothing

instance HasElementEvents e msg (LabelledControlAttrs e msg) where
  configureElementEvent = LabelledControlEvent
  extractElementEvent (LabelledControlEvent c) = Just c
  extractElementEvent _ = Nothing

instance HasFocusOnClickConfig e (LabelledControlAttrs e msg) where
  configureFocusOnClick = LabelledControlFocusOnClick
  extractFocusOnClick (LabelledControlFocusOnClick f) = Just f
  extractFocusOnClick _ = Nothing

instance HasLabelConfig (LabelledControlAttrs e msg) where
  configureLabelCapability = LabelledControlProp
  extractLabelCapability (LabelledControlProp p) = Just p
  extractLabelCapability _ = Nothing

instance HasLabelledContent e msg (LabelledControlAttrs e msg) where
  configureLabelledContent = LabelledControlContent

-- | Every capability a @['LabelledControlAttrs' e msg]@ resolves beyond the
-- common ones 'control' itself already understands: its label (see
-- 'HasLabelConfig') and how to place it (see 'HasLabelledContent').
data ResolvedLabelledConfig e msg = ResolvedLabelledConfig
  { rlcLabel    :: LabelConfig
  , rlcRenderer :: UI e msg () -> UI e msg ()
  }

defaultResolvedLabelledConfig :: ResolvedLabelledConfig e msg
defaultResolvedLabelledConfig = ResolvedLabelledConfig defaultLabelConfig id

resolveLabelledConfig :: [LabelledControlAttrs e msg] -> ResolvedLabelledConfig e msg
resolveLabelledConfig = foldl' apply defaultResolvedLabelledConfig
  where
    apply cfg (LabelledControlProp p)    = cfg { rlcLabel = applyLabelProperty (rlcLabel cfg) p }
    apply cfg (LabelledControlContent f) = cfg { rlcRenderer = f }
    apply cfg _                          = cfg

-- | Translates the capabilities 'control' itself understands down to
-- 'ControlAttrs' -- @Nothing@ for 'HasLabelConfig'\/'HasLabelledContent',
-- which 'control' has no concept of; 'labelledControl' folds those into
-- 'Blink.Control.content' itself instead (see below).
toLabelledControlAttr :: LabelledControlAttrs e msg -> Maybe (ControlAttrs e msg)
toLabelledControlAttr (LabelledControlFocusOnClick f) = Just (focusOnClick f)
toLabelledControlAttr (LabelledControlProp _)         = Nothing
toLabelledControlAttr (LabelledControlContent _)      = Nothing
toLabelledControlAttr a                               = translateCommon a

-- | The generic base for a control that shows a label (see
-- 'HasLabelConfig') as part of its own content: renders the label, places
-- it within the control's content via 'HasLabelledContent' (unchanged, over
-- the whole content area, by default), and runs 'control' with the result.
-- Unlike 'label', this stays focusable by default -- the same as any other
-- plain 'control' -- since a real interactive widget built on it (a button,
-- a checkbox, ...) needs to be.
labelledControl :: Ord e => e -> [LabelledControlAttrs e msg] -> UI e msg ()
labelledControl eid attrs =
  control eid (mapMaybe toLabelledControlAttr attrs ++ [Control.content (rlcRenderer cfg (renderLabelContent (rlcLabel cfg)))])
  where
    cfg = resolveLabelledConfig attrs
