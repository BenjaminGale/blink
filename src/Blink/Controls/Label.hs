{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Text drawn in the resolved style, with no interactive behaviour of its
-- own. 'LabelledConfig' is the reusable fragment behind 'text': not a
-- 'Blink.Controls.Element.elementBase'\/'Blink.Controls.Control.controlBase'-style
-- layer itself (nothing calls into it the way a @Base@ primitive is called
-- into), just a nested field plus a rendering helper, the same category as
-- 'Blink.Controls.Element.ElementConfig'\/'Blink.Controls.Control.ControlConfig'.
-- Anything that wants a caption nests a 'LabelledConfig' field, declares
-- 'HasLabelledConfig', and calls 'renderLabelledContent' itself when
-- building its own content.
--
-- 'label' is a leaf, built directly on 'controlBase'; it needs its own
-- 'LabelConfig' only because 'target' is a label-only capability
-- 'LabelledConfig' has no concept of.
module Blink.Controls.Label
  ( -- * Caption fragment
    LabelledConfig (..)
  , HasLabelledConfig (..)
  , defaultLabelledConfig
  , text
  , renderLabelledContent
  , captionElement

    -- * Label
  , LabelConfig (..)
  , defaultLabelConfig
  , labelStyleKey
  , label
  , target
  ) where

import Control.Monad (void)
import Data.Text (Text)

import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft))
import Blink.Layout.Constraints (HasLayoutConfig (..), Layout (..), Length (..))
import Blink.Style (Style (..))
import Blink.UI (UI, currentStyle, drawText, measureText)
import Blink.UI.Element (Element (..))

-- * Caption fragment

-- | A displayed caption.
newtype LabelledConfig e msg = LabelledConfig
  { lcText :: Text
  }

-- | @\"\"@.
defaultLabelledConfig :: LabelledConfig e msg
defaultLabelledConfig = LabelledConfig { lcText = "" }

-- | Implemented by any config type that nests a 'LabelledConfig', letting
-- 'text' be applied to it directly.
class HasLabelledConfig e msg cfg | cfg -> e msg where
  overLabelled :: Attribute (LabelledConfig e msg) -> Attribute cfg

instance HasLabelledConfig e msg (LabelledConfig e msg) where
  overLabelled = id

-- | Sets the text displayed. Defaults to @\"\"@ when not given.
text :: HasLabelledConfig e msg cfg => Text -> Attribute cfg
text t = overLabelled (Attribute (\lc -> lc { lcText = t }))

-- | Draws @cfg@'s text into the current bounds, in the resolved style's
-- text colour and alignment.
renderLabelledContent :: LabelledConfig e msg -> UI e msg ()
renderLabelledContent cfg = do
  s <- currentStyle
  drawText (styleTextColour s) (styleTextAlign s) (lcText cfg)

-- | A minimal, non-wrapping caption measure -- stand-in for a proper
-- @textBlock@ primitive, which doesn't exist yet. Reports the caption's
-- unwrapped single-line size on both axes. Shared by every control whose
-- content is a plain caption (see 'Blink.Controls.Button.button', 'label').
captionElement :: Text -> Element e msg
captionElement t = Element
  { elLayout  = Layout Fill FitContent TopLeft
  , elMeasure = const (measureText t)
  , elRun     = pure ()
  }

-- * Label

-- | Every capability 'label' resolves: the wrapped 'ControlConfig', its
-- caption, and the element a click on it should redirect focus to (see
-- 'target').
data LabelConfig e msg = LabelConfig
  { lcControl  :: ControlConfig e msg
  , lcLabelled :: LabelledConfig e msg
  , lcLayout   :: Layout
  , lcTarget   :: Maybe e
  }

-- | 'defaultControlConfig' (styled via 'labelStyleKey'), an empty caption,
-- @Layout FitContent FitContent TopLeft@ (see 'label'), and no 'target'.
defaultLabelConfig :: LabelConfig e msg
defaultLabelConfig = LabelConfig
  { lcControl  = defaultControlConfig { ccStyleKey = labelStyleKey }
  , lcLabelled = defaultLabelledConfig
  , lcLayout   = Layout FitContent FitContent TopLeft
  , lcTarget   = Nothing
  }

-- | The 'StyleKey' 'label' resolves its style from unless overridden via
-- 'style'.
labelStyleKey :: StyleKey e
labelStyleKey = Class "label"

instance HasElementConfig e msg (LabelConfig e msg) where
  overElement attr = Attribute (\c -> c { lcControl = runAttribute (overElement attr) (lcControl c) })

instance HasControlConfig e msg (LabelConfig e msg) where
  overControl attr = Attribute (\c -> c { lcControl = runAttribute attr (lcControl c) })

instance HasLabelledConfig e msg (LabelConfig e msg) where
  overLabelled attr = Attribute (\c -> c { lcLabelled = runAttribute attr (lcLabelled c) })

instance HasLayoutConfig (LabelConfig e msg) where
  overLayout attr = Attribute (\c -> c { lcLayout = runAttribute attr (lcLayout c) })

-- | Names the element a click on the label should focus instead of the
-- label itself -- e.g. a caption redirecting a click onto the input beside
-- it. Unset by default, in which case clicking the label does nothing to
-- focus.
target :: e -> Attribute (LabelConfig e msg)
target t = Attribute (\c -> c { lcTarget = Just t })

-- | Displays text (see 'text'). Unlike every other control built on
-- 'controlBase', a label never takes keyboard focus itself, whether by Tab
-- or by being clicked: this is fixed behaviour, not a default -- 'label'
-- always overrides 'isFocusable' to 'False' itself, so it wins regardless
-- of what a caller passes. The only way a click on a label affects focus
-- at all is 'target', which redirects it to a different, named element.
-- Defaults to sizing itself to its own chrome-wrapped caption on both axes
-- -- unlike 'Blink.Controls.Button.button', a label is often placed beside
-- other content in a row (a field name next to its input) rather than
-- spanning it alone, so it shouldn't claim the whole row by default.
-- Override with 'Blink.Layout.Constraints.width'\/'Blink.Layout.Constraints.height'\/'Blink.Layout.Constraints.align'.
label :: Ord e => e -> [Attribute (LabelConfig e msg)] -> Element e msg
label eid attrs = Element
  { elLayout  = lcLayout cfg
  , elMeasure = measureChrome (ccStyleKey (lcControl cfg)) (captionElement (lcText (lcLabelled cfg)))
  , elRun     = void (controlBase eid ctrl)
  }
  where
    cfg   = resolve defaultLabelConfig attrs
    focus = maybe NoFocus FocusTarget (lcTarget cfg)
    ctrl  = (lcControl cfg)
      { ccIsFocusable  = False
      , ccFocusOnClick = focus
      , ccContent      = renderLabelledContent (lcLabelled cfg)
      }
