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
import Blink.Style (Style (..))
import Blink.UI (UI, currentStyle, drawText)

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
  overLabelled :: Attr (LabelledConfig e msg) -> Attr cfg

instance HasLabelledConfig e msg (LabelledConfig e msg) where
  overLabelled = id

-- | Sets the text displayed. Defaults to @\"\"@ when not given.
text :: HasLabelledConfig e msg cfg => Text -> Attr cfg
text t = overLabelled (Attr (\lc -> lc { lcText = t }))

-- | Draws @cfg@'s text into the current bounds, in the resolved style's
-- text colour and alignment.
renderLabelledContent :: LabelledConfig e msg -> UI e msg ()
renderLabelledContent cfg = do
  s <- currentStyle
  drawText (styleTextColour s) (styleTextAlign s) (lcText cfg)

-- * Label

-- | Every capability 'label' resolves: the wrapped 'ControlConfig', its
-- caption, and the element a click on it should redirect focus to (see
-- 'target').
data LabelConfig e msg = LabelConfig
  { lcControl  :: ControlConfig e msg
  , lcLabelled :: LabelledConfig e msg
  , lcTarget   :: Maybe e
  }

-- | 'defaultControlConfig' (styled via 'labelStyleKey'), an empty caption,
-- and no 'target'.
defaultLabelConfig :: LabelConfig e msg
defaultLabelConfig = LabelConfig
  { lcControl  = defaultControlConfig { ccStyleKey = labelStyleKey }
  , lcLabelled = defaultLabelledConfig
  , lcTarget   = Nothing
  }

-- | The 'StyleKey' 'label' resolves its style from unless overridden via
-- 'style'.
labelStyleKey :: StyleKey e
labelStyleKey = Class "label"

instance HasElementConfig e msg (LabelConfig e msg) where
  overElement attr = Attr (\c -> c { lcControl = runAttr (overElement attr) (lcControl c) })

instance HasControlConfig e msg (LabelConfig e msg) where
  overControl attr = Attr (\c -> c { lcControl = runAttr attr (lcControl c) })

instance HasLabelledConfig e msg (LabelConfig e msg) where
  overLabelled attr = Attr (\c -> c { lcLabelled = runAttr attr (lcLabelled c) })

-- | Names the element a click on the label should focus instead of the
-- label itself -- e.g. a caption redirecting a click onto the input beside
-- it. Unset by default, in which case clicking the label does nothing to
-- focus.
target :: e -> Attr (LabelConfig e msg)
target t = Attr (\c -> c { lcTarget = Just t })

-- | Displays text (see 'text'). Unlike every other control built on
-- 'controlBase', a label never takes keyboard focus itself, whether by Tab
-- or by being clicked: this is fixed behaviour, not a default -- 'label'
-- always overrides 'isFocusable' to 'False' itself, so it wins regardless
-- of what a caller passes. The only way a click on a label affects focus
-- at all is 'target', which redirects it to a different, named element.
label :: Ord e => e -> [Attr (LabelConfig e msg)] -> UI e msg ()
label eid attrs = do
  let cfg   = resolve defaultLabelConfig attrs
      focus = maybe NoFocus FocusTarget (lcTarget cfg)
      ctrl  = (lcControl cfg)
        { ccIsFocusable  = False
        , ccFocusOnClick = focus
        , ccContent      = renderLabelledContent (lcLabelled cfg)
        }
  void (controlBase eid ctrl)
