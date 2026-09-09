{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Text drawn in the resolved style, with no interactive behaviour of its
-- own. 'LabelledConfig' is the reusable fragment behind 'text': not a
-- 'Blink.Controls.Control.control'-style layer itself (nothing calls
-- into it the way a @Base@ primitive is called into), just a nested field
-- plus a rendering helper, the same category as
-- 'Blink.Controls.Control.ControlConfig'.
-- Anything that wants a caption nests a 'LabelledConfig' field, declares
-- 'HasLabelledConfig', and calls 'renderLabelledContent' itself when
-- building its own content.
--
-- 'label' is a leaf, built directly on 'control'; it needs its own
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

import Control.Monad (forM_)
import Data.Text (Text)

import Blink.Controls.Control
import Blink.Controls.Label.Style (labelStyleKey)
import Blink.Geometry (Alignment (TopLeft))
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.Style (Style (..))
import Blink.View (View, currentStyle, getCurrentScope, measureText)
import Blink.View.Drawing (drawText)
import Blink.Element (Element (..), HasLayoutConfig (..))

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
renderLabelledContent :: LabelledConfig e msg -> View e msg ()
renderLabelledContent cfg = do
  s <- currentStyle
  drawText (styleTextColour s) (styleTextAlign s) (lcText cfg)

-- | A minimal, non-wrapping caption measure -- stand-in for a proper
-- @textBlock@ primitive, which doesn't exist yet. Reports the caption's
-- unwrapped single-line size on both axes. Shared by every control whose
-- content is a plain caption (see 'Blink.Controls.Button.button', 'label').
captionElement :: Text -> Element e msg
captionElement t = Element
  { elLayout  = Layout fill fitContent TopLeft
  , elMeasure = const (measureText t)
  , elRun     = pure ()
  }

-- * Label

-- | Every capability 'label' resolves: the wrapped 'ControlConfig', its
-- caption, and the element a click on it should redirect focus to (see
-- 'target').
data LabelConfig e msg = LabelConfig
  { lblControl  :: ControlConfig e msg
  , lblLabelled :: LabelledConfig e msg
  , lblLayout   :: Layout
  , lblTarget   :: Maybe e
  }

-- | 'defaultControlConfig' (styled via 'labelStyleKey'), an empty caption,
-- @Layout fitContent fitContent TopLeft@ (see 'label'), and no 'target'.
defaultLabelConfig :: LabelConfig e msg
defaultLabelConfig = LabelConfig
  { lblControl  = defaultControlConfig { ccStyleKey = labelStyleKey }
  , lblLabelled = defaultLabelledConfig
  , lblLayout   = Layout fitContent fitContent TopLeft
  , lblTarget   = Nothing
  }

instance HasControlConfig e msg (LabelConfig e msg) where
  overControl attr = Attribute (\c -> c { lblControl = runAttribute attr (lblControl c) })

instance HasLabelledConfig e msg (LabelConfig e msg) where
  overLabelled attr = Attribute (\c -> c { lblLabelled = runAttribute attr (lblLabelled c) })

instance HasLayoutConfig (LabelConfig e msg) where
  overLayout attr = Attribute (\c -> c { lblLayout = runAttribute attr (lblLayout c) })

-- | Names the element a click on the label should focus instead of the
-- label itself -- e.g. a caption redirecting a click onto the input beside
-- it. Unset by default, in which case clicking the label does nothing to
-- focus.
target :: e -> Attribute (LabelConfig e msg)
target t = Attribute (\c -> c { lblTarget = Just t })

-- | Displays text (see 'text'). Unlike every other control built on
-- 'control', a label never takes keyboard focus itself, whether by Tab
-- or by being clicked: this is fixed behaviour, not a default -- 'label'
-- always overrides 'focusPolicy' to 'NotFocusable' itself, so it wins
-- regardless of what a caller passes. The only way a click on a label affects focus
-- at all is 'target': unlike a control taking focus for itself, which
-- happens on mouse-down, redirecting focus onto a /different/ element only
-- takes effect once the click completes -- so dragging off the label
-- before releasing backs out of the redirect, the same way dragging off
-- any other clickable control backs out of its click.
-- Defaults to sizing itself to its own chrome-wrapped caption on both axes
-- -- unlike 'Blink.Controls.Button.button', a label is often placed beside
-- other content in a row (a field name next to its input) rather than
-- spanning it alone, so it shouldn't claim the whole row by default.
-- Override with 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
label :: Ord e => e -> [Attribute (LabelConfig e msg)] -> Element e msg
label eid attrs = Element
  { elLayout  = lblLayout cfg
  , elMeasure = measureChrome (ccStyleKey (lblControl cfg)) (captionElement (lcText (lblLabelled cfg)))
  , elRun     = do
      scope <- getCurrentScope
      ci    <- control ctrl
      forM_ (lblTarget cfg) (\t -> focusTargetOnClick scope t ci)
  }
  where
    cfg  = resolve defaultLabelConfig attrs
    ctrl = (lblControl cfg)
      { ccFocusPolicy = NotFocusable
      , ccContent     = const (renderLabelledContent (lblLabelled cfg))
      , ccElementId   = Just eid
      }
