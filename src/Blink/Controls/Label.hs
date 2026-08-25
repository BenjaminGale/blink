{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Text drawn in the resolved style, with no interactive behaviour of its
-- own -- see 'HasLabelledConfig' via "Blink.Controls.Labelled" for 'text'.
-- A leaf, built directly on 'controlBase'; needs its own 'LabelConfig' only
-- because 'target' is a label-only capability
-- "Blink.Controls.Labelled"'s 'Blink.Controls.Labelled.LabelledConfig' has
-- no concept of.
module Blink.Controls.Label
  ( LabelConfig (..)
  , defaultLabelConfig
  , labelStyleKey
  , label
  , target
  ) where

import Blink.Controls.Core
import Blink.Controls.Labelled
import Blink.UI (UI)

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
  () <$ controlBase eid ctrl
