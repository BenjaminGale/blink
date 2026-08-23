{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Text drawn in the resolved style, set via 'text'.
module Blink.Label
  ( LabelAttributes
  , LabelConfig
  , label
  , labelStyleKey
  , text
  , target
  , isEnabled
  , style
  , StyleKey (..)
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

import Blink.Control
import Blink.Style (Style (..))
import Blink.UI (UI, currentStyle, drawText)

-- | 'Blink.Label.label'\'s own closed attrs type: the common capabilities
-- every control has, plus 'text' and 'target'. Doesn't expose 'isFocusable'
-- -- a label never takes keyboard focus itself, whether by Tab or by being
-- clicked; this is fixed behaviour, not a default, so 'label' simply never
-- exports a smart constructor that could set it, even though the
-- 'HasControlConfig' instance carries the capability generically like every
-- other widget's.
data LabelAttributes e msg
  = LabelCommon (ControlProperties e)
  | LabelEvent (ElementEvents e msg)
  | LabelText Text
  | LabelTarget e

instance HasControlConfig e (LabelAttributes e msg) where
  configureControlCapability = LabelCommon
  extractControlCapability (LabelCommon c) = Just c
  extractControlCapability _ = Nothing

instance HasElementEvents e msg (LabelAttributes e msg) where
  configureElementEvent = LabelEvent
  extractElementEvent (LabelEvent c) = Just c
  extractElementEvent _ = Nothing

instance HasTextConfig (LabelAttributes e msg) where
  configureText = LabelText
  extractText (LabelText t) = Just t
  extractText _ = Nothing

-- | Names the element a click on the label should focus instead of the
-- label itself -- e.g. a caption redirecting a click onto the input beside
-- it. Unset by default, in which case clicking the label does nothing to
-- focus.
target :: e -> LabelAttributes e msg
target = LabelTarget

-- | Configuration for 'label', resolved from a @['LabelAttributes' e msg]@.
data LabelConfig e = LabelConfig
  { lcfgText   :: Text
  , lcfgTarget :: Maybe e
  }

-- | The 'StyleKey' 'label' resolves its style from unless overridden via
-- 'style'.
labelStyleKey :: StyleKey e
labelStyleKey = Class "label"

defaultLabelConfig :: LabelConfig e
defaultLabelConfig = LabelConfig { lcfgText = "", lcfgTarget = Nothing }

resolveLabelConfig :: [LabelAttributes e msg] -> LabelConfig e
resolveLabelConfig = foldl' apply defaultLabelConfig
  where
    apply cfg (LabelText t)   = cfg { lcfgText = t }
    apply cfg (LabelTarget t) = cfg { lcfgTarget = Just t }
    apply cfg _               = cfg

toLabelControlAttr :: LabelAttributes e msg -> Maybe (ControlAttrs e msg)
toLabelControlAttr (LabelText _)   = Nothing
toLabelControlAttr (LabelTarget _) = Nothing
toLabelControlAttr a               = translateCommon a

-- | Displays text in the resolved style. Unlike every other control built
-- on 'control', a label never takes keyboard focus itself, whether by Tab
-- or by being clicked: this is fixed behaviour, not a default -- 'label'
-- simply never exposes 'isFocusable', and always overrides it to 'False'
-- itself. The only way a click on a label affects focus at all is
-- 'target', which redirects it to a different, named element.
label :: Ord e => e -> [LabelAttributes e msg] -> UI e msg ()
label eid attrs = control eid (mapMaybe toLabelControlAttr attrs ++ [isFocusable False, focusOnClick focusTarget, content bodyContent])
  where
    cfg = resolveLabelConfig attrs
    focusTarget = maybe NoFocus FocusTarget (lcfgTarget cfg)
    bodyContent = do
      s <- currentStyle
      drawText (styleTextColour s) (styleTextAlign s) (lcfgText cfg)
