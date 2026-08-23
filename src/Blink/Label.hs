{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Text drawn in the resolved style, set via 'text'.
module Blink.Label
  ( LabelConfig
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

import Data.Text (Text)

import Blink.Control
  ( ControlConfig, FocusOnClick (..), HasControlConfig (..), HasTextConfig (..)
  , control, defaultControlConfig, isEnabled, isFocusable, style, text
  )
import Blink.Element
  ( Attr, ElementEvent (..)
  , configAny, configure
  , onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed, onFocusGained, onFocusLost
  )
import Blink.Style (Style (..), StyleKey (..))
import Blink.UI (UI, currentStyle, drawText)

-- | Configuration for 'label', set via 'text' and 'target'. Defaults to no
-- text and no target.
data LabelConfig e = LabelConfig
  { labelConfigControl :: ControlConfig e
  , labelConfigText    :: Text
  , labelConfigTarget  :: Maybe e
  }

-- | The 'StyleKey' 'label' resolves its style from unless overridden via
-- 'style'.
labelStyleKey :: StyleKey e
labelStyleKey = Class "label"

defaultLabelConfig :: LabelConfig e
defaultLabelConfig = LabelConfig
  { labelConfigControl = defaultControlConfig labelStyleKey
  , labelConfigText    = ""
  , labelConfigTarget  = Nothing
  }

instance HasControlConfig e (LabelConfig e) where
  controlConfig    = labelConfigControl
  setControlConfig cc cfg = cfg { labelConfigControl = cc }

instance HasTextConfig (LabelConfig e) where
  setText t cfg = cfg { labelConfigText = t }

-- | Names the element a click on the label should focus instead of the
-- label itself -- e.g. a caption redirecting a click onto the input beside
-- it. Unset by default, in which case clicking the label does nothing to
-- focus.
target :: e -> Attr e ev msg (LabelConfig e)
target t = configAny $ \cfg -> cfg { labelConfigTarget = Just t }

-- | Displays text in the resolved style. Unlike every other control built
-- on 'control', a label never takes keyboard focus itself, whether by Tab
-- or by being clicked: this is fixed behaviour, not a default, so passing
-- @isFocusable@\/@focusOnClick@ in @attrs@ has no effect on it. The only way
-- a click on a label affects focus at all is 'target', which redirects it
-- to a different, named element.
label :: Ord e => e -> [Attr e ElementEvent msg (LabelConfig e)] -> UI e msg ()
label eid attrs = control eid focusOnClick cfg attrs $ do
  s <- currentStyle
  drawText (styleTextColour s) (styleTextAlign s) (labelConfigText cfg)
  where
    -- isFocusable appended last so a caller can't override it -- 'target'
    -- is the only supported way to affect a label's focus behaviour.
    cfg = configure defaultLabelConfig (attrs ++ [isFocusable False])
    focusOnClick = maybe NoFocus FocusTarget (labelConfigTarget cfg)
