{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Text drawn in the resolved style, set via 'text'.
module Blink.Label
  ( LabelConfig
  , label
  , text
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

import Blink.Attributes
  ( Attr, ControlConfig (..), HasControlConfig (..), HasTextConfig (..)
  , FocusOnClick (..), defaultControlConfig, configure, text
  )
import Blink.Control (control, getStyle)
import Blink.Element
  ( ElementEvent (..)
  , onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed, onFocusGained, onFocusLost
  )
import Blink.Style (Style (..))
import Blink.UI (UI, drawText)

-- | Configuration for 'label', set via 'text'. Defaults to @\"\"@.
data LabelConfig e = LabelConfig
  { labelConfigControl :: ControlConfig e
  , labelConfigText    :: Text
  }

defaultLabelConfig :: LabelConfig e
defaultLabelConfig = LabelConfig { labelConfigControl = defaultControlConfig, labelConfigText = "" }

instance HasControlConfig e (LabelConfig e) where
  controlConfig    = labelConfigControl
  setControlConfig cc cfg = cfg { labelConfigControl = cc }

instance HasTextConfig (LabelConfig e) where
  setText t cfg = cfg { labelConfigText = t }

-- | Displays text in the resolved style. Unlike every other control built
-- on 'control', a label never takes keyboard focus itself, whether by Tab
-- or by being clicked: this is fixed behaviour, not a default, so passing
-- @isTabStop@\/@focusOnClick@ in @attrs@ has no effect on it.
label :: Ord e => e -> [Attr e ElementEvent msg (LabelConfig e)] -> UI e msg ()
label eid attrs = control eid cfg attrs $ do
  style <- getStyle eid
  drawText (styleTextColour style) (styleTextAlign style) (labelConfigText cfg)
  where
    -- Applied after resolving attrs, so a caller can't override this via
    -- isTabStop\/focusOnClick even by importing them directly from
    -- "Blink.Attributes".
    cfg = fixFocusBehaviour (configure defaultLabelConfig attrs)
    fixFocusBehaviour c = setControlConfig ((controlConfig c) { ccIsTabStop = False, ccFocusOnClick = NoFocus }) c
