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

-- | 'Blink.Label.label'\'s own closed attrs type: most of the common
-- capabilities every control has, plus 'text' and 'target'. Doesn't expose
-- 'isFocusable' -- a label never takes keyboard focus itself, whether by
-- Tab or by being clicked; this is fixed behaviour, not a default, so
-- 'label' has to still implement a @LabelIsFocusable@ constructor for its
-- 'HasControlConfig' instance to type-check, but simply never exports a
-- smart constructor that could build one.
data LabelAttributes e msg
  = LabelIsFocusable Bool
  | LabelIsEnabled Bool
  | LabelStyle (StyleKey e)
  | LabelTabNavigation NavigationMode
  | LabelIsArrowNavigationEnabled Bool
  | LabelOnClicked (EventHandler e msg)
  | LabelOnFocusGained (EventHandler e msg)
  | LabelOnFocusLost (EventHandler e msg)
  | LabelOnMouseEntered (EventHandler e msg)
  | LabelOnMouseExited (EventHandler e msg)
  | LabelOnMouseDown (EventHandler e msg)
  | LabelOnMouseUp (EventHandler e msg)
  | LabelOnKeyPressed (KeyEventHandler e msg)
  | LabelText Text
  | LabelTarget e

instance HasControlConfig e (LabelAttributes e msg) where
  configureIsFocusable = LabelIsFocusable
  extractIsFocusable (LabelIsFocusable b) = Just b
  extractIsFocusable _ = Nothing
  configureIsEnabled = LabelIsEnabled
  extractIsEnabled (LabelIsEnabled b) = Just b
  extractIsEnabled _ = Nothing
  configureStyle = LabelStyle
  extractStyle (LabelStyle k) = Just k
  extractStyle _ = Nothing
  configureTabNavigation = LabelTabNavigation
  extractTabNavigation (LabelTabNavigation m) = Just m
  extractTabNavigation _ = Nothing
  configureIsArrowNavigationEnabled = LabelIsArrowNavigationEnabled
  extractIsArrowNavigationEnabled (LabelIsArrowNavigationEnabled b) = Just b
  extractIsArrowNavigationEnabled _ = Nothing

instance HasElementEvents e msg (LabelAttributes e msg) where
  configureOnClicked = LabelOnClicked
  extractOnClicked (LabelOnClicked f) = Just f
  extractOnClicked _ = Nothing
  configureOnFocusGained = LabelOnFocusGained
  extractOnFocusGained (LabelOnFocusGained f) = Just f
  extractOnFocusGained _ = Nothing
  configureOnFocusLost = LabelOnFocusLost
  extractOnFocusLost (LabelOnFocusLost f) = Just f
  extractOnFocusLost _ = Nothing
  configureOnMouseEntered = LabelOnMouseEntered
  extractOnMouseEntered (LabelOnMouseEntered f) = Just f
  extractOnMouseEntered _ = Nothing
  configureOnMouseExited = LabelOnMouseExited
  extractOnMouseExited (LabelOnMouseExited f) = Just f
  extractOnMouseExited _ = Nothing
  configureOnMouseDown = LabelOnMouseDown
  extractOnMouseDown (LabelOnMouseDown f) = Just f
  extractOnMouseDown _ = Nothing
  configureOnMouseUp = LabelOnMouseUp
  extractOnMouseUp (LabelOnMouseUp f) = Just f
  extractOnMouseUp _ = Nothing
  configureOnKeyPressed = LabelOnKeyPressed
  extractOnKeyPressed (LabelOnKeyPressed f) = Just f
  extractOnKeyPressed _ = Nothing

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
