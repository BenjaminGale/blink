{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A small reusable config fragment for anything that displays a caption
-- -- not a 'Blink.Controls.Core.elementBase'\/'Blink.Controls.Core.controlBase'-style
-- layer itself (nothing calls into it the way a @Base@ primitive is called
-- into), just a nested field plus a rendering helper, the same category as
-- 'Blink.Controls.Core.ElementConfig'\/'Blink.Controls.Core.ControlConfig'.
--
-- Anything that wants a caption nests a 'LabelledConfig' field, declares
-- 'HasLabelledConfig', and calls 'renderLabelledContent' itself when
-- building its own content.
module Blink.Controls.Labelled
  ( LabelledConfig (..)
  , HasLabelledConfig (..)
  , defaultLabelledConfig
  , text
  , renderLabelledContent
  ) where

import Data.Text (Text)

import Blink.Controls.Core (Attr (..))
import Blink.Style (Style (..))
import Blink.UI (UI, currentStyle, drawText)

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
