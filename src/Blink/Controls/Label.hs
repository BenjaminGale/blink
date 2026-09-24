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
  , mnemonic
  , renderLabelledContent
  , captionElement

    -- * Label
  , LabelConfig (..)
  , defaultLabelConfig
  , labelStyleKey
  , label
  , target
    -- * Style
  , labelStyle
  , defaultStyleEntries
  ) where

import Control.Monad (forM_, when)
import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft), Rectangle (..), Size (..), uniform)
import Blink.Input (InputState (inputAltHeld))
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.Rendering (TextAlign (..))
import Blink.View (View, charOffset, currentStyle, getBounds, getCurrentScope, getInput, measureText, withBounds)
import Blink.View.Drawing (drawText, fillRect)
import Blink.Element (Element (..), HasLayoutConfig (..))
import Blink.Style
import Blink.Controls.Style (transparent)

-- * Caption fragment

-- | A displayed caption, with an optional mnemonic letter (see 'mnemonic').
data LabelledConfig e msg = LabelledConfig
  { lcText     :: Text
  , lcMnemonic :: Maybe Char
  }

-- | @\"\"@, no mnemonic.
defaultLabelledConfig :: LabelledConfig e msg
defaultLabelledConfig = LabelledConfig { lcText = "", lcMnemonic = Nothing }

-- | Implemented by any config type that nests a 'LabelledConfig', letting
-- 'text' be applied to it directly.
class HasLabelledConfig e msg cfg | cfg -> e msg where
  overLabelled :: Attribute (LabelledConfig e msg) -> Attribute cfg

instance HasLabelledConfig e msg (LabelledConfig e msg) where
  overLabelled = id

-- | Sets the text displayed. Defaults to @\"\"@ when not given.
text :: HasLabelledConfig e msg cfg => Text -> Attribute cfg
text t = overLabelled (Attribute (\lc -> lc { lcText = t }))

-- | Marks a letter of the caption as its mnemonic: 'renderLabelledContent'
-- underlines the caption's first occurrence of it (matched
-- case-insensitively), and "Blink.Controls.MenuBar"\/"Blink.Controls.Menu"
-- use it together with 'Blink.Input.mnemonicActivated' to let Alt+letter
-- open a top-level menu or activate an item. Unset by default, in which
-- case no letter is underlined and Alt+letter does nothing for this
-- caption.
mnemonic :: HasLabelledConfig e msg cfg => Char -> Attribute cfg
mnemonic c = overLabelled (Attribute (\lc -> lc { lcMnemonic = Just c }))

-- | Draws @cfg@'s text into the current bounds, in the resolved style's
-- text colour and alignment, underlining its mnemonic letter (if any --
-- see 'mnemonic') while Alt is held, the same convention native menus use
-- so a caption's mnemonic doesn't permanently clutter it.
renderLabelledContent :: LabelledConfig e msg -> View e msg ()
renderLabelledContent cfg = do
  s       <- currentStyle
  altHeld <- inputAltHeld <$> getInput
  drawText (styleTextColour s) (styleTextAlign s) (lcText cfg)
  when altHeld $ forM_ (lcMnemonic cfg) (drawMnemonicUnderline s (lcText cfg))

-- | Underlines @t@'s first occurrence of @c@ (matched case-insensitively)
-- with a thin rule just under the text, positioned by 'charOffset' and
-- shifted to match how the backend itself places @t@ under @styleTextAlign@
-- horizontally and centres it vertically within the current bounds -- see
-- @alignedTextRect@ in @app/Rendering.hs@, whose formula this mirrors on
-- both axes so the rule lands under the drawn glyph itself rather than the
-- bottom of @bounds@, which is usually taller than the glyph (e.g. a
-- 'Blink.Controls.MenuBar.menuBar' label filling the whole bar's height).
drawMnemonicUnderline :: Style -> Text -> Char -> View e msg ()
drawMnemonicUnderline s t c = forM_ (T.findIndex ((== toUpper c) . toUpper) t) $ \i -> do
  bounds     <- getBounds
  targetSize <- measureText t
  loX        <- charOffset t i
  hiX        <- charOffset t (i + 1)
  let textTop = rectY bounds + (rectHeight bounds - sizeHeight targetSize) / 2
      startX  = case styleTextAlign s of
        AlignLeft   -> rectX bounds
        AlignCenter -> rectX bounds + (rectWidth bounds - sizeWidth targetSize) / 2
        AlignRight  -> rectX bounds + rectWidth bounds - sizeWidth targetSize
      -- Clamped so a glyph taller than bounds never pushes the rule past bounds' own clip.
      underlineY = min (textTop + sizeHeight targetSize) (rectY bounds + rectHeight bounds - 1)
      underlineRect = Rectangle
        { rectX      = startX + realToFrac loX
        , rectY      = underlineY
        , rectWidth  = realToFrac (hiX - loX)
        , rectHeight = 1
        }
  withBounds underlineRect (fillRect (styleTextColour s))

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

-- * Style

-- | The 'StyleKey' 'Blink.Controls.Label.label' resolves its style
-- from unless overridden via 'Blink.Controls.Control.style'.
labelStyleKey :: StyleKey e
labelStyleKey = Class "label"

labelMetrics :: Metrics
labelMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 6
  }

-- | A plain, transparent label style with no border.
labelStyle :: Palette -> StyleSet
labelStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorder       = noBorder
      }
  , styleOverrides = Map.singleton CommonDisabled (\s -> s { styleTextColour = paletteTextMuted p })
  }

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (labelStyleKey, (labelMetrics, labelStyle p)) ]
