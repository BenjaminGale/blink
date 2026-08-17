-- | The sample app's theme.
--
-- = Style contract
--
-- Every control here falls into one of two families, and every state
-- transition follows the same rule within its family:
--
--   * __Bordered-box family__ (buttons, text inputs, the checkbox glyph,
--     scroll\/slider tracks, the checkbox\/radio row): 'styleBorderColour'
--     is @Just@ in /every/ state, never 'Nothing' — see 'invisibleBorder'.
--     States that shouldn't show a border use it instead of dropping the
--     border outright. Hover\/press\/focus are then communicated purely by
--     stepping the border colour (@invisible -> borderDefault ->
--     borderHover -> accent@ depending on the control), never by adding or
--     removing it. This is what keeps a control's measured chrome — and
--     therefore its on-screen size — identical across states; toggling
--     'styleBorderColour' to 'Nothing' changes the insets 'measureChrome'
--     \/ 'renderChrome' report for that state alone, which is what causes a
--     control to visibly resize (\"jump\") the instant it's hovered,
--     focused, or disabled. Buttons are the one control whose *background*
--     also flips to a bold accent fill on press — they are a primary
--     action surface, so the pressed state should be unmissable. Other
--     bordered controls keep their normal background and vary only the
--     border, since a full fill would fight with the glyph\/text drawn on
--     top of them. The checkbox glyph is square and gets its own bordered
--     box; the radio glyph deliberately does not (radio buttons are round,
--     not square), so its row ('mkRadioRowStyle') tints its *background*
--     on hover\/press instead, to still give some feedback.
--   * __Flat-fill family__ (scroll\/slider thumbs, progress bars, labels,
--     the checkbox\/radio glyph's own label and mark text): border edges
--     are 'noBorder' (zero width) in every state, so there's no inset to
--     vary in the first place. Feedback, where there is any, comes
--     entirely from background colour.
--
-- Hover always uses the @*Hover@ palette entries; anything else is a
-- deviation from the contract above and should be treated as a bug.
module Theme
  ( Element (..)
  , lightTheme
  , darkTheme
  ) where

import Blink.Controls
import Blink.Geometry
import Blink.Rendering
import Blink.Style
import qualified Data.Map.Strict as Map

data Element = Label
             | StatusBar
             | Btn Int
             | NavBtn Int
             | TextInput1
             | NumberInput1
             | PasswordInput1
             | CheckboxN Int CheckboxPart
             | ProgressBar1 | ProgressBar2
             | Slider1 SliderPart
             | ScrollRegion1 ViewportPart
             | ScrollItem1 Int
             | RadioSize RadioGroupPart
             | RadioSizeNoTab RadioGroupPart
  deriving (Eq, Ord)

data Palette = Palette
  { palAccent :: Colour
  , palAccentDark :: Colour
  , palAccentLight :: Colour
  , palTextPrimary :: Colour
  , palTextOnAccent :: Colour
  , palTextMuted :: Colour
  , palBorderDefault :: Colour
  , palBorderHover :: Colour
  , palSurfaceButton :: Colour
  , palSurfaceButtonHover :: Colour
  , palSurfaceButtonDisabled :: Colour
  , palSurfaceInput :: Colour
  , palSurfaceInputHover :: Colour
  , palSurfaceInputDisabled :: Colour
  , palProgressTrack :: Colour
  }

lightPalette :: Palette
lightPalette = Palette
  { palAccent                = RGBA 0.102 0.435 0.831 1
  , palAccentDark            = RGBA 0.071 0.306 0.584 1
  , palAccentLight           = RGBA 0.667 0.769 0.941 1
  , palTextPrimary           = RGBA 0.11  0.11  0.12  1
  , palTextOnAccent          = RGBA 1.0   1.0   1.0   1
  , palTextMuted             = RGBA 0.682 0.682 0.698 1
  , palBorderDefault         = RGBA 0.600 0.600 0.620 1
  , palBorderHover           = RGBA 0.400 0.400 0.420 1
  , palSurfaceButton         = RGBA 0.878 0.878 0.898 1
  , palSurfaceButtonHover    = RGBA 0.800 0.800 0.824 1
  , palSurfaceButtonDisabled = RGBA 0.898 0.898 0.910 1
  , palSurfaceInput          = RGBA 1.0   1.0   1.0   1
  , palSurfaceInputHover     = RGBA 0.97  0.97  0.97  1
  , palSurfaceInputDisabled  = RGBA 0.95  0.95  0.95  1
  , palProgressTrack         = RGBA 0.878 0.878 0.898 1
  }

darkPalette :: Palette
darkPalette = Palette
  { palAccent                = RGBA 0.055 0.647 0.914 1  -- sky-500
  , palAccentDark            = RGBA 0.012 0.412 0.631 1  -- sky-700
  , palAccentLight           = RGBA 0.027 0.349 0.522 1  -- sky-800, focused button bg
  , palTextPrimary           = RGBA 0.945 0.961 0.976 1  -- slate-100
  , palTextOnAccent          = RGBA 1.0   1.0   1.0   1
  , palTextMuted             = RGBA 0.580 0.639 0.722 1  -- slate-400
  , palBorderDefault         = RGBA 0.278 0.333 0.412 1  -- slate-600
  , palBorderHover           = RGBA 0.580 0.639 0.722 1  -- slate-400
  , palSurfaceButton         = RGBA 0.200 0.255 0.333 1  -- slate-700
  , palSurfaceButtonHover    = RGBA 0.278 0.333 0.412 1  -- slate-600
  , palSurfaceButtonDisabled = RGBA 0.118 0.161 0.231 1  -- slate-800
  , palSurfaceInput          = RGBA 0.118 0.161 0.231 1  -- slate-800
  , palSurfaceInputHover     = RGBA 0.200 0.255 0.333 1  -- slate-700
  , palSurfaceInputDisabled  = RGBA 0.059 0.090 0.165 1  -- slate-900
  , palProgressTrack         = RGBA 0.200 0.255 0.333 1  -- slate-700
  }

controlMargin :: Insets
controlMargin = uniform 3

controlPadding :: Insets
controlPadding = uniform 6

-- | An invisible border colour (zero alpha). 'measureChrome' \/
-- 'renderChrome' only skip a border's *space* when 'styleBorderColour' is
-- 'Nothing' — a state that sets it to 'Nothing' while its siblings set it
-- to @Just@ reserves *less* space than they do, so a control resizes the
-- instant it enters that state (e.g. gaining focus, or going disabled).
-- Using this colour instead keeps the border's footprint constant across
-- every state; only its visibility changes, since 'Blink.UI.withBorder'
-- skips the actual stroke for a fully transparent colour (mirroring
-- 'Blink.UI.withBackground').
invisibleBorder :: Colour
invisibleBorder = RGBA 0 0 0 0

mkBtnStyle :: Palette -> StyleSet
mkBtnStyle p = StyleSet
  { styleSetNormal   = base { styleBackground = palSurfaceButton p,         styleTextColour = palTextPrimary p,  styleBorderColour = Just (palBorderDefault p) }
  , styleSetHovered  = base { styleBackground = palSurfaceButtonHover p,    styleTextColour = palTextPrimary p,  styleBorderColour = Just (palBorderHover p) }
  , styleSetPressed  = base { styleBackground = palAccent p,                styleTextColour = palTextOnAccent p, styleBorderColour = Just (palAccentDark p) }
  , styleSetFocused  = base { styleBackground = palAccentLight p,           styleTextColour = palTextPrimary p,  styleBorderColour = Just (palAccent p) }
  , styleSetDisabled = base { styleBackground = palSurfaceButtonDisabled p, styleTextColour = palTextMuted p,    styleBorderColour = Just (palBorderDefault p) }
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 1
      , styleTextColour   = RGBA 0 0 0 1
      , styleTextAlign    = AlignCenter
      , styleMargin       = controlMargin
      , stylePadding      = controlPadding
      , styleBorderColour = Just invisibleBorder
      , styleBorderEdges  = uniformBorder 1
      }

mkTextInputStyle :: Palette -> StyleSet
mkTextInputStyle p = StyleSet
  { styleSetNormal   = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palBorderDefault p) }
  , styleSetHovered  = base { styleBackground = palSurfaceInputHover p,    styleBorderColour = Just (palBorderHover p) }
  , styleSetPressed  = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palAccent p) }
  , styleSetFocused  = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palAccent p) }
  , styleSetDisabled = base { styleBackground = palSurfaceInputDisabled p, styleTextColour   = palTextMuted p, styleBorderColour = Just (palBorderDefault p) }
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 1
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleMargin       = controlMargin
      , stylePadding      = controlPadding
      , styleBorderColour = Just invisibleBorder
      , styleBorderEdges  = uniformBorder 1
      }

mkProgressBarStyle :: Palette -> StyleSet
mkProgressBarStyle p = StyleSet
  { styleSetNormal   = base
  , styleSetHovered  = base
  , styleSetPressed  = base
  , styleSetFocused  = base
  , styleSetDisabled = base { styleTextColour = palTextMuted p }
  }
  where
    base = Style
      { styleBackground   = palProgressTrack p
      , styleTextColour   = palAccent p
      , styleTextAlign    = AlignLeft
      , styleMargin       = controlMargin
      , stylePadding      = uniform 0
      , styleBorderColour = Nothing
      , styleBorderEdges  = noBorder
      }

-- | For a checkbox's glyph sub-part: the visible mark box (background and
-- border), filling its 20x20 slot with minimal inset so the checkmark
-- isn't squeezed. Checkboxes are conventionally square, so this is the one
-- glyph that gets its own bordered box — 'RadioGlyph' deliberately does
-- not (see 'mkCheckboxSubPartStyle'\/'mkRadioRowStyle').
--
-- Focused stays identical to normal: focus belongs to the outer row (see
-- 'mkCheckboxRowStyle'), and re-drawing it here too would double the ring.
mkCheckboxGlyphStyle :: Palette -> StyleSet
mkCheckboxGlyphStyle p = StyleSet
  { styleSetNormal   = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palBorderDefault p) }
  , styleSetHovered  = base { styleBackground = palSurfaceInputHover p,    styleBorderColour = Just (palBorderHover p) }
  , styleSetPressed  = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palAccent p) }
  , styleSetFocused  = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palBorderDefault p) }
  , styleSetDisabled = base { styleBackground = palSurfaceInputDisabled p, styleTextColour   = palTextMuted p, styleBorderColour = Just (palBorderDefault p) }
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 1
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignCenter
      , styleMargin       = uniform 0
      , stylePadding      = uniform 2
      , styleBorderColour = Just invisibleBorder
      , styleBorderEdges  = uniformBorder 1
      }

-- | For a checkbox's outer row (the whole clickable area spanning the
-- glyph and label together) and its label sub-part: invisible in every
-- state, so the glyph's own box\/border ('mkCheckboxGlyphStyle') is the
-- only permanent chrome visible — matching the pre-migration checkbox,
-- where only the mark itself was ever a bordered box, never the label or
-- the row as a whole. The row still gets a focus ring border, since
-- keyboard focus belongs to the row (the whole thing is one activation
-- target), not to the glyph alone.
--
-- The border is present (as 'invisibleBorder') in every state, not just
-- Focused, and padding leaves it a couple of pixels clear of the glyph\/
-- label: reserving the same chrome in every state means the row never
-- resizes when it gains or loses focus, and the couple of pixels of
-- padding keep the ring from touching the glyph's own border when it
-- does appear.
mkCheckboxRowStyle :: Palette -> StyleSet
mkCheckboxRowStyle p = StyleSet
  { styleSetNormal   = base
  , styleSetHovered  = base
  , styleSetPressed  = base
  , styleSetFocused  = base { styleBorderColour = Just (palAccent p) }
  , styleSetDisabled = base
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 0
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleMargin       = uniform 0
      , stylePadding      = uniform 2
      , styleBorderColour = Just invisibleBorder
      , styleBorderEdges  = uniformBorder 1
      }

-- | For a radio item's outer row: tints its background on hover\/press,
-- unlike 'mkCheckboxRowStyle'. A checkbox gets that feedback from its own
-- glyph box ('mkCheckboxGlyphStyle'); a radio glyph deliberately has no box
-- of its own (radio buttons are round, not square — see
-- 'mkCheckboxSubPartStyle'), so without this the row would give no hover\/
-- press feedback at all.
--
-- Unlike 'mkCheckboxRowStyle', this does /not/ reserve constant border\/
-- padding chrome: @rowRadioGroup@\/@rowRadioGroupNoTab@ in @app\/UI.hs@
-- give each radio row a hardcoded @Exactly 30@ height rather than computing
-- it from 'measureChrome' the way @rowCheckboxes@ does for the checkbox
-- row. Any nonzero chrome reserved here would eat straight into that fixed,
-- chrome-unaware budget and clip the label text — the background tint
-- costs nothing here since backgrounds don't add insets, but the border
-- must stay 'Nothing' except when focused.
mkRadioRowStyle :: Palette -> StyleSet
mkRadioRowStyle p = StyleSet
  { styleSetNormal   = base
  , styleSetHovered  = base { styleBackground = palSurfaceButtonHover p }
  , styleSetPressed  = base { styleBackground = palAccentLight p }
  , styleSetFocused  = base { styleBorderColour = Just (palAccent p) }
  , styleSetDisabled = base
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 0
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleMargin       = uniform 0
      , stylePadding      = uniform 0
      , styleBorderColour = Nothing
      , styleBorderEdges  = uniformBorder 1
      }

-- | For a checkbox's or radio button's label sub-part only: invisible in
-- every state (text colour aside), since the label never carries its own
-- chrome — see the style contract at the top of this module.
mkCheckboxSubPartStyle :: Palette -> StyleSet
mkCheckboxSubPartStyle p = StyleSet
  { styleSetNormal   = base
  , styleSetHovered  = base
  , styleSetPressed  = base
  , styleSetFocused  = base
  , styleSetDisabled = base { styleTextColour = palTextMuted p }
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 0
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleMargin       = uniform 0
      , stylePadding      = uniform 0
      , styleBorderColour = Nothing
      , styleBorderEdges  = noBorder
      }

mkLabelStyle :: Palette -> StyleSet
mkLabelStyle p = StyleSet
  { styleSetNormal   = base
  , styleSetHovered  = base
  , styleSetPressed  = base
  , styleSetFocused  = base
  , styleSetDisabled = base { styleTextColour = palTextMuted p }
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 0
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleMargin       = uniform 0
      , stylePadding      = controlPadding
      , styleBorderColour = Nothing
      , styleBorderEdges  = noBorder
      }

-- | The border is present (as 'invisibleBorder') in every state, not just
-- Focused, so the track doesn't resize when it gains\/loses focus — see
-- 'invisibleBorder'.
mkScrollTrackStyle :: Palette -> StyleSet
mkScrollTrackStyle p = StyleSet
  { styleSetNormal   = base
  , styleSetHovered  = base
  , styleSetPressed  = base
  , styleSetFocused  = base { styleBorderColour = Just (palAccent p) }
  , styleSetDisabled = base { styleBackground = palSurfaceButtonDisabled p }
  }
  where
    base = Style
      { styleBackground   = palProgressTrack p
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignCenter
      , styleMargin       = uniform 0
      , stylePadding      = uniform 2
      , styleBorderColour = Just invisibleBorder
      , styleBorderEdges  = uniformBorder 1
      }

mkScrollThumbStyle :: Palette -> StyleSet
mkScrollThumbStyle p = StyleSet
  { styleSetNormal   = base { styleBackground = palAccent p }
  , styleSetHovered  = base { styleBackground = palAccentDark p }
  , styleSetPressed  = base { styleBackground = palAccentDark p }
  , styleSetFocused  = base { styleBackground = palAccent p }
  , styleSetDisabled = base { styleBackground = palTextMuted p }
  }
  where
    base = Style
      { styleBackground   = palAccent p
      , styleTextColour   = palTextOnAccent p
      , styleTextAlign    = AlignCenter
      , styleMargin       = uniform 0
      , stylePadding      = uniform 0
      , styleBorderColour = Nothing
      , styleBorderEdges  = noBorder
      }

mkStatusBarStyle :: Palette -> StyleSet
mkStatusBarStyle p = StyleSet
  { styleSetNormal   = base
  , styleSetHovered  = base
  , styleSetPressed  = base
  , styleSetFocused  = base
  , styleSetDisabled = base
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 0
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleMargin       = uniform 0
      , stylePadding      = uniform 0
      , styleBorderColour = Just (palBorderDefault p)
      , styleBorderEdges  = BorderEdges { edgeTop = 1, edgeRight = 0, edgeBottom = 0, edgeLeft = 0 }
      }

mkTheme :: Palette -> Theme Element
mkTheme p = Theme
  { themeElementStyles = Map.fromList $
      [ (Label,                     mkLabelStyle p)
      , (StatusBar,                 mkStatusBarStyle p)
      , (ProgressBar1,              mkProgressBarStyle p)
      , (ProgressBar2,              mkProgressBarStyle p)
      , (TextInput1,                mkTextInputStyle p)
      , (NumberInput1,              mkTextInputStyle p)
      , (PasswordInput1,            mkTextInputStyle p)
      , (Slider1 SliderTrack,       mkScrollTrackStyle p)
      , (Slider1 SliderThumb,       mkScrollThumbStyle p)
      ] ++ [ (ScrollRegion1 (ViewportH ScrollTrack), mkScrollTrackStyle p)
           , (ScrollRegion1 (ViewportH ScrollThumb), mkScrollThumbStyle p)
           , (ScrollRegion1 (ViewportV ScrollTrack), mkScrollTrackStyle p)
           , (ScrollRegion1 (ViewportV ScrollThumb), mkScrollThumbStyle p)
           ]
        ++ [ style
           | i <- [1 .. 5]
           , style <- [ (CheckboxN i CheckboxBox,   mkCheckboxRowStyle p)
                      , (CheckboxN i CheckboxGlyph, mkCheckboxGlyphStyle p)
                      , (CheckboxN i CheckboxLabel, mkCheckboxSubPartStyle p)
                      ]
           ]
        ++ [ (RadioSize RadioGroup, mkRadioRowStyle p), (RadioSizeNoTab RadioGroup, mkRadioRowStyle p) ]
        ++ [ style
           | i <- [0 .. 2]
           , mk <- [RadioSize, RadioSizeNoTab]
           , style <- [ (mk (RadioItem i RadioBox),   mkRadioRowStyle p)
                      , (mk (RadioItem i RadioGlyph), mkCheckboxSubPartStyle p)
                      , (mk (RadioItem i RadioLabel), mkCheckboxSubPartStyle p)
                      ]
           ]
  , themeDefaultStyle = mkBtnStyle p
  }

lightTheme :: Theme Element
lightTheme = mkTheme lightPalette

darkTheme :: Theme Element
darkTheme = mkTheme darkPalette
