module Theme
  ( Element (..)
  , lightTheme
  , darkTheme
  ) where

import Blink
import qualified Data.Map.Strict as Map

data Element = Btn Int | TextInput1
             | CheckboxBox1 | CheckboxLabel1
             | CheckboxBox2 | CheckboxLabel2
             | CheckboxBox3 | CheckboxLabel3
             | ProgressBar1
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

mkBtnStyle :: Palette -> StyleSet
mkBtnStyle p = StyleSet
  { normal   = base { background = palSurfaceButton p,         textColour = palTextPrimary p,  borderColour = Just (palBorderDefault p) }
  , hovered  = base { background = palSurfaceButtonHover p,    textColour = palTextPrimary p,  borderColour = Just (palBorderHover p) }
  , pressed  = base { background = palAccent p,                textColour = palTextOnAccent p, borderColour = Just (palAccentDark p) }
  , focused  = base { background = palAccentLight p,           textColour = palTextPrimary p,  borderColour = Just (palAccent p) }
  , disabled = base { background = palSurfaceButtonDisabled p, textColour = palTextMuted p }
  }
  where
    base = Style
      { background   = RGBA 0 0 0 1
      , textColour   = RGBA 0 0 0 1
      , textAlign    = AlignCenter
      , margin       = controlMargin
      , padding      = controlPadding
      , borderColour = Nothing
      , borderWidth  = 0
      }

mkTextInputStyle :: Palette -> StyleSet
mkTextInputStyle p = StyleSet
  { normal   = base { background = palSurfaceInput p,         borderColour = Just (palBorderDefault p) }
  , hovered  = base { background = palSurfaceInputHover p,    borderColour = Just (palBorderHover p) }
  , pressed  = base { background = palSurfaceInput p,         borderColour = Just (palAccent p) }
  , focused  = base { background = palSurfaceInput p,         borderColour = Just (palAccent p) }
  , disabled = base { background = palSurfaceInputDisabled p, textColour   = palTextMuted p }
  }
  where
    base = Style
      { background   = RGBA 0 0 0 1
      , textColour   = palTextPrimary p
      , textAlign    = AlignLeft
      , margin       = controlMargin
      , padding      = controlPadding
      , borderColour = Nothing
      , borderWidth  = 1
      }

mkLabelStyle :: Palette -> StyleSet
mkLabelStyle p = StyleSet
  { normal   = base
  , hovered  = base
  , pressed  = base
  , focused  = base
  , disabled = base { textColour = palTextMuted p }
  }
  where
    base = Style
      { background   = RGBA 0 0 0 0
      , textColour   = palTextPrimary p
      , textAlign    = AlignLeft
      , margin       = uniform 0
      , padding      = uniform 0
      , borderColour = Nothing
      , borderWidth  = 0
      }

mkProgressBarStyle :: Palette -> StyleSet
mkProgressBarStyle p = StyleSet
  { normal   = base
  , hovered  = base
  , pressed  = base
  , focused  = base
  , disabled = base { textColour = palTextMuted p }
  }
  where
    base = Style
      { background   = palProgressTrack p
      , textColour   = palAccent p
      , textAlign    = AlignLeft
      , margin       = controlMargin
      , padding      = uniform 0
      , borderColour = Nothing
      , borderWidth  = 0
      }

mkCheckboxBoxStyle :: Palette -> StyleSet
mkCheckboxBoxStyle p = StyleSet
  { normal   = base { background = palSurfaceInput p,         borderColour = Just (palBorderDefault p) }
  , hovered  = base { background = palSurfaceInputHover p,    borderColour = Just (palBorderHover p) }
  , pressed  = base { background = palSurfaceInput p,         borderColour = Just (palAccent p) }
  , focused  = base { background = palSurfaceInput p,         borderColour = Just (palBorderDefault p) }
  , disabled = base { background = palSurfaceInputDisabled p, textColour   = palTextMuted p }
  }
  where
    base = Style
      { background   = RGBA 0 0 0 1
      , textColour   = palTextPrimary p
      , textAlign    = AlignCenter
      , margin       = controlMargin
      , padding      = uniform 2
      , borderColour = Nothing
      , borderWidth  = 1
      }

mkTheme :: Palette -> Theme Element
mkTheme p = Theme
  { elementStyles = Map.fromList
      [ (ProgressBar1,   mkProgressBarStyle p)
      , (TextInput1,     mkTextInputStyle p)
      , (CheckboxBox1,   mkCheckboxBoxStyle p)
      , (CheckboxBox2,   mkCheckboxBoxStyle p)
      , (CheckboxBox3,   mkCheckboxBoxStyle p)
      , (CheckboxLabel1, mkLabelStyle p)
      , (CheckboxLabel2, mkLabelStyle p)
      , (CheckboxLabel3, mkLabelStyle p)
      ]
  , defaultStyle = mkBtnStyle p
  }

lightTheme :: Theme Element
lightTheme = mkTheme lightPalette

darkTheme :: Theme Element
darkTheme = mkTheme darkPalette
