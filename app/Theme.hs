module Theme
  ( Element (..)
  , lightTheme
  , darkTheme
  ) where

import Blink
import qualified Data.Map.Strict as Map

data Element = Btn Int | TextInput1
             | CheckboxBox1 | CheckboxBox2 | CheckboxBox3
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
  { styleSetNormal   = base { styleBackground = palSurfaceButton p,         styleTextColour = palTextPrimary p,  styleBorderColour = Just (palBorderDefault p) }
  , styleSetHovered  = base { styleBackground = palSurfaceButtonHover p,    styleTextColour = palTextPrimary p,  styleBorderColour = Just (palBorderHover p) }
  , styleSetPressed  = base { styleBackground = palAccent p,                styleTextColour = palTextOnAccent p, styleBorderColour = Just (palAccentDark p) }
  , styleSetFocused  = base { styleBackground = palAccentLight p,           styleTextColour = palTextPrimary p,  styleBorderColour = Just (palAccent p) }
  , styleSetDisabled = base { styleBackground = palSurfaceButtonDisabled p, styleTextColour = palTextMuted p }
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 1
      , styleTextColour   = RGBA 0 0 0 1
      , styleTextAlign    = AlignCenter
      , styleMargin       = controlMargin
      , stylePadding      = controlPadding
      , styleBorderColour = Nothing
      , styleBorderWidth  = 0
      }

mkTextInputStyle :: Palette -> StyleSet
mkTextInputStyle p = StyleSet
  { styleSetNormal   = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palBorderDefault p) }
  , styleSetHovered  = base { styleBackground = palSurfaceInputHover p,    styleBorderColour = Just (palBorderHover p) }
  , styleSetPressed  = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palAccent p) }
  , styleSetFocused  = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palAccent p) }
  , styleSetDisabled = base { styleBackground = palSurfaceInputDisabled p, styleTextColour   = palTextMuted p }
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 1
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleMargin       = controlMargin
      , stylePadding      = controlPadding
      , styleBorderColour = Nothing
      , styleBorderWidth  = 1
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
      , styleBorderWidth  = 0
      }

mkCheckboxBoxStyle :: Palette -> StyleSet
mkCheckboxBoxStyle p = StyleSet
  { styleSetNormal   = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palBorderDefault p) }
  , styleSetHovered  = base { styleBackground = palSurfaceInputHover p,    styleBorderColour = Just (palBorderHover p) }
  , styleSetPressed  = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palAccent p) }
  , styleSetFocused  = base { styleBackground = palSurfaceInput p,         styleBorderColour = Just (palBorderDefault p) }
  , styleSetDisabled = base { styleBackground = palSurfaceInputDisabled p, styleTextColour   = palTextMuted p }
  }
  where
    base = Style
      { styleBackground   = RGBA 0 0 0 1
      , styleTextColour   = palTextPrimary p
      , styleTextAlign    = AlignCenter
      , styleMargin       = controlMargin
      , stylePadding      = uniform 2
      , styleBorderColour = Nothing
      , styleBorderWidth  = 1
      }

mkTheme :: Palette -> Theme Element
mkTheme p = Theme
  { themeElementStyles = Map.fromList
      [ (ProgressBar1,   mkProgressBarStyle p)
      , (TextInput1,     mkTextInputStyle p)
      , (CheckboxBox1,   mkCheckboxBoxStyle p)
      , (CheckboxBox2,   mkCheckboxBoxStyle p)
      , (CheckboxBox3,   mkCheckboxBoxStyle p)

      ]
  , themeDefaultStyle = mkBtnStyle p
  }

lightTheme :: Theme Element
lightTheme = mkTheme lightPalette

darkTheme :: Theme Element
darkTheme = mkTheme darkPalette
