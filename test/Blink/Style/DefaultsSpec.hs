module Blink.Style.DefaultsSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import Blink.Controls.Button (buttonStyleKey)
import Blink.Controls.Checkbox (checkboxStyleKey)
import Blink.Controls.Label (labelStyleKey)
import Blink.Controls.ProgressBar (progressBarStyleKey)
import Blink.Controls.RadioButton (radioButtonStyleKey)
import Blink.Controls.Slider (sliderStyleKey)
import Blink.Controls.TextInput (textInputStyleKey)
import Blink.Controls.Toggle (toggleButtonStyleKey, toggleChecked)
import Blink.Rendering (Colour (..))
import Blink.Style
import Blink.Style.Defaults (defaultTheme)

testPalette :: Palette
testPalette = Palette
  { paletteAccent          = RGBA 1 0 0 1
  , paletteFocusRing       = RGBA 0 1 0 1
  , paletteSurface         = RGBA 0.2 0.2 0.2 1
  , paletteSurfaceHover    = RGBA 0.3 0.3 0.3 1
  , paletteSurfaceDisabled = RGBA 0.1 0.1 0.1 1
  , paletteTextPrimary     = RGBA 0.9 0.9 0.9 1
  , paletteTextMuted       = RGBA 0.5 0.5 0.5 1
  , paletteTextOnAccent    = RGBA 1 1 1 1
  , paletteBorder          = RGBA 0.4 0.4 0.4 1
  , paletteBorderHover     = RGBA 0.6 0.6 0.6 1
  }

resolvedAt :: StyleKey () -> Set.Set VisualState -> Style
resolvedAt key active =
  case Map.lookup key (themeElementStyles thm) of
    Just (_, ss) -> resolveStyle ss active
    Nothing      -> resolveStyle (snd (themeDefaultStyle thm)) active
  where
    thm = defaultTheme testPalette :: Theme ()

spec :: Spec
spec = describe "Blink.Style.Defaults" $ do
  describe "defaultTheme" $ do
    it "registers every built-in control's default StyleKey" $ do
      Map.keys (themeElementStyles (defaultTheme testPalette :: Theme ()))
        `shouldMatchList`
          [buttonStyleKey, toggleButtonStyleKey, checkboxStyleKey, radioButtonStyleKey, textInputStyleKey, progressBarStyleKey, sliderStyleKey, labelStyleKey]

    it "gives a button the surface colour at rest" $ do
      styleBackground (resolvedAt buttonStyleKey (Set.singleton CommonNormal)) `shouldBe` paletteSurface testPalette

    it "gives a button the accent fill while pressed" $ do
      styleBackground (resolvedAt buttonStyleKey (Set.singleton CommonPressed)) `shouldBe` paletteAccent testPalette

    it "gives a button the focus ring border while focused" $ do
      styleBorderColour (resolvedAt buttonStyleKey (Set.fromList [CommonNormal, FocusFocused]))
        `shouldBe` Just (paletteFocusRing testPalette)

    it "gives a toggle button the accent fill while checked" $ do
      styleBackground (resolvedAt toggleButtonStyleKey (Set.fromList [CommonNormal, toggleChecked]))
        `shouldBe` paletteAccent testPalette

    it "keeps a checkbox transparent at rest, tinted on hover" $ do
      styleBackground (resolvedAt checkboxStyleKey (Set.singleton CommonMouseOver))
        `shouldBe` paletteSurfaceHover testPalette

    it "mutes a disabled label's text" $ do
      styleTextColour (resolvedAt labelStyleKey (Set.singleton CommonDisabled)) `shouldBe` paletteTextMuted testPalette
