module Blink.Style.DefaultsSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import Blink.Controls.Button (buttonStyleKey)
import Blink.Controls.Checkbox (checkboxStyleKey)
import Blink.Controls.Divider (dividerStyleKey)
import Blink.Controls.Style (iconStyleKey)
import Blink.Controls.Image (imageStyleKey)
import Blink.Controls.Label (labelStyleKey)
import Blink.Controls.List.Style (listItemStyleKey, listStyleKey)
import Blink.Controls.MenuBar.Style (menuBarLabelStyleKey, menuBarListStyleKey, menuBarStyleKey)
import Blink.Controls.MenuButton.Style (menuButtonListStyleKey)
import Blink.Controls.ProgressBar (progressBarStyleKey)
import Blink.Controls.RadioButton (radioButtonStyleKey)
import Blink.Controls.ScrollBar (scrollBarButtonStyleKey, scrollBarStyleKey, scrollBarTrackStyleKey)
import Blink.Controls.Slider (sliderStyleKey)
import Blink.Controls.TextInput (textInputStyleKey)
import Blink.Controls.ToggleButton (toggleButtonStyleKey, toggleChecked)
import Blink.Controls.ToggleGroup (radioButtonGroupStyleKey, toggleButtonGroupStyleKey)
import Blink.Controls.Table.Style (tableColumnDividerStyleKey, tableHeaderStyleKey)
import Blink.Controls.Tree.Style (treeChevronStyleKey)
import Blink.Controls.Fixtures (noInput)
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (Colour (..))
import Blink.Style
import Blink.Style.Defaults (defaultTheme)
import Blink.View (emptyViewContext, getStyleSet, runView)

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
  , paletteIcon            = RGBA 0.8 0.8 0.8 1
  , paletteIconHover       = RGBA 0.7 0.7 1.0 1
  }

-- | The resolved 'Style' the real render path would produce for @key@ with
-- @active@ pseudo-states, driven through the actual 'getStyleSet' lookup
-- rather than a hand-duplicated fallback.
resolvedAt :: StyleKey () -> Set.Set VisualState -> IO Style
resolvedAt key active = do
  ((_, ss), _) <- runView (getStyleSet key) ctx
  pure (resolveStyle ss active)
  where
    ctx = emptyViewContext (Rectangle 0 0 100 100) noInput (defaultTheme testPalette :: Theme ())

spec :: Spec
spec = describe "Blink.Style.Defaults" $ do
  describe "defaultTheme" $ do
    it "registers every built-in control's default StyleKey" $ do
      Map.keys (themeElementStyles (defaultTheme testPalette :: Theme ()))
        `shouldMatchList`
          [ buttonStyleKey, toggleButtonStyleKey, checkboxStyleKey, radioButtonStyleKey, textInputStyleKey
          , progressBarStyleKey, sliderStyleKey, dividerStyleKey, imageStyleKey, labelStyleKey
          , toggleButtonGroupStyleKey, radioButtonGroupStyleKey
          , scrollBarStyleKey, scrollBarButtonStyleKey, scrollBarTrackStyleKey
          , listStyleKey, listItemStyleKey
          , treeChevronStyleKey
          , tableHeaderStyleKey, tableColumnDividerStyleKey
          , iconStyleKey
          , menuButtonListStyleKey
          , menuBarStyleKey, menuBarLabelStyleKey, menuBarListStyleKey
          ]

    it "gives a button the surface colour at rest" $ do
      style <- resolvedAt buttonStyleKey (Set.singleton CommonNormal)
      styleBackground style `shouldBe` paletteSurface testPalette

    it "gives a button the accent fill while pressed" $ do
      style <- resolvedAt buttonStyleKey (Set.singleton CommonPressed)
      styleBackground style `shouldBe` paletteAccent testPalette

    it "gives a button the focus ring border while focused" $ do
      style <- resolvedAt buttonStyleKey (Set.fromList [CommonNormal, FocusFocused])
      styleBorderColour style `shouldBe` Just (paletteFocusRing testPalette)

    it "gives a toggle button the accent fill while checked" $ do
      style <- resolvedAt toggleButtonStyleKey (Set.fromList [CommonNormal, toggleChecked])
      styleBackground style `shouldBe` paletteAccent testPalette

    it "keeps a checkbox transparent at rest, tinted on hover" $ do
      style <- resolvedAt checkboxStyleKey (Set.singleton CommonMouseOver)
      styleBackground style `shouldBe` paletteSurfaceHover testPalette

    it "mutes a disabled label's text" $ do
      style <- resolvedAt labelStyleKey (Set.singleton CommonDisabled)
      styleTextColour style `shouldBe` paletteTextMuted testPalette

    -- Every control below is focusable under its own default 'FocusPolicy'
    -- (see each control module's own 'Blink.Controls.Control.focusPolicy'
    -- for the ones deliberately excluded, e.g. a scrollbar's own buttons and
    -- track, which are 'Blink.Controls.Control.NotFocusable'). Missing an
    -- entry here means a focused instance draws no focus ring at all.
    -- 'sliderStyleKey' is excluded: a slider draws its focus ring directly
    -- in accent rather than through 'styleBorderColour' (see
    -- 'Blink.Controls.Slider.drawTrack'), so its resolved style never
    -- changes on focus even though it does draw a ring.
    describe "focus ring coverage" $ do
      let focusableStyleKeys =
            [ buttonStyleKey, toggleButtonStyleKey, checkboxStyleKey, radioButtonStyleKey
            , textInputStyleKey
            ]
      forM_ focusableStyleKeys $ \key ->
        it ("gives " <> show key <> " a distinct look while focused") $ do
          focused <- resolvedAt key (Set.singleton FocusFocused)
          normal  <- resolvedAt key Set.empty
          focused `shouldNotBe` normal

    -- 'toggleButtonStyleKey' is the only one of these with no glyph of its
    -- own to show checked/unchecked, so it's the only one whose 'StyleSet'
    -- needs a 'toggleChecked' entry -- a checkbox/radio button shows it via
    -- their own glyph instead (see 'Blink.Controls.Style.flatRowStyle').
    describe "toggle pseudo-state coverage" $ do
      let key = toggleButtonStyleKey :: StyleKey ()
      it ("gives " <> show key <> " a distinct look while checked") $ do
        checked <- resolvedAt key (Set.fromList [CommonNormal, toggleChecked])
        normal  <- resolvedAt key (Set.singleton CommonNormal)
        checked `shouldNotBe` normal
