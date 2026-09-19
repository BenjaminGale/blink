module Blink.StyleSpec (spec) where

import Test.Hspec

import Blink.Geometry
  ( BorderLayer (..)
  , Colour (..)
  , EdgeVisibility (..)
  , allEdgesVisible
  , uniformRadii
  )
import Blink.Style (soloBorder, withBorderColour)

red, blue :: Colour
red = RGBA 1 0 0 1
blue = RGBA 0 0 1 1

spec :: Spec
spec = describe "Blink.Style" $ do
  describe "soloBorder" $
    it "builds a single square, fully-visible layer at offset 0 with the given colour and width" $
      soloBorder red 3 `shouldBe`
        [ BorderLayer
            { layerColour = red
            , layerWidth = 3
            , layerOffset = 0
            , layerRadii = uniformRadii 0
            , layerVisible = allEdgesVisible
            }
        ]

  describe "withBorderColour" $ do
    it "recolours every layer, keeping each one's shape unchanged" $
      let base =
            [ BorderLayer red 2 0 (uniformRadii 4) allEdgesVisible
            , BorderLayer red 1 2 (uniformRadii 0) (allEdgesVisible { edgeBottomVisible = False })
            ]
      in withBorderColour blue base `shouldBe`
           [ BorderLayer blue 2 0 (uniformRadii 4) allEdgesVisible
           , BorderLayer blue 1 2 (uniformRadii 0) (allEdgesVisible { edgeBottomVisible = False })
           ]

    it "leaves an empty border empty" $
      withBorderColour blue [] `shouldBe` []
