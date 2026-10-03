{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink.SDL2 (Config (..), runApp)
import UI (demoApp)

main :: IO ()
main = runApp Config
  { windowTitle = "blink"
  , fontPath    = "assets/fonts/Inter-Regular.ttf"
  , fontSize    = 14
  } demoApp
