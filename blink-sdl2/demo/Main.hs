{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink (FontWeight (..))
import Blink.SDL2 (Config (..), FontFile (..), runApp)
import Data.List.NonEmpty (NonEmpty (..))
import UI (demoApp)

main :: IO ()
main = runApp Config
  { windowTitle = "blink"
  , fontFiles   = FontFile "Inter" Regular "assets/fonts/Inter-Regular.ttf" :| []
  } demoApp
