{-# LANGUAGE OverloadedStrings #-}
module UI (demoView) where

import Blink

demoView :: UI ()
demoView = do
  let r = Rectangle 100 100 200 150
  layout r $ do
    _ <- button "Click me"
    return ()
