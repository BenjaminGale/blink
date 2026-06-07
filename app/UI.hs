{-# LANGUAGE OverloadedStrings #-}
module UI (demoView) where

import Blink

data Element = MyButton
  deriving (Eq, Ord)

demoView :: UI Element ()
demoView = do
  let r = Rectangle 100 100 200 150
  layout r $ do
    _ <- button MyButton "Click me"
    return ()
