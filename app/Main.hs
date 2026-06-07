{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink (backgroundColor)
import SDL
import Data.Word (Word8)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "blink" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer window
  destroyRenderer renderer
  destroyWindow window
  quit

appLoop :: Renderer -> Window -> IO ()
appLoop renderer window = do
  shouldQuit <- handleEvents
  if shouldQuit
    then return ()
    else do
      render renderer
      appLoop renderer window

handleEvents :: IO Bool
handleEvents = do
  any isQuit <$> pollEvents
  where
    isQuit e = eventPayload e == QuitEvent

render :: Renderer -> IO ()
render renderer = do
  let (r', g', b', a') = backgroundColor
      (r, g, b, a)     = (fromIntegral r', fromIntegral g', fromIntegral b', fromIntegral a')
  rendererDrawColor renderer $= V4 r g b (a :: Word8)
  clear renderer
  present renderer
