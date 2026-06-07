module UI (demoView) where

import Blink

demoView :: UI ()
demoView = do
  mouse <- getMousePos
  button <- getLeftButton
  let r = Rectangle 100 100 200 150
      colour
        | containsPoint r mouse && button == ButtonDown = RGBA 0.7 0.2 0.1 1
        | containsPoint r mouse = RGBA 1 0.4 0.2 1
        | otherwise = RGBA 0.4 0.4 0.4 1
  layout r $ fillRect colour
