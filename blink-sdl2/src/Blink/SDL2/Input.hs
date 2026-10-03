-- | Translates SDL events into the pieces of a 'FrameInput'.
module Blink.SDL2.Input
  ( updateButton
  , toKeyEvents
  , toTypedText
  , toWheelDelta
  , sdlPoint
  ) where

import Blink
import qualified SDL
import Data.Char (chr, toUpper)
import Data.Text (Text)
import Foreign.C.Types (CInt)

updateButton :: Bool -> SDL.Event -> Bool
updateButton current e = case SDL.eventPayload e of
  SDL.MouseButtonEvent d
    | SDL.mouseButtonEventButton d == SDL.ButtonLeft ->
        case SDL.mouseButtonEventMotion d of
          SDL.Released -> False
          SDL.Pressed  -> True
  _ -> current

-- | The uppercase letter a letter keycode ('SDL.KeycodeA' through
-- 'SDL.KeycodeZ', whose underlying codes are the ASCII lowercase range)
-- represents, or 'Nothing' for any other keycode.
letterKeycode :: SDL.Keycode -> Maybe Char
letterKeycode kc
  | code >= SDL.unwrapKeycode SDL.KeycodeA && code <= SDL.unwrapKeycode SDL.KeycodeZ
  = Just (toUpper (chr (fromIntegral code)))
  | otherwise = Nothing
  where code = SDL.unwrapKeycode kc

toKeyEvents :: SDL.Event -> [KeyEvent]
toKeyEvents e = case SDL.eventPayload e of
  SDL.KeyboardEvent d
    | SDL.keyboardEventKeyMotion d == SDL.Pressed
    , let keysym = SDL.keyboardEventKeysym d
    , let mods   = SDL.keysymModifier keysym
    , let alt    = SDL.keyModifierLeftAlt mods || SDL.keyModifierRightAlt mods
    , Just c <- letterKeycode (SDL.keysymKeycode keysym)
    , alt
    -> [KeyEvent { key = KeyChar c, modifiers = [Alt], keyRepeat = SDL.keyboardEventRepeat d }]
  SDL.KeyboardEvent d
    | SDL.keyboardEventKeyMotion d == SDL.Pressed
    -> let rep = SDL.keyboardEventRepeat d
       in case SDL.keysymKeycode (SDL.keyboardEventKeysym d) of
         SDL.KeycodeTab ->
           let mods    = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyTab, modifiers = [Shift | shifted], keyRepeat = rep }]
         SDL.KeycodeReturn    -> [KeyEvent { key = KeyReturn,    modifiers = [], keyRepeat = rep }]
         SDL.KeycodeBackspace -> [KeyEvent { key = KeyBackspace, modifiers = [], keyRepeat = rep }]
         SDL.KeycodeDelete    -> [KeyEvent { key = KeyDelete,    modifiers = [], keyRepeat = rep }]
         SDL.KeycodeSpace     -> [KeyEvent { key = KeySpace,     modifiers = [], keyRepeat = rep }]
         SDL.KeycodeA         ->
           let mods  = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               ctrld = SDL.keyModifierLeftCtrl mods || SDL.keyModifierRightCtrl mods
           in [KeyEvent { key = KeyA, modifiers = [Ctrl | ctrld], keyRepeat = rep }]
         SDL.KeycodeLeft      ->
           let mods    = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyLeft,  modifiers = [Shift | shifted], keyRepeat = rep }]
         SDL.KeycodeRight     ->
           let mods    = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyRight, modifiers = [Shift | shifted], keyRepeat = rep }]
         SDL.KeycodeUp        -> [KeyEvent { key = KeyUp,        modifiers = [], keyRepeat = rep }]
         SDL.KeycodeDown      -> [KeyEvent { key = KeyDown,      modifiers = [], keyRepeat = rep }]
         SDL.KeycodeEscape    -> [KeyEvent { key = KeyEscape,    modifiers = [], keyRepeat = rep }]
         _ -> []
  _ -> []

toTypedText :: SDL.Event -> [Text]
toTypedText e = case SDL.eventPayload e of
  SDL.TextInputEvent d -> [SDL.textInputEventText d]
  _                    -> []

-- | Vertical wheel movement for this event, or 0 if it isn't a wheel event
-- -- see 'Blink.Input.inputWheelDelta' for the sign convention. SDL reports
-- @y@ positive "away from the user" (scrolling up/back) unless the
-- platform's natural-scrolling setting flips it, so both cases are negated
-- to land on "positive scrolls down/forward".
toWheelDelta :: SDL.Event -> Double
toWheelDelta e = case SDL.eventPayload e of
  SDL.MouseWheelEvent d ->
    let SDL.V2 _ y = SDL.mouseWheelEventPos d
        flipSign = case SDL.mouseWheelEventDirection d of
          SDL.ScrollNormal  -> 1
          SDL.ScrollFlipped -> -1
    in negate (fromIntegral y * flipSign)
  _ -> 0

sdlPoint :: SDL.Point SDL.V2 CInt -> Point
sdlPoint (SDL.P (SDL.V2 x y)) = Point (fromIntegral x) (fromIntegral y)
