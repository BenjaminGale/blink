-- | Translates SDL events into the pieces of a 'FrameInput'.
module Blink.SDL2.Input
  ( updateButton
  , toKeyEvents
  , toModifiers
  , toTypedText
  , toWheelDelta
  , sdlPoint
  ) where

import Blink.Backend
import Control.Applicative ((<|>))
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

-- | The 'Key' for each SDL keycode Blink reports, or 'Nothing' for any
-- other keycode.
toKey :: SDL.Keycode -> Maybe Key
toKey kc = case kc of
  SDL.KeycodeTab       -> Just KeyTab
  SDL.KeycodeReturn    -> Just KeyReturn
  SDL.KeycodeKPEnter   -> Just KeyReturn
  SDL.KeycodeBackspace -> Just KeyBackspace
  SDL.KeycodeDelete    -> Just KeyDelete
  SDL.KeycodeInsert    -> Just KeyInsert
  SDL.KeycodeSpace     -> Just KeySpace
  SDL.KeycodeEscape    -> Just KeyEscape
  SDL.KeycodeLeft      -> Just KeyLeft
  SDL.KeycodeRight     -> Just KeyRight
  SDL.KeycodeUp        -> Just KeyUp
  SDL.KeycodeDown      -> Just KeyDown
  SDL.KeycodeHome      -> Just KeyHome
  SDL.KeycodeEnd       -> Just KeyEnd
  SDL.KeycodePageUp    -> Just KeyPageUp
  SDL.KeycodePageDown  -> Just KeyPageDown
  _ -> lookup kc functionKeys <|> charKey kc

functionKeys :: [(SDL.Keycode, Key)]
functionKeys = zip
  [ SDL.KeycodeF1, SDL.KeycodeF2, SDL.KeycodeF3, SDL.KeycodeF4, SDL.KeycodeF5, SDL.KeycodeF6
  , SDL.KeycodeF7, SDL.KeycodeF8, SDL.KeycodeF9, SDL.KeycodeF10, SDL.KeycodeF11, SDL.KeycodeF12
  ]
  (map KeyFunction [1 ..])

-- | A letter or digit keycode as 'KeyChar', with letters in upper case.
-- SDL's letter keycodes are the ASCII lowercase range and its digit
-- keycodes the ASCII digit range.
charKey :: SDL.Keycode -> Maybe Key
charKey kc
  | inRange SDL.KeycodeA SDL.KeycodeZ = Just (KeyChar (toUpper (chr (fromIntegral code))))
  | inRange SDL.Keycode0 SDL.Keycode9 = Just (KeyChar (chr (fromIntegral code)))
  | otherwise                         = Nothing
  where
    code = SDL.unwrapKeycode kc
    inRange lo hi = code >= SDL.unwrapKeycode lo && code <= SDL.unwrapKeycode hi

-- | The modifier keys held, in a fixed order, so the same combination
-- always produces the same list.
toModifiers :: SDL.KeyModifier -> [Modifier]
toModifiers mods =
  [ Shift | SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods ]
  ++ [ Ctrl  | SDL.keyModifierLeftCtrl mods  || SDL.keyModifierRightCtrl mods ]
  ++ [ Alt   | SDL.keyModifierLeftAlt mods   || SDL.keyModifierRightAlt mods ]
  ++ [ Super | SDL.keyModifierLeftGUI mods   || SDL.keyModifierRightGUI mods ]

toKeyEvents :: SDL.Event -> [KeyEvent]
toKeyEvents e = case SDL.eventPayload e of
  SDL.KeyboardEvent d
    | SDL.keyboardEventKeyMotion d == SDL.Pressed
    , let keysym = SDL.keyboardEventKeysym d
    , Just k <- toKey (SDL.keysymKeycode keysym)
    -> [KeyEvent { key = k, modifiers = toModifiers (SDL.keysymModifier keysym), keyRepeat = SDL.keyboardEventRepeat d }]
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
