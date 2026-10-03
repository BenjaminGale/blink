{-# LANGUAGE OverloadedStrings #-}
-- | A colour picker built from three Blink sliders, written outside the
-- library with nothing but @import Blink@.
module ColourPicker (colourPicker) where

import Blink

-- | One slider per colour channel. The picker takes a single id from the
-- app; each slider is a part of it (@"red"@, @"green"@, @"blue"@), so the
-- app's id type needs no separate ids for them.
colourPicker :: Ord e => e -> Colour -> (Colour -> msg) -> Element e msg
colourPicker pickerId (RGBA r g b a) onChange =
  vBox
    [ spacing 4
    , children
        [ channel "red"   r (\v -> RGBA v g b a)
        , channel "green" g (\v -> RGBA r v b a)
        , channel "blue"  b (\v -> RGBA r g v a)
        ]
    ]
  where
    channel name current set =
      part pickerId name $
        slider pickerId
          [ value current
          , onValueChanged (onChange . set)
          , height (exactly 24)
          ]
