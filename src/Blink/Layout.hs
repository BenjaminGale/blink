{- |
Module: Blink.Layout

= How layout works

By default, a 'Blink.UI.UI' action fills the full space it is given by its parent.

>  +------------------------------------------+
>  |                                          |
>  |                component                 |
>  |        (fills parent by default)         |
>  |                                          |
>  +------------------------------------------+

This module is a barrel over "Blink.Layout.Core" (single-child sizing and
positioning), "Blink.Layout.Box" (arranging several children along an
axis), and "Blink.Layout.Border" (dividing space into named regions) --
combinators that allow more control over layout, in order of increasing
scope. Reach for the smallest one that does what you need.
-}
module Blink.Layout
  ( -- * Single control layout
    layoutWithConstraints
  , Layout (..)
  , Length (..)
    -- * Box layout
  , hBox
  , vBox
  , BoxConfig
  , defaultBoxConfig
  , boxSpacing
  , boxMargin
  , boxAlignment
  , boxFillCross
  , boxTotalSpacing
    -- * Border layout
  , borderLayout
  , BorderContent (..)
  , emptyBorderContent
    -- * Utilities
  , preferredSize
  , AddLength (..)
  , MaxLength (..)
  , addLength
  , maxLength
    -- * Advanced layout
    -- | Everything above covers a fixed arrangement decided in advance.
    --   Sometimes that isn't enough, in one of two ways:
    --
    --   * The arrangement itself needs to depend on something only known at
    --     render time — the current context.
    --   * A slot needs a constraint more specific than 'Fill' or a single
    --     hardcoded 'Exactly' — a finer-grained choice of 'Length'.
    --
    --   Both are special cases: they draw on 'Blink.UI.getBounds' \/ 'Blink.UI.withBounds', the
    --   application state passed into the enclosing view, a box combinator
    --   ('hBox' \/ 'vBox'), 'layoutWithConstraints', and the 'Length'
    --   arithmetic above all at once, rather than being handled by any one
    --   of them.
    --
    --   == Depending on the current context
    --
    --   \"Context\" here means either of two things available while
    --   building a view: the current layout bounds, queried with
    --   'Blink.UI.getBounds' from inside the 'Blink.UI.UI' monad, or the application state,
    --   which the enclosing view function already has in scope as an
    --   ordinary argument (see "Blink.App"). Branch on either, or both,
    --   with ordinary @if@\/@case@ — there is no dedicated combinator for
    --   this because a plain 'Blink.UI.UI' action and a plain function argument
    --   already do the job:
    --
    --   @
    --   contextual :: AppState -> UI Element msg ()
    --   contextual state = do
    --     bounds <- getBounds
    --     if appCompactMode state || rectWidth bounds < 600
    --       then vBox [] [ (Layout Fill (Exactly 200) TopLeft, sidebar)
    --                     , (Layout Fill Fill        TopLeft, content)
    --                     ]
    --       else hBox [] [ (Layout (Exactly 200) Fill TopLeft, sidebar)
    --                     , (Layout Fill         Fill TopLeft, content)
    --                     ]
    --   @
    --
    --   Both branches use ordinary 'hBox'\/'vBox' — only the choice of
    --   which one to call, and how the two children are ordered, depends on
    --   the context. Here that context is a window narrower than 600px /or/
    --   an explicit compact-mode flag in the application state; the two
    --   compose freely, so either can drive the decision on its own or
    --   together.
    --
    --   == Choosing a finer-grained constraint
    --
    --   A slot's 'Length' does not have to be a value you wrote down ahead
    --   of time as 'Fill' or 'Exactly' — it can be computed, and it does
    --   not have to be either of those two constructors. 'AtLeast',
    --   'AtMost', and 'Between' give a slot room to flex within bounds
    --   instead of being either fully rigid or fully flexible; picking the
    --   right one is itself a form of custom layout:
    --
    --   @
    --   -- A sidebar that shrinks with the window but never drops below a
    --   -- readable width, alongside content that takes whatever is left.
    --   hBox []
    --     [ (Layout (AtLeast 180) Fill TopLeft, sidebar)
    --     , (Layout Fill          Fill TopLeft, content)
    --     ]
    --   @
    --
    --   An exact constraint can be computed the same way, rather than
    --   hardcoded. A caption's width is not known ahead of time either
    --   (it depends on the text and the active font), so sizing a slot
    --   tightly around it means computing an 'Exactly' from
    --   'Blink.UI.measureText' rather than writing one down:
    --
    --   @
    --   -- Sizes a label column exactly as wide as its own caption,
    --   -- instead of guessing a fixed width.
    --   labelledRow :: Text -> UI Element msg () -> UI Element msg ()
    --   labelledRow caption content = do
    --     Size textW _ <- measureText caption
    --     hBox []
    --       [ (Layout (Exactly (realToFrac textW)) Fill MiddleLeft, label Label [text caption])
    --       , (Layout Fill                         Fill TopLeft,    content)
    --       ]
    --   @
    --
    --   The same pattern extends to a row of controls that must each be
    --   exactly as wide as their own content demands: compute a 'Length'
    --   per child this way, then pass the list straight to 'hBox' instead
    --   of a fixed 'Layout' for every slot.
  ) where

import Blink.Layout.Border (BorderContent (..), borderLayout, emptyBorderContent)
import Blink.Layout.Box
  (BoxConfig, boxAlignment, boxFillCross, boxMargin, boxSpacing, boxTotalSpacing, defaultBoxConfig, hBox, vBox)
import Blink.Layout.Core
  (AddLength (..), Layout (..), Length (..), MaxLength (..), addLength, layoutWithConstraints, maxLength, preferredSize)
