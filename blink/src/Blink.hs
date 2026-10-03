{- |
Module: Blink

Everything an application needs to build a user interface from Blink's
ready-made controls: the 'App' record, the 'Update' monad that applies
messages to your state, the controls and layout containers, and theming.

A view is a function from your state to an 'Element' tree, rebuilt every
frame. Controls report what the user did by sending messages, which
'update' applies to the state once the frame completes, in the order they
were sent:

@
data Msg = AddItem

view :: AppState -> Element ControlId Msg
view s = button AddButton [text \"Add\", onActivated (post AddItem)]

update :: Msg -> Update AppState ControlId Msg ()
update AddItem = modify addItem
@

Every 'App' is parameterised over three types:

  * @e@ identifies each interactive control, usually a sum type with one
    constructor per control. Blink uses it to look up styles and to track
    hover, focus and scroll state between frames.
  * @msg@ is the type of messages the view sends.
  * @s@ is your application state.

= Other modules

  * "Blink.View" for writing a custom control by hand.
  * "Blink.Testing" for running a view or control on its own, in tests.
  * "Blink.Backend" for connecting Blink to a window system and renderer.
    The @blink-sdl2@ package provides one.
  * Each control's own module, such as "Blink.Controls.Button", for its
    style keys and the functions other controls are built from.

For a narrative walkthrough of how Blink works, see the
<https://github.com/BenjaminGale/blink/blob/main/docs/concepts/README.md concepts guide>.
-}
module Blink
  ( -- * Application
    App (..)
    -- * Updating state
  , Update
  , get
  , put
  , gets
  , modify
  , cmd
  , runUpdate
    -- ** Requesting UI changes from update
  , requestScrollTo
  , requestScrollBy
  , requestFocus
  , requestClearFocus
  , requestSelectionAt
  , Selection (..)
    -- * Elements
  , Element
  , Attribute
  , spacer
  , emptyElement
  , part
    -- * Controls
  , module Blink.Controls
    -- * Layout
  , module Blink.Layout
    -- * Theming
  , module Blink.Style
  , defaultTheme
    -- * Geometry and colour
  , module Blink.Geometry
  , TextAlign (..)
    -- * Keyboard input
  , KeyEvent (..)
  , Key (..)
  , Modifier (..)
  ) where

import Blink.App (App (..))
import Blink.Controls
import Blink.Element (Attribute, Element, emptyElement, part, spacer)
import Blink.Geometry
import Blink.Input (Key (..), KeyEvent (..), Modifier (..))
import Blink.Layout
import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Style.Defaults (defaultTheme)
import Blink.Update (Update, cmd, get, gets, modify, put, runUpdate)
import Blink.View (Selection (..), requestClearFocus, requestFocus, requestScrollBy, requestScrollTo, requestSelectionAt)
