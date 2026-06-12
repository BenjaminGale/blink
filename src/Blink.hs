{- |
Module: Blink

Blink is a retained-mode UI library: the UI is a pure function of state, and
state changes only through modifiers dispatched by the UI. Blink acts as a
library — it does not own the main loop. The backend drives the loop and calls
into Blink each frame.

= Architecture

An application is described by an 'App', which bundles the initial state,
theme, view, and update handler. Passing an 'App' to 'configureContinuous' or
'configureEventDriven' produces a 'BlinkHandle'. The backend then calls
'stepFrame' each iteration, passing a 'FrameInput' assembled from platform
events and receiving a 'FrameResult' containing draw commands and updated state:

@
loop handle state = do
  input  <- collectFrameInput       -- assemble FrameInput from platform events
  result <- stepFrame handle input state
  case result of
    Continue draws state' -> render draws >> loop handle state'
    Quit     draws _      -> render draws
@

= Type parameters

Every 'App' is parameterised over two types:

  * @e@ — the /element type/, a sum type with one constructor per interactive
    control. Used to look up styles from the 'Theme' and to route keyboard
    focus. See "Blink.UI".
  * @s@ — the /application state/, owned by the host, read by the UI tree via
    'getAppState', and changed only through modifiers queued with 'dispatch'
    and 'dispatchAsync'. Presentational state (scroll positions, selections) is
    baked into the 'UIContext' and accessed through dedicated primitives. See
    "Blink.UI".

= Module guide

  * "Blink.App"      — Application definition and backend integration.
                       Start here when implementing a new backend.
  * "Blink.UI"       — The UI monad: drawing, interaction, focus, and style
                       queries. Start here when building views.
  * "Blink.Controls" — Ready-made controls: buttons, text inputs, checkboxes,
                       progress bars, and labels.
  * "Blink.Layout"   — Box layout and constraint-based sizing.
  * "Blink.Style"    — Themes and per-state styles.
  * "Blink.Rendering"— The draw command list produced each frame.
  * "Blink.Geometry" — Primitive geometry types.
-}
module Blink
  ( module Blink.App
  , module Blink.Controls
  , module Blink.Geometry
  , module Blink.Input
  , module Blink.Layout
  , module Blink.Rendering
  , module Blink.Style
  , module Blink.UI
  ) where

import Blink.App
import Blink.Controls
import Blink.Geometry
import Blink.Input
import Blink.Layout
import Blink.Rendering
import Blink.Style
import Blink.UI
