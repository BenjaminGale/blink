{- |
Module: Blink

Blink is an immediate-mode UI library: the whole view tree is rebuilt from
scratch every frame as a pure function of state, rather than retained and
mutated incrementally. State changes only through messages emitted by the
view. Blink acts as a library — it does not own the main loop. The backend
drives the loop and calls into Blink each frame.

= Architecture

An application is described by an 'App', which bundles the initial state,
theme, a view returning an 'Blink.Element.Element', and an update handler.
Passing an 'App' to 'configureContinuous' or
'configureEventDriven' produces a 'BlinkHandle'. The backend then calls
'stepFrame' each iteration, passing a 'FrameInput' assembled from platform
events and receiving a 'FrameResult' containing draw commands and the
resulting state — 'BlinkHandle' tracks the current state internally, so the
loop itself only ever threads the handle:

@
loop handle = do
  input  <- collectFrameInput       -- assemble FrameInput from platform events
  result <- stepFrame handle input
  case result of
    Continue draws _ -> render draws >> loop handle
    Quit     draws _ -> render draws
@

= Type parameters

Every 'App' is parameterised over three types:

  * @e@ — the /element type/, a sum type with one constructor per interactive
    control. Used to look up styles from the 'Theme' and to route keyboard
    focus. See "Blink.View".
  * @msg@ — the type of messages the view emits. See "Blink.View".
  * @s@ — the /application state/, owned by the host and passed into the view
    explicitly each frame. Views never mutate it directly; they queue @msg@
    values with 'emit', which 'update' folds into the state once the frame
    completes, in emission order. See "Blink.Update". Presentational state
    (scroll positions, selections) is baked into the 'ViewContext' and accessed
    through dedicated primitives instead. See "Blink.View".

= Module guide

New to Blink? The module docs below are reference material; for a narrative
walkthrough of the paradigm — immediate mode, the frame loop, and how focus
and messages flow through it — start with the
<https://github.com/BenjaminGale/blink/blob/main/docs/concepts/README.md concepts guide>
instead.

  * "Blink.App"       — Application definition and backend integration.
                        Start here when implementing a new backend.
  * "Blink.View"       — The View monad: drawing, interaction, focus, and style
                        queries. Start here when building views.
  * "Blink.Element"    — The 'Blink.Element.Element' type every view
                        returns: a component's size request paired with how
                        to measure and run it. Every ready-made control and
                        layout container already produces one; reach for
                        'Blink.Element.elementWithLayout' only when placing
                        a hand-written 'Blink.View.View' action as a container
                        child. Re-exported here; if the application's own
                        element identity type (@e@ — see above) is itself
                        named @Element@, import that module qualified
                        alongside this one instead.
  * "Blink.Update"     — The Update monad: turns a message emitted by the view
                        into an updated application state, optionally
                        requesting a "Blink.Cmd" along the way.
  * "Blink.Controls"   — Every ready-made widget's constructor and
                        attribute functions, re-exported here; see
                        "Blink.Controls.Control" for the shared control
                        primitive every one of them is built from -- focus,
                        chrome, and events.
  * "Blink.Layout"     — Box layout and constraint-based sizing.
  * "Blink.Style"      — Themes and per-state styles.
  * "Blink.Rendering"  — The draw command list produced each frame.
  * "Blink.Geometry"   — Primitive geometry types.
  * "Blink.Input"      — Raw keyboard and mouse types assembled by the backend
                        each frame.
-}
module Blink
  ( module Blink.App
  , module Blink.Cmd
  , module Blink.Geometry
  , module Blink.Input
  , module Blink.Controls
  , module Blink.Element
  , module Blink.Layout
  , module Blink.Rendering
  , module Blink.Style
  , module Blink.View
  , module Blink.Update
  ) where

import Blink.App
import Blink.Cmd
import Blink.Geometry
import Blink.Input
import Blink.Controls
import Blink.Element
import Blink.Layout
import Blink.Rendering
import Blink.Style
import Blink.View
import Blink.Update
