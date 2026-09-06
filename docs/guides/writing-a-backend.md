# Writing a backend

Blink doesn't own a main loop, a window, or a renderer — a backend
supplies all three and calls into Blink once per frame. This guide walks
through what a backend actually has to do, using the included SDL2 backend
(`app/Main.hs`, `app/Rendering.hs`) as the worked example. Read
[the application-and-backend concept](../concepts/03-runtime/02-application-and-backend.md)
first if you haven't already — this guide assumes you know what `App`,
`BlinkHandle`, and `stepFrame` are for.

A backend has four responsibilities: measure text, configure a handle,
translate platform events into a `FrameInput` each iteration, and turn the
`DrawCommand`s that come back into pixels.

## 1. Provide a `TextMeasurer`

Blink lays out text-sensitive controls (a text input's cursor, anything
sized to fit its label) without knowing anything about your font backend.
You supply three functions against your platform's real font metrics:

```haskell
data TextMeasurer = TextMeasurer
  { tmCharOffset   :: Text -> Int -> IO Float  -- x offset of character n
  , tmCharAtOffset :: Text -> Float -> IO Int  -- character index closest to an x offset
  , tmTextSize     :: Text -> IO Size          -- pixel size of a rendered string
  }
```

The SDL2 backend builds this from `SDL.Font` glyph metrics in
`app/Rendering.hs`'s `mkTextMeasurer`. If you're prototyping without a real
font library yet, `noOpTextMeasurer` (all zeros) is enough to get a
backend running before text layout needs to be pixel-accurate.

## 2. Choose a driving mode and configure a handle

Decide whether your platform's event loop blocks waiting for input, or
redraws unconditionally every iteration (see
[the application-and-backend concept](../concepts/03-runtime/02-application-and-backend.md)
for what each implies), then call the matching function once at startup:

```haskell
handle <- configureEventDriven demoApp notify measurer
-- or: handle <- configureContinuous demoApp measurer
```

`configureEventDriven` additionally takes a `notify :: IO ()` callback.
Blink runs its own animation ticker on a background thread; when a
control has called `requiresAnimation` and the ticker fires, `notify` is
invoked so your backend can unblock whatever it's blocking on (its event
wait) to go process that tick. The SDL2 backend implements this by
registering a custom SDL event and pushing it from `notify`, so the
blocked `SDL.waitEvent` call returns.

## 3. Translate one iteration of platform events into a `FrameInput`

Every loop iteration, before calling `stepFrame`, assemble one
`FrameInput` from whatever your platform handed you:

```haskell
data FrameInput = FrameInput
  { mousePosition   :: Point
  , mouseButtonDown :: Bool
  , keyEvents       :: [KeyEvent]
  , typedText       :: [Text]
  , windowSize      :: Size
  , quitRequested   :: Bool
  , isAnimationTick :: Bool
  }
```

Most of this is a direct translation — cursor position, whether the left
button is physically held, the window's current size, whether the platform
signalled a close request. Two fields need more care:

* **`keyEvents`** only needs the keys Blink's controls act on (see `Key` in
  `Blink.Input`) — Tab, Return, Backspace, Space, and the arrow keys.
  Everything else can be dropped. `app/Main.hs`'s `toKeyEvents` shows the
  full translation from SDL keysyms, including reading `keyRepeat` off the
  platform's own auto-repeat flag and carrying Shift as a `Modifier` for
  Shift+Tab and Shift+arrow.
* **`isAnimationTick`** should be `True` exactly when this iteration was
  woken by the `notify` callback from step 2, not by real input — that's
  how the frame loop knows to advance animation state rather than treat
  this as an ordinary input frame. The SDL2 backend does this by
  registering a distinct SDL event type for `notify` and checking each
  polled event against it (`checkAnimTick` in `app/Main.hs`).

Text entry is separate from `keyEvents`: `typedText` carries the actual
Unicode text your platform's input method produced (composed characters,
IME, etc.), independent of which physical keys were involved.

## 4. Render the `DrawCommand`s

`stepFrame` returns a `FrameResult` — either `Continue [DrawCommand] s` or
`Quit [DrawCommand] s`, both carrying this frame's draw commands and the
resulting state (`Quit` still carries commands so you can render the final
frame before exiting). `DrawCommand` is a small, backend-agnostic list:

```haskell
data DrawCommand
  = FillRect Rectangle Colour
  | StrokeBorder Rectangle Colour BorderEdges
  | DrawText Rectangle Text Colour TextAlign
  | PushClip Rectangle
  | PopClip
```

Walk the list in order, issuing the equivalent draw call in your
rendering API for each, and treat `PushClip`/`PopClip` as a stack —
everything drawn between a push and its matching pop should be clipped to
the intersection of that rectangle with whatever was already on the
clip stack. `app/Rendering.hs`'s `submitDrawCommand` does this against
SDL2's renderer, including a simple glyph texture cache for `DrawText` so
each string isn't re-rendered to a texture every frame.

## Putting it together

The actual loop is small once the above four pieces exist — this is
`app/Main.hs`'s `loop`, trimmed to the shape:

```haskell
loop handle = do
  event <- waitForPlatformEvent
  input <- assembleFrameInput event    -- step 3
  result <- stepFrame handle input
  case result of
    Continue draws _ -> renderDrawCommands draws >> loop handle  -- step 4
    Quit     draws _ -> renderDrawCommands draws
```

Everything about *what* a frame does — running the view, resolving focus,
folding messages into state — is entirely Blink's concern, covered by the
concept docs. A backend's job is exactly this translation at the edges:
platform events in, draw commands out.
