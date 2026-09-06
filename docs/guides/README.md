# Blink: guides

Task-oriented walkthroughs — how to accomplish something specific with
Blink, as opposed to [`docs/concepts`](../concepts/README.md)'s
explanation of why the library works the way it does. Each guide assumes
you've read the concepts it links to; none require reading the others
first.

- [Composing a layout](composing-a-layout.md) — building a screen out of
  nested `hBox`/`vBox`/`borderLayout` containers, sizing children, and
  converting between `Element` and a bare `View` action.
- [Building a custom control](building-a-custom-control.md) — hand-writing
  a widget from `Blink.View` primitives when a ready-made one doesn't fit.
- [Writing a backend](writing-a-backend.md) — what a backend must provide
  to drive Blink: text measurement, translating platform events into a
  `FrameInput`, and rendering the resulting `DrawCommand`s.
