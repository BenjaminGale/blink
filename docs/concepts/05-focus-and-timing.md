# 4. Focus and timing

[Section 3](03-the-frame-loop.md) mentioned that some presentational state
— focus, scroll, selection — persists across frames, and that changes to it
are applied at different times: focus changes **immediately**, mid-frame;
scroll and selection are **queued** and only take effect on the *next*
frame. This section is about why that split exists, since it's the one
piece of Blink's timing model that isn't obvious just from "state persists
between frames."

## The problem: two controls, one frame, one race

Say you have two buttons rendered one after another, and pressing Tab on
the first should move focus to the second. Both controls run in the same
frame, in the order the tree is walked — `buttonA` first, then `buttonB`.

`buttonA`'s logic is roughly "if I'm focused and Tab was pressed, give up
focus." `buttonB`'s logic is roughly "if nothing is focused, take it."
Both of those checks run **in the same frame**, moments apart. If focus
changes were queued the same way scroll is — applied only on the *next*
frame — then `buttonB`'s check would still see `buttonA` as focused, because
the frame it's reading state from is the one *before* `buttonA`'s change
takes effect:

```
  buttonA (frame N)                          buttonB (frame N)

  |                                          |
  | Tab pressed                              |
  | calls clearFocus                         |
  | (queued, not applied                     |
  | until next frame)                        |
  |                                          |
  |-------------- same frame ---------------->
  |                                          | isFocused? reads
  |                                          | current focus: buttonA
  |                                          | (stale -- clearFocus
  |                                          | hasn't applied yet)
  |                                          | still focused, so
  |                                          | does nothing

------------------------ next frame boundary -------------------------

  queued clearFocus finally applies: focus := Nothing
  ...one frame too late for buttonB to have seen it
```

Tab silently does nothing. The keypress is consumed, `buttonA` genuinely
gave up focus — just one frame later than `buttonB` needed to see it.

## The fix: focus changes are immediate

`setFocus` and `clearFocus` mutate the ambient scope's focus state the
moment they're called, not on a queue. Because the tree walk is sequential,
`buttonB`'s check — running moments later in the *same* frame — sees the
result of `buttonA`'s change immediately:

```
  buttonA                                    buttonB

  |                                          |
  | Tab pressed                              |
  | calls clearFocus                         |
  | focus := Nothing                         |
  | (applied immediately,                    |
  | same frame)                              |
  |                                          |
  |---- tree walk continues, same frame ----->
  |                                          |
  |                                          | isFocused? reads
  |                                          | current focus: Nothing
  |                                          |
  |                                          | nothing focused,
  |                                          | so setFocus buttonB
  |                                          | focus := buttonB
  |                                          |
  v                                          v
```

Mouse capture works the same way and for the same reason: an element
deciding "should I take the mouse?" needs to see what the *previous*
sibling in this same frame just decided, or two overlapping elements could
both believe they have it.

## Why scroll and selection don't need this

Scroll position and text selection aren't contended for the way focus is.
Nothing else in the tree is racing to claim "the" scroll offset of a
particular list — only that list's own state is at stake, and only *it*
ever reads or writes it. There's no sibling whose correctness this frame
depends on seeing the change, so there's nothing to race.

Because of that, these are queued (`emitUi`, applied by
`nextFrameContext` at the start of the *next* frame) rather than applied
immediately. That's a strictly easier model to reason about — a write
made partway through a frame simply isn't visible to anything reading in
that same frame — and correctness doesn't depend on same-frame visibility
here the way it does for focus.

## The practical rule

When writing a custom control:

* If a value you're setting needs to be visible to another element **later
  in the same frame** — anything involving arbitration between siblings,
  like focus or capture — use the immediate primitives (`setFocus`,
  `clearFocus`).
* If it's local, single-owner state that nothing else reads — like a
  list's own scroll offset — queue it with `emitUi` instead.

Next: [section 6](06-application-and-backend.md) closes the loop — how the
messages controls like this one emit actually turn into a new application
state, and how a real backend drives the frames described in
[section 3](03-the-frame-loop.md) in the first place.

See also
[../guides/building-a-custom-control.md](../guides/building-a-custom-control.md)
for a worked example that puts sections 1–5 together to hand-write a
minimal button from these primitives.
