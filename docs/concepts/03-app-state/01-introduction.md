# 1. The `Update` monad

[Effects](../01-immediate-mode-api/04-effects.md) established that
`update` folds each queued message into your application state, once per
message, in emission order. Your state `s` is just a plain Haskell type —
usually a record — holding whatever your app needs to remember across
frames:

```haskell
data AppState = AppState { counter :: Int, name :: Text }
```

`Update s` is the monad that runs in: a small state-threading computation
over `s`, with `get`/`put`/`gets`/`modify` — the same shape as any state
monad you've used before. Messages describe what happened; `update` folds
each one into the state:

```haskell
data Msg = Increment | SetName Text

update :: Msg -> Update AppState ()
update Increment   = modify (\s -> s { counter = counter s + 1 })
update (SetName t) = modify (\s -> s { name = t })
```

`Update` only reads and modifies your state — nothing else. If a handler
needs to trigger a side effect (writing a file, making a network call),
that belongs in your backend's loop, driven by the resulting state, not
inside `Update` itself.

Application-state logic in Blink is expressed entirely as `Update`
handlers built from `get`/`put`/`gets`/`modify`. Growth in this area
(commands, subscriptions, effect-tracking) belongs here rather than in
[`04-runtime`](../04-runtime/01-the-frame-loop.md), which is scoped to
the infrastructure that drives Blink, not your application logic.
