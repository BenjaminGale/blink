# 1. The `Update` monad

[Effects](../01-immediate-mode-api/04-effects.md) established that
`update` folds each queued message into your application state, once per
message, in emission order. `Update s` is the monad that runs in: a small
state-threading computation over your state `s`, with `get`/`put`/`gets`/
`modify` — the same shape as any state monad you've used before.

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

This is currently the entire vocabulary for application-state logic in
Blink — there's no built-in equivalent of commands, subscriptions, or
effect-tracking yet. As that grows, it belongs in this area rather than
[`04-runtime`](../04-runtime/01-the-frame-loop.md), which stays scoped to
the infrastructure that drives Blink rather than how you write your own
application logic.
