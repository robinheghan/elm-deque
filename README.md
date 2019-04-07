# elm-deque

A double ended queue for Elm. Think of it like a `List`, but with equally fast access to the front and the back (`List` _only_ has fast access to the front).

This data structure uses a finger tree implementation, and has amortized O(1) operations for access and pop to the front and back portions of the queue. It also provies amortized O(1) concatination/appending.

## Differences from folkertdev/elm-deque

`folkertdev/elm-deque` uses a different implementation which, in theory, provides faster pops and pushes, but which will occasionally spend a long time doing each of those due to rebalancing the elements. A finger tree implementation has a higher (but still small) overhead when doing stuff, but has more predictable performance. `folkertdev/elm-deque` also doesn't provide O(1) concatination/appending.

This implementation doesn't provide all the functions that you find in `folkertdev/elm-deque`. If that's a problem for you, let me know and I'll provide those functions in this package as well.
