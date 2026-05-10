### Added

- `verifyCalledWith(name, args...)` now accepts **function-typed**
  arguments (`fn(...) -> R` parameters and capture closures), compared
  by **pointer equality** on the underlying `{thunk_ptr, env_ptr}` pair
  extracted from the uniform closure struct.
  - The same closure value passed twice matches (e.g. `let g = lambda;
    f(g); f(g); verifyCalledWith("f", g)` returns `2`); two
    independently-constructed but structurally identical lambdas do
    not match.
  - Bare `@public fn` references and `let g = f` aliases share the same
    cached forwarding thunk, so passing `f` and `g` interchangeably
    matches as expected.
  - Capture closures with different captured environments (e.g.
    `makeAdder(5)` vs `makeAdder(6)`) are distinguished by the
    per-instance `env_ptr` even though they share a single cached
    capturing thunk.
  - The fn-snapshot side-table holds `{thunk_ptr, env_ptr}` pairs as
    plain copies — closure environments are not ARC-retained because
    the issue's contract is pointer equality only; the caller scope
    keeps the underlying closure alive for the duration of the test
    block. (#1707)
