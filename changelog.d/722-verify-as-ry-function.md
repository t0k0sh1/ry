### Changed

- `verify()` is now an ordinary `@public fn verify(name: str) -> int`
  in `share/std/testing/testing.ry` that delegates to a new
  `@native("testing")` runtime call (`_mockGetCallCount`). The
  compiler-level special cases for `verify` were removed: the
  string-coercion sugar in the parser, the dispatch arm in
  `codegen_call_dispatch.cpp`, and the `verify` entry in
  `module_loader.cpp`'s testing-intrinsic allow-list are all gone.
  `verify` now flows through the ordinary import + user-fn
  resolution machinery — the same path used by `fail` since #718.
  (#722)

### Removed

- The bare-identifier form `verify(fnName)` is no longer accepted —
  the argument must be a string literal or `str`-typed expression
  (e.g. `verify("fnName")`). All in-tree call sites already used the
  string form, so no spec migration was required, but external users
  who relied on the identifier form must quote the function name.
  (#722)
- Compile-time validation that the function name passed to `verify`
  refers to a real function has been removed alongside the dispatch
  special case. `verify("nonexistent")` now compiles cleanly and
  returns `0` at runtime — the same value `verify` returns for any
  function that has not been mocked / called. (#722)

### Fixed

- Without `from testing import verify`, calling `verify(...)` now
  fails with the standard `undefined function: verify` diagnostic
  instead of the bespoke `'verify' requires 'from testing import
  verify'` message. The behavior is unchanged for legitimate users
  (the import is still required), and the diagnostic is now
  consistent with every other unimported function. (#722)
