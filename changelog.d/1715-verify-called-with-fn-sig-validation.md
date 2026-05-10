### Changed

- `verifyCalledWith(name, args...)` now enforces **exact fn signature
  match** for function-typed arguments (`fn(...) -> R` parameters
  introduced in #1707). Mismatched parameter count, parameter types, or
  return type are rejected at compile time with a diagnostic that
  includes both the recorded parameter signature and the verify-side
  value's signature (e.g. `verifyCalledWith: argument 1 of 'takesFn' is
  declared as fn(int) -> int but expected value has type fn(str) -> int`).
  Previously the signature was opaque to `verifyCalledWith`, so passing
  a fn value with a different signature compiled but always returned
  `0` (closure pair identity could never be equal across signatures) —
  silently masking test bugs. v1 requires exact match; variance and
  subtyping are not supported. (#1715)
