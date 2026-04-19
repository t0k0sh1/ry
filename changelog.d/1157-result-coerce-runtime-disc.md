### Fixed

- `coerceResultType` no longer silently drops the active payload when a
  function-returned `Result` is bound to a variable with a different `Result`
  annotation. Such mismatches are now rejected at compile time with an explicit
  type-error message (#1157)
