### Added

- `verifyCalledWith(name: str, args...) -> int` testing intrinsic.
  Returns the number of recorded mock calls whose arguments exactly
  match `args...`, complementing `verify()` which counts all calls
  regardless of arguments. The function name must be a string literal
  so that the compiler can validate the remaining argument types
  against the original function's signature. v1 supports `int`,
  `float`, `bool`, and `str` arguments; `List<T>` / `Map<K, V>` /
  `Set<T>` / record / tuple / function-typed arguments are rejected
  at compile time and tracked for follow-up. Requires
  `from testing import verifyCalledWith`. (#1677)
- Capture-based closures can now be used as the replacement passed to
  `mock(target, replacement)`. The closure may read or mutate
  variables from the enclosing scope, which is the canonical pattern
  for recording call history (e.g. appending arguments to a captured
  `List<int>`). The captured environment is retained when the mock is
  registered and released automatically when the `it` block ends.
  (#1678)
