### Fixed

- Fix refcount imbalance when pattern-matching `Some(...)` on a value declared with the `T?` shorthand (e.g., `str?`, `List<int>?`). `extractGenericTypeArg` now recognises the `T?` suffix form as equivalent to `Option<T>`, ensuring the typed ARC retain path (Path 2a) is selected instead of the heuristic fallback (#1015).
