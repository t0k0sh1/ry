### Added

- `verifyCalledWith(name, args...)` now accepts `List<T>` arguments
  where `T ∈ {int, float, bool, str}`. The recorded call's list is
  deep-snapshotted at call time and compared element-wise against the
  verify-side snapshot, so `verifyCalledWith("f", [1, 2, 3])` matches
  only calls where `f` was invoked with a list of identical length and
  values. `str` elements are compared NUL-safely via length+`memcmp`.
  Mismatched arity or element types (e.g. passing `List<str>` against
  a `List<int>` parameter, or a scalar against a `List<T>` parameter)
  are rejected at compile time. Internally this introduces a snapshot
  ABI (kind tag 6 = list) reserved for future `Set<T>`, `Map<K, V>`,
  record, tuple, and function-value extensions of `verifyCalledWith`.
  (#1703)
