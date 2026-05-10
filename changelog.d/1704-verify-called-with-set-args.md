### Added

- `verifyCalledWith(name, args...)` now accepts `Set<T>` arguments
  where `T ∈ {int, float, bool, str}`. The recorded call's set is
  deep-snapshotted at call time and compared **unordered** against the
  verify-side snapshot, so `verifyCalledWith("f", {1, 2, 3})` matches
  any call where `f` was invoked with a set of the same length and the
  same elements regardless of insertion order or hash-bucket layout
  (e.g. `{3, 2, 1}` and `{1, 2, 3}` are equivalent). `str` elements are
  compared NUL-safely via length+`memcmp`. Mismatched arity, container
  kind (e.g. `Set<int>` against a `List<int>` parameter, or a scalar
  against a `Set<T>` parameter), or element types (e.g. `Set<int>`
  against a `Set<str>` parameter) are rejected at compile time. This
  reuses the snapshot ABI introduced in #1703 (kind tag 7 = set;
  storage layout shared with `List<T>`, only the comparison semantics
  differ). (#1704)
