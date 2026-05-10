### Added

- `verifyCalledWith(name, args...)` now accepts `Map<K, V>` arguments
  where `K, V ∈ {int, float, bool, str}`. The recorded call's map is
  deep-snapshotted at call time (independent copies of every key and
  value, with `str` slots ARC-retained) and compared **unordered** by
  key→value pair against the verify-side snapshot, so
  `verifyCalledWith("f", {"a": 1, "b": 2})` matches any call where `f`
  was invoked with a map having the same key set and the same value at
  each key, regardless of insertion order or hash-bucket layout (e.g.
  `{"b": 2, "a": 1}` and `{"a": 1, "b": 2}` are equivalent). `str` keys
  and `str` values are compared NUL-safely via length+`memcmp`.
  Mismatched arity, container kind (e.g. `Map<str, int>` against a
  `List<int>` / `Set<int>` / scalar parameter), key types, or value
  types are rejected at compile time. This reuses the snapshot ABI
  introduced in #1703 / #1704 (kind tag 8 = map; parallel keys and
  values arrays mirror the existing `MockListSnapshot` / `MockSetSnapshot`
  layouts). (#1705)
