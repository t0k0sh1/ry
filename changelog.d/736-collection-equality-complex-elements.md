### Changed

- `List<T>` and `Map<K, V>` `==` / `!=` now support complex element/value types: records, tuples, and nested collections (`List<List<T>>`, `List<Map<K,V>>`, `Map<str, List<T>>`, `Map<str, Map<K,V>>`, etc.) (#736).

### Fixed

- `List<Set<T>>` and `List<Map<K,V>>` equality no longer silently falls back to pointer comparison, which produced incorrect results (#736).
- Clearer compile-time error for `Set<T>` equality with non-primitive element types, with reference to tracking issue (#736).
