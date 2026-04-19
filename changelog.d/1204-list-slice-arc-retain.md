### Fixed

- `slice(lst, a, b)` / `lst[a..b]` now correctly retains ARC-managed
  reference-typed elements (`List<str>`, `List<List<T>>`, `List<Map<K,V>>`,
  closures), preventing use-after-free when the source list is dropped (#1204)
