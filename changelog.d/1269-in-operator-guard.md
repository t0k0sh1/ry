### Fixed

- The `in` / `not in` operator on a list now emits a compile error for lists
  of non-string pointer elements such as `List<List<T>>`, `List<Map<K, V>>`,
  `List<Set<T>>`, and `List<function(...) -> R>`. Previously there was no
  guard at all and the linear-search loop fell through to `strcmp` on
  non-C-string pointers (Map/Set/closure/list headers), which is undefined
  behaviour. Mirrors the `distinct()` (#1262) and `remove()` (#1268) guards.
  (#1269)
