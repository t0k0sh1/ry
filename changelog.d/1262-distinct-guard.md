### Fixed

- `distinct()` now emits a compile error for lists of non-string pointer
  elements such as `List<Map<K, V>>`, `List<function(...) -> R>`, and
  `List<Set<T>>`. Previously the guard only rejected `List<List<T>>` and
  silently fell through to a `strcmp` on non-C-string pointers, which is
  undefined behaviour. (#1262)
