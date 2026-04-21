### Fixed

- `remove()` on a list now emits a compile error for lists of non-string
  pointer elements such as `List<List<T>>`, `List<Map<K, V>>`, `List<Set<T>>`,
  and `List<function(...) -> R>`. Previously the guard only rejected
  `List<List<T>>` and silently fell through to a `strcmp` on non-C-string
  pointers, which is undefined behaviour. (#1268)
