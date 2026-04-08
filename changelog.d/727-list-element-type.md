### Fixed

- Element type metadata is now preserved when accessing elements of `List<Map<K,V>>`, `List<Set<T>>`, and `List<closure>` by index or in a `for` loop (#727)
  - `xs[0]["key"]` on `List<Map<str, int>>` now works correctly
  - `for m in xs: m["key"]` on `List<Map<str, int>>` now works correctly
  - `xs[0]` on `List<Set<int>>` supports the `in` operator
  - Closures stored in a list (`fns[0](arg)`) are now callable after retrieval
