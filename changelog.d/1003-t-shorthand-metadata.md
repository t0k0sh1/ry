### Fixed

- `T?` shorthand return type now propagates collection metadata identically to
  `Option<T>` — `xs.length()`, index access, and equality now work correctly for
  functions declared as `-> List<T>?` / `-> Map<K,V>?` / `-> Set<T>?` (#1003)
