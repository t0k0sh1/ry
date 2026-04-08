### Added

- `==` and `!=` operators now work for `List<T>`, `Set<T>`, `Map<K,V>`, `Result<T,E>`, and union types (#725)
  - List: element-wise comparison (supports `int`, `float`, `str`, `bool` elements)
  - Set: unordered equality — `{1,2,3} == {3,2,1}` is `true`
  - Map: key/value equality — maps with the same key-value pairs are equal regardless of insertion order
  - Result: compares `is_ok` flag and the inner `Ok` or `Err` value
  - Union (`A|B`): compares tag (variant kind) first, then the inner value for matching tags
