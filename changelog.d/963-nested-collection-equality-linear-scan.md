### Fixed

- Nested-collection equality (`Set<List<T>>`, `Set<Map<K,V>>`, `Set<Set<T>>`) now
  returns correct results regardless of insertion order (#963)
- `Set.contains(elem)`, `elem in set`, `set.add(elem)`, and `set.remove(elem)` now
  use structural equality when the element type is a nested collection, instead of
  incorrectly treating the element pointer as a C string (#963)
