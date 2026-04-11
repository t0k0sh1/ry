### Fixed

- `xs[i] += v` and `m[k] += v` now dispatch correctly when the element
  type is itself an ARC-managed collection (`List<List<T>>`,
  `Map<K, List<V>>`, and nested combinations reached via chained LHS such
  as `rec.items[i] += v`). Previously the loaded slot value lost its
  type metadata, so `emitArithmeticOp`'s list-concat dispatch fell
  through to the string path and produced a misleading
  `operator '+' not supported between str and non-str types` error.
  The fix propagates the container's element type name onto the loaded
  SSA value via `propagateTypeMeta` — the same pattern the formatter
  already uses for nested element loads. As a secondary fix, the
  empty-declaration path (`xs: List<List<int>> = []`) now records
  `list_elem_type_name` symmetric to the existing `List<Map>` /
  `List<Set>` branches so compound ops work on append-grown containers
  as well. (#858)
