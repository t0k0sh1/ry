### Fixed

- `emitListConcat` (the LLVM IR codegen for the list `+` operator)
  now calls `propagateMeta(lhs, newHeader)` after `setTypeMeta`, so
  element-type metadata such as `map_key_type_name` /
  `map_value_type_name` propagates to the concatenated result. Before
  this fix, an inferred binding like `ys = a + b` where
  `a, b: List<Map<str, int>>` lost the Map-element metadata and was
  treated as `List<str>`, causing subsequent `ys[i]["k"]` access to
  fail at codegen with `str does not support index access`. This
  brings `emitListConcat` in line with `emitListSlice` and
  `emitMapMergeCore`, both of which already pair `setTypeMeta` with
  `propagateMeta` per the existing rule. (#1648)
