### Fixed

- Seven additional same-element-type collection helpers that previously
  called `setTypeMeta(TypeMeta::ListElem|SetElem, …)` without the
  matching `propagateMeta(src, newHeader)` now propagate source-level
  metadata correctly: `filter` and `emitSortCore` in
  `codegen_call_higher_order.cpp`, `emitStrOp_reverse` (List branch)
  in `codegen_call_string.cpp`, and the four set operations
  `emitSetUnionCore` / `emitSetOp_intersection` /
  `emitSetOp_difference` / `emitSetOp_symmetric_difference` in
  `codegen_call_set_ops.cpp`. Before this fix, source-level metadata
  such as `list_elem_type_name`, `map_value_type_name`,
  `set_elem_fn_type_info`, `nested_list_elem`, and `resource_kinds`
  was silently dropped on the output collection — for example
  `filter(xs, p)` where `xs: List<Map<str, int>>` lost the inner
  `Map<str, int>` metadata, so a subsequent `ys[0]["k"]` access
  failed at codegen with `str does not support index access`. The
  redundant manual `set_elem_type_name` copy at each set-op site is
  also removed because `propagateMeta` already copies that field.
  This completes the codegen sweep started in #1648 (`emitListConcat`)
  and brings every same-element-type collection helper in line with
  `emitListSlice` / `emitMapMergeCore`. (#1651)
