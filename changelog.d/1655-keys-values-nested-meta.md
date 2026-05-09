### Fixed

- `keys(map)` and `values(map)` now propagate the source map's key /
  value type metadata onto the returned `List`. Before this fix,
  `emitBuiltinKeys` and `emitBuiltinValues` (`src/codegen_call.cpp`)
  stamped only the LLVM `TypeMeta::ListElem` slot via `setTypeMeta`,
  leaving `list_elem_type_name` and the derived `nested_list_elem` /
  `list_elem_fn_type_info` empty. The result list's elements were
  therefore dispatched as `str` by downstream operations, so
  `len(values(m)[0])` returned `0` (reading the List header's
  `weak_count` as `byte_len`) and `keys(m)[0][0]` /
  `values(m)[0]["k"]` raised `str does not support index access` at
  codegen for nested-collection key / value types like
  `Map<List<int>, str>` or `Map<str, List<int>>`. The fix snapshots
  `map_key_type_name` / `map_value_type_name` from the source map's
  metadata, calls `propagateTypeMeta("List<…>", newHeader)` after
  `setTypeMeta` to populate every derived slot, and pairs the element
  buffer `memcpy` with `emitCowRetainArcElements` when the element
  type is ARC-managed (#1204 / #1242 — required because the newly
  propagated `list_elem_type_name` flips the result's destructor to
  recurse into the inner ARC elements). The analogous bug in
  `items(map)` (`src/codegen_call_collection.cpp`) is tracked
  separately as #1659 because its tuple element type requires a
  different fix shape. (#1655)
