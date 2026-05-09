### Fixed

- `items(map)` now stamps the source map's key/value type names onto
  the returned `List<(K, V)>` as `list_elem_type_name = "(K, V)"`.
  Before this fix, `emitCollOp_items`
  (`src/codegen_call_collection.cpp`) stamped only the LLVM tuple
  `TypeMeta::ListElem` slot via `setTypeMeta`, leaving
  `list_elem_type_name` empty. The for-loop destructure
  `for k, v in items(m):` relies on `splitTupleSig` reading that
  name to split the tuple into per-component metadata; without it,
  K/V components fell back to `str` and operations like `v[0]` on
  `Map<str, List<int>>` raised `str does not support index access`
  at codegen. The fix snapshots `map_key_type_name` /
  `map_value_type_name` before any `getOrCreateMeta` call (per the
  #858 name-snapshot-before-rehash discipline) and writes
  `list_elem_type_name = "(K, V)"` after `setTypeMeta`, mirroring
  the format used by `enumerate` and `zip`. Unlike the sibling fix
  for `keys()` / `values()` (#1655), no `emitCowRetainArcElements`
  is needed because the destructor for `List<(K, V)>` does not
  recurse into tuple fields — `fieldTypeIsArcManaged` returns false
  for tuple-syntax `list_elem_type_name`, so adding a retain would
  leak. (#1659)
