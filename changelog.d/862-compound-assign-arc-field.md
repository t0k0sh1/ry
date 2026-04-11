### Fixed

- `rec.arcField += v` now dispatches correctly when the field type is
  itself an ARC-managed collection (`List<T>`, `List<List<T>>`, etc.).
  This covers plain record field assignment (`b.items += [3]`), nested
  record field access (`outer.inner.items += [3]`), and chained LHS
  through a list of records (`lst[0].items += [3]`). Previously the
  field extracted from the struct lost its type metadata, so
  `emitArithmeticOp`'s list-concat dispatch fell through to the string
  path and produced a misleading `operator '+' not supported between
  str and non-str types` error. The fix propagates the field's declared
  type name onto the extracted SSA value via `propagateTypeMeta` at all
  three `FieldAssignStmt` compound branches — sibling fix to #858,
  which addressed the same class of metadata-loss bug on the
  `IndexAssignStmt` compound path. (#862)
