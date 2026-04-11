### Fixed

- `rec.arcField = newList` now releases the previously-stored ARC-managed
  collection (`List`/`Map`/`Set`) before the overwrite, matching the
  element-slot fix from #855. Applies to plain and compound assignment on
  `VariableExpr`, `FieldAccessExpr` (chained `outer.inner.items = ...`),
  and `IndexExpr` (`list[i].arcField = ...`) left-hand sides. Sibling
  `fieldTypeIsArcManaged` predicate added so record field types are
  classified from their declared AST type rather than container metadata.
  (#857)
