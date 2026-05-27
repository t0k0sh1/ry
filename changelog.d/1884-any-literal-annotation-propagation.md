### Added

- Mixed-type literals for `Map<K, any>`, `List<any>`, and `Set<any>`
  variable declarations and reassignments. Previously
  `m: Map<str, any> = {"a": 1, "b": "two", "c": true}` failed at
  codegen with `map values must all have the same type` because the
  `MapExpr` / `ListExpr` / `SetExpr` emitters strictly required
  identical LLVM types across elements and the annotation-driven
  `wrapInAny` auto-wrap (which works for `x: any = 1`) was never
  reached. A `LiteralAnyHintGuard` RAII helper, installed at the
  three assignment-system call sites (var-decl, function-local
  reassignment, module-global reassignment in `src/codegen_stmt.cpp`),
  signals to the literal emitters that each element should be
  individually wrapped via `wrapInAny` and that the strict
  same-type gate should be skipped. Element-type metadata
  (`list_elem_type_name` / `set_elem_type_name` /
  `map_value_type_name`) is stamped as `"any"` on the literal header
  so downstream destructor dispatch picks the right release path.
  `Map<any, V>` (any-typed keys) is intentionally out of scope because
  the rehash dispatch (`__ry_ht_rehash_i64` / `_f64` / `_str`) has no
  16-byte struct variant; mixed-key annotations continue to be
  rejected at the strict same-type check. `Map<str, int> = {...}` and
  other concrete-element annotations continue to enforce strict type
  equality. (#1884)
