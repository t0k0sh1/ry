### Fixed

- `Err(e):` bindings on `Result<ARC-type, str>` (e.g.
  `Result<List<int>, str>`, `Result<Map<str, int>, str>`,
  `Result<Set<int>, str>`) now preserve `str` typing on `e`. Previously,
  metadata from the Ok side (collection element-type) leaked through the
  bulk `propagateMeta(subjectAlloca, varAlloca)` call in
  `emitPatternBindings`, making `e` look like a collection: `"prefix: " + e`
  failed compilation with `operator '+' not supported between str and
  non-str types`, and `f"prefix: {e}"` typechecked but crashed at runtime
  with a SIGSEGV when the `Err` arm executed (the str-pointer payload was
  dispatched through the list `valueToString` path). The fix introduces a
  lossless `source_type_name` field on `ValueMetadata`, stamped by
  `propagateTypeMeta` at the `Result<...>` / `Option<...>` / `T?` branch
  entries, and routes the Ok/Err/Some pattern arms through
  `propagateTypeMeta(innerSig, varAlloca)` instead of bulk
  `propagateMeta`. Each binding now receives only the metadata that
  corresponds to its actual type. `Result<int, str>`, `Result<int, int>`,
  and the Error-typed `Err(e)` paths are unaffected. (#1638)
