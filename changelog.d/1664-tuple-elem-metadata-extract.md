### Fixed

- Numeric tuple-field access (`xs[0].1`) and chained subscripts
  (`xs[0].1[0]`) on `List<(K, V)>` results now carry per-component
  metadata through the extraction. Before this fix, the IndexExpr List
  path called `propagateTypeMeta(elemTypeName, elem)` with
  `elemTypeName = "(K, V)"`, but `propagateTypeMeta` is single-value by
  design (per `.claude/rules/codegen-type-and-metadata.md` —
  *"propagateTypeMeta is single-value; callers decompose tuples"*),
  so the tuple components received no metadata. The downstream
  FieldAccessExpr numeric-index arm then emitted a bare
  `CreateExtractValue` that fell through to the `str` dispatch, so
  `print(enumerate(xs)[0].1[0])` (for `xs: List<List<int>>`) raised
  `str does not support index access` at codegen — the same shape the
  for-loop destructure path already handled correctly via
  `splitTupleSig` (`src/codegen_stmt_loop.cpp`). The fix stamps the
  tuple sig onto the loaded element via the
  `ValueMetadata::source_type_name` channel (the same lossless slot
  used for `Result<T, E>` / `Option<T>` since #1638) at the IndexExpr
  List path in `src/codegen_expr_literal.cpp`, then decomposes
  per-component via `splitTupleSig` and propagates the matching
  component's name onto the extracted field at the FieldAccessExpr
  numeric-index arm in the same file. `enumerate(xs)` and
  `zip(xs, ys)` work end-to-end because their codegen sites already
  stamp `list_elem_type_name = "(int, T)"` / `"(T, U)"`. The analogous
  `items(m)` case is tracked separately as #1659 / PR #1665 because
  the gap there is in the `items()` emitter (it does not stamp
  `list_elem_type_name` at all). (#1664)
