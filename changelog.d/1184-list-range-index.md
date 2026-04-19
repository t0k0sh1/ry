### Fixed

- `lst[a..b]` (list range-indexing) no longer crashes at codegen with `ICmp`
  type mismatch between `ptr` and `i64`. The indexing path now detects a
  `RangeExpr` as the first index, negative-wraps each bound against the list
  length, and routes to the shared slice helper. Semantics match
  `slice(lst, a, b + 1)` (inclusive, out-of-bounds clamped, negatives wrap).
  (#1184)
