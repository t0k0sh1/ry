### Fixed

- `for x in xs[i]:` now snapshots the indexed collection via ARC retain, preventing
  use-after-free when the same slot is mutated (`append!`/`add`/`xs[i][k] = v`) inside
  the loop body. Extends the guard from #1021 (`VariableExpr`) and #1041 (`FieldAccessExpr`)
  to `IndexExpr` iterables. (#1091)
