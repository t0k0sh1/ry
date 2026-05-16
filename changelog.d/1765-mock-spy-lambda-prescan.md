### Fixed

- `mock()` / `spy()` pre-scan now walks lambda bodies inside every
  `ExprPtr` slot, not just `CallStmt.args`. Targets defined inside a
  lambda stored in `AssignStmt.value`, `ReturnStmt.value`,
  `CallExpr.args` (nested at any depth), `IfStmt.condition`, or any
  other AST position are now detected, so the mock dispatch gate fires
  for callsites compiled before the lambda runs. (#1765)
