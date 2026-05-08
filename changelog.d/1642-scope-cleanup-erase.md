### Fixed

- ARC release IR is now correctly emitted on every exit path of a scope
  that has both an early-exit (`return` / `break` / `continue` / `?`)
  and a natural-exit path. Previously, `CodeGen::emitScopeCleanupToDepth`
  emitted the release IR and then erased the alloca from the relevant
  ARC side-table (`arc_managed_vars_`, `weak_managed_vars_`,
  `arc_field_record_vars_`, `arc_tagged_union_vars_`); when an early-exit
  cleanup ran first, the natural-exit `popScope()` found no entry and
  emitted no IR on the fall-through path, leaking one ARC header per
  iteration in loops with conditional early returns. The side-table
  erase responsibility has moved into `popScope` so each runtime path
  through the scope releases exactly once. The bug pre-dated #1640 and
  affected every ARC side-table, not just the new tagged-union one.
  (#1642)
