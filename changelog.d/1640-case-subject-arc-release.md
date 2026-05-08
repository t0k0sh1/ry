### Fixed

- `case` expressions and statements with `Result<T, E>` or `Option<T>`
  subjects no longer leak ARC headers across iterations. The struct
  alloca that materializes the subject value (`{i1, T, E}` for `Result`,
  `{i1, T}` for `Option`) is now registered with the new
  `arc_tagged_union_vars_` side-table so that scope cleanup releases
  the active payload slot at scope exit. Previously, the
  construction-time retain emitted by `buildOkValue` / `buildErrValue`
  / `buildSomeValue` had no balancing release on the subject alloca,
  so each `case` evaluation leaked one ARC header per ARC-managed
  active slot. The release dispatches on the runtime tag and only
  touches ARC-managed slots, so `Result<int, int>` and `Option<int>`
  remain zero-cost. (#1640)
