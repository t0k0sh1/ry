### Fixed

- `thread_join(t)` now returns the worker's value wrapped in `Ok(v)`
  instead of always `Ok(0)`. Workers using an expression-bodied lambda
  may return `int`, `float`, `bool`, or `Unit`. Joining an
  already-joined thread returns `Err("thread already joined")`. ARC
  types (`str`, `List`, `Map`, `Set`, records) and sum types
  (`Option`, `Result`, enums), block-bodied lambdas with a non-`Unit`
  return value, and panic-to-`Err` propagation remain unsupported and
  are tracked as follow-up issues. (#828)
