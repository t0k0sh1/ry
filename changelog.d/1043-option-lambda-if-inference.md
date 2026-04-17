### Fixed

- Lambda return-type inference now unifies `Some(T)` and `None()` branches in
  if-expr, matching the `Ok`/`Err` behavior added in #1024. Previously
  `(x: int) => if cond => Some(x) else None()` failed with `undefined function: None`,
  and even `(x: int) => Some(x)` alone failed with a return-type mismatch (#1043)
