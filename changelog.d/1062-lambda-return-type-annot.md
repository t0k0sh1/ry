### Fixed

- Lambda with explicit return type annotation (e.g. `(a, b) -> int => a + b`)
  now correctly coerces `any`-typed body expressions to the declared return
  type. Previously this failed at compile time when lambda parameters were
  untyped (which default to `any`), blocking the common
  `reduce(xs, (a, b) -> int => a + b)` pattern. Fix applies to both
  expression-body and block-body lambdas, and to `return` statements in
  regular functions. (#1062)
