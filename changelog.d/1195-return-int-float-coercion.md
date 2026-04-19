### Changed

- Function return values now support implicit `int` ↔ `float` coercion, matching
  the behavior at variable declaration and reassignment sites. `-> float`
  functions accept `int` return values (widening), and `-> int` functions
  accept `float` return values (truncation toward zero). Low-level numeric
  types (`i64`, `f32`, etc.) still require explicit `as` casts. (#1195)
