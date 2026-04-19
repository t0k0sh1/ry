### Fixed

- `x: int = 2 ** 3`, `x: int = 10 / 2`, and `x **= n` / `x /= n` (where `x: int`) now compile successfully. `**` and `/` still return `float`, but high-level `int` and `float` variables implicitly accept cross-type values at declaration, reassignment, and compound assignment (#1192).

### Changed

- `x: float = 10` (int → float widening) and `x: int = 3.14` (float → int truncation toward zero) are now accepted without an explicit `as` cast. The same coercion applies to record field compound assign (`r.n **= 2`) and collection-element compound assign (`xs[0] **= 2`, `m["k"] **= 2`). Low-level numeric types (`i64`, `f32`, etc.) still require exact type match, and narrowing is still rejected at function arg / return / if-expr branch sites (#1192).
