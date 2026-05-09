### Added

- `expect(actual).toBeCloseTo(expected)` /
  `expect(actual).toBeCloseTo(expected, decimals)` — new test matcher
  for float approximate equality, modeled on Jest's `toBeCloseTo`.
  Asserts `|actual - expected| < 0.5 * 10^-decimals`, which makes
  `expect(0.1 + 0.2).toBeCloseTo(0.3)` pass even though strict
  `toEq` (FCmpOEQ) does not. `decimals` defaults to `2` and must be
  a non-negative integer literal in `[0, 15]` (the upper bound is
  the f64 mantissa precision; larger values would underflow the
  threshold to `0` and silently degrade to strict equality). Both
  `actual` and `expected` accept `int` or `float`, and mixed
  combinations (e.g. `expect(1).toBeCloseTo(1.0)`) are promoted to
  `f64` before comparison. Non-numeric operands and non-literal
  `decimals` are rejected at compile time. (#1675)
