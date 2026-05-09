### Added

- `expect(actual).toBeCloseTo(expected)` /
  `expect(actual).toBeCloseTo(expected, decimals)` — new test matcher
  for float approximate equality, modeled on Jest's `toBeCloseTo`.
  Asserts `|actual - expected| < 0.5 * 10^-decimals`, which makes
  `expect(0.1 + 0.2).toBeCloseTo(0.3)` pass even though strict
  `toEq` (FCmpOEQ) does not. `decimals` defaults to `2` and must be
  a non-negative integer literal in `[0, 15]` (the upper bound is
  tied to practical `f64` precision; larger values no longer
  provide meaningful decimal-place guarantees because adjacent
  representable doubles differ by more than `0.5 * 10^-decimals`).
  Both
  `actual` and `expected` accept `int` or `float`, and mixed
  combinations (e.g. `expect(1).toBeCloseTo(1.0)`) are promoted to
  `f64` before comparison. Non-numeric operands and non-literal
  `decimals` are rejected at compile time. (#1675)
