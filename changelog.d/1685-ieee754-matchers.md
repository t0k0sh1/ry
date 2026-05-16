### Added

- Added IEEE 754 special-value matchers `toBeNaN()`, `toBeInfinity()`,
  and `toBeFinite()` to the testing framework. Because `NaN == NaN` is
  false in IEEE 754, `expect(0.0/0.0).toEq(NAN)` always failed and
  tests had to rely on indirect idioms such as
  `expect(x == x).toBeFalse()`. The new matchers express the intent
  directly: `expect(0.0/0.0).toBeNaN()`,
  `expect(1.0/0.0).toBeInfinity()` (matches both `+∞` and `-∞`), and
  `expect(3.14).toBeFinite()`. All three accept `float` only and emit
  a `codegenError` for other types. Complements stdlib `math.isNan` /
  `math.isInf` (assertion vs. conditional branch). (#1685)
