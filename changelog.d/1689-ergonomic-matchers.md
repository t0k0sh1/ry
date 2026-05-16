### Added

- Added ergonomic matchers `toBeBetween(min, max)` and
  `toBeOneOf(list)` to the testing framework. Both express common
  assertion patterns that previously required verbose combinations:
  `expect(x).toBeBetween(1, 10)` replaces
  `expect(x).toBeGreaterThanOrEq(1)` plus
  `expect(x).toBeLessThanOrEq(10)`, and
  `expect(status).toBeOneOf([200, 201, 204])` replaces the
  argument-order-reversed `expect([200, 201, 204]).toContain(status)`.
  `toBeBetween` is inclusive on both bounds and accepts `int` /
  `float` operands (mixed int/float is allowed); `toBeOneOf` accepts a
  `List` whose element type matches the actual value (`int`, `float`,
  `str`, or `bool`). Both emit `codegenError` for type or shape
  mismatches. (#1689)

### Fixed

- Fixed the formatter dropping extra arguments on `expect` matchers
  with more than one argument. Previously `expect(x).toBeCloseTo(1.0,
  4)` was reformatted as `expect(x).toBeCloseTo(1.0)`, silently
  discarding the `decimals` argument; the same gap would have affected
  the new `toBeBetween(min, max)`. The formatter now emits every
  argument in `ExpectStmt.extra_args` alongside the primary
  `expected`. (#1689)
