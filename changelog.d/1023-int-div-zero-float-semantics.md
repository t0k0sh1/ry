### Fixed

- `int / 0` now follows IEEE 754 and returns `inf` (or `-inf` for negative
  dividends; `nan` for `0 / 0`), consistent with `10.0 / 0` and `10 / 0.0`
  which already returned `inf`. The `/` operator is documented as always
  returning `float`, so integer operands are promoted before division and
  IEEE 754 semantics apply. This reverts the integer-specific runtime-error
  guard added in #754; `//` (floor division) and `%` (modulo) retain
  integer semantics and still raise a runtime error on a zero divisor for
  integer operands (#1023).
