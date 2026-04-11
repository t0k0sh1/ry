### Added

- `math.round(x, digits)`, `math.floor(x, digits)`, and `math.ceil(x, digits)`
  overloads for rounding a `float` to a given number of decimal places,
  returning a `float`. Negative `digits` rounds to powers of ten
  (`round(1234.5, -2) == 1200.0`). The two-argument forms reuse C99
  half-away-from-zero semantics so the result matches the one-argument
  `round()` applied to the scaled value — note this differs from Python's
  banker's rounding (`round(2.675, 2) == 2.68`, not `2.67`). `NaN` and `±Inf`
  pass through unchanged. (#842)
- `math.log(x, base)` overload for computing a logarithm with an arbitrary
  base, defined as `log(x) / log(base)`. Domain errors on either argument
  propagate as `NaN` or `-Inf`. (#842)
- `math.pow(x, y)` overload for `(int, int) -> int` using fast-exponentiation
  (O(log y)). A negative exponent raises a runtime error
  (`pow() integer exponent must be non-negative`). Overflow wraps silently,
  matching Ry's existing integer arithmetic model. (#842)
