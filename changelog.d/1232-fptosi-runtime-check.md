### Fixed

- `as int` / `as i64` / `as i32` / `as i16` / `as i8` / `as u8` /
  `as u16` / `as u32` / `as u64` casts and the implicit `float → int`
  coercions (`x: int = 1.0 / 0.0`, compound assignments such as `x /= 0`
  where `x: int`) now raise a runtime error and exit with status 1 when
  the source value is `NaN`, `±inf`, or outside the target integer's
  representable range. Previously these silently produced LLVM poison
  (undefined behavior) via `fptosi` / `fptoui`. (#1232)
- `floor()`, `ceil()`, `round()`, and `trunc()` now correctly accept
  `-9.223372036854776e+18` (exactly `INT64_MIN`) as input. The previous
  `fabs(x) >= 2^63` overflow guard incorrectly rejected this value, and
  also missed cases where the result rounded out of range (e.g.
  `ceil(9.22e+18)` rounding past `INT64_MAX`). (#1232)
