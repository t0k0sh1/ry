### Added

- Scientific notation float literals (`1e10`, `1.5e-3`, `2.5E+2`, `1_000e3`). Overflowing exponents (`1e400`) produce `+Inf` to match the runtime `to_float` converter (#819)

### Fixed

- `u64` maximum value (`18446744073709551615`) now parses successfully when written with a `u64` suffix or under a `u64` / unsigned type annotation. Hex and binary forms (`0xFFFFFFFFFFFFFFFFu64`, `0b11...1u64`) are accepted too; range checking for `int` / `i64` / `u8`-`u32` happens in codegen against the target type (#807)
