### Fixed

- `math.abs(INT_MIN)` now traps with `runtime error: integer overflow` and exits with status 1 instead of silently returning `INT_MIN`. The post-condition `abs(x) >= 0` is preserved by detecting the unrepresentable result before negation. (#1591, #1592)
- `INT_MIN // -1` and `INT_MIN % -1` now trap with `runtime error: integer overflow` and exit with status 1 instead of returning poison from LLVM's `sdiv` / `srem`. The new check matches the existing trap behavior of `+` / `-` / `*` / unary `-`. (#1591, #1592)
