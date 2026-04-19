### Fixed

- `@native` stdlib functions (`math.sqrt`, `math.sin`, `math.cos`, `math.tan`, `math.asin`, `math.acos`, `math.atan`, `math.atan2`, `math.hypot`, `math.exp`, `math.log2`, `math.log10`, and other table-driven natives) now accept `int` arguments with implicit `int → float` widening, matching user-defined function overload resolution. Exact-match precedence is preserved: `pow(2, 3)` still dispatches to the `(int, int) -> int` overload (#1193)
