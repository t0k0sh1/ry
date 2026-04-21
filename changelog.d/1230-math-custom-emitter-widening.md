### Fixed

- `math` custom emitters (`floor` / `ceil` / `round` / `log` / `pow` mixed-type) now accept `int` arguments via implicit `int → float` widening, completing the fix started in #1193 for table-driven `@native` dispatch. Exact-match precedence is preserved: `pow(2, 3)` still returns int `8`, while `pow(2.0, 3)` and `pow(2, 3.0)` now return float `8.0` instead of erroring (#1230)
