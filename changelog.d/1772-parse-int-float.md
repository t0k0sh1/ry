### Added

- Added `parseInt(value: str) -> Result<int, Error>` and `parseFloat(value: str) -> Result<float, Error>` as explicit fallible string-parsing APIs. They mirror the parsing behavior of `toInt` / `toFloat` (which are unchanged) and emit `parseInt:` / `parseFloat:` prefixed diagnostics, providing a safe replacement path before any future rename of `toInt` / `toFloat`. (#1772)
