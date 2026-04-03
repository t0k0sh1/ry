### Fixed

- `print()` now supports `Result` types directly, displaying `Ok(value)` or `Err(error)` (#612)
- `to_str()` now correctly converts `Result` and `Option` types to their string representation instead of returning the internal tag value (#611)
