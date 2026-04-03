### Added

- `and_then` and `map` method chaining for `Result` type, enabling flat error handling without nested `match` (#597)
- Parser now accepts keyword tokens (e.g., `and`, `or`, `not`) as method names after `.` for UFCS calls
