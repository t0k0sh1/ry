### Changed

- `to_int(str)` now returns `Result<int, Error>` instead of bare `int`, properly detecting invalid input (#543)
