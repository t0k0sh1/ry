### Changed

- `to_float(str)` now returns `Result<float, Error>` instead of `float`, matching the shape of `to_int(str)`. Invalid input previously returned `0.0` silently; it now returns `Err(Error(...))`. Empty strings, non-numeric content, and out-of-range values are reported as errors. **Breaking change**: existing code must unwrap the `Result` (e.g., via `match` or `?`). (#806)
