### Fixed

- `bs: List<u8> = [97, 0, 98]` now compiles correctly; the `List<u8>` annotation propagates `u8` to each integer literal element so the list has 8-bit element stride and passes the `bytes_to_str` / `write_bytes` compile-time type gate (#1079)
