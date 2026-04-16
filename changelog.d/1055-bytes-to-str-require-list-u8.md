### Fixed

- `bytes_to_str()` and `write_bytes()` now reject non-`u8` list arguments at compile time instead of silently producing garbage output. Plain integer list literals like `[97, 0, 98]` use 64-bit element layout incompatible with the byte-list runtime; passing them previously caused corrupted output. Use `[97u8, 0u8, 98u8]` (explicit `u8` literals) or `to_bytes("...")` instead (#1055).
