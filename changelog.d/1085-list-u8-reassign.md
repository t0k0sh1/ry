### Fixed

- Reassignment to a `List<u8>` (or other `List<T>` with low-level integer element type) variable now propagates the element suffix so `bytes_to_str`, `write_bytes`, and TLS/TCP byte-list consumers accept the list, matching the declaration-time behavior from #1079 (#1085)
