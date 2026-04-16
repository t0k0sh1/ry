### Changed

- `str` now stores an explicit byte length (`StringHeader` layout: `strong_count`, `weak_count`, `byte_len` prefix before the character data). The operations `byte_len`, `length`, `==`, `!=`, `<`, `>`, `+`, `*`, and Map/Set key lookup are fully NUL-safe; strings containing embedded NUL bytes (`\0`) are no longer silently truncated. (#1022)

### Fixed

- `bytes_to_str` now preserves embedded NUL bytes instead of rejecting them. (#1022)
- `weak str` upgrade no longer returns `None` instead of `Some` when the strong reference is alive; codegen now uses the correct `STRING_HEADER_SIZE` (24) offset to reach `strong_count` instead of the collection `ARC_HEADER_SIZE` (16). (#1022)
