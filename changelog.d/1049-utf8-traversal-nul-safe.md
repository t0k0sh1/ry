### Fixed

- `substring`, `char_at`, `reverse`, `split("", "")`, `for c in str:`, and `enumerate(str)` now honour embedded NUL bytes instead of truncating at the first `\0` (#1049).
