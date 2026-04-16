### Fixed

- `replace` now honours embedded NUL bytes in the haystack, needle, and replacement instead of truncating at the first `\0` (#1048).
