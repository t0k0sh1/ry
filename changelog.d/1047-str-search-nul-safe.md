### Fixed

- `contains`, `starts_with`, `ends_with`, and `find` now honour embedded NUL bytes instead of truncating at the first `\0` (#1047).
