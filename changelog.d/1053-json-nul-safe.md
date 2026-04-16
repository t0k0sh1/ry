### Fixed

- `json.parse` now accepts `\u0000` in string values and object keys (previously rejected with an error) (#1053)
- `json.stringify` now emits `\u0000` for embedded NUL bytes instead of truncating the string (#1053)
- `json.to_str`, `json.get`, and `json.keys` now correctly handle strings and keys containing embedded NUL bytes (#1053)
