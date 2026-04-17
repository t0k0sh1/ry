### Fixed

- Regex literal `\0` escape now produces a NUL byte in the pattern, matching string literal behavior (`/a\0b/` now correctly matches `"a\0b"`) (#1076)
