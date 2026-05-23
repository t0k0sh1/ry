### Added

- `io.open(path, mode)` now accepts `"ab"` (append binary), completing
  the binary-mode trio alongside `"rb"` / `"wb"` (added in #1848) and
  restoring parity with the text-mode triple `"r"` / `"w"` / `"a"`.
  `"ab"` maps to `O_WRONLY | O_CREAT | O_APPEND` (same POSIX flags as
  `"a"`); writes always go to end-of-file, the file is created if
  missing. The invalid-mode error message now reads `(must be "r",
  "w", "a", "rb", "wb", or "ab")`. (#1862)
