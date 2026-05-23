### Added

- `io.open(path, mode)` now accepts `"rb"` (binary read) and `"wb"`
  (binary write) in addition to the existing `"r"` / `"w"` / `"a"`
  text modes. The internal `fopen_nofollow` helper already mapped
  `"rb"` / `"wb"` to `O_RDONLY` / `O_WRONLY | O_CREAT | O_TRUNC`; only
  the strcmp guard at the entry of `__ry_io_file_open` was rejecting
  them. The invalid-mode error message now reads `(must be "r", "w",
  "a", "rb", or "wb")` to reflect the extended set. This is a
  prerequisite for the future `readBytes(f: File)` / `writeBytes(f:
  File, bytes)` overloads (#1816 follow-up). Append-binary `"ab"`
  remains unsupported and is tracked separately in #1862. (#1848)
