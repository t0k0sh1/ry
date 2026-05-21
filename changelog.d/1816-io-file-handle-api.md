### Added

- File handle API for the `io` module: `open(path, mode)`,
  `readAll(f)`, `readLine(f)`, `writeText(f, s)`, and `close(f)`.
  `open` returns `Result<File, Error>`; valid modes are `"r"`, `"w"`,
  and `"a"`. `readLine` returns `Result<Option<str>, Error>` — `Ok(None)`
  signals EOF cleanly. `File` is an opaque ARC resource handle: the file
  is closed automatically when the last reference drops; calling `close`
  explicitly allows earlier release and is idempotent. Path and mode
  arguments are checked for embedded NUL bytes at the runtime boundary.
  (#1816)
