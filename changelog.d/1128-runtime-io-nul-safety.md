### Fixed

- Rejected embedded NUL bytes in path arguments of `io.read_text`, `io.write_text`,
  `io.append_text`, `io.delete_file`, `io.read_bytes`, and `io.write_bytes`; each
  now returns `Err(Error{ message: "<fn>: argument contains an embedded NUL byte" })`
  instead of silently truncating the C string and operating on an unintended file.
  `io.exists` returns `false` for such paths (no error channel available). Brings
  `io` to parity with the existing guards in `filesystem` and `path` (#1128).
