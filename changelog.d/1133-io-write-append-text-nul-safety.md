### Fixed

- `io.write_text` and `io.append_text` silently truncated content at the first
  embedded NUL byte because they used `fputs(content, f)`. They now use
  `fwrite(content, 1, stringByteLen(content), f)` for binary-transparent writes,
  matching the already-safe `io.write_bytes` path. `fclose` return code is still
  checked so buffered-write errors surface as `Err` (#1133).
