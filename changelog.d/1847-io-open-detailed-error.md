### Fixed

- `io.open(path, mode)` now surfaces the detailed runtime error message
  on failure instead of the static string `"open failed"`. Concretely,
  `Err(e).message` now carries `"open: cannot open '<path>' in mode
  '<mode>'"` / `"open: invalid mode '<mode>'"` / `"open: path contains
  an embedded NUL byte"` / `"open: mode contains an embedded NUL byte"`
  (the strings set by `setLastError` in `__ry_io_file_open`). Previously
  `emitFileOpen` used `emitPtrToResult(..., "open failed", rk_file)`
  which embedded a static error string and discarded the runtime
  message. The fix switches to `wrapPtrAsResult(ptr)` (default
  `errFnName = "__ry_get_last_error"`) + explicit
  `addResourceKind(res, rk_file)`, matching the pattern already used by
  `emitFileReadAll` and other `io` functions. (#1847)
