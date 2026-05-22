### Fixed

- `net.accept()` and `net.tlsConnect()` now surface the detailed
  runtime error message on failure instead of the static strings
  `"accept failed"` / `"TLS connection failed"`. Concretely,
  `Err(e).message` now carries strings such as `"accept: timed out
  waiting for connection"`, `"accept: listener shut down"`,
  `"tlsConnect: cannot connect to host:port: <strerror>"`,
  `"tlsConnect: TLS handshake failed: <openssl>"`, and `"tlsConnect:
  certificate verification failed: <reason>"`. Previously `emitNetAccept`
  and `emitNetConnect` (TLS branch) used `emitPtrToResult(..., "...
  failed", rk_*)` which embedded a static error string and discarded
  the runtime message. The fix adds `setLastError` calls to
  `__ry_accept` (`runtime_net.cpp`) and `tls_handshake` /
  `__ry_tls_connect{,_resolved}` (`runtime_tls.cpp`, gated by a new
  `DEFINE_LAST_ERROR(tls)` thread-local channel exposed via
  `__ry_tls_get_last_error`), and switches both codegen sites to
  `wrapPtrAsResult(ptr, "__ry_<mod>_get_last_error")` +
  `addResourceKind(res, rk_<kind>)` — matching the pattern already used
  by `emitNetBind` / non-TLS `emitNetConnect` / `emitFileOpen`
  (#1847). (#1858)
