### Added

- `using` statement for scope-based resource release. `using f =
  open(path, "r"): ...` binds `f` to the block body and calls `close(f)`
  automatically on every exit path: normal block end, `return`, `?`
  propagation, and `break` / `continue`. When the initializer itself
  propagates an error via `?`, no binding is established and no `close`
  is invoked. Nested `using` releases resources in reverse order of
  acquisition. The current scope is `io.File`; passing any other type
  produces the compile error `using requires an io.File value`. Panic /
  uncaught-runtime-error paths are tracked separately. (#1817)
