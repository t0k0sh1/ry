### Fixed

- `Result<JsonValue, Error>` returned by `json.get` / `json.at` no longer
  sneaks past JSON type checks via metadata alone. `isJsonValue()` now
  also requires the underlying LLVM value to be a pointer, so passing a
  `Result` to `kind` / `stringify` / `get` / `at` produces the existing
  "requires a JsonValue argument" diagnostic instead of an LLVM IR verify
  error. `to_str(result)` and `print(result)` still work and format as
  `Ok(...)` / `Err(...)` via the generic `valueToString` path (#805).
- Using `List` / `str` / `Map` / `Set` (or any other ptr-backed value) as
  a boolean condition in `if` / `while` / `when` or under the unary `not`
  operator now produces a clear compile-time error suggesting
  `length(x) > 0` or `not is_empty(x)`, replacing the previous
  `icmp ne ptr, i0 0` IR verify failure (#818).
- `exit(0)` followed by more statements no longer triggers
  `Terminator found in the middle of a basic block`. `emitExit()` now
  switches to a fresh dead basic block so trailing IR lands on a valid
  (unreachable) block and LLVM DCE removes it during optimization (#821).
