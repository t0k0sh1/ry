### Added

- `json.load(f: File) -> Result<any, Error>` and
  `json.dump(f: File, value: any) -> Result<Unit, Error>` /
  `json.dump(f: File, value: any, indent: int) -> Result<Unit, Error>`
  File-handle overloads. `load(f)` fuses `io.readAll(f)? → load(text)?`
  and `dump(f, value [, indent])` fuses `stringify(value [, indent]) →
  io.writeText(f, ...)?` so callers can avoid the intermediate `str`
  buffer. Io errors (closed handle, write failure, etc.) are propagated
  as `Err(Error{message})` alongside the existing parse-error channel.
  Argument order follows the `io` module convention (File first), and
  the `dump` overloads accept `indent < 0` as a fall-through to compact
  form, matching `stringify(value, indent)`.

  `loadAs[T](f: File)` was deliberately **not** added: Ry currently
  does not support overload-by-arg-type for generic user-defined
  functions — a second `fn loadAs<T>(...)` silently shadows the first
  — so a File-form `loadAs[T]` would have broken `loadAs[T](text)`. For
  File inputs, use `load(f)?` and assign to a `T`-typed variable to
  drive the same `any → T` coerce that `loadAs[T](text)` performs;
  follow-up work to lift the generic-overload restriction is tracked
  separately. (#1854)
