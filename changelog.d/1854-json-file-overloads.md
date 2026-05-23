### Added

- `json.load(f: File) -> Result<any, Error>`,
  `json.dump(f: File, value: any) -> Result<Unit, Error>` /
  `json.dump(f: File, value: any, indent: int) -> Result<Unit, Error>`,
  and `json.loadAs[T](f: File) -> Result<T, Error>` File-handle
  overloads. `load(f)` fuses `io.readAll(f)? → load(text)?` and
  `dump(f, value [, indent])` fuses
  `stringify(value [, indent]) → io.writeText(f, ...)?` so callers can
  avoid the intermediate `str` buffer. `loadAs[T](f)` reuses the
  existing `any → T` coerce path (same supported `T` set as the str
  overload: `int` / `float` / `str` / `bool` / `List<any>` /
  `Map<str, any>`). Io errors (closed handle, write failure, etc.) are
  propagated as `Err(Error{message})` alongside the existing parse-error
  channel. Argument order follows the `io` module convention (File
  first), and the `dump` overloads accept `indent < 0` as a fall-through
  to compact form, matching `stringify(value, indent)`. (#1854)

### Fixed

- Generic user-defined function dispatch on the **explicit type-args**
  path (`f[T1, T2, ...](args)`) now resolves the correct overload by
  substituting the type arguments into each candidate template's
  parameter signature and comparing the substituted signature against
  the call-site argument types. Previously this path hard-coded
  template index 0, silently routing every explicit-`[T]` call to the
  first declared overload regardless of arg types — for example
  `loadAs[int](file)` would route through the `loadAs[T](text: str)`
  body and fail with a confused type mismatch. The inferred-type-args
  path (`f(args)`) was already correct in #1874; this fix closes the
  remaining gap on the explicit path. Single-template programs are
  unaffected (the legacy templateIndex=0 fast path is preserved).
  (#1854)
