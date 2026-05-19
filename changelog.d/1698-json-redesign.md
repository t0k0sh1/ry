### Changed

- **BREAKING**: Redesigned the `json` module around the `any` type. The
  opaque `JsonValue` handle and its 13 low-level accessors (`parse`,
  `get`, `at`, `toStr`, `toInt`, `toFloat`, `toBool`, `kind`, `len`,
  `keys`, `stringify(JsonValue, ...)`, `jsonFree`) are removed without a
  deprecation period. The new API consists of four entry points:
  - `load(text: str) -> Result<any, Error>` parses JSON into a tag-typed
    `any` payload (`Null` / `Bool` / `Int` / `Float` / `Str` /
    `List<any>` / `Map<str, any>`).
  - `loadAs[T](text: str) -> Result<T, Error>` is a generic wrapper that
    parses and then coerces to `T` via the existing
    `let v: T = anyVal` slot-coercion path. Supported `T` in this
    release: `int` / `float` / `str` / `bool` and homogeneous
    `List<...>` / `Map<str, ...>` of those primitives. `T = record`,
    `T = Set<...>`, `T = Option<...>`, and `T = Result<...>` are not
    supported in this release and surface as a runtime type-mismatch
    `Err` from the coerce step.
  - `stringify(value: any) -> str` produces compact JSON.
  - `stringify(value: any, indent: int) -> str` pretty-prints with the
    given indent width (`indent < 0` falls back to compact form).

  Lifetime of the parsed payload is now driven by codegen's standard ARC
  machinery — the `jsonFree` early-return discipline is no longer
  required. Map iteration order for `stringify` is the underlying map's
  insertion order. Tags that JSON cannot represent (`Set`, `Record`,
  `Enum`, and `Map` keyed by non-`str`) panic from `stringify` since the
  return type is `-> str` and offers no `Result` channel. File-handle
  overloads (`load(f: File)` / `dump(value, f: File)`) are intentionally
  out of scope for this PR and will land alongside `io.File`. (#1698)
