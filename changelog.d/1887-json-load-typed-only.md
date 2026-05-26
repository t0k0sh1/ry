### Changed

- **Breaking** — `json.load` is now typed-only. The pre-#1887
  non-generic overloads `load(text: str) -> Result<any, Error>` and
  `load(f: File) -> Result<any, Error>` have been removed because they
  exposed no safe accessor into the resulting `any`: callers had to
  reach the payload via an unchecked `xs: List<T> = v` cast that #1883
  later rejected at compile time. The remaining `loadAs[T]` API
  (#1852) was renamed to `load[T]`, consolidating the typed-deserialize
  path under a single name. Every JSON parse now picks an explicit
  type argument; `load[any]` is intentionally not supported and falls
  through `tryUnwrapFromAny`'s `non-record struct target not yet
  supported` rejection (use `load[Map<str, any>]` / `load[List<any>]`
  for the JSON-shape-typed equivalents). The error-message prefix
  produced by the coerce path flipped from `loadAs[...]: ...` to
  `load[...]: ...` to stay in sync with the API name. A direct call
  to `load(text)` without a type argument now emits a compile-time
  diagnostic that lists concrete-`T` alternatives instead of silently
  resolving to "undefined function: load". Migration:
  ```ry
  # Before (#1852-era)
  case loadAs[Map<str, int>](text):
    Ok(m): ...
    Err(e): ...

  # After (#1887)
  case load[Map<str, int>](text):
    Ok(m): ...
    Err(e): ...

  # Before (pre-#1852 untyped path — already discouraged after #1883)
  case load(text):
    Ok(v):
      m: Map<str, any> = v
      ...

  # After (#1887)
  case load[Map<str, any>](text):
    Ok(m): ...
    Err(e): ...
  ```
  (#1887)
