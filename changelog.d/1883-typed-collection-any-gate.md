### Fixed

- The compiler now rejects assignments from an `any` whose source type
  is unknown into a typed collection (`List<T>` / `Map<K, V>` /
  `Set<T>` where `T` / `V` is not `any`). The motivating hazard is
  `json.load(text)` returning `Result<any, Error>`: previously
  ```ry
  case load(text):
    Ok(v):
      xs: List<str> = v   # compiled cleanly, crashed at runtime
  ```
  compiled without a diagnostic, then either segfaulted (`List<str>` /
  `List<float>` — the 8-byte typed stride walked off the end of the
  16-byte `RyAny` payload) or silently produced garbage (`List<int>`
  read the `RyAny` tag bytes as the payload). The same trap applied to
  `Map<_, T>` and `Set<T>`. `emitVarDecl` now uses the
  `source_type_name` metadata stamped by `registerAnyManagedVar` to
  distinguish the legitimate roundtrip
  `xs: List<int> = ...; a: any = xs; ys: List<int> = a` (allowed —
  stamped) from the `case Ok(v):` extraction whose binding has no
  collection element metadata (rejected — empty source name). The
  diagnostic suggests `loadAs[T]` or per-element `case`, which were
  already the safe alternatives. `List<any>` / `Map<str, any>` /
  `Set<any>` annotations remain unconditional (the payload stride
  matches the destination). Round-trips whose source type is itself
  `any` (e.g. through a function returning `any`) are treated as
  ambiguous and deferred to `unwrapFromAny`'s runtime tag check,
  preserving shipped behavior. The same hazard in the reassignment
  path (`xs = v` after `xs: List<str>` is already declared) and across
  function argument / return boundaries with concrete-mismatched
  element strides is not yet covered and will be addressed in a
  follow-up. (#1883)
