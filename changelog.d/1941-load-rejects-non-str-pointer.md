### Fixed

- `json.load[T]()` now correctly rejects `List`, `Map`, `Set`, closure, and other non-`str` pointer-typed arguments at compile time with `load[T]() requires a str or File argument`. Previously the non-`File` branch only checked for LLVM opaque pointer type, which let any reference value through and reached `__ry_json_parse_to_any` as if it were JSON text (invalid read under ASan/UBSan). The new guard uses `isStringValue` so the non-`File` branch is symmetric with the `isFile` branch. (#1941)
