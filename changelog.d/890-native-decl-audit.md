### Fixed

- Corrected stdlib `@native` declaration return types that had silently drifted from their codegen dispatcher implementations (#890):
  - `items(map: Map<str, int>)` now declared as `-> List<(str, int)>` (was `-> List<int>`)
  - `enumerate(values: List<int>)` now declared as `-> List<(int, int)>` (was `-> List<int>`)
  - `zip(values: List<int>, other_values: List<int>)` now declared as `-> List<(int, int)>` (was `-> List<int>`)

  The dispatchers (`emitCollOp_items`, `emitBuiltinQuery` for `enumerate`/`zip`) always returned lists of tuples; only the declarations were wrong. No behavior change — this corrects the stdlib documentation to match reality.
