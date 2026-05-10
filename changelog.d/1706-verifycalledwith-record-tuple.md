### Added

- `verifyCalledWith(name, args...)` now accepts **record** and **tuple**
  arguments whose fields / elements are all in `{int, float, bool, str}`.
  - Records are compared by **declared type name** plus field-by-field
    equality. Two records with structurally identical fields but
    different declared names (e.g. `Point(1, 2)` vs `Vec(1, 2)`) do not
    match and are rejected at compile time when the parameter type is
    fixed.
  - Tuples are compared by **arity** plus element-by-element equality.
    Tuples with different arity do not match and are rejected at
    compile time.
  - Each field / element is per-slot deep-snapshotted at call time
    (independent copies; `str` slots ARC-retained) and compared
    byte-exactly for `int` / `float` / `bool` and via length+`memcmp`
    for `str` (NUL-safe). LLVM struct padding is sidestepped by
    serializing each slot to an i64 value array plus an i8 kind array
    instead of memcmp'ing the raw struct.
  - Records or tuples whose fields / elements include nested
    collections (`List<T>` / `Set<T>` / `Map<K, V>`), nested records,
    nested tuples, or function values are rejected at compile time and
    are tracked for v0.0.x follow-up. (#1706)
