### Fixed

- `for a, b in setOfTuples:` no longer fails with "for loop destructuring requires a list of tuples". The multi-variable for-loop binding path now handles `Set<(T, U)>` alongside maps and lists of tuples, and source-level element type names on `Set<T>` annotations are propagated for non-primitive inner types (collections, records, enums, tuples). (#1350)
