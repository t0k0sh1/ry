### Fixed

- `List<str>` and `Set<str>` literals now correctly retain locally-constructed str elements, preventing dangling pointers when source variables go out of scope. Mirrors the `Map<str, str>` literal fix from #1353. (#1354)
