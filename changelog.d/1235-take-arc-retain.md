### Fixed

- `take(lst, n)` now ARC-retains reference-typed elements, preventing
  use-after-free when the source list is released (same defect class
  as #1204 for `emitListSlice`). (#1235)
