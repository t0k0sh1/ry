### Fixed

- `List + List` concatenation now ARC-retains reference-typed elements,
  preventing use-after-free when either source list is released (same
  defect class as #1204 for `emitListSlice` and #1235 for `take()`). (#1236)
