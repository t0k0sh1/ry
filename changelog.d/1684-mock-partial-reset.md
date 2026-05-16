### Added

- Added `mockClear(name)`, `mockReset(name)`, and `mockResetAll()` to
  the testing framework for partial mock state reset within an `it`
  block (Jest / Vitest compatible). `mockClear` resets the call count
  while keeping the mock active; `mockReset` removes a single mock and
  restores the original implementation; `mockResetAll` removes every
  mock currently registered, equivalent to the automatic cleanup that
  runs at the end of each `it` block but explicit and usable
  mid-block. All three accept the function name as a string (same
  convention as `verify`) and are no-ops when the name is not
  currently mocked. (#1684)
