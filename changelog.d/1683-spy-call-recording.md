### Added

- Added `spy(name)` to the testing framework for recording calls
  without replacing the implementation. Unlike `mock(name, replacement)`
  which fully replaces the function body, `spy` keeps the original
  implementation running and only adds call-count and argument-recording
  instrumentation around it. The argument is the function's name as a
  string literal; overloaded functions and non-existent names are
  rejected at compile time. `verify(name)` and
  `verifyCalledWith(name, args...)` work uniformly on spied functions
  (same internal call-recording registry as `mock`), and
  `mockClear(name)` / `mockReset(name)` / `mockResetAll()` apply
  identically. A function may be both mocked and spied across different
  `it` blocks; when both are active in the same block, `mock` takes
  precedence and the real implementation is bypassed. Spy registrations
  are automatically cleared at the end of each `it` block. (#1683)
