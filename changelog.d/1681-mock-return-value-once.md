### Added

- Added `mockReturnValueOnce(name, value)` to the testing framework for
  Jest-compatible per-call queued mock returns. Each call to
  `mockReturnValueOnce` enqueues `value` for the named function; the next
  call to that function dequeues and returns the head of the queue.
  When the queue empties, calls fall back to the function set via
  `mock(name, replacement)` (if any), then to the original implementation
  — matching Jest's fallback chain. All return types are supported
  (primitives, `str`, `List` / `Map` / `Set`, records, `Result`,
  `Option` including bare `None`). The first argument must be a string
  literal naming the function; overloaded functions, unknown names,
  Unit-returning functions, and value type mismatches are rejected at
  compile time. `mockClear(name)` preserves the queue (only resets the
  call counter), while `mockReset(name)` and `mockResetAll()` discard
  the queue. Queues are auto-cleared at the end of each `it` block.
  Note: `verify(name)` counts only queue-served and default-mock-served
  calls; calls that fall through to the original implementation are not
  counted (matching `mockReset` semantics). (#1681)
