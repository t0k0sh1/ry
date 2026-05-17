### Added

- Added `@timeout(ms)` testing directive for per-test millisecond-precision
  timeouts. `@timeout(N) @it("...")` aborts the test if its body runs
  longer than `N` milliseconds, marks it as `failed` with a
  "(timeout after Nms)" suffix, and continues execution with the next
  test. This replaces the previous "alarm + `_exit(124)`" behavior for
  affected tests, which terminated the entire test process and lost all
  subsequent test results. The `ms` argument must be a **positive integer
  literal**; zero, negative, non-literal, or non-integer arguments are
  rejected at compile time. Combining `@timeout` with `@each` or
  `@property` is a compile error in this release (MVP scope). The timer
  is delivered via `setitimer(ITIMER_REAL, ms)` and `siglongjmp` from the
  signal handler — the test runner stays single-threaded and TSan-safe.
  Known limitation: on timeout, ARC release is skipped for objects
  allocated inside the test body (leaks are reclaimed at process exit
  but may be flagged by leak detectors). (#1688)
