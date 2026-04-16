### Added

- `tests/spec/concurrency_stress.test.ry`: stress tests for `@parallel for` with Map/Set captures (CoW semantics), GC collect() during parallel execution, nested `@parallel for`, many `thread_spawn` workers sharing a str capture, and Lock high-contention (4 threads × 2000 iterations) (#872)
- `tests/test_runtime_arc_contention_stress.cpp`: C++ GoogleTest suite exercising concurrent atomic `retain`/`release` on a single ARC header (16 threads × 10,000 iterations); part of the required `build-tsan/ry_tests` gate (#872)
- `tests/test_runtime_lock_stress.cpp`: C++ GoogleTest suite for `__ry_lock_acquire`/`release` under high contention (8 threads × 10,000 iterations, sequential reacquire, and independent-lock baselines) (#872)

### Changed

- `ConcurrencySpecSuite` (in-process `@parallel for` / async spec suite) is now enabled under ASan builds; the `DISABLED_` guard added in commit `fb010ea` was removed after #630's atomic-ARC fix resolved the root cause (non-atomic ARC ops racing with ASan shadow-memory interceptors) (#872)
