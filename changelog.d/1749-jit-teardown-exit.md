### Fixed

- `ry test` / `ry run` / `ry -c` on Linux and macOS now bypass the C++
  static-destructor chain via `_exit(rc)` after a successful JIT run
  (gated on a `jitWasInitialized()` flag set inside
  `src/jit_runner.cpp` immediately after `LLJITBuilder().create()`
  succeeds). The existing triple-stage leak (`rtCleanup.release()`,
  `(void)jit.release()`, `(void)cg.release()`) already suppresses the
  `~LLJIT()` / `~CodeGen()` frames of the #1187 / #1657 LLVM ORC
  teardown family, but residual LLVM `ManagedStatic` / `llvm_shutdown`
  state run from `atexit` handlers intermittently aborted inside glibc
  `_int_malloc` heap consolidation (exit 134) after the test result
  had already been printed. Non-JIT exits (help printing, formatter,
  parse-time errors before any LLJIT instance is created) still run
  normal C++ teardown. (#1749)
