### Fixed

- Unified the JIT trap-path exit across the C++ runtime so it matches
  the codegen-emitted trap path from #1838. A new shared helper
  `ry_runtime_trap_exit()` in `include/ry/runtime_alloc.hpp` is now used
  by all 16 `exit(1)` sites that previously lived in
  `src/runtime_any.cpp`, `src/runtime_utf8.cpp`, `src/runtime_json.cpp`,
  and `src/runtime_regex.cpp`. The helper calls `fflush(stdout)` +
  `fflush(stderr)` then `std::_Exit(1)` — bypassing the `atexit` chain
  (and the LLVM `ManagedStatic` destructors that ran on the still-live
  JIT heap, causing `free(): invalid pointer` SIGABRT before the
  expected `ExitedWithCode(1)` was observed). `CodeGen::emitRuntimeError`
  in `src/codegen_call_user.cpp` was retrofitted to emit matching
  `fflush(stdout)` / `fflush(stderr)` IR calls immediately before the
  existing `_Exit(1)` IR call, so panic messages and any preceding
  `print` output survive even when stdio buffering was line-buffered
  to a pipe. A new CI `lint` step ("Check for banned direct exit()
  calls in runtime") blocks regressions by rejecting any direct
  `exit(...)` / `std::exit(...)` in `src/runtime_*.cpp`; allowed forms
  (`_Exit`, `_exit`, `quick_exit`, and the codegen helpers
  `getStdlibExit` / `getStdlibImmediateExit`) are not affected.
  (#1840)
