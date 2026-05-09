### Fixed

- TSan no longer SEGVs on macOS during `~CodeGen()` teardown after
  combinatorial spec tests that nest `@describe` / `@it` (e.g.
  `tests/spec/combinatorial/collection_element_option_iterate.test.ry`).
  Previously `~CodeGen()` walked
  `functions_ → vector<OverloadEntry> → ~OverloadEntry() → unique_ptr<unordered_map<size_t, FnTypeInfo>>::reset()`
  on a heap whose state had already been disturbed by LLVM ORC JIT
  teardown, and intermittently called `free()` on a garbage pointer
  (e.g. `0x4800000001135036`). ASan + UBSan were both clean on the
  same binary and test, confirming this was the same #1187 family
  ORC teardown heap corruption — TSan exposed the disturbed-heap
  sequel that the existing `(void)jit.release()` + `rtCleanup.release()`
  block did not cover. `runRySource` (`src/jit_runner.cpp`) now
  heap-allocates `CodeGen` via `std::make_unique<CodeGen>(...)` and
  leaks it via `(void)cg.release()` alongside the existing LLJIT
  releases under `#if defined(__linux__) || defined(__APPLE__)`. The
  process exits immediately after `runRySource` returns, so the leak
  is bounded by process lifetime. This is still a workaround — the
  upstream LLVM ORC / JITLink heap corruption pattern that propagates
  into the codegen heap is unidentified — but it suppresses the
  `~CodeGen()` / `~OverloadEntry()` SEGV reliably under TSan, ASan,
  UBSan, and default builds. (#1657)
