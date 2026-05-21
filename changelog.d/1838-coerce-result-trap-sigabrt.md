### Fixed

- `emitRuntimeError` (the codegen helper that emits an `fprintf`
  diagnostic and aborts the program from JIT'd code paths such as
  `unwrapEnumFromAny`'s tag / descriptor mismatch trap) now calls
  `_Exit(1)` (C11) instead of `exit(1)`. `exit(1)` runs the libc
  atexit chain, which on Linux glibc invokes LLVM `ManagedStatic`
  destructors (`PassRegistry::~PassRegistry`, etc.) on a heap still
  referenced by the live JIT module — producing intermittent
  `free(): invalid pointer` SIGABRT before the `EXPECT_EXIT`-expected
  `ExitedWithCode(1)` is observed. The new
  `CodeGenTest.CoerceResultOkNestedResultMismatchTrapsArcPayload`
  death test exercises the same trap path with an ARC-bearing inner
  enum payload as additional regression coverage. ASan and the macOS
  libSystem malloc both masked the issue; the abort was only visible
  on the default Linux build. The C++ runtime helper used for normal
  program exit (`finalizeAfterPossibleJit` → `_exit(rc)`) already
  bypasses the same atexit chain, so this change closes the
  remaining JIT-triggered abort hole without altering user-visible
  `exit()` semantics. (#1838)
