### Fixed

- Linux CI flake (~5-10 %) where `tests/spec/collection_meta_propagation.test.ry`
  printed `25 passed, 0 failed` and then SIGABRT'd inside `~FnStmt()`
  with glibc tcache assertions (`malloc(): invalid next->prev_inuse` /
  `corrupted size vs. prev_size`). The parsed AST (`Program prog` in
  `runRySource`) was destructed via stack unwind after JIT execution
  had already disturbed the heap — `~Program()` walks
  `vector<StmtNode>` → `unique_ptr<FnStmt>` → `~FnStmt()` over lambda
  body / capture chains, triggering glibc 2.40's tcache integrity
  check on freed chunks the JIT had touched. The fix extends the
  existing LLJIT / CodeGen teardown suppression in
  `src/jit_runner.cpp` with a sixth step: `new Program(std::move(prog))`
  inside the existing `#if defined(__linux__) || defined(__APPLE__)`
  block, so the AST is intentionally leaked alongside the LLJIT and
  CodeGen instances. The OS reclaims memory on process exit. Same
  #1187 family workaround; root cause in LLVM ORC / JITLink heap
  patterns is still unidentified upstream. macOS Docker did not
  reproduce locally (50/50 then 200/200 PASS), so the fix is validated
  via the mechanistic argument that `~FnStmt()` no longer runs after
  JIT teardown, with CI statistics as the post-merge oracle. (#1895)
