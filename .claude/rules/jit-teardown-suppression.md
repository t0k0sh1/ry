---
paths:
  - "src/jit/**/*.cpp"
  - "src/jit/**/*.hpp"
  - "include/ry/jit/**/*.hpp"
  - "src/app/main.cpp"
  - "src/codegen_call_user.cpp"
---

# JIT Teardown Suppression

This file covers only hazards that are not visible from reading the code. The LLVM ORC / glibc teardown workarounds are deliberately nested suppressions; each entry records what re-surfaces when a given step is removed.

### LLVM ORC JIT intermittent teardown crash (`~LLJIT()` / `removeResourceTracker` / `~CodeGen()` / `~FnStmt()`)

**Tags**: asan, llvm, orc, jit, teardown, glibc, macOS, gotcha, rust-migration-candidate

A family of crashes that occur during JIT teardown after all `it` blocks pass: Linux glibc `cfree` heap consolidation / macOS parallel-mode worker SEGV / Linux glibc tcache assertion in `~FnStmt()` (`malloc(): invalid next->prev_inuse` / `corrupted size vs. prev_size`).

**6-step suppression** (dropping any one yields residual failures):

`runRySource` (`src/jit/jit_runner.cpp`, `#if defined(__linux__) || defined(__APPLE__)` gate):

1. `rtCleanup.release()` — cancels the `scope_exit` that would call `RT->remove()`.
2. `(void)jit.release()` — leaks LLJIT to skip `~LLJIT()`.
3. `(void)cg.release()` — leaks CodeGen so `~CodeGen()` does not walk `functions_` on a JIT-disturbed heap.
4. `new Program(std::move(prog))` — leaks the parsed AST so `~Program()` → `~unique_ptr<FnStmt>()` → `~FnStmt()` does not traverse lambda body / capture chains on a disturbed heap (#1895).

`main()` (`src/app/main.cpp`):

5. `finalizeAfterPossibleJit(rc)` calls `_exit(rc)` on Linux/macOS when `ry::jitWasInitialized()` is true (flag is set inside `runRySource` immediately after `LLJITBuilder().create()`) — bypasses the C++ static-destructor chain (LLVM `ManagedStatic` / `llvm_shutdown`) that aborts on `_int_malloc` on a disturbed heap (#1749, exit 134 after test output).

`emitRuntimeError` (`src/codegen_call_user.cpp`):

6. The trap path emits `_Exit(1)` (not `exit(1)`) via the `getStdlibImmediateExit` helper (#1838). The path where JIT'd code calls libc abort directly is not covered by `finalizeAfterPossibleJit` (control never returns to `main()`) — without `_Exit`, the libc atexit chain invokes `PassRegistry::~PassRegistry` while the JIT module is live, producing double-free + SIGABRT (`CoerceResultOkNestedResultMismatchTraps`, masked under ASan).

Non-JIT exits (help / formatter / parse-time failure) run normal teardown — the flag gates step (5) narrowly. The user-level Ry `exit()` builtin (`emitExit` in `codegen_match.cpp`) intentionally stays as `exit()`; only the trap path uses `_Exit`.

These are workarounds, not root-cause fixes (the underlying LLVM ORC / JITLink heap pattern is unidentified upstream). When this signature appears on Linux CI, it belongs to the known teardown family — re-run before treating as a real failure. Under ASan with persistent reproduction, the failure is user-code OOB/UAF, not the teardown family. Suppressing the LLVM crash reporter or adding `|| true` is prohibited.

**Parallel test runner perturbation**: The per-file subprocess fan-out in `src/jit/test_runner.cpp` accepts worker count via `-p N` / `--parallel=N` (#2177). Teardown failures are parallelism-dependent — at 8 workers, the `collection_meta_propagation.test.ry` glibc SIGABRT from #2172 reproduced in 3 Linux runs (20/20 pass when run alone). First triage step for intermittent SIGABRT: reduce or remove `-p N`.

**Subprocess-runner perturbation (#2236)**: Changes to `src/jit/test_runner.cpp` / `include/ry/jit/test_runner.hpp` / `main.cpp` test-subcommand dispatch perturb per-spawn allocation and alter the #1895 fire rate. Example: building the outline argv as `std::vector<const char *>` produced a 31% fail rate (9/29); reverting to stack `const char *argv[]` dropped it to 8% (8/100). Sanitizers report clean (the mechanism is teardown timing, not a fresh UAF). **Rule**: any PR touching test-runner subprocess dispatch must include a 30-run baseline comparison against `origin/main` (`for i in $(seq 1 30); do ./build-rust/ry test; done`) in the PR description. Keep `runTestFileSubprocess` argv stack-allocated (no per-spawn `std::vector` / `std::string`) — this is the only mitigation short of an upstream fix.

**Policy enforcement (#2238)**: The 1-source-file = 1-process boundary is encoded as a code-side guard rule in `.claude/rules/test-runner-isolation.md`.

**Rust migration priority**: This teardown family is a priority candidate for Rust migration of the codegen / JIT-runner layer (v0.0.26+ — #1949 / #1950 / #1993). The 6-step suppression only masks the residual failure rate (leaking and bypassing C++ teardown ordering); each recurrence adds another suppression. Reimplementing module / JIT lifetime using Rust `Drop` order / lifetimes addresses the root cause.

### `~CodeGen()` glibc heap-check crashes are not destructor bugs

**Tags**: codegen, arc, glibc, debugging, heap-corruption, asan, gotcha

**Scope**: crashes that reproduce persistently even under ASan (ASan itself reports heap-buffer-overflow / use-after-free). Crashes only in teardown that disappear on re-run belong to the `LLVM ORC JIT intermittent teardown crash` above — those are a pre-existing LLVM ORC teardown family and are not user-code bugs.

**Rule**: When `ry::CodeGen::~CodeGen()` aborts on Linux glibc with `corrupted size vs. prev_size while consolidating` and **reproduces persistently** (especially under ASan), triage as an **earlier** OOB write or UAF in an ARC-managed runtime buffer (Map/List/Set CoW copy / ARC header) that glibc detects at adjacent-chunk free during `ThreadSafeModule` teardown — not a destructor-ordering or `CodeGen` member-lifetime bug.

**Why `~CodeGen()` is not the culprit**:

- `~CodeGen` is compiler-generated. `mod_` / `ctx_` are moved into `ThreadSafeModule` (`src/codegen.cpp:575`); the destructor does not touch IR objects.
- The ARC tracking set (`include/ry/codegen.hpp:184-193`) holds bare `llvm::Value*` only; walking hash buckets at destruction does not dereference them.

**Diagnostic checklist**:

1. Reproduce on Linux + ASan (ARM64 Docker `docker/run.sh asan`; x86_64 CI asan job on `v*` push). macOS libSystem malloc does not expose glibc chunk metadata checks, so the same bug may crash only on Linux.
2. Let ASan pin the actual OOB / UAF origin rather than the late `free()`.
3. Start with recent changes to `emitCowCheckSlot` / `emitCowDeepCopy*` / `emitMapKeyLookup` / `emitSetElementLookup` — ARC retain gating / `keyName` / `elemName` propagation / hash-vs-linear-scan selection are the change class that introduced #1161.

### `ConcurrencySpecSuite` ASan `DISABLED_` guard was stale after #630's atomic-ARC fix

**Tags**: asan, concurrency, parallel_for, arc, testing

`ConcurrencySpecSuite` was disabled under ASan at commit `fb010ea` (2026-03-31) because non-atomic ARC retain/release inside `@parallel for` workers raced with ASan's shadow-memory interceptor, causing a deadlock (SIGALRM, exit 142 after 300 s on Linux). The P0 fix in #630 made all thunk ARC ops `atomicrmw seqcst`, eliminating the root cause; the guard was removed in #872 (macOS ~55 ms). If non-atomic ARC is re-introduced into thunks in the future, restore the guard and investigate the hang before merging.

### UBSan must disable `vptr` and `function` checks on this project

**Tags**: ubsan, sanitizer, cmake, llvm, cxx-flags

When enabling `-fsanitize=undefined`, **always pair it with `-fno-sanitize=vptr,function`**:

1. `vptr` — ry builds with `-fno-rtti` (matching LLVM's build flag, set in `CMakeLists.txt:25`). The `vptr` check requires RTTI; under `-fno-rtti` it provides zero coverage and may produce hard errors depending on the toolchain.
2. `function` — LLVM exposes many C-style function pointers through `void *`. UBSan's `function` check flags all of ry's JIT symbol resolution / runtime dispatch as type mismatches, burying real signals.

Modifying the LLVM interop code to satisfy these checks is not the approach — the checks themselves are not suited to this codebase. UBSan and ASan are compatible; the `asan` preset (`ENABLE_ASAN=ON` + `ENABLE_UBSAN=ON`) enables both simultaneously.

### Rust cdylib (`emit`) internal memory is outside the C++ sanitizer instrumentation scope

**Tags**: sanitizer, asan, ubsan, rust, corrosion, cdylib, emit, instrumentation-scope, ffi-boundary

The internal memory of the Rust cdylib `crates/emit` (LLVM IR emission shared library) is **outside** the C++ sanitizer (ASan / UBSan) instrumentation scope. Corrosion does not propagate CMake's `-fsanitize=*` flags to the cargo build (`CMakeLists.txt` only calls `corrosion_set_env_vars(... LLVM_SYS_211_PREFIX=...)`, not `corrosion_add_target_rustflags`; `crates/emit/build.rs` emits only the macOS `dynamic_lookup` link arg). The CI Rust toolchain is stable; `-Zsanitizer` is nightly-only. The FFI boundary / host process / `ry_runtime_*` libraries are fully instrumented — only the cdylib's own internal allocation and accesses are non-instrumented. This is the same structure as LLVM itself, which has been linked uninstrumented since the C++ era. Adopting nightly Rust `-Zsanitizer=*` is out of scope (deferred, #1993).
