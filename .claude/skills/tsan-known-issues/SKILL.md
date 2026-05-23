---
name: tsan-known-issues
description: TSan (ThreadSanitizer) known issues and policy for the ry project — required vs warn-only job split, LargeMmapAllocator upstream bug, and parallel_for_depth_ design rationale. Use when TSan CI fails, when adding concurrency features, or when evaluating whether a TSan failure is a real race or an upstream TSan allocator bug.
allowed-tools: Bash
---

# TSan Known Issues

Reference for ThreadSanitizer policy and known infrastructure limitations in the ry project.

---

### TSan job — C++ tests required, Ry self-tests warn-only

**Source**: #868 (warn-only landed), #874 (split policy)
**Tags**: tsan, sanitizer, ci, concurrency

**Rule**: The C++ TSan step (`./docker/run.sh tsan ry_tests`) is required
and gates #630's P0 race fixes via `ConcurrencySpecSuite` (which
runs `tests/spec/concurrency.test.ry`). The Ry self-test TSan step
(`./docker/run.sh tsan ry test -p`) runs as `continue-on-error: true`
until upstream https://github.com/google/sanitizers/issues/1716
is fixed — see the separate `LargeMmapAllocator` entry. A PR that
introduces a new race must fix it in the same PR; warn-only is a
workaround for the TSan runtime bug, not tolerance for real races.
`parallel_for_depth_` only propagates to ARC ops emitted **inside**
the thunk body, not through helper calls; if a helper-alias race
surfaces, widen via #872 rather than blindly expanding the flag.

---

### TSan `LargeMmapAllocator` CHECK on Linux self-test runs

**Source**: #868 / #874
**Tags**: tsan, sanitizer, ci, ubuntu, gotcha

**Rule**: Keep `./docker/run.sh tsan ry test -p` as `continue-on-error: true`
until upstream https://github.com/google/sanitizers/issues/1716
is fixed: the self-test driver aborts inside TSan's
`LargeMmapAllocator::Deallocate` — a runtime bug, not a race in
ry code. Pinning `ubuntu-22.04` and lowering `vm.mmap_rnd_bits=28`
were both tried. `./docker/run.sh tsan ry_tests` (which includes
`ConcurrencySpecSuite`) runs cleanly on the same binary, so that
is the required gate for #630's race fixes. macOS is unaffected
**by this specific upstream LargeMmapAllocator CHECK** — but the
`continue-on-error` policy also protects against the orthogonal
LLVM ORC teardown family (see next entry), which TSan exposes on
both OSes.

---

### TSan exposes LLVM ORC teardown heap corruption (`~CodeGen()` / `~OverloadEntry()` SEGV on macOS)

**Source**: #1657 (2026-05-09) macOS Darwin 25.3.0
**Tags**: tsan, sanitizer, ci, macos, llvm, orc, jit, teardown, gotcha

**Rule**: A SEGV inside `~CodeGen()` → `~unordered_map(functions_)`
→ `~OverloadEntry()` → `unique_ptr<...>::reset()` on a garbage
pointer (e.g. `0x4800000001135036`) under TSan on macOS is **not a
real codegen UAF** — it is the same #1187 family LLVM ORC heap
corruption that already forces `~LLJIT()` and `removeResourceTracker`
to leak. Suppression is now a three-step `(void)jit.release()` +
`rtCleanup.release()` + `(void)cg.release()` block in
`src/jit_runner.cpp`. **Why:** ASan + UBSan are clean on the same
binary and same test, indicating the heap corruption originates in
the JIT teardown path and only the disturbed-heap sequel shows up
in `~CodeGen()` under TSan's stricter free-list checking. **How to
apply:** When a TSan crash report on macOS points at `~OverloadEntry()`
or any other `~CodeGen()` member destructor, do not start hunting
for codegen-side `unique_ptr` ownership bugs first — first check
whether ASan is also clean. If yes, treat as ORC teardown family
and follow the suppression pattern in `src/jit_runner.cpp`. The
`LargeMmapAllocator` warn-only policy above continues to protect
against this family on Linux as well; promotion of Ry self-test
TSan to required is gated on the upstream LLVM ORC root cause, not
just the LargeMmapAllocator fix. See full details in
`.claude/rules/codegen-llvm-ir-conventions.md` "LLVM ORC JIT
intermittent crash" entry.

---

### TSan + Linux libtsan deadlocks on `siglongjmp` from a signal handler

**Source**: #1688 (2026-05-17, `@timeout(ms)` directive)
**Tags**: tsan, sanitizer, ci, linux, signal-handler, siglongjmp, setitimer, gotcha

**Rule**: `@timeout(ms)` (`src/codegen_test.cpp::emitItWithTimeout` + `src/jit_runner.cpp::test_timeout_handler`) installs a SIGALRM handler that calls `siglongjmp` back to a JIT-emitted continuation. On **Linux + TSan**, libtsan's `siglongjmp` interceptor deadlocks when invoked from signal context (subprocess never returns; CI hits the 30s WNOHANG backstop with empty / truncated stdout). The exact same code completes in ~1.4s on **macOS TSan** and passes under default + ASan+UBSan builds, so this is an upstream TSan + Linux libtsan interaction — not a race in ry code.

**How to apply**: When adding tests that exercise `@timeout` via a forked-child subprocess (`tests/test_spec_timeout.cpp`), gate the test body with:

```cpp
#if defined(__SANITIZE_THREAD__) || \
    (defined(__has_feature) && __has_feature(thread_sanitizer))
#  define RY_TSAN_BUILD 1
#endif

TEST_F(..., ...) {
#ifdef RY_TSAN_BUILD
    GTEST_SKIP() << "Skipped under TSan: libtsan's siglongjmp interceptor "
                    "deadlocks when invoked from the SIGALRM handler ...";
#endif
    ...
}
```

Full coverage (timeout fires, fail counted, next test runs) is preserved on the **required** non-TSan gates (default / ASan+UBSan / filecheck) and on local macOS TSan. Promoting `@timeout` to TSan-required is gated on either an upstream libtsan fix or a rework of the timeout mechanism to avoid `siglongjmp` from a signal handler.

**Why not just extend the backstop further**: the failure is a deterministic deadlock — both `InfiniteLoopTestTimesOutAndContinuesToNext` (empty stdout) and `MixedTimeoutsContinueExecution` (truncated at `+ normal one` before the first `@timeout` test) hit the exact backstop window. Increasing the backstop only postpones the same failure.

---

### `@parallel for` captures must be retained AND ARC-backed inside the thunk

**Source**: #630 / #874
**Tags**: parallel-for, arc, cow, codegen, fnscope, gotcha

**Rule**: For `@parallel for` / `threadSpawn` captures: before
`FnScope` clears the flags, snapshot each `isArcManaged(src)` /
`arc_backed_vars_.count(src)`; re-apply on the thunk's `dst`
alloca (propagateMeta's own `markArcManaged` branch silently
no-ops under FnScope clearing); emit an atomic
`emitArcRetain(hdr, true)` at entry, matched by `popScope()`
before the terminator while `parallel_for_depth_ > 0`. Without
the retain, CoW's `> 1` check fails and workers mutate shared
buffers in place (libmalloc `POINTER BEING FREED WAS NOT
ALLOCATED`). Repro: `tests/spec/concurrency.test.ry`
"parent list is unchanged when workers mutate captured list";
impl: `src/codegen_stmt_loop.cpp::emitParallelForRange`.
