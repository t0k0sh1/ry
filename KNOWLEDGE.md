# KNOWLEDGE

未分類知見の暫定バッファ。新たな教訓のうち既存 `.claude/rules/` / `.claude/skills/` のどれにも該当 entry を持たないものをここに蓄積し、安定後に rules または skills に昇格させる。

蓄積・参照・昇格・外部参照ポリシーの詳細は `/knowledge-md-management` 参照。

<!-- Entry format:
### <短く具体的な heading>

**Source**: <PR / issue / commit など出典>
**Tags**: <空白区切りキーワード>
**Rule**: <教訓本文>
-->

### `tests/spec/<name>/` directories collide with stdlib module names

**Source**: #1687 (2026-05-16 implementation)
**Tags**: testing, module-loader, stdlib, tests-spec, layout, collision, gotcha

**Context**: While adding new `tests/spec/testing/directive_*.test.ry` files for the `@skip` / `@only` / `@todo` directives, every test in `tests/` started failing with `'it' not found in module 'testing'`. The cause was the directory name `tests/spec/testing/` itself — `ry test`'s module loader treats every directory on the resolution path as a candidate package root, and the local `tests/spec/testing/` (which has no `it` symbol) shadows `share/std/testing/`. The collision is silent at build time and surfaces only as runtime import errors during test discovery.

**Rule**: Do not create a subdirectory under `tests/spec/` whose name matches a stdlib module (e.g. `testing`, `math`, `path`, `filesystem`, `crypto`, `io`, `json`, `regex`, `thread`, `time`, `http`, `str`, `list`, `map`, `set`, `option`, `result`). Place per-file tests at the top level of `tests/spec/` instead (`tests/spec/directive_skip.test.ry`, not `tests/spec/testing/directive_skip.test.ry`). Existing subdirectories such as `tests/spec/braced_import/`, `tests/spec/combinatorial/`, `tests/spec/concurrency/` are safe because their names do not match any stdlib module.

**How to apply**:
- When adding a new `.test.ry` file under a new subdirectory, grep `share/std/` first: `ls share/std/ | sort | uniq` — pick a directory name that does not appear in the list.
- If a stdlib module is renamed in the future, audit `tests/spec/` for newly-colliding directories at the same time.

### Pre-scan AST walkers must traverse every `ExprPtr` slot, or document the limitation inline

**Source**: #1765 (2026-05-16 implementation)
**Tags**: codegen, pre-scan, ast-walker, mock, spy, slot-coverage, blind-spot

**Context**: `collectTestTargetsFromStmt` (`src/codegen.cpp`) was introduced for the test-mode pre-scan that populates `mocked_functions_` / `spied_functions_`. The first version walked `LambdaExpr.body` only inside `CallStmt.args`, missing identical lambdas stored in `AssignStmt.value`, `ReturnStmt.value`, `CallExpr.args` (nested at any depth), `IfStmt.condition`, and other ExprPtr slots. The bug surfaces only when the gated callsite is codegen'd BEFORE the lambda body runs — typically inside a module-level helper fn declared above the `@describe` block. `emitMockCall` populates the same set at codegen time, masking the bug whenever the callsite happens to follow the mock-setup statement in emission order.

**Rule**: A pre-scan AST walker that needs to find anonymous nested constructs (lambda bodies, embedded directives, etc.) must traverse **every** `ExprPtr` and `StmtNode` slot reachable from each variant — not just the slots that happen to host the construct on the day the walker was written. Concretely: implement an `expr` visitor over all `ExprNode` alternatives in tandem with the stmt visitor, and route every stmt's ExprPtr field (`AssignStmt.value`, `ReturnStmt.value`, `IfStmt.branch.condition`, `WhileStmt.condition`, `ForStmt.iterable`, `ExpectStmt.{actual,expected,extra_args}`, etc.) through it. The expr visitor descends into compound nodes (`CallExpr.args`, `BinaryExpr.{lhs,rhs}`, `IfBlockExpr.{then_body,else_body}` — the only Expr that contains StmtNodes — and so on) so an anonymous LambdaExpr buried any number of levels deep is reached.

**Why**: Slot coverage gaps are silent at compile time and only surface as runtime correctness bugs in narrow code paths. The fix is mechanical (mirror the AST variant list), the test is unstable to write (depends on emission order to manifest), and the recurrence cost is high if a future pre-scan extension forgets the same slots.

**How to apply**:
- When adding a new pre-scan walker (`stmtHasOnlyDirective` analogues, future test-mode discovery passes, lint passes), implement the full ExprNode + StmtNode visitor pair up-front. If a slot is intentionally excluded (e.g. `RecordStmt.invariants` is dead for this walker's purpose), add an inline comment naming the slot and stating the reason — silent omission is the bug.
- When adding a new AST variant (new `ExprNode` or `StmtNode` alternative), grep for existing walkers in `src/codegen.cpp` and update them to include the new variant. Walkers that should `if constexpr (...)` over every variant include `collectTestTargetsFromStmt`, `collectTestTargetsFromExpr` (#1765), and `stmtHasOnlyDirective`.
- The `LambdaExpr` and `IfBlockExpr` alternatives are the only ExprNode variants that carry `std::vector<StmtNode>` — both must hand off to the stmt-walker (`collectTestTargetsFromStmts`) explicitly. Forgetting one of them re-introduces the same class of bug.

### Hot-path helpers that gate on a rarely-populated map should be lookup-first, not normalize-first

**Source**: #1888 (2026-05-27 implementation)
**Tags**: codegen, allocation, heap-perturbation, alias-resolution, lookup-pattern, latent-bug

**Context**: `registerResourceByTypeName` is called for every fn parameter, variable annotation, and propagate-type-meta site that might be a resource handle — many thousands of times per typical compilation. The naive #1888 fix prefixed `resolveTypeAlias(typeName)` to the `ResourceKindRegistry::lookupByTypeName` call so aliased resource types (`from io import File as MyFile`) resolve to their canonical name. But `resolveTypeAlias` always allocates a `std::unordered_set<std::string>` for cycle detection and copies the input string, even when `type_aliases_` is empty (the overwhelmingly common case — most fns have no `type X = ...` alias in scope). The extra allocation churn measurably shifted heap layout and amplified an unrelated pre-existing latent memory bug in `tests/spec/collection_meta_propagation.test.ry` from a ~3% baseline reproduction rate (origin/main equivalent, 1/30) to ~63% (11/30). Lookup-first restores the failure rate to baseline (~3%, 2/60 across same-session stash and unstash) — it mitigates the amplification, not the underlying bug. The bug surface was "runtime error: map key not found" with exit 1 — not a sanitizer hit (ASan masked it) and not the #1895 SIGABRT teardown family.

**Underlying bug fixed by #2246 (2026-06-19)**: root cause was `CodeGen::wrapInAny` (`src/codegen_any.cpp`) calling `isStringValue(val)` as a **negative-evidence predicate** (`ptrTy_ && !isNonStrPointer`) on container element fresh loads (`filter` / `slice` / `map` body's `load ptr, %elem_ptr`). The metadata-less ptr was misclassified as str and retained at StringHeader offset `-24` instead of the ArcHeader offset `-16`, corrupting the adjacent allocation's tail 8 bytes — often a sibling Map's `keys` buffer, whose damaged str ptr then failed the `"a"` hash compare. The str retain branch was introduced in #1799 (2026-05-18) without the metadata-gated discipline that #1266 had already established for the analogous `propagateTypeMeta("str")` path. #2246 added a positive-evidence gate (`arc_str_owned_values_` / `arc_str_managed_vars_` / `GlobalVariable` / `meta->str_elem`) in front of the retain. Validated: a 3-line minimum repro (`xs: List<Map<str,int>> = [...]; ys = filter(xs, m => true); print(ys[2]["a"])`) drops from ~15% process abort on macOS to 0/100; `collection_meta_propagation.test.ry` flake drops to 0/30. Detection in CI lives in `tests/test_regression_2246_str_metadata_gate.cpp` (subprocess loop; the heap-layout sensitivity means an `@it`-internal loop does NOT detect — the testing framework's per-iteration scaffolding stabilises the heap). Long-term predicate hygiene (the `isStringValue` signature itself, 48 callsites) is tracked in #2248; upstream metadata propagation in higher-order combinators is #2247.

**Rule**: When extending a hot-path lookup helper to support a new naming convention (alias, qualified name, deprecated form), try the **direct** lookup first and fall back to the normalization path **only when the direct lookup misses AND the normalization map is non-empty**. The two-clause guard skips both the allocation and the function call entirely for the common case, preserving the original heap signature. Single-clause guards (only "direct lookup misses") still pay the allocation when the map is empty.

**How to apply**:
- Pattern (canonical form, `src/codegen_fn.cpp:104-111`):
  ```cpp
  int rk = ResourceKindRegistry::instance().lookupByTypeName(typeName);
  if (rk == ResourceKindRegistry::NONE && !type_aliases_.empty()) {
      rk = ResourceKindRegistry::instance().lookupByTypeName(resolveTypeAlias(typeName));
  }
  ```
- The `!type_aliases_.empty()` test must use the actual map's `empty()` (cheap — single load), not a heuristic.
- Detection in code review: any prefix of the form `lookup(normalize(x))` inside a per-fn / per-statement / per-expression codegen helper is a candidate. Bisect by reverting only that prefix and re-running a flaky downstream test 30x; if the failure rate drops to baseline (origin/main), the allocation churn is the trigger.
- Latent memory bugs exposed this way should be filed separately via `/triage-side-finding` Q4(b) — the lookup-first fix mitigates the symptom but does not fix the root cause.

## サニタイザー既知問題

> サニタイザー固有の past-incident 参照を集中管理するセクション。新規エントリは該当サブセクションに追記する。サブセクション規約 (`### <Sanitizer>` + `#### <entry>`) は `/knowledge-md-management` §2 を参照。

### TSan

#### TSan build commands and preset isolation

**Source**: AGENTS.md historical note (migrated from inline sanitizer section, 2026-05-28)
**Tags**: tsan, sanitizer, cmake, preset, build, isolation

**Rule**: TSan ビルドは `asan` preset とは別ディレクトリで行う:

```bash
cmake --preset tsan                                     # Debug + TSan (build-tsan/)
cmake --build build-tsan                                # ビルド
./build-tsan/ry_tests                                   # C++ tests
./build-tsan/ry test -p                                 # Ry self-tests
```

`tsan` preset は ASan / UBSan と排他であり、別ディレクトリ `build-tsan/` にビルドされる。同じディレクトリで sanitizer を切り替えると CMake キャッシュが壊れるため、必ず別 build tree に分離する。

**How to apply**: TSan ジョブを走らせる前に `build-tsan/` の存在を確認し、無ければ `cmake --preset tsan` から作る。`build-asan/` と `build-tsan/` を混ぜないこと。既知の upstream バグ (LargeMmapAllocator / LLVM ORC teardown / signal-handler siglongjmp) は本セクションの他エントリ参照。

#### TSan job — C++ tests required, Ry self-tests warn-only

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

#### TSan `LargeMmapAllocator` CHECK on Linux self-test runs

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

#### TSan exposes LLVM ORC teardown heap corruption (`~CodeGen()` / `~OverloadEntry()` SEGV on macOS)

**Source**: #1657 (2026-05-09) macOS Darwin 25.3.0
**Tags**: tsan, sanitizer, ci, macos, llvm, orc, jit, teardown, gotcha

**Rule**: A SEGV inside `~CodeGen()` → `~unordered_map(functions_)`
→ `~OverloadEntry()` → `unique_ptr<...>::reset()` on a garbage
pointer (e.g. `0x4800000001135036`) under TSan on macOS is **not a
real codegen UAF** — it is the same #1187 family LLVM ORC heap
corruption that already forces `~LLJIT()` and `removeResourceTracker`
to leak. Suppression is now a three-step `(void)jit.release()` +
`rtCleanup.release()` + `(void)cg.release()` block in
`src/jit/jit_runner.cpp`. **Why:** ASan + UBSan are clean on the same
binary and same test, indicating the heap corruption originates in
the JIT teardown path and only the disturbed-heap sequel shows up
in `~CodeGen()` under TSan's stricter free-list checking. **How to
apply:** When a TSan crash report on macOS points at `~OverloadEntry()`
or any other `~CodeGen()` member destructor, do not start hunting
for codegen-side `unique_ptr` ownership bugs first — first check
whether ASan is also clean. If yes, treat as ORC teardown family
and follow the suppression pattern in `src/jit/jit_runner.cpp`. The
`LargeMmapAllocator` warn-only policy above continues to protect
against this family on Linux as well; promotion of Ry self-test
TSan to required is gated on the upstream LLVM ORC root cause, not
just the LargeMmapAllocator fix. See the
`LLVM ORC JIT intermittent teardown crash` entry under `### ASan`
below for the six-step suppression details.

#### TSan + Linux libtsan deadlocks on `siglongjmp` from a signal handler

**Source**: #1688 (2026-05-17, `@timeout(ms)` directive)
**Tags**: tsan, sanitizer, ci, linux, signal-handler, siglongjmp, setitimer, gotcha

**Rule**: `@timeout(ms)` (`src/codegen_test.cpp::emitItWithTimeout` + `src/jit/jit_runner.cpp::test_timeout_handler`) installs a SIGALRM handler that calls `siglongjmp` back to a JIT-emitted continuation. On **Linux + TSan**, libtsan's `siglongjmp` interceptor deadlocks when invoked from signal context (subprocess never returns; CI hits the 30s WNOHANG backstop with empty / truncated stdout). The exact same code completes in ~1.4s on **macOS TSan** and passes under default + ASan+UBSan builds, so this is an upstream TSan + Linux libtsan interaction — not a race in ry code.

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

#### `@parallel for` captures must be retained AND ARC-backed inside the thunk

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

### ASan

#### `ConcurrencySpecSuite` ASan `DISABLED_` guard was stale after #630's atomic-ARC fix

**Source**: #872
**Tags**: asan, concurrency, parallel_for, arc, testing

**Rule**: `ConcurrencySpecSuite` was disabled under ASan in commit `fb010ea`
(2026-03-31) because non-atomic ARC retain/release ops in `@parallel for`
workers raced with ASan's shadow-memory interceptors, causing a deadlock
(SIGALRM, exit 142 after 300 s on Linux). After #630's P0 fix made all ARC
ops inside `@parallel for` thunks use `atomicrmw seqcst`, the root cause
was removed. The `DISABLED_` guard was removed in #872; on macOS the suite
runs in ~55 ms. If a future change reintroduces non-atomic ARC inside a
`@parallel for` thunk, re-enable the guard and investigate the hang before
merging.

#### `~CodeGen()` glibc heap-check crashes are not destructor bugs

**Source**: #1161 / #1167 (2026-04-18)
**Tags**: codegen, arc, glibc, debugging, heap-corruption, asan, gotcha

**Scope**: This entry covers crashes that **persist under ASan** (i.e. ASan itself reports
a heap-buffer-overflow or use-after-free). For crashes where all test cases pass and only
the teardown crashes — and the crash disappears on re-run — see the `LLVM ORC JIT
intermittent teardown crash` entry below instead; those are pre-existing LLVM ORC
flakiness, not user-code bugs.

**Rule**: When `ry::CodeGen::~CodeGen()` aborts on Linux glibc with
`corrupted size vs. prev_size while consolidating` **and the crash reproduces persistently**
(especially under ASan), treat it as an **earlier** OOB write or UAF in ARC-managed
runtime buffers (Map/List/Set CoW copies, ARC headers) that glibc only detects when a
neighbouring chunk is freed during `ThreadSafeModule` teardown — not as a destructor-ordering
or member-lifetime bug in `CodeGen` itself.

**Why `~CodeGen()` is not the culprit**:
- `~CodeGen` is compiler-generated. `mod_` / `ctx_` are moved into
  `ThreadSafeModule` at `src/codegen.cpp:575`; the destructor never touches IR objects.
- ARC tracking sets (`include/ry/codegen.hpp:184-193`) store bare `llvm::Value*`
  and only walk hash buckets at destruction — no dereference.

**Diagnostic checklist**:
1. Reproduce on Linux with ASan (ARM64 Docker via `docker/run.sh asan`; x86_64 CI
   asan job on `v*` push). macOS libSystem malloc does not expose glibc's chunk
   metadata checks, so the crash is Linux-only even with the same bug.
2. ASan will pinpoint the actual OOB/UAF site instead of just the late `free()`.
3. Look first at recent changes to `emitCowCheckSlot` / `emitCowDeepCopy*` /
   `emitMapKeyLookup` / `emitSetElementLookup`: ARC retain gating, `keyName` /
   `elemName` propagation, and hash-vs-linear-scan branch selection are the
   class of change that introduced `~CodeGen()` crash #1161.

**History**: #1161 crash (`corrupted size vs. prev_size`) was observed after PR #1148
landed. The bug was most likely a heap-layout-dependent OOB that became non-reproducible
after PR #1160 (`emitCowCheckSlot` `doKeyRetain` unconditional + `emitMapKeyLookup`
`keyName` fix). CI ASan clean on x86_64 Linux was confirmed at commit `998edc42`
(CI run #24601715375, 2026-04-18). The exact commit that fixed it was not bisected;
if the crash re-appears, revert to the diagnostic checklist above.

#### LLVM ORC JIT intermittent teardown crash (`~LLJIT()` / `removeResourceTracker` / `~CodeGen()` / `~FnStmt()`)

**Source**: #1187 / #1657 / #1749 / #1838 / #1895
**Tags**: asan, llvm, orc, jit, teardown, glibc, macOS, gotcha, rust-migration-candidate

**Rule**: Self-tests pass all `it` blocks then crash during JIT teardown — Linux glibc `cfree` heap consolidation, macOS parallel-mode worker SEGV, macOS TSan `~OverloadEntry()`, Linux glibc tcache assertion in `~FnStmt()` (`malloc(): invalid next->prev_inuse` / `corrupted size vs. prev_size`). Fix is a **six-step** suppression. Four live in `runRySource` (`src/jit/jit_runner.cpp`) guarded by `#if defined(__linux__) || defined(__APPLE__)`: (1) `rtCleanup.release()` cancels the `RT->remove()` `scope_exit`, (2) `(void)jit.release()` leaks LLJIT so `~LLJIT()` never runs, (3) `(void)cg.release()` leaks CodeGen so `~CodeGen()` does not walk `functions_` on a heap disturbed by JIT teardown, (4) `new Program(std::move(prog))` leaks the parsed AST so `~Program()` → `~unique_ptr<FnStmt>()` → `~FnStmt()` never traverses lambda body / capture chains on the disturbed heap (#1895: collection_meta_propagation.test.ry, 25 passed + SIGABRT, ~5-10 % Linux CI flake). The fifth (5) is in `main()` (`src/app/main.cpp`): `finalizeAfterPossibleJit(rc)` calls `_exit(rc)` on Linux/macOS when `ry::jitWasInitialized()` returns true (a flag set in `runRySource` immediately after `LLJITBuilder().create()` succeeds), bypassing the C++ static-destructor chain that still touched LLVM `ManagedStatic` / `llvm_shutdown` state on the disturbed heap and intermittently aborted in glibc `_int_malloc` (#1749, exit 134 after the test had already printed its result). **The sixth (6) is in `emitRuntimeError` (`src/codegen_call_user.cpp`)**: the trap path emits `_Exit(1)` instead of `exit(1)`, via a parallel `getStdlibImmediateExit` helper (#1838). The JIT'd code reaches `emitRuntimeError` by calling the libc abort function directly from the JIT module — `finalizeAfterPossibleJit` does NOT cover this path because control never returns to `main()`. Without `_Exit`, the libc atexit chain fires while the JIT module is still live, invoking `PassRegistry::~PassRegistry` (and other `ManagedStatic` destructors) which then double-free against the JIT heap and intermittently SIGABRT before the expected `ExitedWithCode(1)` is observed (`CoerceResultOkNestedResultMismatchTraps`, ASan-masked). Non-JIT exits (help, formatter, parse-time failures before any LLJIT instance is created) still run normal teardown — the flag gates the (5) bypass narrowly, and (6) only affects JIT'd code's own abort path. The user-facing Ry `exit()` builtin (`emitExit` in `codegen_match.cpp`) deliberately keeps `exit()` semantics; only the internal trap path moves to `_Exit`. All six are required — dropping any one leaves a residual failure rate. These are workarounds, not a root-cause fix; underlying LLVM ORC / JITLink heap pattern is unidentified upstream. On Linux CI, treat this signature as flake (re-run); on ASan, persistent crashes mean user-code OOB/UAF, not flake. Do NOT suppress the LLVM crash reporter or add `|| true`.

The parallel spec runner (`src/jit/test_runner.cpp`) caps worker count only by the
number of discovered test files and by the user-supplied value (`-p N` /
`--parallel=N`; bare `-p` defaults to `std::thread::hardware_concurrency`). The
hard-coded four-worker cap was removed in #2177. The LLVM/JIT teardown crash
family is parallelism-dependent — at eight workers, #2172 reproduced the same
`collection_meta_propagation.test.ry` glibc SIGABRT within 3 Linux runs while
the same file passed 20/20 when run alone. Higher worker counts amplify
reproduction rate. When triaging intermittent SIGABRT under parallel test runs,
reduce `-p N` or drop `-p` as the first diagnostic step before assuming a new
regression.

**Rust migration priority**: this teardown family is a priority candidate for the codegen / JIT-runner layer's Rust migration (the v0.0.26+ direction — #1949 / #1950 / #1993). The six-step suppression above only *masks* the residual failure rate: it deliberately leaks `LLJIT` / `CodeGen` / the parsed AST and `_exit`s past the static-destructor chain to dodge a C++ teardown ordering that double-frees against the JIT-disturbed heap — every recurrence so far has been answered by adding another suppression step. The root cause (LLVM ORC / JITLink module ownership colliding with glibc heap-consolidation timing during teardown) is better addressed by reimplementing the module / JIT lifetime in Rust, where explicit `Drop` order and lifetimes can replace the leak-and-bypass workaround instead of accreting a seventh step. Still live as of PR #2012's CI `test` job (Actions run 26868650549): `collection_meta_propagation.test.ry` printed all passing `it` blocks, then exited 134 with `malloc(): invalid next->prev_inuse` during teardown — that PR's diff was `.claude/`-only, confirming the crash is orthogonal to the change under test.

**Subprocess-runner perturbation note (#2236)**: changes to `src/jit/test_runner.cpp` / `include/ry/jit/test_runner.hpp` / the `main.cpp` `test`-subcommand dispatch can **modulate** the #1895 fire rate by perturbing per-spawn allocation, even when the diff never touches the JIT or the failing test's code path. Empirical example from #2236: building outline argv as `std::vector<const char *>` instead of a stack `const char *argv[]` raised the fail rate to ~31% (9/29) on macOS — far above the 5–10% documented band. Switching back to a stack array brought it down to ~8% (8/100), within the documented baseline; origin/main's 0/53 sample at the same time is within sampling noise of that baseline too (P(0/53) at p=5% ≈ 0.07). So the actionable modulation is "heap churn in the spawn path amplifies #1895"; there is no measured residual above baseline once the heap churn is eliminated. Sanitizers stay clean (ASan 208/208, TSan 208/208 in #2236) because the underlying mechanism is teardown timing, not a fresh UAF. **How to apply**: PRs that touch the test-runner subprocess dispatch should run a baseline comparison (`for i in $(seq 1 30); do ./build-rust/ry test -p; done` against origin/main) and report main vs branch fail counts in the PR description; "I didn't touch JIT, so it's unrelated" is contradicted by the modulation data when a 30-run sample lands outside the documented band. Stack-allocate the argv buffer at `runTestFileSubprocess` (don't introduce per-spawn `std::vector` / `std::string` constructions) — that is the only mitigation available short of an upstream fix.

**Policy enforcement (#2238)**: the per-file process boundary this suppression structurally requires is encoded as a code-side guard rule in `.claude/rules/test-runner-isolation.md` — see the sibling entry below for the policy summary.

#### Test execution isolation: per-file process is the only valid unit until the six-step suppression is removed

**Source**: #2238 (formalising the #2232 design verdict on top of the #2234 subprocess fan-out unification)
**Tags**: testing, jit, llvm, orc, isolation, design-invariant

**Rule**: `ry test` の隔離単位は per-file subprocess のみ。in-process sequential loop（削除済み `runTestFilesSequential` の形）の復活、parent process 内での `runRySource` / `runRyFile` ループ、`@it` / `@describe` 粒度の並列化はすべて禁止。

**Why**: the six-step ORC teardown suppression (sibling entry above) deliberately leaks `LLJIT` / `CodeGen` / `Program` AST, so process `_exit` is the only memory-reclaim path — "1 source file = 1 process" is a structural requirement, not a preference. Sub-file granularity additionally violates the `@beforeAll` / `@afterAll` lifecycle contract, which is defined per source file. Code-side enforcement and per-bullet rationale: `.claude/rules/test-runner-isolation.md`.

**Workaround status**: revisitable when #742's root-cause fix or the Rust JIT migration (#1949 / #1950 / #1993) lands — re-read this entry together with the code-side rule before any in-process orchestration is reintroduced.

#### LLVM 17+ source build: `compiler-rt` belongs in `LLVM_ENABLE_RUNTIMES`, not `LLVM_ENABLE_PROJECTS`

**Source**: #1505 follow-up (2026-05-02)
**Tags**: ci, docker, llvm, compiler-rt, cmake, sanitizer, asan, tsan

**Context**: The first `ry-ci` / `ry-ci-glibc-old` image build for
issue #1505 only set
`-DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra"` in the LLVM cmake
invocation, which produces clang+clang-tools but **not** compiler-rt.
The CI `asan` job failed at link time with
`cannot find /usr/local/llvm/lib/clang/21/lib/x86_64-unknown-linux-gnu/libclang_rt.asan_static.a`,
and the `tsan` job failed with the same pattern for
`libclang_rt.tsan{,_cxx}.a`. From LLVM 17 onward, the runtime
libraries (`compiler-rt`, `libcxx`, `libcxxabi`, `libunwind`) must
be built via `LLVM_ENABLE_RUNTIMES` rather than `LLVM_ENABLE_PROJECTS`
so that the *just-built* clang is used to compile them — putting
`compiler-rt` in `PROJECTS` is silently legacy and produces broken
or no libraries on modern LLVM.

**Rule**: When source-building LLVM in `docker/ci.Dockerfile` /
`docker/ci-glibc-old.Dockerfile`, always include
`-DLLVM_ENABLE_RUNTIMES="compiler-rt"` alongside
`-DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra"`. Disable the
sub-projects ry doesn't use to keep build time bounded
(`COMPILER_RT_BUILD_PROFILE=OFF`, `COMPILER_RT_BUILD_XRAY=OFF`,
`COMPILER_RT_BUILD_MEMPROF=OFF`, `COMPILER_RT_BUILD_ORC=OFF`) but
keep `BUILD_SANITIZERS=ON`, `BUILD_BUILTINS=ON`, `BUILD_LIBFUZZER=ON`
(libFuzzer is gated off in CI but the harness build script needs
the static archive to exist).

**How to verify**: After image rebuild,
`docker run --rm ghcr.io/<owner>/ry-ci:llvm-21 ls /usr/local/llvm/lib/clang/21/lib/x86_64-unknown-linux-gnu/`
should list `libclang_rt.asan.a`, `libclang_rt.asan_static.a`,
`libclang_rt.tsan.a`, `libclang_rt.tsan_cxx.a`,
`libclang_rt.ubsan_standalone.a`, and `libclang_rt.fuzzer.a`. CI
`asan` and `tsan` jobs link cleanly.

#### ASan CI exposed a COMDAT-collision crash from `namespace ry` TU-local name shadowing

**Source**: PR #1729 (2026-05-11)
**Tags**: asan, odr, comdat, linker, libstdc++, runtime, gotcha

**Rule**: When two translation units declare same-named types inside `namespace ry { ... }` —
e.g. `struct Parser` in `src/runtime/native/json.cpp` and `class ry::Parser` in `include/ry/parser.hpp` —
their compiler-generated destructors share the same mangled symbol (`_ZN2ry6ParserD1Ev`). On
Linux libstdc++ the linker COMDAT-picked the public header's destructor and applied it across
every TU, including `__ry_json_parse`. PR #1723 added `std::unordered_set<std::string>
imported_modules_` to `ry::Parser`, making its destructor non-trivial; from that point on
`JsonParser` instances were destructed by code that ran `~unordered_set()` on JSON-struct
memory, reading garbage as the hashtable bucket count and triggering `__asan_memset` of
multi-GB → `AddressSanitizer: unknown-crash`.

**Why ASan was the channel**: The bug was **latent for years** — both destructors were
trivially equal until the qualified-import work added a non-POD member. macOS libc++ happens
to avoid the crash because of memory-layout differences; the Linux libstdc++ asan CI job hit
it deterministically. The default (non-ASan) Linux build *would* have crashed eventually, but
the COMDAT pick + non-trivial destructor produced a multi-GB `memset` that ASan immediately
caught at the shadow-memory boundary — so the asan job pinpointed it while default Linux just
saw a generic `__asan_memset` stall.

**How to apply**: The coding rule (don't shadow public class names from TU-local types in
`namespace ry`) lives in `.claude/rules/runtime-memory-safety.md`. The ASan-side lesson is
that a sudden `__asan_memset` for gigabytes against TU-local memory should be triaged as a
COMDAT-pick collision first — before chasing the apparent crash site as a real heap bug.

#### `arc_alloc` vs `checked_malloc` mismatch is masked under ASan

**Source**: #1007 / #1011 / PR #997 (2026-04-16)
**Tags**: asan, arc, runtime, memory-safety, gotcha

**Rule**: Runtime functions that return heap-allocated structs to Ry code (e.g.
`IOListHeader`, `ListHeader`, `MapHeader`) must allocate with `arc_alloc`, not
`checked_malloc`, because `emitVarDecl` emits a retain that reads `header_ptr -
ARC_HEADER_SIZE` (16 bytes) to bump the strong-count. When the header is `checked_malloc`'d,
that region is malloc metadata, not an ARC counter. Corrupting it and then calling
`arc_release` / `__ry_arc_free_counted` on scope exit causes
`malloc: *** error for object 0x...: pointer being freed was not allocated` on default builds.

**Why ASan masks this**: ASan's allocator uses a different internal layout, so the metadata
region happens not to be misinterpreted as zero — the retain reads "harmless" bytes and the
release succeeds without an OS-level crash report. The default (non-ASan) build is the
canonical reproducer for this category of allocator-mismatch bug; ASan absence of a crash
does NOT prove the runtime function is allocating correctly.

**How to apply**: Reproduce allocator-mismatch suspicions on the **default** (non-ASan)
build first. If a runtime function works under ASan but crashes with `pointer being freed
was not allocated` under default, the most likely cause is an `arc_alloc` vs `checked_malloc`
mismatch on a struct returned to Ry code. The coding rule (use `arc_alloc` for any header
returned to Ry) lives in `.claude/rules/runtime-memory-safety.md` along with the affected-sites
table.

#### `_Exit` vs `exit` SIGABRT trap path is masked under ASan and on macOS

**Source**: #1838 (2026-05-21)
**Tags**: asan, macOS, libsystem, runtime, _Exit, atexit, managed-static, gotcha

**Rule**: Ry's trap path (`emitRuntimeError` codegen + `ry_runtime_trap_exit` C++ helper) calls
`_Exit(1)` rather than `exit(1)` so that the `atexit` chain (LLVM `ManagedStatic` destructors
including `PassRegistry::~PassRegistry`) does not run on a heap still referenced by the JIT'd
module that triggered the trap. The wrong choice (`exit`) produces `free(): invalid pointer`
SIGABRT replacing the expected `ExitedWithCode(1)` before death-test assertions can observe it.

**Why ASan and macOS mask this**: ASan's allocator interceptor catches the would-be SIGABRT
internally — the symptom only reproduces on default (non-ASan) Linux builds. macOS libSystem
malloc tolerates the same atexit-vs-JIT-heap pattern silently because its allocator's
free-tolerance is more permissive. The bug surfaces in `CoerceResultOkNestedResultMismatchTraps`
on default Linux only.

**How to apply**: When testing trap paths (panics, divide-by-zero, OOB, contract violations,
JSON parse errors, etc.), reproduce on the **default Linux** build first; do not assume ASan
green or macOS green imply the trap is wired correctly. The coding rule (`_Exit` not `exit`,
plus the `fflush(stdout); fflush(stderr)` pair that replaces what `atexit` would have done)
lives in `.claude/rules/runtime-memory-safety.md`. The CI `lint` step "Check for banned direct
exit() calls in runtime" auto-blocks regressions.

### UBSan

#### UBSan must disable `vptr` and `function` checks on this project

**Source**: #630 (2026-04-11, implementation)
**Tags**: ubsan, sanitizer, cmake, llvm, cxx-flags

**Context**: When enabling UBSan (`-fsanitize=undefined`) on ry, two
sub-checks fail in ways that have nothing to do with actual
undefined behavior:

1. `vptr` — ry compiles with `-fno-rtti` (to match LLVM's build
   flags, see `CMakeLists.txt:25`). The `vptr` check requires RTTI
   to resolve virtual-call types, so enabling it under `-fno-rtti`
   produces no coverage and in some toolchains hard-errors.
2. `function` — LLVM exposes many C-style function pointers that
   ry casts through `void *` (JIT symbol resolution, runtime
   dispatch, etc.). UBSan's `function` check flags every one of
   these as a type mismatch, drowning out real signal.

**Rule**: When adding or extending UBSan flags in `CMakeLists.txt`,
always pair `-fsanitize=undefined` with
`-fno-sanitize=vptr,function`. Do not try to fix the false positives
by changing the LLVM interop code — the checks themselves are the
wrong tool for this codebase.

**Also**: UBSan and ASan are compatible and are enabled together via
the `asan` CMake preset (`ENABLE_ASAN=ON` + `ENABLE_UBSAN=ON`). TSan
is **not** compatible with either and lives in its own `tsan` preset
(`build-tsan/`). `CMakeLists.txt` enforces this with a
`FATAL_ERROR` if `ENABLE_TSAN` is combined with the others.

#### `static_cast<uint64_t>(-static_cast<int64_t>(mag))` triggers UBSan when `mag == INT64_MIN`

**Source**: #1025 (2026-04-16)
**Tags**: ubsan, codegen, numeric-literal, unary-minus, int64-min, signed-overflow

**Rule**: Negating an `int64_t` whose value is `INT64_MIN` is signed-integer overflow
and UBSan reports it at runtime. The `-<NumberExpr>` fast-path in
`src/codegen_expr.cpp::emitExprVariant(UnaryExpr)` originally computed
`static_cast<uint64_t>(-static_cast<int64_t>(mag))` to convert a magnitude back to its
unsigned bit pattern; this is UB precisely at `mag == 2^63` (the literal
`-9223372036854775808` case). Use unsigned two's-complement arithmetic
(`const uint64_t negBits = static_cast<uint64_t>(0) - mag;`) instead — well-defined,
produces the same bits, and silences UBSan.

**Why UBSan was the channel**: The bug was a correctness issue only in the specific
literal-form `-9223372036854775808` (INT64_MIN, which is the canonical edge for any 64-bit
signed conversion). It compiled and produced the right bits on most toolchains because
the codegen never actually used the overflowed value — but UBSan's
`signed-integer-overflow` sanitizer caught the offending negation regardless of whether
the result was used. The unsigned-subtraction fix is portable; the previous form was
toolchain-fragile.

**How to apply**: The coding rule (always use unsigned subtraction for `negBits`; never
negate a `uint64_t` value via `static_cast<int64_t>` when the magnitude may equal 2^(N-1))
lives in `.claude/rules/parser-conventions.md` under the `UnaryExpr fast-path covers bare
int for INT64_MIN` entry. The UBSan-side lesson is that the UBSan job is the most reliable
channel for catching `INT64_MIN`-edge bugs — even if the wrong-bits result happens not to
be observed by any test, UBSan flags the offending negation immediately.

### Rust (corrosion)

#### Rust cdylib (`emit`) internal memory is outside the C++ sanitizer instrumentation scope

**Source**: #1993 (2026-06-03, cutover to Rust)
**Tags**: sanitizer, asan, ubsan, tsan, rust, corrosion, cdylib, emit, instrumentation-scope, ffi-boundary

**Rule**: The Rust cdylib `crates/emit` (the LLVM IR emission shared library) has internal memory that is **outside** the instrumentation scope of the C++ sanitizers (ASan / UBSan / TSan). corrosion does not propagate the CMake `-fsanitize=*` flags into the cargo build (`CMakeLists.txt` calls only `corrosion_set_env_vars(... LLVM_SYS_211_PREFIX=...)`, never `corrosion_add_target_rustflags`; `crates/emit/build.rs` emits only the macOS `dynamic_lookup` link arg), and the CI Rust toolchain is stable (`-Zsanitizer` is nightly-only). The FFI boundary, the host process, and the `ry_runtime_*` libraries remain fully instrumented — only the cdylib's own internal allocations/accesses are not. This mirrors how LLVM itself is linked uninstrumented in both the C++ and Rust eras. Adopting nightly Rust `-Zsanitizer=*` for the cdylib is out of scope (deferred, per #1993).

### libFuzzer

_(follow-up issue で集約予定 — 現状の手順は `.claude/skills/libfuzzer-harness/SKILL.md`)_
