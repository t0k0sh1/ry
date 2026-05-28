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

> サニタイザー固有の past-incident 参照を集中管理するセクション。新規エントリは該当サブセクションに追記する。本セクションは現状の flat entry 規約 (`### <heading>`) を一時的に level 3 (sanitizer 名) + level 4 (`#### <entry>`) の階層構造に拡張している (#1945 で導入)。正式な format 規約更新は follow-up issue で扱う。

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
just the LargeMmapAllocator fix. See full details in
`.claude/rules/codegen-llvm-ir-conventions.md` "LLVM ORC JIT
intermittent crash" entry.

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

_(follow-up issue で集約予定 — 現状のエントリは `.claude/rules/runtime-memory-safety.md` 等に散在)_

### UBSan

_(follow-up issue で集約予定)_

### libFuzzer

_(follow-up issue で集約予定 — 現状の手順は `.claude/skills/libfuzzer-harness/SKILL.md`)_
