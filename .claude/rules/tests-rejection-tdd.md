---
paths:
  - "tests/test_*.cpp"
  - "tests/spec/**/*.test.ry"
---

# Tests — Rejection TDD

### Every new rejection branch needs a test that triggers it directly

**Source**: #841 PR review (CodeRabbit, 2026-04-10)
**Tags**: testing, tdd, codegen-error, regression

**Context**: In #841, `@parallel for` gained a new check that rejects
writes to module globals (`src/codegen_stmt_loop.cpp:345-347`). The
committed tests only covered **legal shadowing** cases
(`ParallelForNestedLoopVarShadowsModuleGlobal`,
`ParallelForInFunctionBodyAllowsShadowOfModuleGlobal`). The rejection
branch itself was never executed by any test, so CodeRabbit flagged
it as a major issue: the branch could silently regress.

**Rule**: When you add a `codegenError(...)` / `parserError(...)` /
`return std::nullopt` / any validation check, you MUST add a
regression test whose input directly triggers that error. Happy-path
tests of adjacent legal cases do NOT count as coverage of the
rejection branch.

**How to verify before opening a PR**:

```bash
git diff origin/<base> -- 'src/**' 'include/**' \
  | grep -nE '^\+.*(codegenError|parserError|return std::nullopt)'
```

For each hit, confirm a test exists that executes that exact line.

### New rejections that narrow a form need a positive test for the preserved sibling

**Source**: #1210 (2026-04-20, implementation — advisor call-out)
**Tags**: testing, tdd, parser-error, regression, expect-throw, blind-spot

**Context**: In #1210, `parseFnStatement` was split so that `And` / `Or` / `Not` moved out of the symbolic operator arm (which gained a "no whitespace between `operator` and the symbol" adjacency check) into a keyword arm that still permits a space. Existing `EXPECT_THROW` tests for these keyword operators (`OperatorAndMustReturnBool`, `OperatorOrMustReturnBool`, `OperatorNotMustReturnBool` in `tests/test_parser.cpp`) throw on the bool-return-type check at `parser_decl.cpp:237`, not on parse acceptance. If the split had mistakenly left them under the adjacency-checked arm, those tests would still pass (for the wrong reason: both paths throw) and the regression would ship.

**Rule**: When adding a rejection branch that narrows a previously legal form, and the legal sibling form is only exercised by `EXPECT_THROW` tests that throw on unrelated downstream validation (return type, parameter count, typecheck), add a positive `EXPECT_EQ(prog.size(), 1u)` test whose input satisfies every downstream check. That way the only possible failure is the new rejection itself, and the positive test actually proves the legal form still parses.

**How to apply**: For #1210 this meant `OperatorAndKeywordAcceptsSpaceForm` (`-> bool` + valid params), `OperatorOrKeywordAcceptsSpaceForm`, `OperatorNotKeywordAcceptsSpaceForm`, `OperatorInKeywordAcceptsSpaceForm`, `OperatorAsKeywordAcceptsSpaceForm` alongside the four `OperatorXxxRejectsSpaceForm` negative tests for symbolic operators.

### Relaxing a rejection branch requires flipping (not deleting) existing `EXPECT_THROW` tests that lock in the old narrow spec

**Source**: #1402 (2026-04-27, implementation — advisor call-out); same pattern previously seen in #1397
**Tags**: testing, tdd, expect-throw, expect-no-throw, narrowing, blind-spot, directive

**Context**: #1397 relaxed the user-defined `@directive` argument check so required parameters could be passed positionally (previously named-only). #1402 did the same for defaulted parameters. In both cases there were pre-existing `EXPECT_THROW` tests whose input string (e.g. `@logged("warn")` against `fn logged(label: str = "info")`) was the very form being legalized. Those tests would silently continue to pass for the wrong reason if the new code accidentally still threw on a different validation, hiding a regression in the legalization itself.

**Rule**: When relaxing a rejection branch (changing `throw` → `no-throw` for some input class), grep `tests/` for existing `EXPECT_THROW` blocks whose source string falls inside the newly-legal class. Each such test must be **flipped to `EXPECT_NO_THROW`** (and renamed if the test name encoded the old rejection — e.g. `DeprecatedPositionalArgOnRecordError` → `DeprecatedPositionalArgOnRecordAccepted`), not deleted. Flipping locks the new positive spec under the same input that previously locked the old negative spec; deleting silently surrenders coverage of that exact wording.

**How to apply**: For #1402 this caught `DeprecatedPositionalArgOnRecordError` in `tests/test_codegen_directive.cpp` (line 467, the `@deprecated("old")` form on records). The grep:

```bash
grep -nE 'EXPECT_THROW.*<input-pattern-being-legalized>' tests/
```

Run it for every input shape your relaxation legalizes (e.g. positional defaulted, positional required, named required), not just the central case. Combine with the existing rule "New rejections that narrow a form need a positive test for the preserved sibling" — these are mirror images: narrowing needs new positive tests; widening needs flipped negative tests.

### Defensive ptr-shape validation guards are unreachable from Ry source — no regression test required

**Source**: #987 (2026-04-16, fix)
**Tags**: codegen, collections, set, defensive, testing

**Context**: #987 added a guard at each of the five `emitSetElementLookup`
call sites that pass `ptrTy_` elements: after reading `elemName` from
`getSetElemName`, the guard calls `inferCollectionTypeName(elem)` and emits
`codegenError` if the two names differ (e.g. element is `Map<str,int>` but
set expects `List<int>`).

These branches are **unreachable from Ry source** today: the Ry type checker
rejects a kind-mismatch before codegen is reached, and no unsafe-cast
operator exists to bypass it. A Ry-source-level regression test therefore
cannot be written without access to a type-checking bypass.

**Decision**: ship the guards without regression tests, consistent with
commit 136166b which added the sibling set-literal consistency guard under
the same reasoning. Instead, document here so a future reader does not:
- waste time trying to write a Ry snippet that triggers the branch, or
- conclude that the branches are dead and remove them.

Affected sites (all in `codegen_call_collection.cpp`, `codegen_call_string.cpp`,
`codegen_expr.cpp`, and `codegen_expr_literal.cpp`):
- `emitCollOp_add` — `"add(): element type mismatch: expected '...', got '...'"`
- `emitSetRemove` — `"remove(): element type mismatch: ..."`
- `emitStrOp_contains` Set path — `"contains(): element type mismatch: ..."`
- `in`/`not in` Set branch — `"'in'/'not in' operator: element type mismatch: ..."`
- `emitSetLiteral` (commit 136166b) — `"set literal has inconsistent element types: '...' vs '...'"`

**If a reachable path is found** (e.g. via a future union type edge case), add
a direct Ry-source regression test per the `/tdd-cycle` skill and remove this exception entry.

### Codegen dispatch chains over a parser whitelist must end with a defensive `else codegenError` — no regression test required

**Source**: #1453 (2026-04-30, hardening — CodeRabbit nitpick deferred from #1448 / #1452)
**Tags**: codegen, dispatch-chain, defensive, parser-whitelist, testing, expect-stmt, matcher

**Context**: `CodeGen::emitExpect` (`src/codegen_test.cpp`) routes `ExpectStmt` to per-matcher IR via an `if (s.matcher == "...") { ... } else if (...) { ... }` chain that assigns `cmpResult` in each arm. The accepted matcher names are constrained by the parser whitelist (`matchers_with_arg` / `matchers_no_arg` in `src/parser.cpp`), so any value reaching codegen is one of the 20 known names today (`matchers_with_arg` 12 + `matchers_no_arg` 7 + the `toBeCloseTo` special-case). Without a closing `else`, a future whitelist extension that forgets the codegen counterpart would either reuse the previous arm's `cmpResult` or feed a `nullptr`/dangling value into the trailing `builder_.CreateCondBr(cmpResult, contBB, failBB)` — i.e. malformed IR caught only at LLVM verify time.

**Rule**: When introducing or refactoring a codegen dispatch chain whose name set is gated by a parser-side whitelist, terminate the chain with `else { codegenError("line " + std::to_string(<loc>.line) + ": unknown <kind> '" + <name> + "'"); }`. This catches parser ↔ codegen drift at the codegen stage with an actionable diagnostic, rather than relying on whatever the last arm left in a downstream variable. `codegenError` is `[[noreturn]]` (`include/ry/codegen.hpp:1033-1034`), so the compiler correctly proves no fall-through into the trailing IR emitter and `cmpResult`-uninitialized warnings do not fire.

**Decision**: this branch is unreachable from Ry source today. Triggering it requires either modifying the parser (defeating the purpose) or constructing an `ExpectStmt` AST with an out-of-whitelist matcher via direct C++ — no public API supports this. Ship the guard without a regression test, consistent with the sibling rule above ("Defensive ptr-shape validation guards ...").

**Affected sites**:
- `CodeGen::emitExpect` matcher chain end (`src/codegen_test.cpp` after the `toBeCloseTo` arm) — `"line N: unknown matcher 'X'"`

### `ry_llvm_emit` ABI input-validation guards: test what a supported API can trigger, document the corrupted-ctx remainder

**Source**: #2028 (2026-06-05, PR review hardening — CodeRabbit)
**Tags**: testing, tdd, ffi, abi, llvm-emit, defensive, rust, sentinel

**Context**: The `crates/ry_llvm_emit/src/` ABI entry points are `#[no_mangle] pub unsafe extern "C" fn`. Several (`ry_emit_build_error_from_runtime`, `ry_emit_result_branch`, `ry_emit_get_runtime_fn`) gained NULL/None input-validation guards that return the sentinel (`0` for `RyValueId`, `std::ptr::null_mut()` for `RyValueRef`) instead of dereferencing a bad handle or `unwrap`-panicking across the extern "C" boundary (panic-across-FFI is UB). They mirror the pre-existing guards in `ry_emit_runtime_call`. The CodeGen caller never passes NULL handles or NULL `RyBuildValueFn` trampolines, so **these branches are unreachable from Ry source** — a `ry test -p` / `ry_tests` pipeline run cannot reach them.

**The #987/#1453 line is "triggerable via a supported API?", not "reachable from Ry source?"** Unlike the codegen-internal guards above, the `ry_llvm_emit` ABI is a *public, supported* C entry point and `ry_tests` links the cdylib (`CMakeLists.txt`), so a C++ test can call it directly. Whatever a supported call CAN trigger MUST get a direct regression test (primary rule); only the remainder that needs a *corrupted* ctx falls under the no-test exception.

**Rule / decision for these ABI guards**:
- **Tested directly** (`tests/test_emit_abi_guards.cpp`, runs in the required `test` job): the `ctx == NULL` guard on each function (→ sentinel), and the `build_ok`/`build_err == NULL` guard on `ry_emit_result_branch` (→ sentinel). The None-callback case is reachable with a real `ry_emit_ctx_create` ctx + non-null `res_ty` + NULL callbacks because the `let-else` sits before any builder use. Note: pre-fix behavior was a panic/abort, so author the test as a post-fix lock-in (a clean "red" is impossible — the old code aborts the process).
- **Documented, no test**: the inner `c.module` / `c.context` / `c.builder == NULL` guards. Reaching them needs a *corrupted* `EmitCtxImpl` (a non-NULL ctx whose fields are NULL), which no supported ABI call produces (`ry_emit_ctx_create` always stores the caller's live module/builder/context). Same reasoning as the two sibling entries above — ship the guard, don't write an unreachable test.

**How to apply**: When adding an input-validation guard to a `ry_llvm_emit` ABI function, ask "can a *supported* ABI call sequence reach this branch?" If yes (NULL handle / NULL callback passed straight in) → add a direct-call case to `tests/test_emit_abi_guards.cpp`. If it needs a corrupted internal ctx → document here, do not test. Do not add Rust `#[cfg(test)]` unit tests for these: the crate is `crate-type = ["cdylib"]` and CI's `lint` job runs only `cargo fmt`/`clippy` (no `cargo test`), so a Rust unit test would not be CI-gated — the C++ `test` job is the protected harness.
