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

**Context**: `CodeGen::emitExpect` (`src/codegen_test.cpp`) routes `ExpectStmt` to per-matcher IR via an `if (s.matcher == "...") { ... } else if (...) { ... }` chain that assigns `cmpResult` in each arm. The accepted matcher names are constrained by the parser whitelist (`matchers_with_arg` / `matchers_no_arg` in `src/parser.cpp`), so any value reaching codegen is one of the 18 known names today. Without a closing `else`, a future whitelist extension that forgets the codegen counterpart would either reuse the previous arm's `cmpResult` or feed a `nullptr`/dangling value into the trailing `builder_.CreateCondBr(cmpResult, contBB, failBB)` — i.e. malformed IR caught only at LLVM verify time.

**Rule**: When introducing or refactoring a codegen dispatch chain whose name set is gated by a parser-side whitelist, terminate the chain with `else { codegenError("line " + std::to_string(<loc>.line) + ": unknown <kind> '" + <name> + "'"); }`. This catches parser ↔ codegen drift at the codegen stage with an actionable diagnostic, rather than relying on whatever the last arm left in a downstream variable. `codegenError` is `[[noreturn]]` (`include/ry/codegen.hpp:1033-1034`), so the compiler correctly proves no fall-through into the trailing IR emitter and `cmpResult`-uninitialized warnings do not fire.

**Decision**: this branch is unreachable from Ry source today. Triggering it requires either modifying the parser (defeating the purpose) or constructing an `ExpectStmt` AST with an out-of-whitelist matcher via direct C++ — no public API supports this. Ship the guard without a regression test, consistent with the sibling rule above ("Defensive ptr-shape validation guards ...").

**Affected sites**:
- `CodeGen::emitExpect` matcher chain end (`src/codegen_test.cpp` after the `toEndWith` arm) — `"line N: unknown matcher 'X'"`
