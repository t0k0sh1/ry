# KNOWLEDGE.md

Accumulated lessons from PR reviews, implementation work, debugging,
and planning. This file is the project's long-term memory — things that
are not obvious from reading the code but that future contributors
(human or AI) should know.

**Search**: Use `grep -nE` on this file. Each entry has a `**Tags**:`
line for fast lookup, e.g.:

```bash
grep -nE '\*\*Tags\*\*:.*testing' KNOWLEDGE.md
grep -nE '\*\*Tags\*\*:.*codegen' KNOWLEDGE.md
```

**Writing rules**:

- English is preferred (so CodeRabbit / Codex / GitHub search can use it)
- One entry per non-obvious lesson, not per PR
- Each entry must be actionable — state the rule or the fact clearly
- Include a `Source` line (PR number, issue, commit, or `implementation`)
  so future readers can verify context
- Prefer concise entries. When an entry grows longer (e.g. multi-step
  invariants or audit tables), split it or link out to code / docs for
  deeper detail instead of padding the entry.

---

## Index

- [Testing](#testing)
- [Codegen](#codegen)
- [Parser / Lexer](#parser--lexer)
- [Runtime / Memory](#runtime--memory)
- [Regex Engine](#regex-engine)
- [Build / CI](#build--ci)
- [Documentation](#documentation)
- [Commands / Environment gotchas](#commands--environment-gotchas)
- [Stdlib](#stdlib)
- [Review feedback patterns](#review-feedback-patterns)

---

## Testing

### `case` arms in `.test.ry` files must have a non-empty body

**Source**: #804 (2026-04-14, implementation)
**Tags**: testing, case, option, ry-syntax

**Rule**: Every arm of a `case` expression in Ry must have at least one statement. An arm with no body (e.g. `None:` followed immediately by the closing `)`) causes a parser error pointing at the enclosing `describe` or `it` call — which can be confusing.

```ry
# ❌ Parser error — empty arm not allowed
case opt:
    Some(v): expect(v).to_eq(1)
    None:           # ← triggers "unexpected token ')'"

# ✅ Use a flag variable to verify the None path
got_none = false
case opt:
    Some(v): fail("unexpected Some")
    None: got_none = true
expect(got_none).to_eq(true)
```

### CodeGenTest::runSource cannot compile code that imports stdlib packages

**Source**: #842 (2026-04-11, implementation)
**Tags**: testing, codegen-test, stdlib, module-loader, harness

**Context**: `CodeGenTest::runSource` / `expectCompileError` in
`tests/test_codegen_common.hpp` goes directly from `Parser` to
`CodeGen::compile` without invoking `ModuleLoader`. Source that contains
`from math import ...` (or any stdlib import) therefore fails with
`error: unresolved import: math (ModuleLoader should have resolved this)`
because codegen expects the import node to have been pre-resolved.

The only test harness that currently runs `ModuleLoader` is
`ImportTest` (`tests/test_codegen_stmt.cpp:617`), which uses a tempdir
+ `writeFile()` — it is designed for user-level imports of files you
write yourself, NOT for pulling in the real `share/std/*` packages.

**Consequences for custom-emitter compile-error tests**: Rejection
branches inside stdlib-package custom emitters
(e.g. `emitMathPow`'s "requires (float, float) or (int, int)" error)
cannot be covered via `expectCompileError` today. Workarounds:

- Smoke-verify the error via `printf '...\n' | ./build/ry -c` during development
  and document the expected error text in the PR description.
- Add a happy-path test in `tests/spec/<pkg>.test.ry` that exercises the
  *successful* branches of the custom emitter, so any refactor that
  breaks dispatch is still caught by the Ry self-test suite.
- A proper fix is to extend the C++ test harness with a helper that
  sets up a `ModuleLoader` pointing at the repo's `share/` directory.
  Tracked as a future enhancement; not blocking feature work.

**Rule**: If you need a C++ unit test for a rejection branch inside a
stdlib custom emitter, the harness limitation applies — fall back to
smoke tests + document the gap in the PR. Don't add failing
`expectCompileError` tests for `from math import` / `from json import`
/ etc.

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

### Use `-> Unit` in @it/@describe rejection tests to isolate the directive check

**Source**: #1122 (2026-04-18, implementation)
**Tags**: testing, directives, codegen-test

**Rule**: When writing a C++ rejection test for `@it` or `@describe` return-type enforcement, use `-> Unit` as the return type annotation — not `-> int`, `-> bool`, `-> str`, etc.

**Why**: If the test function body (e.g. `expect(1).to_eq(1)`) doesn't return a value of the declared type, codegen fires a secondary error — "function does not return a value on all code paths" — **before** the directive check. The test then passes even if the directive enforcement is removed, silently breaking the regression guard.

`-> Unit` is safe because `expect(...)` naturally returns `Unit`, so the body satisfies the return type. The only path that throws is the directive enforcement itself.

### ARC leak regression tests use `runtime_internal.arc_live_count()` delta assertions

**Source**: #859 (2026-04-16, implementation)
**Tags**: testing, arc, leak-detection, runtime-instrumentation

**Context**: macOS ASan has no LSan; CI runs with `detect_leaks=0`.  The
`runtime_internal` stdlib package (bare `@native`, no separate shared lib —
resolves from the host process's `ry_lib` symbols) exposes a single function:

```ry
from runtime_internal import arc_live_count
```

It returns the running balance of ARC header allocations minus frees
(`int64_t`, relaxed-atomic, monotonic).

**Rule**: To write a leak regression test for an ARC operation, snapshot
`arc_live_count()` before and after, then assert the *delta* (not the
absolute value) is at most a small constant:

```ry
before = arc_live_count()
# ... N iterations that each overwrite an ARC-typed slot ...
delta = arc_live_count() - before
expect(delta).to_eq(k)   # k = #containers still live, not proportional to N
```

Why delta (not absolute): the collection destructor does NOT recursively
release ARC-managed elements (pre-existing "element leak on destructor",
KNOWLEDGE line ≈692).  Absolute counts are therefore always non-zero after
any collection is created.  Delta-based assertions isolate the overwrite
path from this background noise.

**Coverage**: `tests/spec/arc_release_on_index_overwrite.test.ry` contains
the canonical examples.  The counter tracks only ARC *header* allocs/frees
(codegen path via `__ry_arc_alloc_counted` / `__ry_arc_free_counted` and the
C++ helper path in `include/ry/runtime_arc.hpp`).  COW buffer reallocs and
collection internal buffers are NOT counted.

---

## Codegen

### Adding a new type kind requires cross-checking every other type registry

**Source**: #815 (2026-04-11, implementation), extended by #850 (2026-04-11)
**Tags**: codegen, types, registry, duplicate-check, naming

**Context**: Type definitions in `include/ry/codegen.hpp` are stored
across four **user-name** registries: `record_types_` (records),
`enum_types_` (concrete enums), `generic_enum_templates_` (generic
enum templates), and `type_aliases_`. All four share a single user-facing
type name space. The `rejectIfTypeNameTakenByOtherKind()` helper
(`src/codegen_stmt_misc.cpp`) now consults all four before allowing a
new declaration. Each `emitStmt(<Decl>Stmt &)` path (Record, Enum concrete,
Enum generic, TypeAlias) must (a) reject its own same-kind duplicate
with a kind-specific message, and (b) call
`rejectIfTypeNameTakenByOtherKind()` for cross-kind collisions.

`union_type_info_` (`include/ry/codegen.hpp`) is **not** a user-name
registry — it is an **anonymous cache** populated by `resolveType()` in
`src/codegen_type.cpp` when an inline union like `int | str` is
encountered. Its key is a flattened type string, not a user-chosen
name. Named unions like `type Foo = int | str` go through
`TypeAliasStmt` and land in `type_aliases_`, so a cross-kind check
against `union_type_info_` is unnecessary and would be wrong (it would
reject legitimate anonymous cache hits).

**Rule**: When introducing a new type kind (or touching an existing
`emitStmt(<Decl>Stmt &)` path), reject the name if it collides with
**any other user-name type registry**. Pair
record/enum/generic-enum/alias as a single name space. Do not treat
`union_type_info_` as a user-name registry.

**How to verify**: grep the codegen header for
`std::unordered_map<std::string, .*(?:Info|Template|Type)>` to find
every type registry, then confirm each `emitStmt` for a type
declaration consults all user-name registries before inserting. Add a
regression test (`EXPECT_THROW` with `runSource`) per cross-category
pair in both orders — legal cases alone won't catch a missing guard.

### Canonicalization that may collapse shape must be handled at all call sites

**Source**: #844 PR review (CodeRabbit, critical)
**Tags**: codegen, canonicalization, union, types

**Context**: `flattenUnionWithAliases("A | int")` where
`type A = int` collapses to just `"int"`. Downstream `wrapInUnion`
then did `union_type_info_.find("int")`, missed, and dereferenced
`end()`. The fix was to early-return when the flattened result is
no longer a non-literal union.

**Rule**: When introducing a normalization/canonicalization step that
can change a type's **category** (union → scalar, variadic → single,
tuple → element, alias → target), audit every caller that assumes the
original category. Add an explicit early-return for the collapsed case.

**How to verify**:

```bash
grep -n '<normalizer_name>' src/
```

For each hit ask "what if the result is no longer a union/variadic/
tuple?". If the caller indexes a map keyed on the original category,
it needs a guard.

### valueToString element loads must propagate metadata via propagateTypeMeta

**Source**: #811/#836/#810 sweep (2026-04-10, implementation)
**Tags**: codegen, formatter, metadata, collection, nested

**Context**: The formatter in `src/codegen_tostring.cpp` iterates over
`List` / `Map` / `Set` elements by loading them from the container's
data array. Each load yields a fresh SSA `Value*` with no metadata. If
the element type is itself a collection (e.g. `Map<str, List<int>>`),
calling `valueToString(elem)` without propagating the inner type name
falls through to the raw string-pointer path and prints empty strings or
garbage bytes. The `List` branch already had this propagation
(`list_elem_type_name` + `list_elem_fn_type_info` snapshot), but the
`Map` and `Set` branches did not — which is why #811 / #810 went
undetected until this sweep.

**Rule**: Whenever you add a new outer container type to
`valueToString()`, the element-load block MUST:

1. Snapshot both the element type name and any `*_fn_type_info` field
   from the outer container's metadata **before** calling
   `getOrCreateMeta()` (which may rehash `value_metadata_` and
   invalidate pointers).
2. Call `propagateTypeMeta(elemTypeName, elem)` to rebuild the element's
   collection metadata (`list_elem`, `map_value_type_name`, etc.).
3. If the snapshot had `*_fn_type_info`, store it on the element's
   metadata so closures format as `<closure>`.

The union-variant branch needs the same pattern — see the post-#836
code in `codegen_tostring.cpp:200-230`.

The **ADT variant field-load path** in `getOrCreateADTToStringFn()`
(same file) has the same requirement. #1238 surfaced this: without
propagation, fields like `List<Tree>` inside `Tree::Node(int,
List<Tree>)` printed as `""`, enums printed as raw tag integers,
`Option<int>` printed as `Some(Some(0))`, and so on. Call
`propagateTypeMeta(fieldTypeName, fieldVal)` on each field load
instead of just propagating `low_level_type_name`.

**Codegen-time recursion**: ADT formatting MUST go through a per-type
helper function (`getOrCreateADTToStringFn`) rather than inlining the
switch body at every `valueToString` call site. Inlining would recurse
forever at codegen time for self-referential ADTs (Tree → List<Tree> →
Tree → …). The helper caches the emitted function *before* it emits
the body, so subsequent references resolve to an ordinary IR `call`
and codegen terminates. Do not revert this to an inline body.

**How to verify before opening a PR**:

```bash
grep -nE 'CreateLoad.*elem|CreateLoad.*valVal|CreateLoad.*fval' src/codegen_tostring.cpp
```

For each hit, confirm that a metadata snapshot + `propagateTypeMeta`
call follows within ~5 lines, or that the outer container has no
corresponding `*_type_name` metadata slot (e.g. fixed-length arrays).

### wrapInUnion must disambiguate same-LLVM-type variants by metadata

**Source**: #836 sweep (discovered while fixing union collection variants)
**Tags**: codegen, union, variant-dispatch, collection, metadata

**Context**: `src/codegen_match.cpp::wrapInUnion` originally picked the
first variant whose LLVM type matched the value's LLVM type. For unions
like `List<int> | Map<str, int>` this always picks the first variant
because both are `ptrTy_`, so a `Map` value gets wrapped with tag=0
(List) and is then interpreted as a List at dispatch time, producing
garbage output. This bug was invisible before #836 because the formatter
rejected collection variants entirely.

**Rule**: When matching a value to a union component, resolve in three
passes: (1) reconstruct the full source-level type name from the value's
metadata via `buildTypeNameFromMeta()` and match it against
`componentNames` exactly — this handles same-kind variants like
`List<int> | List<str>`; (2) fall back to coarse kind match
(`isListTypeName` / `isMapTypeName` / `isSetTypeName` /
`isFunctionTypeName`) against the same metadata fields — this handles
cases where the canonical name couldn't be built (literals without
annotations, aliases); (3) fall back to LLVM-type equality. The value's
`CodeGen::ValueMetadata` fields (`list_elem`, `list_elem_type_name`,
`map_key` / `map_value` / `map_value_type_name`, `set_elem` /
`set_elem_type_name`, `fn_type_info`) are the source of truth.

**How to apply**: Any future dispatch that uses LLVM type equality to
route between ptr-backed Ry types must first consult the metadata.
`src/codegen_match.cpp::wrapInUnion()` and the
`buildTypeNameFromMeta()` helper in `src/codegen_builtin.cpp` are the
canonical pattern.

### Float formatter uses std::to_chars shortest round-trip, NOT %.17g

**Source**: #808 sweep (2026-04-10), superseded by #1031 (2026-04-16)
**Tags**: formatter, float, precision, tests

**Context**: There are two tempting but wrong approaches for `float` →
`string` conversion:

- `%g` (= `%.6g`): rounds `0.30000000000000004` to `"0.3"`, so `print`
  output contradicts equality comparisons (`0.1 + 0.2 == 0.3` → `false`
  despite `print(0.1 + 0.2)` showing `"0.3"`). This was the original
  approach, fixed in #1031.
- `%.17g`: always 17 digits, turns `3.14` into `"3.1400000000000001"`,
  breaking many existing tests. **Rejected** in the #808 plan.

The correct approach (adopted in #1031) is **`std::to_chars(buf, end, x)`
overload 3 (no format arg, C++17)**, which gives the *shortest* decimal
that round-trips exactly — `"3.14"` for `3.14`, `"0.30000000000000004"`
for `0.1 + 0.2`. No existing literal tests break.

**Rule**: `__ry_any_fmt_float` (`src/runtime_any.cpp`) uses
`std::to_chars` overload 3, then appends `".0"` when the result has no
decimal point, no exponent, and is not `nan`/`inf`. A parallel
`__ry_any_fmt_f32` function handles `f32` values with `float`-typed
`to_chars` (to avoid FPExt changing the shortest representation).

**How to apply**: When modifying float formatting:
- Do NOT switch to `%.17g` (breaks `"3.14"` assertions).
- Do NOT revert to `%g` (loses precision, breaks round-trip).
- Use `std::to_chars` overload 3 (the one with no `chars_format` arg)
  for both `double` and `float` types separately.
- Keep the `".0"` suffix logic: it detects `.`/`e`/`E`/`n`/`N`/`i`/`I`
  to skip correction for nan, inf, and already-fractional values.

### Diagnose type / control-flow mismatches at the frontend, never at LLVM verify

**Source**: #805 / #818 / #821 sweep (2026-04-10)
**Tags**: codegen, diagnostics, ir-verify, frontend

**Context**: Three separate bugs all surfaced as cryptic LLVM IR verify
errors (`icmp ne ptr, i0 0`, `Terminator found in the middle of a
basic block`, `Call parameter type does not match function signature`)
because codegen emitted invalid IR and relied on the LLVM verifier to
flag it. Users saw LLVM internals instead of actionable messages:
- #805: `Result<T, E>` routed to a `T`-expecting runtime call because
  `isJsonValue()` trusted a metadata flag without checking `ptrTy_`.
- #818: `toBool(ptr)` fell through to `ICmpNE(v, ConstantInt::get(ptrTy_,
  0))` which LLVM printed as `icmp ne ptr, i0 0`.
- #821: `emitExit()` created an `unreachable` terminator but did not
  switch insertion blocks, so trailing statements emitted into a
  terminated block.

**Rule**: Whenever codegen is about to emit IR that depends on a type or
control-flow invariant (argument type, terminator absence, block
validity), check the invariant in C++ and call `codegenError(...)` with
a user-facing message **before** the invalid IR is written. Never treat
LLVM verification as a user-visible error surface.

**How to verify**: grep for these smells in your change:

```bash
git diff origin/<base> -- 'src/**' \
  | grep -nE 'ConstantInt::get\(.*->getType\(\)'
git diff origin/<base> -- 'src/**' \
  | grep -nE 'CreateUnreachable|CreateRet|CreateBr'
```

Every `ConstantInt::get(v->getType(), ...)` site must be guarded by an
`isIntegerTy()` check. Every terminator-emitting call that is not the
last thing in its basic block must either be followed by a new block
creation + `SetInsertPoint`, or the caller loop must break on
`GetInsertBlock()->getTerminator()`.

### emitRuntimeError terminates its block; callers must pre-split to continue

**Source**: #745/#795/#796 implementation (2026-04-11)
**Tags**: codegen, runtime-error, unreachable, branch, control-flow

**Context**: `CodeGen::emitRuntimeError` (`src/codegen_call_user.cpp:600-618`)
emits `fprintf(stderr, ...)` → `call exit(1)` → `CreateUnreachable`. Unlike
`emitExit`, it does **not** switch to a fresh dead block afterwards. If the
caller still wants to continue emitting IR along a happy path, the split
must be done **before** the call:

```cpp
llvm::BasicBlock *okBB  = llvm::BasicBlock::Create(*ctx_, "foo.ok",  fn_);
llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "foo.err", fn_);
builder_.CreateCondBr(cond, okBB, errBB);

builder_.SetInsertPoint(errBB);
emitRuntimeError("error: %s\n", ".foo_err", {msgPtr});
// errBB is now terminated by `unreachable` — do NOT keep emitting here.

builder_.SetInsertPoint(okBB);  // continue happy path here
```

Callers that just dump `emitRuntimeError` into the current block without
creating a separate happy-path block will lose any IR they emit afterward
(it gets attached to a block that already ends in `unreachable`, which LLVM
verify rejects).

**Rule**: Treat `emitRuntimeError` like `CreateRet` / `CreateBr` — the call
site owns the basic-block split. See `emitIntZeroDivGuard`
(`src/codegen_call_user.cpp:625-636`) and the top-level `?` path in
`emitExprVariant(ErrorPropagateExpr)` for canonical examples.

### `/` is always float + IEEE 754; `//` and `%` are integer and error on zero

**Source**: #1023 (2026-04-16, implementation)
**Tags**: codegen, operators, ieee754, division, floor-div, modulo

**Rule**: The `/` operator always returns `float` and follows IEEE 754 for
zero divisors — `x / 0` → `±inf` (sign follows the dividend), `0 / 0` → `nan`.
Do NOT add an integer zero-division guard to the `/` branch of
`emitArithmeticOp` (`src/codegen_expr.cpp:1333`) or to the `(Int, Int)`
branch of `__ry_any_div` (`src/runtime_any.cpp:229`).

The integer-semantics operators `//` and `%` do error on zero divisor
(`src/codegen_expr.cpp:1318,1364` and `__ry_any_floordiv` / `__ry_any_mod`).
Low-level native-width integer `/` and `%` (`i32`, `u8`, …) also error
(`src/codegen_expr.cpp:1170-1178`) because they are true integer division,
not float-promoted.

Issue #754 previously added an int-specific guard to `/` that contradicted
the "always float" spec; #1023 reverts that decision for `/` while keeping
integer semantics (and the guards) for `//` and `%`.

### Implicit int↔float coercion is annotation-driven and limited to assignment-like sites

**Source**: #1192 (2026-04-19, implementation)
**Tags**: codegen, type-coercion, narrowing, widening, int, float, coerceToLowLevelType, applyCompoundOp

**Rule**: High-level `int` and `float` variables accept cross-type values
implicitly at **var decl**, **reassign**, and **compound assign** sites. The
fix lives in two places — `coerceToLowLevelType`
(`src/codegen_call_user.cpp`) adds f64→i64 / i64→f64 branches for var decl
and reassign, and `applyCompoundOp` (`src/codegen_stmt_misc.cpp`) adds the
same pair for compound assign, propagating automatically to all 8 call sites
(scalar local/global + 3 field-assign branches + array/map/list element).
Narrowing uses `CreateFPToSI` (truncation toward zero, same as `x as int`).

**Why**: Prior to #1192, `x: int = 2 ** 3` and `x **= 2` failed because `**`
and `/` always return `f64` (see the #1023 entry above). Forcing users to
write `(2 ** 3) as int` everywhere was unergonomic, so we coerce at the
receiver based on the annotation instead of changing the operator's return
type.

**How to apply**:
- The gate is `!isLowLevelTypeName(typeName)` in `coerceToLowLevelType` and
  `getLowLevelTypeName(currentVal).empty()` in `applyCompoundOp`. Low-level
  types (`i64`, `f32`, `u8`, …) never participate — they still require
  explicit `as` casts.
- **Do NOT** extend this to function arguments, if-expr branch unification,
  or struct field init. Those still reject narrowing; users must write
  `as int` at those sites. Return values **are** now coerced implicitly as of
  #1195 (2026-04-19) — the same `coerceToLowLevelType` call is inserted in
  both `emitStmt(ReturnStmt)` (`src/codegen_fn.cpp`, covers named fns and
  block-body lambdas) and the lambda expression-body path
  (`src/codegen_lambda.cpp`). Rejection tests in
  `tests/test_codegen.cpp::IntFloatCoercionBoundaryRejections` (args only)
  and `IntFloatCoercionReturnLowLevelStillRejected` (low-level return types)
  guard the remaining boundaries.
- Low-level compound assign mixing (e.g. `x: i64 = 5i64; x **= 2`) is
  rejected upstream in `emitArithmeticOp` via `checkLowLevelTypeMix` before
  reaching `applyCompoundOp`, **provided the loaded slot value carries its
  container's low-level metadata**. Fixed-size arrays (`buf: i64[1]`) require
  an explicit `propagateTypeMeta(array_elem_type_names_[ai], oldVal)` call
  before `applyCompoundOp`, mirroring the list/map element paths. Without
  that propagation the f64→i64 branch silently truncates. Canary
  `IntFloatCoercionCanaryLowLevelStillRejected` exercises both
  `x: i64 = 5i64; x **= 2` and `buf: i64[1] = [5i64]; buf[0] **= 2`.
- `fptosi` / `fptoui` on NaN, ±inf, or out-of-range values now goes
  through the unified `CodeGen::emitCheckedFPToInt` helper (see the
  `Checked float→int narrowing` entry below). NaN / ±inf / out-of-range
  inputs raise a runtime error and exit with status 1 rather than
  producing poison.

### Checked float→int narrowing: always go through `emitCheckedFPToInt`

**Source**: #1232 (2026-04-20)
**Tags**: codegen, conversion, runtime-check, fptosi, fptoui

**Rule**: New code MUST NOT call `builder_.CreateFPToSI` or
`builder_.CreateFPToUI` directly on a user-visible value. Use
`CodeGen::emitCheckedFPToInt(val, targetTy, typeName, bbPrefix, siteLabel)`
instead. The helper inserts a NaN / ±inf / range guard before the
narrowing LLVM op and branches to `emitRuntimeError` on failure, matching
the project-wide convention (integer overflow, division by zero, bounds
checks) of exiting with status 1 instead of producing poison.

**Why**: LLVM `fptosi` / `fptoui` return poison (see LangRef) when the
source is NaN, ±inf, or outside the target's representable range. Poison
propagates silently through downstream arithmetic and is true undefined
behavior. #1192 surfaced this in the implicit `float → int` coercion
path, and the fix sweeps every existing FPToSI / FPToUI site
(`src/codegen_expr_cast.cpp`, `src/codegen_call_user.cpp`,
`src/codegen_stmt_misc.cpp`, and the math rounding path in
`src/codegen_call.cpp`) to share one helper.

**How to apply**:
- `typeName` drives signedness (via `isUnsignedLowLevelName` → `fptoui`)
  and the error message. Pass `"int"`, `"i32"`, `"u8"`, etc.
- `bbPrefix` names the fail/ok basic blocks and the error global;
  keep it unique per call site (`"cast_i"`, `"cast_u8"`, `"floor_i"`, …).
- `siteLabel` (optional) prefixes the error with call-site context, e.g.
  `"floor()"` → `runtime error: floor(): cannot convert %g to int`.
  Leave empty for direct `as` casts.
- The helper accepts both f32 and f64 inputs: f32 is silently promoted
  to f64 for the range check, and the final narrowing uses the original
  `val` so no extra precision is lost.
- Boundary semantics: signed W-bit uses the half-open range
  `[-2^(W-1), 2^(W-1))`, unsigned uses `[0, 2^W)`. This correctly accepts
  `INT64_MIN` (exactly representable in f64) while rejecting `2^63`
  (which f64 rounds from `INT64_MAX`). Do NOT fall back to the symmetric
  `fabs(x) >= 2^(W-1)` check — that spuriously rejects `INT64_MIN`.
- When adapting math-style rounding (`floor` / `ceil` / `round` /
  `trunc`), pass the **result** of the runtime call — not the input —
  because `ceil(9.22e+18)` can round up past `INT64_MAX` even though the
  input was representable.

**How to verify**: `grep -nE 'CreateFPTo(SI|UI)' src/ include/` should
match only the single call inside `emitCheckedFPToInt` itself.

**Related note**: The inverse direction (`SIToFP` / `UIToFP`) is total
— every integer is representable in f64 modulo IEEE-754 rounding, with
no NaN / inf / out-of-range failure mode — so no runtime check is needed.
Do not reopen this as a separate issue.

### Dual-path builtins must share terminator / cleanup handling

**Source**: #821 sweep (2026-04-10)
**Tags**: codegen, builtins, exit, diverging-call

**Context**: `exit()` had two dispatch paths: (1) as an expression via
`emitBuiltinCore` in `src/codegen_call.cpp`, which explicitly created a
fresh `exit.dead` block after emitting `unreachable`, and (2) as a
statement via `builtins_["exit"]` lambda in `src/codegen.cpp`, which
just called `emitExit()` and left the insertion point on the terminated
block. The statement path (the most common usage) broke LLVM basic
block invariants for any trailing code. Putting the dead-block switch
inside `emitExit()` itself made both paths behave consistently.

**Rule**: When a builtin or helper has post-emit fixup — dead block
creation, ARC cleanup, metadata propagation, trace exit — put the
fixup inside the core helper (`emitExit`, `emitReturn`, ...) so every
dispatch path inherits it. If a fixup lives only in one caller, the
other caller is a latent bug waiting to be triggered.

**How to verify**: grep for all callers of the core helper and confirm
each one either expects the helper's post-state or explicitly
re-establishes its own. If any caller is silent about the post-state,
the fixup belongs in the helper.

### Named arguments for builtins: generic parse, builtin-only dispatch

**Source**: #747 (2026-04-14)
**Tags**: codegen, builtins, named-args, parser, print

**Context**: `print(end="", sep=", ")` required named/keyword argument
support, which did not exist in Ry's function call syntax.

Three options were considered:
1. Full language-wide named args (large scope: AST, type checker,
   overload resolution)
2. Special-case `print` in the parser (parser knows builtin names —
   violates separation of concerns)
3. Generic named-arg parse + builtin-only codegen restriction ← **chosen**

Option 3 adds `std::vector<NamedArg> named_args` to `CallExpr` /
`CallStmt`, teaches `parseArgList()` to detect `ident = expr` (same
lookahead as directive parsing), and restricts usage to builtins at
codegen time. Non-builtin calls with named args emit a codegen error.

The `=` token is never valid inside an expression (equality uses `==`),
so the lookahead is unambiguous.

**Rule**: When adding the next named-arg builtin, only `emitPrint()` in
`src/codegen_call_io.cpp` and the `builtins_` map in `src/codegen.cpp`
need updating. To lift the builtin-only restriction later, remove the
check in `codegen_call_user.cpp` and add type-checker support.

### propagateTypeMeta is single-value; callers decompose tuples

**Source**: #820 + #813 sweep (2026-04-11, implementation)
**Tags**: codegen, metadata, propagation, tuple, destructure, enum

**Context**: Fixing function-return / for-loop destructure metadata
propagation (#820, #813) required teaching `propagateTypeMeta` to
recognize enum type names (concrete and generic templates), but
deliberately *not* to recognize tuple type strings like
`"(int, List<int>)"`. Tuples are not a single metadata carrier — the
components are stored in separate bound variables during destructure,
so the decomposition must happen at the call site that owns those
variables (`emitTupleDestructure` in `src/codegen_stmt_loop.cpp`), not
inside `propagateTypeMeta`. The rejected alternative was to add a
`tuple_elem_type_names: std::vector<std::string>` field to
`ValueMetadata`; this was declined because it would require updating
`propagateMeta`, `hasAnyMeta`, and every metadata-copy site with no
benefit over parsing the string once at the caller (compile-time O(n)).

**Rule**: `propagateTypeMeta(name, val)` fills metadata for a single
value. Composite types (tuples, future records) that need to split
across multiple downstream values must be parsed by the caller using
`splitTypeArgs`, which already understands nested `<>` and `()`.

**How to apply**: When you see a tuple type string arriving at a
destructure or unpacking site (for-loop, `let (a, b) = ...`), parse the
components via `splitTypeArgs(inner)` after stripping the outer parens
and call `propagateTypeMeta(component[i], boundVar[i])` for each
bound variable. Do **not** extend `propagateTypeMeta` itself to write
to a `tuple_elem_type_names` slot — keep the helper single-purpose.

### Enum metadata propagation: list literals of enum values need a direct check

**Source**: #820 sweep (2026-04-11, implementation)
**Tags**: codegen, list, enum, metadata, propagation

**Context**: `emitExprVariant(ListExpr)` in
`src/codegen_expr_literal.cpp` guards its `inferCollectionTypeName`
fallback behind `if (elemTy == ptrTy_)` because list-of-list,
list-of-map, and list-of-set all use pointer-typed elements. Simple
enums are `i64` and ADT enums are LLVM struct values — neither is a
pointer type — so the pointer-guarded path skipped them entirely and
`list_elem_type_name` was never set on the list header. Consequently
`valueToString`'s list branch (which relies on
`propagateTypeMeta(list_elem_type_name, loaded_elem)` to rebuild enum
metadata on each loaded element) received an empty string and printed
the raw i64 bit pattern.

**Rule**: When a container literal is built over a non-pointer element
type that still carries *source-level* metadata (enums today,
potentially small-struct values in future work), copy the element's
`enum_value_type` (or equivalent) into the container's
`list_elem_type_name` explicitly, outside the `elemTy == ptrTy_`
guard. The guard exists for nested-collection tracking, not for
generic metadata propagation.

**How to verify**: grep for `if (elemTy == ptrTy_)` in container
literal paths (`codegen_expr_literal.cpp`) and confirm each of them
handles non-pointer elements whose metadata still needs to reach the
container's element-type-name slot.

### `inferCollectionTypeName` List branch must prefer stored `list_elem_type_name` over `reverseResolveTypeName`

**Source**: #1094 + #1095 (2026-04-17, implementation)
**Tags**: codegen, metadata, list, inferCollectionTypeName, reverseResolveTypeName, nested-list, tuple

**Context**: The List branch of `inferCollectionTypeName`
(`src/codegen_builtin.cpp`) originally called
`reverseResolveTypeName(elemTy)` unconditionally.
`reverseResolveTypeName` is lossy:
- `ptrTy_` → `"str"` (L981 of `codegen_lambda.cpp`), so a
  `List<List<int>>` whose middle list correctly stored
  `list_elem_type_name = "List<int>"` had that info discarded, and the
  outer list was annotated as `"List<str>"` instead of
  `"List<List<int>>"` (#1095: for-loop over `outer[0][0]` ran 0 times).
- Unnamed `StructType` (tuples) → fall-through to `"any"` (L989), so
  a `List<List<(int,int)>>` was annotated as `"List<any>"`, bypassing
  `emitVarDecl`'s annotation fallback and causing the second destructured
  variable to receive wrong metadata (#1094).

The Map branch (`L243-250`) already prefered stored names via
`meta->map_key_type_name` / `meta->map_value_type_name` — the List
branch simply wasn't updated.

**Rule**: In `inferCollectionTypeName`, check
`meta->list_elem_type_name` before falling back to
`reverseResolveTypeName`. Additionally, return `""` for unnamed
`StructType` elements (tuples) so callers (`emitVarDecl`'s annotation
fallback, `codegen_stmt.cpp:491-512`) can derive the correct name from
the source annotation. Do **not** return `""` for `ptrTy_`
unconditionally — plain `List<str>` lists set no `list_elem_type_name`
(string literals have no collection metadata), so returning `""` for
them would break nested-list display (e.g. `to_str([["a","b"],["c"]])`
prints `["",""]` instead of `[["a","b"],["c"]]`).

**How to apply**: When extending `inferCollectionTypeName` for Set or
other container kinds, mirror the same pattern: (1) stored name first,
(2) return `""` only for types with no canonical LLVM-to-name mapping
(currently unnamed StructType), (3) fall back to `reverseResolveTypeName`
for everything else.

### Helper-function returns need both declared-type metadata and dynamic return metadata

**Source**: #1317 (2026-04-22, implementation)
**Tags**: codegen, metadata, function-return, resource, thread

**Context**: Reapplying only `propagateTypeMeta(entry->returnTypeName, callResult)`
at user-function call sites is not enough for values whose runtime metadata
extends beyond the declared type name. `Thread` helper returns need the
resource kind so `thread_join(t)` / `lock_acquire(lock)` type checks pass, but
they may also carry dynamic metadata such as `thread_result` from
`thread_spawn(() => 7)`. With only the declared type restored, helper returns
compiled but `thread_join(mk_thread())` silently fell back to the Unit join path
and produced `0` instead of `7`.

**Rule**: Rebuild canonical metadata from the declared return type at the call
site, but store dynamic return metadata in dedicated per-function slots — not by
copying the full `ValueMetadata` onto `llvm::Function`. Full-copying is not
branch-safe: a function with multiple `return` sites can overwrite or merge
single-valued metadata like `thread_result` / `task_result` / `fn_type_info` and
then replay the wrong snapshot at every call site. Instead, record only the
needed dynamic fields (`ThreadResult`, `TaskResult`, function-return
`FnTypeInfo`) and reject inconsistent values across return branches with a
compile-time error.

### New primitive types must be wired into every type-reflection site

**Source**: #825 PR review (CodeRabbit, 4 comments)
**Tags**: codegen, primitive-type, type-reflection, generics

**Context**: #825 added `Type` primitive + `type_of` builtin. The
main codegen path worked but `reverseResolveType()`, `inferTypeArgs()`
(`src/codegen_fn_generic.cpp`), and `inferExprType()` (lambda
call-site inference) were not updated. Lambdas like
`(x: int) => type_of(x)` inferred their return as `i64`, and generic
inference collapsed `Type` to `any`.

**Rule**: When adding a new primitive type, grep for existing
primitives and update every site. Common sites to check:
`reverseResolveType`, `inferExprType`, `inferTypeArgs`, `resolveType`,
pretty printers, docs type tables.

**How to verify**:

```bash
grep -nE 'i64Ty_|boolTy_|strTy_|f64Ty_' src/ include/
```

The new type should appear wherever these existing primitives do
(justify any exceptions explicitly).

### Generic function type inference must walk TypeNode structurally, not string-match `toString()`

**Source**: #823 (2026-04-11, implementation)
**Tags**: codegen, generics, type-inference, unification

**Context**: The original `inferTypeArgs()` in
`src/codegen_fn_generic.cpp` compared the whole parameter type
`toString()` against the set of type-parameter names
(`if (typeParamSet.count(paramType))`). That check only fires when the
parameter type *is* a bare type variable — so `fn identity<T>(x: T)`
worked but `fn first_of<T>(xs: List<T>)` silently produced no
binding and the caller saw "could not infer type parameter 'T'". Every
container, tuple, and function-type parameter was broken the same way
because `"List<T>" != "T"`.

**Rule**: Inference must recurse over the `TypeNode` variant
(`BasicType` / `GenericType` / `TupleType` / `FnType` / `OptionalType`)
and unify each type variable against the matching slice of the
argument's inferred type name (`inferExprTypeName`, which already
produces shaped strings like `"List<int>"`). Use `unifyTypeParam` in
`src/codegen_fn_generic.cpp` as the reference walker, with
`splitGenericTypeName` / `splitTupleTypeName` / `splitFunctionTypeName`
helpers doing the string-side splitting at top-level commas while
respecting `<`, `>`, `>>`, `>>>`, parens, and brackets. Keep the
LLVM-type fallback (alloca type → `reverseResolveTypeName`) for the
bare-variable + scalar-argument case so existing test coverage keeps
working.

**How to verify**: `tests/spec/generic_infer_container.test.ry` covers
`List`, `Map`, `Set`, tuple, nested, named-local, and cross-parameter
unification cases. `tests/test_codegen_fail.cpp` covers the two error
paths (empty-container "could not infer" and conflicting-bindings).

### `inferExprTypeName` must read alloca metadata for container locals, not just primitives

**Source**: #823 (2026-04-11, implementation)
**Tags**: codegen, type-inference, value-metadata, generics

**Context**: For a local `xs: List<int> = [1, 2, 3]`, the alloca
carries `TypeMeta::ListElem = i64Ty_` but **not** the string field
`list_elem_type_name` — that string is populated only for nested /
non-primitive inner types (`List<Map<str, int>>`). When
`inferExprTypeName(VariableExpr)` fell through to
`reverseResolveTypeName(inferExprType(...))`, it got `ptrTy_` (the
opaque-pointer alloca type in LLVM 15+) and returned `"str"`, which
then caused generic inference to bind `T = str` for a list argument.

**Rule**: In `inferExprTypeName` for `VariableExpr`, look up the alloca
with `findVar(name)` and check `getTypeMeta(TypeMeta::ListElem|MapKey|
MapValue|SetElem, alloca)` FIRST to build shaped names like
`"List<int>"`. Only fall back to `reverseResolveTypeName(alloca->
getAllocatedType())` for structs and primitives. The struct-local
fallback must use `getAllocatedType()`, not the alloca's pointer type,
or bounded generic calls like `get_name<T: Animal>(dog_local)` will
regress to inferring `T = str`.

### `inferReturnType` and `inferReturnTypeName` must be maintained in lockstep

**Source**: #790 (2026-04-11, follow-up to #788)
**Tags**: codegen, lambda, returnTypeName, inferReturnType, type-inference

**Context**: `#788` added `inferExprTypeName` + `returnTypeName` propagation
for the **expression-bodied** lambda branch in `emitExprVariant(LambdaExpr)`.
The symmetric **block-bodied** branch only called `inferReturnType` (which
returns `llvm::Type*` and loses `List<int>` / `Map<K,V>` shape), leaving
`returnTypeName` empty. Downstream `propagateTypeMeta()` is gated on
`if (!info.returnTypeName.empty())`, so collection metadata silently never
reached the call-site alloca — breaking `result.length()` and `result[i]`
on values returned by block lambdas with literal-collection returns.

**Rule**: Any inference helper that produces an `llvm::Type*` for a
lambda/function return (currently `inferReturnType` + `collectReturnTypes`)
must have a sibling that produces the shaped type-name string
(`inferReturnTypeName` + `collectReturnTypeNames` in `src/codegen_lambda.cpp`).
When you touch one, update the other. The call site in
`emitExprVariant(LambdaExpr)` must populate `returnTypeName` on every
inferred-return branch (expression-body AND block-body), not just one.

**How to verify**: `tests/spec/lambda_collection_return.test.ry` covers
block-bodied lambdas returning list / map / set literals and branched
returns with consistent element types. Known limitation (tracked separately):
returning a **local** collection variable from a block lambda still loses
shape — see #883.

### Lambda return-name inference must thread declared param type NAMES, not just `llvm::Type*`

**Source**: #886 (2026-04-11, follow-up to #788 / #790 / #884)
**Tags**: codegen, lambda, returnTypeName, inferExprTypeName, paramType, type-inference

**Context**: `inferExprTypeName` recovers shaped collection names
(`List<int>`, `Map<str,int>`, `Set<T>`) for local variables via `findVar(name)`
→ `TypeMeta`. But lambda parameters are NOT yet in `scope_stack_` during
return-type inference in `emitExprVariant(LambdaExpr)` — their allocas are
created only after inference runs. When a lambda body returned one of its
own parameters (`(xs: List<int>) => xs`, or the block-body equivalent),
`findVar` missed and the fallback
`reverseResolveTypeName(inferExprType(expr, paramTypeMap))` collapsed
`ptrTy_` to `"str"`. The lambda's `returnTypeName` came out wrong and
downstream `propagateTypeMeta()` skipped populating `list_elem_type_name` /
`map_*_type_name` / `set_elem_type_name` on the call-site alloca, breaking
`result.length()` / indexing on the returned collection.

**Rule**: Whenever `paramTypeMap` (name → `llvm::Type*`) is built and passed
to `inferExprTypeName` / `inferReturnTypeName` / `collectReturnTypeNames`,
build a parallel `paramTypeNameMap` (name → resolved Ry type-name string via
`resolveTypeAlias(p.type->toString())`) and thread it through the same three
helpers. In the `VariableExpr` case, consult `paramTypeNameMap` BEFORE
falling back to the `inferExprType` path. For nested `LambdaExpr` inside a
lambda body, extend BOTH maps symmetrically. This is the parameter-side
analog of "`inferReturnType` and `inferReturnTypeName` must be maintained in
lockstep" — the string path needs its own data source because `llvm::Type*`
is lossy for opaque-pointer collection types.

**How to verify**: `tests/spec/lambda_collection_return.test.ry` under the
"lambda returning its collection parameter" describe block covers both
expression-body and block-body returns of `List<int>`, `Map<str, int>`,
`Set<int>`, and nested `List<List<int>>` parameters, plus a branched
block-body case that exercises `collectReturnTypeNames` → `IfStmt` walking.

### Ry records are SSA struct values, not pointers — nested field writes unroll up to the root alloca

**Source**: #812 (2026-04-11, implementation)
**Tags**: codegen, records, lvalue, insertvalue, chained-lhs

**Context**: Records in Ry are *value types*: a `record Point: x: int; y: int`
is emitted as `{i64, i64}` stored in an alloca. There is no stable address
for an individual field — `CreateLoad(struct) → ExtractValue(x) → ...` gives
you an SSA value, not a pointer you can `CreateStore` into. The #812 fix
for `rec.a.b = v` therefore has to be: load the outer struct, `InsertValue`
at the right field index, and if the target lives deeper in a chain
(`rec.a.b.c = v`) walk back up inserting each updated inner struct into its
parent until we reach the root alloca and do a single `CreateStore`.

**Rule**: When implementing any write through a record field chain, treat
every intermediate record as an SSA value. Never try to take a pointer into
a struct field — use `ExtractValue` / `InsertValue` instead, and end the
chain with one store at the root alloca. See `writeBackFieldChain` in
`src/codegen_stmt_misc.cpp` for the reference pattern.

### Compound assignment must be expanded in codegen, not desugared in parser

**Source**: #812 (2026-04-11, implementation)
**Tags**: codegen, compound-assign, side-effects, chained-lhs

**Context**: Naively desugaring `a[f()] += v` as `a[f()] = a[f()] + v` in
the parser would evaluate `f()` twice — once for the load and once for the
store. That violates the "each subexpression evaluates once" rule and
multiplies CoW work. The fix for #812 introduced `applyCompoundOp` in
`src/codegen_stmt_misc.cpp` and expanded `IndexAssignStmt` / `FieldAssignStmt`
with an optional `compound_op` field that codegen interprets as
"load at the resolved slot → apply op → store at the same slot" — a single
index evaluation per statement.

**Rule**: When adding a new compound-assignment-capable statement form, do
NOT desugar in the parser. Reuse `applyCompoundOp` and evaluate the LHS
address exactly once in codegen. For maps, a compound op on a missing key
is a runtime error (`"compound assignment to missing map key"`) — users
must insert the key explicitly before accumulating.

### Path copy-on-write isolates chained writes through nested collections

**Source**: #812 (shallow baseline) + #854 (path CoW) (2026-04-11)
**Tags**: codegen, cow, arc, nested-collections, path-cow, semantics

**Context**: `emitCowCheck` only copies the top-level header and data
buffer; inner element pointers are bit-copied. The top-level 1-hop CoW
path used by `var[i] = v` / `var.append(...)` keeps this "shallow"
behavior — cheap, documented in `docs/reference/collections.md`, tested
by `tests/spec/cow.test.ry`. Top-level mutations on simple collections
do not need element retain because the leaf slot-overwrite protocol
(#855) owns ownership transfer.

Chained writes (`b[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`) cannot
use the top-level path: the LHS root would resolve to nullptr CoW anchor
and the mutation would leak through aliases. The #854 fix introduces
**path CoW** — a second code path rooted in `emitPathCowForChain` that
walks the LHS inside-out, privatizes every container level whose
`strong_count > 1`, and retains each ARC element of the cloned buffer
(via the previously-unused `emitCowRetainArcElements`). The new
`emitCowCheckSlot` generalizes `emitCowCheck` to accept any writable
slot (alloca, module-global storage, nested record field GEP, heap
container data GEP) rather than just an alloca.

The collection destructors (`__ry_arc_dtor_list/map/set` in
`src/codegen_arc.cpp:633-665`) still only free internal buffers and do
not release ARC-managed elements. This is the pre-existing "element leak
on destructor" bug, accepted by the `detect_leaks=0` budget. Path CoW's
retain-on-clone amplifies this leak by a small constant (one extra leak
per mutation chain on shared inner collections) — same order of
magnitude, tracked as a separate follow-up along with insertion retain
protocol changes.

**Rule**: When implementing a new mutation path that can reach a
container through a chained LHS, route through `emitPathCowForChain`
(the caller passes `s.object` — everything except the innermost leaf
index). Do NOT evaluate `s.object` with a plain `emitExpr` first —
that re-loads the chain against stale parents after privatization.
The top-level 1-hop CoW in `emitCowCheck` still uses
`retainElements=false`; the path CoW path uses `retainElements=true`.
The asymmetry is deliberate: top-level CoW relies on #855's
retain-then-release-old slot protocol at the leaf store to transfer
inner ownership, whereas path CoW needs each intermediate container's
siblings to survive (inner lists that are not on the mutation path).

**Unsupported shapes** (compile-time error from path CoW):
- Method-call roots: `f().items[i] = v`
- Chains that interleave an index hop inside a record field walk
  (e.g. `rec.arr[0].items[i] = v`). Assign the intermediate to a local.
These match the narrow #854 scope; record-of-records chains without
interleaved index hops (`d.out.items[i] = v`) ARE supported via a
multi-level struct GEP through the root alloca.

### Compound-op loaded slot values must propagate container metadata

**Source**: #858, #862 (2026-04-11)
**Tags**: codegen, compound-assign, metadata, arc, collection, index-assign, field-assign

**Context**: `emitStmt(IndexAssignStmt &)` and `emitStmt(FieldAssignStmt &)`
in `src/codegen_stmt_misc.cpp` load the compound LHS from the slot via a
bare `CreateLoad` (for collection slots, #858) or `CreateExtractValue`
(for record fields, #862) before passing it into `applyCompoundOp`. When
the element / field type is an ARC-managed collection (`List<T>`,
`Map<K, V>`, `List<List<T>>`, …) the loaded value is an opaque pointer
with no metadata attached. The value then flows through
`applyCompoundOp → emitBinaryOp → emitArithmeticOp`, whose list-concat
dispatch at `src/codegen_expr.cpp:1015-1023` reads `TypeMeta::ListElem`
from the SSA value (via `getListElementType(lhs)`), **not** the `lhsHint`
string parameter. Without metadata on the load, dispatch falls through
to the str-vs-non-str rejection path and emits a completely misleading
error for a legal `List<int> + List<int>` operation.

The fix is to call `propagateTypeMeta(name, loadedVal)` immediately
after the load, matching the canonical pattern already documented for
`valueToString` element loads. `propagateTypeMeta` internally rebuilds
every `TypeMeta::*` slot from the name string, so one call restores the
full dispatch context. The name source depends on the code path:

- `IndexAssignStmt` (#858): pull from the container's metadata
  (`container_meta->list_elem_type_name` or `map_value_type_name`).
  **Snapshot the name into a local `std::string` *before* calling
  `propagateTypeMeta`** — the helper inserts into `value_metadata_` and
  may rehash, invalidating the `ValueMetadata *` returned by `getMeta`.
- `FieldAssignStmt` (#862): use `info.fields[fieldIdx].type->toString()`
  directly. `.toString()` returns a new `std::string` by value, so the
  rehash aliasing concern from #858 does not apply here. This matches
  the canonical pattern at `src/codegen_expr_literal.cpp:120-122`.

Also note that `src/codegen_stmt.cpp` (empty-list-declaration path) must
record `list_elem_type_name` for `List<List<T>>` exactly the way it
already does for `List<Map>` and `List<Set>` — the asymmetry in the
pre-#858 code caused the `xs: List<List<int>> = []; xs.append(...); xs[0]
+= ...` case to fail even after the `codegen_stmt_misc.cpp` fix.

**Rule**: Any codegen site that loads an element from a container slot
and then feeds it into a **type-dispatched operation** (formatter, binary
op, builtin call) MUST propagate the container's element type name onto
the loaded value. The bare `LoadInst` / `CreateExtractValue` result is
metadata-less. Hint parameters on helper signatures (`lhsHint` on
`emitBinaryOp`) are not the dispatch key — `TypeMeta::*` slots on the SSA
value are.

**How to verify** (grep before opening a PR):

```bash
grep -nE 'CreateLoad.*elem|CreateExtractValue.*field|applyCompoundOp' \
    src/codegen_stmt_misc.cpp src/codegen_tostring.cpp
```

Every such load that flows into `applyCompoundOp`, `emitBinaryOp`, or
`valueToString` should be followed by a metadata-propagation block (or
be an i64/bool/float path where metadata is unnecessary). The remaining
fixed-length array compound path at `src/codegen_stmt_misc.cpp:597-600`
is **not** a missing-metadata site: fixed-length array element types
are restricted to low-level primitive types by a hard compile-time
check at `src/codegen_type.cpp:125` (`array element type must be a
low-level type`), so ARC-managed collection element types are rejected
at type resolution and never reach the compound path.

### Same-element-type collection helpers must pair `setTypeMeta` with `propagateMeta`

**Source**: #1205 (2026-04-19, implementation); #961 precedent (map merge)
**Tags**: codegen, metadata, ListHeader, MapHeader, propagateMeta, setTypeMeta, slice, appended, take, distinct

**Context**: `setTypeMeta(TypeMeta::ListElem, newHeader, elemTy)` only
records the LLVM-level element type. Source-level string metadata
(`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
`map_value_type_name`, etc.) is NOT touched. Without it, downstream
code that needs source-level type names (nested indexing on the result,
`valueToString` formatting, generic dispatch) silently falls through to
defaults and emits confusing errors like "str does not support index
access" for a legal `slice(xs, 0, 1)[0][0]` where `xs: List<List<int>>`.

**Rule**: Helpers that allocate a new collection header and copy
elements verbatim from a source collection of the **same element type**
(slice, take, drop, appended, distinct, …) must call BOTH:

1. `setTypeMeta(TypeMeta::ListElem, newHeader, elemTy)` — LLVM type
2. `propagateMeta(src, newHeader)` — string names, `list_elem_fn_type_info`,
   `nested_list_elem`, `map_value_type_name`, `resource_kinds`, etc.

`propagateMeta` is rehash-safe by construction (`src/codegen_metadata.cpp:
125-131` calls `getOrCreateMeta(dst)` first, then re-fetches `getMeta(src)`),
so no local snapshot is required at the call site — unlike
`propagateTypeMeta(name, val)` which IS subject to the #858 rehash-
invalidation gotcha documented in the entry above.

**Canonical references**: `src/codegen_call_collection.cpp:1204-1208`
(map merge, #961) and `src/codegen_call_collection.cpp:555-558`
(`emitListSlice`, #1205).

**How to apply**: When adding or reviewing a collection helper that
returns `List<SameT>` / `Map<SameK, SameV>` / `Set<SameT>`, confirm the
trailing `setTypeMeta` block is followed by `propagateMeta(src,
newHeader)`. Helpers that change element type (`flatten`, `enumerate`,
`zip`, `map`) need a different approach — see the `enumerate`/`zip`
precedent in `src/codegen_call.cpp:366-443` that manually constructs
`list_elem_type_name = "(T1, T2)"`.

### Collection ops on pointer elements: guard on `list_elem_type_name`, not on `NestedListElem` alone

**Source**: #1262, #1268, #1269 (2026-04-21, bugfix)
**Tags**: codegen, collections, distinct, remove, in, guard, UB, strcmp, list_elem_type_name, positive-whitelist

**Rule**: When a collection helper's pointer-element branch uses `strcmp` (or any other C-string operation), the guard **must** use a positive allowlist based on `list_elem_type_name`, not a single-field blacklist via `getNestedListElementType`:

```cpp
if (elemTy == ptrTy_) {
    const ValueMetadata *meta = getMeta(listVal);
    const std::string &elemName = meta ? meta->list_elem_type_name : std::string{};
    const bool isNonStrName = !elemName.empty() && elemName != "str";
    const bool hasNestedList = meta && meta->nested_list_elem != nullptr;
    const bool hasFnInfo = meta && meta->list_elem_fn_type_info.has_value();
    if (isNonStrName || hasNestedList || hasFnInfo)
        codegenError("<op>() is only supported for lists of primitive values or strings");
}
```

**Why**: `getNestedListElementType` only checks `TypeMeta::NestedListElem`, which `propagateTypeMeta` sets only in the `isListTypeName` branch. Map/Set/function element kinds live in different fields (`map_value_type_name`, `list_elem_fn_type_info`). A blacklist on a single field is structurally incomplete — `List<Map<K,V>>` / `List<fn(...)>` / `List<Set<T>>` all slip through and `strcmp` runs on Map/Set/closure headers (undefined behaviour). `inferCollectionTypeName` (`src/codegen_builtin.cpp:242-274`) returns non-empty `"Map<...>"` / `"List<...>"` / `"Set<...>"` for non-str pointer types, so treating `""` as str is safe (see sibling entry "List<str> literals can have empty list_elem_type_name"). Resource element lists (`List<TcpStream>`) get a non-empty `list_elem_type_name` and are caught by `isNonStrName`; do not check the list value's own `resource_kinds` — lists are not resources themselves, so that field is always empty for list containers. Access `nested_list_elem` directly on the cached `meta` pointer instead of calling `getNestedListElementType` to avoid a second `value_metadata_` lookup.

**How to apply**: Fixed in `emitCollOp_distinct` by #1262, `emitListRemove` by #1268, and the `in` / `not in` list branch (`src/codegen_expr.cpp` around line 1555) by #1269. Any new collection op that takes a `List<T>` and compares pointer elements must apply the same positive-allowlist guard before any `strcmp` call. Always pair with direct regression tests: add a `List<Map<str,int>>` / `List<fn(...)>` / `List<Set<T>>` case that expects `expectCompileError`; a single-element list must not be the sole reproduction because `curOutLen == 0` masks the symptom on the first iteration of the dedup loop.

**Related**: #1241 (`propagateMeta` added to `distinct` — did not address the guard), "List<str> literals can have empty list_elem_type_name" (#1235), "Same-element-type collection helpers must pair `setTypeMeta` with `propagateMeta`".

### New dispatch branches in `emitArithmeticOp` must precede the str-vs-non-str reject

**Source**: #863 (2026-04-11)
**Tags**: codegen, arithmetic, type-error, collection, dispatch-order

**Context**: `src/codegen_expr.cpp::emitArithmeticOp` uses a
str-vs-non-str reject (`if (lhsIsStr || rhsIsStr) codegenError(...)`) as
the last line of defense for ill-typed `+` operands. The catch is that
`isStringValue()` (`src/codegen_any.cpp:28`) returns `true` for **any**
`ptrTy_` value whose metadata side table has no `hasAnyMeta()` flag —
it is effectively "unknown pointer type", not "string". Consequently
any dispatch branch added AFTER this reject is unreachable for
metadata-less pointer operands (they are already classified as str and
rejected with a misleading message), and adding a branch BEFORE it must
be careful not to steal legitimate str mismatches like `"x" + myMap`.

**Rule**: When adding a new type-specific branch for `+` in
`emitArithmeticOp`, insert it **after** the list-concat success path
but **before** the str-vs-non-str reject. Use the typed metadata
accessors (`getMapKeyType` / `getMapValueType` / `getSetElementType` /
`getListElementType`) to recognize the operand kind. The current order
in `codegen_expr.cpp::emitArithmeticOp` is:
1. String concat / repeat (early return)
2. List + List success → `emitListConcat`
3. Map + Map success → `emitMapMergeCore` (#866)
4. Set + Set success → `emitSetUnionCore` (#866)
5. Mixed/one-sided Map-or-Set reject (named diagnostic, #863)
6. str-vs-non-str reject

**Related**: The companion #858/#862 entry covers the upstream side of
the same trap — metadata propagation on compound-assign loaded slots —
which must also be correct for the dispatch branch to see accurate
kinds on LHS values produced by `m[k] += ...` and friends.
`emitMapMergeCore` and `emitSetUnionCore` are extracted from
`emitCollOp_merge` / `emitSetOp_union` and shared between `CallExpr`
and `BinaryExpr` sites (#866).

### `promoteToInt` / `promoteToFloat` silently widen `i1` (bool) — callers must gate with `rejectBoolInOperator`

**Source**: PR #1030 (2026-04-17)
**Tags**: codegen, arithmetic, bitwise, bool, type-safety, promoteToInt

`CodeGen::promoteToInt` (`src/codegen.cpp`) `ZExt`s `i1` → `i64` without
any type check — it only rejects struct/pointer types via `ensureNumericType`.
Similarly `promoteToFloat` uses `SIToFP` on `i1`. The `**` branch in
`emitArithmeticOp` uses direct `SIToFP` on the raw operand.

**Rule**: Any arithmetic or bitwise branch that calls `promoteToInt`,
`promoteToFloat`, or a direct `CreateSIToFP` on a Ry operand must call
`rejectBoolInOperator(v, op, "arithmetic"|"bitwise")` FIRST if the
expression must reject bool operands. The helper is declared in
`include/ry/codegen.hpp` and implemented in `src/codegen.cpp`.

Do NOT gate the call inside `promoteToInt` / `promoteToFloat` themselves —
those helpers are also used by ARC / string-repeat paths where i1→i64
widening is intentional or at least harmless. The reject belongs at each
arithmetic/bitwise call site.

### Arithmetic bool-reject must be inside each operator's branch, not at function entry

**Source**: PR #1030 (2026-04-17)
**Tags**: codegen, arithmetic, bitwise, bool, type-safety, dispatch-order

`emitArithmeticOp` processes string concat / repeat and collection-concat
BEFORE the numeric branches. `"x" + true` and `"x" + false` auto-stringify
the bool operand and return a `str` — this is intentional and must remain
working. If `rejectBoolInOperator` were called at function entry, those legal
expressions would break.

**Rule**: Place `rejectBoolInOperator` at the entry of each individual
operator branch in `emitArithmeticOp`, not as a top-of-function guard.
This way each branch independently rejects bool without interfering with
string/collection dispatch. The current order in `emitArithmeticOp`:

1. `checkLowLevelTypeMix` — low-level type validation (no bool issue here)
2. `arithLowLevel` branch — exits early for low-level operands (bool is never low-level)
3. `**` branch — dedicated early-exit branch; **insert reject inside this branch** (before `CreateSIToFP`). This branch runs before string dispatch but that's fine: there is no string `**` operation.
4. String concat/repeat auto-stringify (early return) — must NOT be preceded by any bool reject at function level
5. List/Map/Set concat (early return)
6. str-vs-non-str reject (catch-all)
7. `//`, `/`, `%` branches — **insert reject at each branch entry** (before `promoteToInt`/`promoteToFloat`)
8. `+`, `-`, `*` default — **insert reject before `promoteToInt`**

For `emitBitwiseOp`, there is no string/collection dispatch, so
`rejectBoolInOperator` can be placed before `promoteToInt` after the
`bwLowLevel` block exits.

### Element-slot writes must release the overwritten ARC pointer

**Source**: #855 (2026-04-11)
**Tags**: codegen, arc, collections, index-assign, slot-overwrite

**Context**: A list/map/set slot whose element type is itself an ARC-managed
collection holds the sole owning ref to the inner allocation — `append` /
collection-literal construction insert without retain, and the container's
`emitArcReleaseVar` only releases the *container's* header on scope exit
(its destructor walks elements, but only after the container itself dies).
Therefore overwriting a slot via `list[i] = v`, `m[k] = v`, or their
compound forms MUST load the previously-stored pointer and `emitArcRelease`
it before storing the new one, otherwise the prior inner allocation is
orphaned. The fix mirrors the canonical pattern in
`src/codegen_stmt.cpp::emitStmt(AssignStmt&)` ARC branch (the retain-then-
release-with-null-check sequence) but operates on a heap slot rather than
an alloca-backed variable.

`tryRetainArcSource` handles the retain only for two cases: a `LoadInst`
whose pointer operand is an ARC-managed alloca, and a value already in
`arc_owned_values_` (literals / fresh allocations). It does NOT retain for
a `LoadInst` from a GEP (cross-slot copy `xs[i] = ys[j]`), so the slot-
overwrite path manually retains via `emitArcGetHeaderFromData` +
`emitArcRetain` when `tryRetainArcSource` returns false. Order matters:
**retain new before releasing old** so self-assignment `xs[i] = xs[i]`
cannot transiently drop the refcount to zero.

**Rule**: When adding a write into any collection slot whose element is
pointer-typed and ARC-managed, follow this sequence:
1. Determine "is the slot's element type ARC-managed?" via
   `elementTypeIsArcManaged(containerPtr, kind, &elemKind)` — this checks
   the type *name*, not the LLVM type. Strings, weak refs, closures, and
   records are intentionally excluded.
2. For the **plain form**, retain the new value first
   (`tryRetainArcSource`, falling back to a manual `emitArcRetain` when
   the rhs is a Load from a GEP).
3. Load the old slot value with a null check.
4. Release the old value via `emitArcReleaseLoadedElement(...)` (the
   helper handles null guard + CFG branching).
5. Store the new value.
6. For the **compound form** (`xs[i] += v`), reuse the `oldVal` already
   loaded for `applyCompoundOp` rather than re-loading from the slot —
   preserves the "evaluate index/slot exactly once" rule from #812. The
   compound op produces a fresh ARC allocation that never aliases the
   slot, so no retain of the new value is needed.

**Verification gap** (resolved by #859): The ARC balance counter
(`runtime_internal.arc_live_count()`) is now available for delta-based
leak assertions — see the dedicated KNOWLEDGE entry below. ASan/LSan CI
coverage remains tracked separately.

**Follow-up landed**: Records and record fields with ARC fields
(`rec.arcField = newList`) share the same root cause but live in the
`FieldAssignStmt` write path (`InsertValue`/`store` of whole structs).
Fixed in #857 by mirroring the retain-then-release-old protocol in all
three `FieldAssignStmt` branches (VariableExpr / FieldAccessExpr /
IndexExpr) and introducing a string-based `fieldTypeIsArcManaged`
predicate that consults the AST `FieldDef.type->toString()` rather than
container metadata — record fields are typed by declaration, not by
runtime value metadata.

**Follow-up landed** (#854): Deep-chain middle-hop ARC ownership —
intermediate record hops that are shallow-copied through
`writeBackFieldChain`, and more generally any chained LHS whose root is
not a direct `VariableExpr` — is now handled by path CoW
(`emitPathCowForChain` in `src/codegen_arc_cow.cpp`), which drives
`emitCowCheckSlot` at each hop. Record-to-record assignment retains each
ARC field (`emitRecordArcFieldsRetain`) so path CoW can observe
strong_count > 1 at the record-field slot; scope exit releases the
fields (`emitRecordArcFieldsRelease`). See "Path copy-on-write isolates
chained writes through nested collections" earlier in this file for
the full protocol and unsupported shapes.

### One dispatch-table entry per fn name handles multiple overloaded arities

**Source**: #842 (2026-04-11, math overloads implementation)
**Tags**: codegen, stdlib, dispatch-table, custom-emitter, overload

**Context**: The stdlib dispatch tables (`math_table`, `string_table`,
etc.) in `src/codegen_call*.cpp` are keyed by function name, and
`emitTableDrivenNativeCall` (`src/codegen_call_native.cpp:21-28`) does a
first-match-wins linear scan — so adding a second entry for the same
name is a dead entry. For overloaded functions like `math.round` (1-arg
→ int, 2-arg → float) or `math.pow` (`(float, float)` vs `(int, int)`),
you must use a **single entry whose `customEmitter` handles every
arity / type combination internally**.

The custom-emitter gate at `codegen_call_native.cpp:135-149` dispatches
based on the CALL's actual argument count against all registered
`@native` sigs — not against the table entry's `arity` field. That
means: once the new overload is declared as `@native` in
`share/std/<pkg>/<pkg>.ry`, the custom emitter just checks
`e.args.size()` (and argument LLVM types) and routes accordingly. The
table-entry `arity` is effectively metadata / legacy hint for custom
emitters; only the pure-table path (nullptr `customEmitter`) reads it.

Upstream type matching (the type-match loop at
`codegen_call_native.cpp:172-189`) runs only on the non-custom-emitter
path, so **custom emitters must perform their own argument-type checks
and emit clear `codegenError` messages** when types don't match any
supported overload. Otherwise a mixed-type call like
`pow(1, 2.0)` would silently reach the wrong branch.

**Rule**: When adding an overload for an existing stdlib function with
a custom emitter, (1) add the `@native fn` declaration, (2)
extend the existing custom emitter to handle the new arity / type
combination, (3) add explicit type checks with clear errors for
unsupported combinations — do NOT add a second table entry, it will
never fire. See `emitMathFloorCeilRound` / `emitMathLog` /
`emitMathPow` in `src/codegen_call.cpp` for the canonical pattern.

### Every caller of emitSetElementLookup with a pointer-typed element must thread set_elem_type_name

**Source**: #963 (2026-04-15, bugfix)
**Tags**: codegen, collections, set, linear-scan, metadata, equality, hash-table

**Context**: `emitSetElementLookup` decides whether to use structural
equality (linear scan) or hash-by-pointer-as-C-string (hash-table path,
via `__ry_ht_find_str`) based on the `elemName` argument:

```cpp
const bool needsLinearScan =
    llvm::isa<llvm::StructType>(elemTy) ||
    (!elemName.empty() && elemName != "str" && ...);
```

When `elemName` is empty and `elemTy == ptrTy_`, it falls through to
`emitHashTableLookup`, which calls `__ry_ht_find_str(set_ptr, elem_ptr)`.
`__ry_ht_find_str` uses `strcmp` + string hash, so it reads the list/map/set
header bytes as a null-terminated C string. For any two `List<int>` values
of length 2, the ARC data starts with `\x02` (length=2 in varint encoding)
followed by a zero byte — every length-2 list has the same "string" `"\x02"`,
causing all of them to hash and compare equal. This silently deduplicates
distinct elements during set-literal construction, `add`, `remove`, `in`,
and `contains`.

**Root cause**: Several callers of `emitSetElementLookup` did not pass
`elemName` (set it to the empty string default), triggering the wrong path
for nested collections:

- `emitSetLiteral` (set-literal dedup): `src/codegen_expr_literal.cpp`
- `emitCollOp_add`: `src/codegen_call_collection.cpp`
- `emitSetRemove`: `src/codegen_call_collection.cpp`
- `in`/`not in` operator: `src/codegen_expr.cpp`
- `contains()` on a Set (routed via `emitStrOp_contains`): `src/codegen_call_string.cpp`

**Rule**: Every call site that passes a pointer-typed element to
`emitSetElementLookup` must thread `set_elem_type_name`; every call site
that passes a pointer-typed key to `emitMapKeyLookup` must thread
`map_key_type_name`. Both functions use an empty name as the signal to
skip structural equality and fall back to hash-by-ptr-as-C-string, so
omitting the name silently breaks correctness.

Pattern for sets:
```cpp
std::string elemName = getSetElemName(setPtr);  // reads set_elem_type_name
if (!elemName.empty())
    propagateTypeMeta(elemName, elem);
emitSetElementLookup(setPtr, elem, elemTy, elemName);
```

Pattern for maps (use `getMeta()->map_key_type_name`; there is no dedicated
accessor yet):
```cpp
const ValueMetadata *meta = getMeta(mapPtr);
std::string keyName = meta ? meta->map_key_type_name : std::string{};
if (!keyName.empty())
    propagateTypeMeta(keyName, key);
emitMapKeyLookup(mapPtr, key, keyTy, keyName);
```

Also set the type-name field when building the container (e.g. in the
set/map literal emitter), so downstream callers can retrieve it via `getMeta`.
`emitSubsetCheck` (`src/codegen_call_set_ops.cpp`) already follows this
pattern correctly for sets and is the canonical reference.

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
a direct Ry-source regression test per AGENTS.md and remove this exception entry.

### `in`/`not in` dispatch order in `emitExprVariant`

**Source**: #1032 (2026-04-17, feat)
**Tags**: codegen, in-operator, str, dispatch-order, isStringValue

**Rule**: The dispatch order in `emitExprVariant` for `in` / `not in` is:
user overload (`tryOperatorCall`) → Set → Map → List → **str** → error.
The str branch must come *after* Set/Map/List because `isStringValue(container)`
returns `true` for any `ptrTy_` value without collection/resource metadata —
and since Set, Map, and List headers are all tagged with metadata
(`hasAnyMeta() == true`), `isStringValue` correctly excludes them.
Adding str before Set/Map/List would fire on plain `str` RHS before the
collection branches even run.

**LHS widening**: when the RHS is `str` but the LHS has type `any`, unwrap with
`unwrapFromAny(elem, ptrTy_)`. The "wrapInAny" direction is not needed — str has
no element type to promote into. Any other LHS type (int, float, bool, collection)
emits a compile error: `"'in'/'not in' operator: left side must be str when right side is str"`.

**Empty needle**: `"" in s` is `true` for any `s` — the runtime (`__ry_str_find_byte`)
returns `0` when `nl == 0`, matching Python and the existing `contains` semantics.

### `tryRetainArcSource` LoadInst cases must be metadata-gated

**Source**: #1266 (2026-04-21, fix)
**Tags**: codegen, arc, retain, tryRetainArcSource, container-element, GEP-load, memory-safety

**Context**: `tryRetainArcSource` is the central retain-side helper for AssignStmt
(new variable + reassignment), function return, call-site argument pass, match
binding, type coercion, and lambda capture. Until #1266 it covered only
LoadInst-from-alloca (Case 1), arc_owned values (Case 2), and ExtractValue with
container metadata (Case 3) — leaving GEP-loaded container elements (`xs[i]`,
`m[k]`, `for e in set { ... }`) without a retain. Once #1242 lands the
destructor-side recursive release, this gap immediately becomes a UAF: 
`inner = xs[0]; xs = []` would drop the inner list while `inner` still holds it.

**Rule**: When adding a new LoadInst case to `tryRetainArcSource`, gate on
container-element metadata exactly like Case 3:

```cpp
if (llvm::isa<llvm::LoadInst>(val) && val->getType() == ptrTy_) {
    auto *meta = getMeta(val);
    if (meta && (meta->list_elem || meta->map_key ||
                 meta->map_value || meta->set_elem)) { /* retain */ }
}
```

A bare `ptrTy_` type check is **not** sufficient: weak refs, capturing closures,
bare function pointers, and resource handles are all `ptrTy_` and would receive
spurious retains that crash on `−16` offset GEPs into header memory that does
not belong to ArcHeader. The metadata gate is the same protection #999 added to
Case 3 for ExtractValueInst.

**Why not switch all callers to `retainArcValue` instead?**: `retainArcValue`'s
fallback (`emitArcGetHeaderFromData(val)` + `emitArcRetain(...)`) is unconditional.
That works for IndexAssignStmt / FieldAssignStmt (#1108 / #857) where the
caller has already proven the slot is ARC-managed, but it is unsafe for
`tryRetainArcSource`'s callers — `return 42` (i64), `f(closure_arg)` (closure
ptr), or `let x = weak_ref` (weak ref) all reach these sites without an upstream
type guarantee. Adding the case inside `tryRetainArcSource` keeps the metadata
gate as the single audit point.

**`map_value` belongs in the gate too**: For `Map<K,V>`, `propagateTypeMeta` sets
both `MapKey` and `MapValue` flags — `m["a"]` returns the value side, so a gate
that omits `map_value` would miss it. Case 3 currently uses
`list_elem || map_key || set_elem`; if you ever add a Case-3-style retain on a
new instruction kind, include `map_value` (and double-check whether Case 3
itself is missing it for some scenario). Verified for Case 4 in #1266; not
audited for Case 3 yet.

**str elements need a separate flag and a separate header offset**: A str
container element (`xs: List<str>`, `m: Map<K,str>`) uses
`StringHeader` at offset −24, not `ArcHeader` at −16. Case 4 discriminates via
a dedicated `ValueMetadata::str_elem` bool and dispatches to
`emitStrGetHeaderFromData` instead of `emitArcGetHeaderFromData`.

**`str_elem` must ONLY be stamped in the List<str> / Map<K,str> / Set<str>
indexer paths** — `codegen_expr_literal.cpp` (List indexer reads
`list_elem_is_str`) and wherever a map/set str value is loaded. Do NOT stamp
it from a shared helper like `propagateTypeMeta("str", ...)`: that helper is
called from ~50 sites (pattern bindings, enum variant fields, Result/Option
payload, function return meta, tuple destructure, …) and many of those are
str values that are **not** container-element borrows. Stamping `str_elem`
there makes Case 4 dispatch through `emitStrGetHeaderFromData(val)` —
`val - 24` — on a pointer that does not own a `StringHeader`, corrupting
adjacent heap state. The corruption is often invisible under macOS libSystem
malloc but crashes under glibc (tested in CI Linux on `enum_generic_param_type.test.ry`'s
`MyOpt<str>::MySome(v)` pattern binding, SIGSEGV / exit 139). Do not be
tempted to reuse `low_level_type_name == "str"` either — `isLowLevelTypeName`
only matches numeric types (i8–i64, u8–u64, f32), so the branch would never
fire, and extending it to include `"str"` pollutes the arith/cast/tostring
paths that switch on `low_level_type_name`.

**Do NOT stamp `list_elem_type_name = "str"` as the signal for List<str>**:
That field is also read by `resolveCollectionDestructor`, and setting it to
`"str"` flips the destructor to the str-aware variant which iterates the
element buffer and calls `emitArcRelease` on each element. That is the right
destructor — but it exposes a pre-existing ARC live-count asymmetry: list
headers are allocated via `__ry_arc_alloc_counted` (counter +1) while str
handles go through `makeString` (counter no-op). Every str element release
then subtracts 1 from a counter it never contributed to, breaking
`arc_split_chars.test.ry` with a large negative delta. The destructor switch
belongs to #1242; #1266 only needs to reach the indexer. Use the side-channel
`ValueMetadata::list_elem_is_str` set by the AssignStmt annotation parser
(`codegen_stmt.cpp`) and read only by the list indexer
(`codegen_expr_literal.cpp` in `emitExprVariant(IndexExpr)` list branch) to
stamp `str_elem` on the loaded element without touching
`list_elem_type_name`. Map<K,str> does not need this side-channel because
`map_value_type_name = "str"` is already wired correctly on the destructor
side (Map values are released symmetric with the str allocation in
`makeStringKeyedMap`-style callers).

**Function parameters use a different path and do not need the side-channel**:
`applyParamTypeMeta` in `codegen_fn.cpp` calls `propagateTypeMeta(ptype,
alloca)` directly, so a `xs: List<str>` parameter alloca picks up
`list_elem_type_name = "str"` via the existing path. The list indexer then
sees that name, calls `propagateTypeMeta("str", elem)`, and `str_elem` lands
on the loaded element. This is safe for parameters because the parameter
alloca is a borrowed view — destructor selection happens at the **caller's
allocation site**, not at the parameter slot. Only AssignStmt (which IS the
allocation site for new variables) must avoid `list_elem_type_name = "str"`.

**Narrowing (#1353, #1354)**: The above "never stamp on the allocation path"
rule has since been narrowed to "never stamp **without** a matching insert-side
retain". PR #1353 (Map literals) and this PR's sibling fix for List/Set
literals (#1354) do stamp `map_key_type_name = "str"` /
`map_value_type_name = "str"` / `list_elem_type_name = "str"` /
`set_elem_type_name = "str"` on the literal's `headerPtr`, but only after the
literal path emits an explicit `retainArcValue` for each str handle via the
`isStrHandle` side-table check. The retain pre-increments the ARC counter by
+1 per element, which the str-aware destructor then decrements symmetrically —
the `makeString`/`__ry_arc_alloc_counted` counter asymmetry is compensated at
insert time rather than suppressed at dispatch time. The rule "no stamp
without retain" still governs the pure-annotation AssignStmt path (`xs:
List<str> = some_call()`): valMeta inheritance propagates the stamp set by
the literal / returned map / returned set, but the annotation fallback in
`codegen_stmt.cpp` still avoids stamping `list_elem_type_name = "str"` for
non-literal sources because those sources may not have paid the insert-side
retain (in practice they did, via their own literal path, but the
annotation-fallback branch runs only when valMeta is empty and cannot verify
this). Use `inner_is_str` side-channel there instead.

**Verification pattern**: Behavioural ARC tests using
`runtime_internal.arc_live_count()` cannot detect missed retains directly —
the counter tracks live allocations, not refcount. Use a FileCheck IR golden
under `tests/filecheck/` that asserts the `arc.retain:` / `arc.retain.done:`
basic-block labels appear after the container-element load, and that the
`getelementptr i8, ptr %elem, i64 -16` (ArcHeader) or `i64 -24` (StringHeader)
offset matches the element's actual header layout. See
`tests/filecheck/arc_retain_list_elem_*.ry`,
`tests/filecheck/arc_retain_map_value_new_var.ry`,
`tests/filecheck/arc_retain_list_str_elem.ry`, and
`tests/filecheck/arc_retain_map_str_value.ry` for canonical patterns.

### Pattern binding ARC and `emitPatternBindingArc`

**Source**: #997 (2026-04-16, fix)
**Tags**: codegen, arc, pattern, match, retain, memory-safety

**Rule**: Every pattern binding arm that extracts a sub-value (via
`ExtractValue` or `Load`) must call `emitPatternBindingArc(val, bindAlloca,
typeSig)` after storing the extracted value. Without this, scoped ARC release
will call `free(ptr - 16)` on a pointer that was never ARC-retained, causing
a crash or refcount underflow.

Affected patterns and what typeSig to pass:
- **SomePattern** / **OkPattern**: `innerSig = extractGenericTypeArg(subjectEnumType, "Option<"/"Result<", 0)`
- **ErrPattern**: `innerSig = extractGenericTypeArg(subjectEnumType, "Result<", 1)`
- **EnumConstructorPattern**: `fit->second.fieldTypeNames[bi]` for each field
- **VariablePattern** / **TuplePattern** / **RecordPattern**: pass `""` (fall through to source-alloca heuristic)

**What `emitPatternBindingArc` does**:
1. If `val` is a record struct (`llvm::StructType`) with ARC fields: retain the ARC fields for `LoadInst`/`ExtractValueInst` sources via `emitRecordArcFieldsRetain`, and insert `bindAlloca` into `arc_field_record_vars_`. Return.
2. If `val->getType() != ptrTy_` (scalar, non-ARC) → return.
3. If `typeSig` resolves to a collection type (`isCollectionTypeName`): retain + mark ARC-managed + insert into `arc_backed_vars_`. Return.
4. If `typeSig` is a function type (`isFunctionTypeName`): retain and mark ARC-managed **only for capturing/uniform closures** (detected via `fn_type_info` metadata on the source value). Bare function pointers are skipped. Return.
5. If `typeSig` is `"str"`: retain via `emitStrGetHeaderFromData` (offset −24), mark ARC-managed, insert into `arc_str_managed_vars_`. Return. (see **`str` is ARC-managed** below). Other non-collection non-closure pointer types: do not retain. Return.
6. Otherwise (no `typeSig`): fall through to `tryRetainArcSource(val)` (source-alloca heuristic); then check `arc_owned_values_`; then check collection-type metadata set by `propagateTypeMeta`.

**`str` is ARC-managed (#1046, updated #1105)**: `str` values are fully ARC-managed since PR #1046. Use `emitStrGetHeaderFromData(handle)` (offset −24, `STRING_HEADER_SIZE`) — **not** `emitArcGetHeaderFromData` (offset −16, `ARC_HEADER_SIZE`) — to get the ARC header for a `str`. Dispatch via `resolveTypeAlias(typeName) == "str"` or check `arc_str_managed_vars_.count(alloca) > 0` before calling retain/release helpers. The `arc_str_managed_vars_` side-table records all AllocaInsts whose type is `str`, enabling correct offset selection in `emitArcReleaseVar`. `fieldTypeIsArcManaged("str")` returns true with `outKind = CollectionKind::Str`. Collection destructors for `List<str>`, `Map<K,str>`, `Set<str>` iterate elements and call `emitArcRelease(emitStrGetHeaderFromData(elem))` before freeing the data buffer.

**ARC header offset dispatch — use helpers, not inline `emitArcGetHeaderFromData` (#1105)**: Never call `emitArcGetHeaderFromData` inline at retain/release sites that may handle `str` handles. Use these helpers instead:

- **`emitArcHeaderForAlloca(handle, alloca)`** — checks `arc_str_managed_vars_.count(alloca)` and dispatches to `emitStrGetHeaderFromData` / `emitArcGetHeaderFromData`. Use at all alloca-origin sites (e.g., `AssignStmt` old-value release).
- **`CapturedArcKind::Str`** enum variant — returned by `detectCapturedArcKind` when the captured alloca is in `arc_str_managed_vars_`. Closure construction retain (`codegen_lambda.cpp`) and closure destructor release (`codegen_arc_cow.cpp`) switch on this variant to call `emitStrGetHeaderFromData` instead of the default `−16` path.
- **`arc_str_owned_values_` guard in ExprStmt** — bare `str` temporaries (e.g., `"foo".to_upper()`) are in `arc_str_owned_values_`, not `arc_owned_values_`. `emitStmt(ExprStmt)` checks both sets and dispatches correctly.

**Why inline `emitArcGetHeaderFromData` is dangerous**: Adding a new ARC-managed type with a non-standard header offset (like `str`'s +8 byte `byte_len` field) requires updating every inline callsite. An inline call that misses the update silently corrupts memory — `−16` into a `StringHeader` lands on `weak_count`, so the atomic add increments the wrong field. Use the helpers above to make future additions opt-in-correct rather than opt-in-wrong. The `@parallel for` capture site in `codegen_stmt_loop.cpp` uses a local `capIsArcStr[i]` vector instead of `emitArcHeaderForAlloca` because the thunk's `FnScope` clears `arc_str_managed_vars_` before the thunk body executes; do not change that pattern.

### StringHeader layout — `str` memory representation (#1022)

**Source**: #1022 (2026-04-16)
**Tags**: str, StringHeader, ARC, memory-layout, NUL, byte_len, makeString

**Rule**: Every Ry `str` value is a pointer to the *data* region of a `StringHeader` block. Never treat a Ry `str` handle as a raw `char*` allocated with `malloc`/`strdup`; always use `makeString`/`makeStringUninit` to create them and `freeStringSlot` to free them.

**Layout** (defined in `include/ry/ry_layout.hpp` and `include/ry/runtime_string.hpp`):

```text
Offset from handle:  Field
  handle - 24       strong_count : i64  (ARC_IMMORTAL for literals; 1 for dynamic)
  handle - 16       weak_count   : i64  (0 always in PR #1022)
  handle - 8        byte_len     : i64  (STRING_BYTELEN_OFFSET = 8)
  handle + 0 ..     data bytes          ← the char* passed to Ry / codegen
  handle + byte_len '\0'                (null terminator for libc compat)
```

Key constants:
- `ARC_HEADER_SIZE = 16` (unchanged — collection headers keep this offset)
- `STRING_HEADER_SIZE = 24` (= ARC_HEADER_SIZE + 8 for the byte_len field)
- `STRING_BYTELEN_OFFSET = 8`

**How to get byte length in C++**: `stringByteLen(handle)` — reads `*(int64_t*)(handle - 8)`.  
**How to get byte length in LLVM IR**: `emitStringByteLen(handle)` — emits a GEP + load.  
**How to free a dynamic str**: `freeStringSlot(handle)` — calls `free(handle - STRING_HEADER_SIZE)`.  
**Literal globals**: created by `buildArcGlobal` in `src/codegen.cpp`; `strong_count = ARC_IMMORTAL` so retain/release are no-ops. GEP index is `(0, 3, 0)` into the struct.

**NUL safety**: All string operations are now fully NUL-safe. Complete list:
- `byte_len`, `length`, `==`/`!=`/`<`/`>`, `+`, `*`, Map/Set key hash+compare (#1022)
- `contains`, `starts_with`, `ends_with`, `find` (#1047)
- `replace` (#1048)
- `substring`, `char_at`, `reverse`, `split("", _)`, `for c in str:`, `enumerate(str)` (#1049)
- `is_empty` (#1069)
- `to_upper`, `to_lower`, `trim`, `trim_start`, `trim_end` (#1050)
- `split(str, delim)` with non-empty delimiter (runtime `__ry_str_split` via `memmem`), `join`, `repeat`, `*` operator (#1051)
- `regex_match`, `regex_search`, `regex_replace`, `regex_split`, `regex_find_all` and all UFCS variants (`is_match`, `search`, `replace`, `split`, `find_all`) — subject + pattern + replacement all length-driven, no `strlen` (#1052). Regex *literal* syntax also supports `\0` to encode a NUL byte in the pattern (`/a\0b/` matches `"a\0b"`) since #1076.
- `json.parse`, `json.stringify`, `json.to_str`, `json.get`, `json.keys` (#1053) — `JsonValue::string_val` and `JsonObjectVal::keys[i]` are now StringHeader handles allocated via `makeString`; `stringByteLen` is the single length source; `escape_string` iterates by index and emits `\u0000` for NUL bytes; `Parser` uses explicit `src_len` from `stringByteLen` instead of `strlen`/`'\0'` sentinel. C++ test helpers updated to wrap literals in `makeString` since `__ry_json_parse` / `__ry_json_get` expect StringHeader handles.
- `io.write_text`, `io.append_text` (#1133) — replaced `fputs(content, f)` (stops at first `\0`) with `fwrite(content, 1, stringByteLen(content), f)`. Binary-transparent round-trip for content now matches `io.write_bytes`. `fclose` return code is still checked so buffered-write errors surface as `Err`.

### C++ `\xNN` hex escape consumes ALL following hex digits — never use `\xNNX` when X is a hex char

**Source**: PR #1053 (test authoring, 2026-04-17). **Tags**: c++, test, nul-safe, string-literal

**Rule**: In a C++ string literal, `\x` consumes every subsequent hex character (0–9, a–f, A–F) as part of a single escape sequence. So `"\x00a"` is NOT `NUL + 'a'`; it is the single byte `0x00a = 10 = '\n'`. This silently produces the wrong byte value instead of a compile error.

**Examples of the trap**:
```cpp
// WRONG: "\x00a" = '\n' (0x0a), "\x00b" = '\x0b', "\x00A" = '\n', "\x00F" = '\x0f'
makeString("k\x00a", 3);  // produces k + 0x0a, NOT k + NUL + 'a'

// CORRECT options:
const char raw[] = {'k', '\0', 'a'};        // char array initializer — unambiguous
makeString(raw, 3);

// OR adjacent string literals (concatenated by the compiler):
makeString("k\x00" "a", 3);                 // "k\x00" ends the hex sequence; "a" is next literal
```

**Why it matters here**: NUL-key disambiguation tests build keys like `k\0a` vs `k\0b`. Using `"k\x00a"` / `"k\x00b"` produces identical keys (`k\n`) and the test silently passes for the wrong reason. Always use the char-array or adjacent-literal form when the byte after `\xNN` is a hex character.

The non-empty-delim `split` now uses `__ry_str_split` in `src/runtime_string.cpp` (replaces inline `strstr`/`strlen`/`malloc` IR). The regex ABI was extended to `(pattern, patternLen, text, textLen[, replacement, replacementLen])` across `include/ry/runtime_regex.hpp`, `src/runtime_regex.cpp`, `src/codegen_call_io.cpp`, and `src/codegen_call_string.cpp`.

**`markArcManaged(tmp)` pre-mark must be guarded by `fieldTypeIsArcManaged`, and `str` fields must also be inserted into `arc_str_managed_vars_`** (Source: #1016, updated #1046):
TuplePattern / RecordPattern / EnumConstructorPattern pre-mark a temporary alloca
(`tmp`) as ARC-managed so the recursive leaf `VariablePattern` binding can emit a
single retain via `tryRetainArcSource` Case 1. Without the guard, _any_ `ptrTy_`
field (including bare fn-ptr and resource ptrs) is incorrectly marked.
Fix (post-#1046): use `CollectionKind fk; if (fieldTypeIsArcManaged(elemSig, &fk)) { markArcManaged(tmp); if (fk == CollectionKind::Str) arc_str_managed_vars_.insert(tmp); }` at all three sites (`src/codegen_match.cpp`). `fieldTypeIsArcManaged` returns true for List/Map/Set/str and false for fn-ptr/resource/etc. The `arc_str_managed_vars_` insertion is required so `tryRetainArcSource` Case 1 dispatches to `emitStrGetHeaderFromData` (offset −24) instead of `emitArcGetHeaderFromData` (offset −16) for str fields. Capturing closure in tuple/record/enum fields is intentionally excluded: `fn_type_info` metadata is not propagated by `propagateTypeMeta` onto `ExtractValue` intermediates, so closure detection is impossible here; this was also the pre-#1008 behaviour.

### `str` does not support `[]` index syntax — guard with `isStringValue` before list/map dispatch

**Source**: #1026 (2026-04-16, diagnostic improvement)
**Tags**: codegen, diagnostics, str, index-access, emitExprVariant, IndexExpr, IndexAssignStmt

**Rule**: In `emitExprVariant(IndexExpr)` (`src/codegen_expr_literal.cpp`) and the `IndexAssignStmt` emitter (`src/codegen_stmt_misc.cpp`), `str` values are `ptrTy_` pointers and pass the "must be ptr" gate. Without an explicit guard they fall through the Map check (no map metadata) and then either (a) hit the List fallthrough and emit the misleading `"cannot determine list element type for index access"` error or (b) reach `emitCowCheck(objPtr, ..., CollectionKind::List)` which may corrupt state.

**Fix**: After the `"index operator requires list or map"` gate and before the Map dispatch, check `isStringValue(objPtr)` and emit a targeted diagnostic:
- Read path: `"str does not support index access; use char_at(s, i) instead"`
- Write path: `"str does not support index assignment; strings are immutable"` — and critically, this guard must come **before** the `emitCowCheck(objPtr, receiverAlloca, CollectionKind::List)` call; passing a `str` pointer with `CollectionKind::List` to `emitCowCheck` is unsafe.

**Why `isStringValue` is the right predicate**: `isStringValue(val)` (`src/codegen_any.cpp:28`) returns true when `val->getType() == ptrTy_` and `isNonStrPointer(val)` is false (i.e., no collection/resource metadata). Lists, Maps, Sets, and resource handles all have metadata (`hasAnyMeta() == true`), so only `str` triggers the new guard. This is the canonical predicate used throughout codegen to distinguish `str` from other `ptrTy_` values.

- **#1184 related**: The same emitter also rejects index values that are `RangeExpr` nodes and routes them to the slice path — see "IndexExpr first index is scalar only — RangeExpr routes to emitListSlice" below.

### `emitExprVariant` (CaseExpr): capture `armEndBB` AFTER `popScope()`

**Source**: #997 (2026-04-16, fix)
**Tags**: codegen, arc, match, phi, case-expr, ir-verification

**Rule**: In `emitExprVariant` for CaseExpr (`src/codegen_match.cpp`), always
record `armEndBB = builder_.GetInsertBlock()` **after** `popScope()`, never
before.

**Why it matters**: `emitPatternBindingArc` may insert ARC retain calls
(`arc.retain` / `arc.retain.done` BBs), and `popScope()` → `emitArcReleaseVar`
may insert `arc.var_release` / `arc.var_skip` BBs. Both operations advance the
insert block. Recording `armEndBB` *before* `popScope()` captures a stale BB
as the PHI predecessor; the actual branch to `mergeBB` comes from the final
`arc.var_skip` BB, causing:

```
IR verify error: PHI node entries do not match predecessors!
  %match.expr = phi i64 [ 0, %match.expr.then ], [ %list_len, %arc.retain.done ]
label %arc.retain.done
label %arc.var_skip
```

**Correct pattern**:
```cpp
llvm::Value *armVal = emitExpr(*arm.value);
popScope();                                          // changes insert block
llvm::BasicBlock *armEndBB = builder_.GetInsertBlock(); // correct post-cleanup BB
builder_.CreateBr(mergeBB);
incoming.push_back({armVal, armEndBB});
```

The same rule applies to any code that records an insert-block BB for later
PHI use while a scope with ARC-managed variables is still open.

### `isStringValue` must not be used to dispatch `weak` header offset

**Source**: #1022 (2026-04-16, implementation)
**Tags**: codegen, weak, str, StringHeader, isStringValue, emitWeakExpr

**Rule**: Do not use `isStringValue(val)` to choose between `STRING_HEADER_SIZE` (24) and `ARC_HEADER_SIZE` (16) offsets when computing the ARC header pointer for a `weak` variable. `isStringValue` returns `true` for **any** `ptrTy_` value that has no `hasAnyMeta()` flag — captured `List<int>` / `Map` / `Set` values inside closure bodies lack collection metadata after the capture injection in `codegen_fn.cpp` / `codegen_lambda.cpp`, so they are misclassified as `str` and get the wrong `-24` offset.

**How**: The inner type name is available from the annotation at the `LetStmt` site (`weakInnerTypeName(*annot)`) and from `weak_inner_type_names_[ptr]` at reassignment. Use `innerName == "str"` there:

```cpp
// codegen_stmt.cpp — initial let
llvm::Value *headerPtr = (innerName == "str")
    ? emitStrGetHeaderFromData(val)
    : emitArcGetHeaderFromData(val);

// codegen_stmt.cpp — reassignment (ptr is the existing weak alloca)
auto it = weak_inner_type_names_.find(ptr);
const std::string &weakInner = (it != weak_inner_type_names_.end())
    ? it->second : std::string{};
llvm::Value *headerPtr = (weakInner == "str")
    ? emitStrGetHeaderFromData(val)
    : emitArcGetHeaderFromData(val);
```

`emitExprVariant(WeakExpr)` (codegen_expr.cpp) must return the raw data pointer; the conversion happens at the call sites above, not inside the emitter.

### Resolve type aliases before registry lookup or type-name equality

**Source**: #1060 (2026-04-17), #1155 (2026-04-19)
**Tags**: codegen, type-alias, resolveTypeAlias, registry-lookup, generics, bounds

**Rule**: Before looking up a user-supplied type name in `record_types_` / `enum_types_` / `type_aliases_`, or comparing it by string equality against another type name, always route it through `resolveTypeAlias()`. Always keep the user-written original in `codegenError` messages — rewriting those to the resolved name degrades the diagnostic.

**Canonical examples**:
- `RecordPattern` case (`codegen_match.cpp:381-393`) — correct template.
- `validateTypeBounds` (`codegen_fn_generic.cpp`) — #1155 regression: alias bound / concrete were looked up and compared un-resolved, producing a false negative.
- `emitWeakUpgrade` (`codegen_arc.cpp`) — #1060 regression: `"MyStr" == "str"` returned false and the wrong header offset was chosen.

**Why**: `type A = B` registers `A` only in `type_aliases_`, never in the canonical registries. A `count(name)` or `name == "str"` without alias resolution yields a false negative whenever the user writes an alias, silently breaking the language-level promise that aliases are transparent. Reviews of any diff that contains `count(...)` / `== "<name>"` against a user-supplied type name should always ask "is this resolved?".

---

### `weak T` header-offset dispatch requires alias resolution

**Source**: #1060 (2026-04-17, implementation)
**Tags**: codegen, weak, str, StringHeader, type-alias, resolveTypeAlias, emitWeakUpgrade

**Rule**: `weakInnerTypeName()` only strips the `"weak "` prefix — it does **not** resolve type aliases. Before comparing the result to `"str"` (to pick `STRING_HEADER_SIZE` vs `ARC_HEADER_SIZE`), callers must feed the result through `resolveTypeAlias`. The preferred pattern is to store the resolved name at the single write site (`codegen_stmt.cpp LetStmt`) so all downstream readers get the canonical form:

```cpp
// codegen_stmt.cpp — initial let (resolve at the write site so all readers get canonical name)
std::string innerName = resolveTypeAlias(weakInnerTypeName(*annot));
// stored into weak_inner_type_names_[ptr]

// codegen_arc.cpp — emitWeakUpgrade (defensive resolve inside the emitter)
const std::string resolvedInner = resolveTypeAlias(innerTypeName);
// use resolvedInner == "str" (not innerTypeName) for the two data-pointer dispatch branches
```

**Why**: `type MyStr = str; w: weak MyStr = weak s` passes `"weak MyStr"` to `weakInnerTypeName`, yielding `"MyStr"`. Without `resolveTypeAlias`, `"MyStr" == "str"` is false → `ARC_HEADER_SIZE` (16) is used instead of `STRING_HEADER_SIZE` (24) → `emitWeakRetain`'s atomic add hits `byte_len` (at `handle−8`) instead of `strong_count` → corrupts `byte_len` → UB / wrong comparison result. Keep `weakInnerTypeName` as a pure static string-slice helper so error messages preserve the user-written alias name (not the resolved RHS).

---

## Parser / Lexer

### Only `fn` is a function declaration keyword; `function` is a normal identifier

**Source**: #1343 (2026-04-23, implementation)
**Tags**: lexer, keyword, function-type, canonical-type-id, migration

**Rule**: The lexer maps only `fn` to `TokenKind::Fn`. The string `function` tokenizes as `Ident`, so it can be used as a variable or parameter name. Function types are spelled `fn(T) -> R` in source; `isFunctionTypeName` and `splitFunctionTypeName` accept only the `fn(` prefix. The compile-time / `to_str` category name for function-typed values is the canonical type id string `"fn"` (not `"function"`). When updating examples or C++-embedded Ry in tests, avoid leaving `function` as a reserved spelling.

### Use depth-tracking for nested delimiters, never naive find()

**Source**: #801 PR
**Tags**: parser, lexer, delimiters

**Context**: `parseFnTypeAnnotation()` originally used
`string::find(')')` to locate the closing paren, which broke on
nested function-typed parameters like
`fn((int) -> int) -> int`.

**Rule**: When scanning for a matching closing delimiter (`)`, `]`,
`}`), always use a depth counter that increments on the opening and
decrements on the closing. Never use `string::find(')')` or similar.
Reuse `CodeGen::findMatchingCloseParen()` in `src/codegen_type.cpp`
as the canonical helper.

### NumberExpr.value holds a non-negative bit pattern, not a signed magnitude

**Source**: #807 + #819 sweep (2026-04-10)
**Tags**: parser, codegen, numeric-literal, u64, bit-pattern

**Context**: The parser accepts integer literals up to `UINT64_MAX`
via `strtoull`, but `NumberExpr.value` is declared as `int64_t`. To
support `u64` max literals (`18446744073709551615` = bit pattern
`0xFFFFFFFFFFFFFFFF` = `int64_t(-1)`) without changing the AST type,
the field is documented to store the non-negative magnitude as a bit
pattern. The invariant breaks only if a parser site tries to be
clever and stores a pre-negated value (e.g., the old pattern-literal
path in `parser_decl.cpp` built `NumberExpr{-val, ""}` for `case -1:`).
Codegen's empty-suffix emit path then cannot distinguish "legitimate
negative from unary minus" from "overflow bit pattern >= 2^63".

**Rule**: `NumberExpr.value` is always the unsigned bit pattern of a
non-negative magnitude. Negation is expressed as
`UnaryExpr("-", NumberExpr{magnitude, suffix})`, the same as
`parser_expr.cpp`. Any new parser site that creates a NumberExpr
from a literal that may be negative MUST wrap the node in a UnaryExpr
instead of storing a pre-negated `int64_t`. In codegen, interpret
`static_cast<uint64_t>(value)` when the target type is unsigned, and
for the empty-suffix path treat `value < 0` as "literal exceeds
`INT64_MAX`" → emit a "integer literal out of range for int" error.

**How to apply**: Never write `NumberExpr{-val, ...}`. Always wrap in
UnaryExpr. The empty-suffix `value < 0` check in
`src/codegen_expr.cpp::emitExprVariant(const NumberExpr &)` depends
on this invariant — a regression would either silently accept
`x = 18446744073709551615` (wrong i64 emit) or reject `case -1:`
(false positive).

### Statement-level LHS should reuse parsePostfixContinuation, not re-implement postfix hops

**Source**: #812 (2026-04-11, implementation)
**Tags**: parser, lvalue, lhs, postfix, assignment

**Context**: Before #812, `Parser::parseStatement` dispatched `Ident [ ... ]`
and `Ident . field` with a hardcoded 1-hop switch, rejecting any chained
form (`list[i].field = v`, `record.a.b = v`, `list[i][j] = v`, etc.) with
"expected '=' after index expression" or "expected '=' after field name".
The fix split `parsePostfix` into `parsePrimary` + `parsePostfixContinuation(base)`
and drives the statement LHS by feeding a synthetic `VariableExpr` base
into the continuation, then inspecting the chain tail (`IndexExpr` /
`FieldAccessExpr` / `CallExpr`) to decide which `*AssignStmt` variant to
emit.

**Rule**: When a new statement form needs to accept the same expression
shapes as an lvalue, do NOT duplicate the `.`/`[` loop in the statement
parser. Extract a continuation helper from `parsePostfix` and call it from
the statement entry point. Dispatch on the chain tail node to choose the
stmt variant.

**How to apply**: Any statement that starts with `Ident` and may accept
chained postfix hops (e.g. a future `move var.a.b into ...` or similar)
should call `parsePostfixContinuation(baseExpr)` rather than re-parsing `.`
and `[` inline. UFCS call statements (`ident.method(args)`) remain a
special case detected before entering the continuation, because they
produce a `CallStmt` rather than an `ExprStmt<CallExpr>`.

### Use strtod, not std::stod, for float literal parsing

**Source**: #819 sweep (2026-04-10)
**Tags**: parser, float, scientific-notation, exception-safety

**Context**: `std::stod` throws `std::out_of_range` on overflow
(e.g., `1e400`), which crashes the frontend before diagnostics can
be produced. #819's scope check explicitly allows overflow to
surface as `+Inf`, matching the runtime `to_float` converter.

**Rule**: Use `std::strtod` + `errno` for parsing float literals in
the frontend. Accept `HUGE_VAL` / `-HUGE_VAL` as valid `Inf` results
and only treat non-zero trailing characters as errors. See
`include/ry/parser.hpp::parseFloatLiteral`.

---

### Avoid std::sto* throw-on-fail converters in parser/codegen paths

**Source**: #1259 (2026-04-21, fuzz_parser crash on array size overflow)
**Tags**: parser, codegen, integer-overflow, exception-safety, strtoull, libfuzzer

**Context**: `std::stoull` / `std::stoul` / `std::stoi` throw
`std::out_of_range` on overflow and `std::invalid_argument` on
non-numeric input. These exceptions propagate out of the frontend
uncaught and abort the process (visible to `fuzz_parser` as
`SUMMARY: libFuzzer: deadly signal`). #1259's repro: `T[99…99]`
(84 digits) in `parseTypeNameSingle` via `std::stoull(value)`.

Additionally, `std::stoull` with default base 10 **silently** parses
hex/binary/underscore-containing tokens by stopping at the first
non-digit character — so `T[0xFF]` pre-#1259 silently became `T[0]`,
and `T[1_000]` became `T[1]`. The lexer's `TokenKind::Number`
encompasses hex (`0xFF`) and binary (`0b10`) literals plus
underscore separators (`1_000`), so any `std::stoull` call against
a Number token's raw `value` string is vulnerable to both crashes
and silent miscounts.

**Rule**: Never use `std::sto*` on lexer-produced numeric strings.
Use `std::strtoull` (or `std::strtoul` for narrower targets) with:
1. `errno = 0` reset before the call
2. `char *end` output parameter
3. Explicit base (`10` for decimal-only, `0` for prefix-sensitive)
4. Reject if `errno == ERANGE || end != c_str() + size()`

Reference site: `src/parser_decl.cpp` array-size branch in
`parseTypeNameSingle` (post-#1259) and `NumberExpr.value strtoull`
entry above for integer literal parsing.

**Known hit sites** (all should be audited with this rule):
- `src/parser_decl.cpp:787` — fixed in #1259 (array size `T[N]`)
- `src/codegen_type.cpp:133` — inline-array type resolution from
  string name; tracked in #1281
- `src/codegen_expr_literal.cpp:109` — tuple numeric field access
  (`.0`, `.1`, ...); tracked in #1281

**How to apply**: When you see a new `std::sto*` call anywhere in
`src/parser*.cpp`, `src/codegen*.cpp`, or any frontend path that
handles user-provided numeric strings, replace it before merging.
A fuzz harness (current: `fuzz_parser`, `fuzz_json`, `fuzz_utf8`)
will catch parser-path regressions, but codegen paths have no
harness as of #1259 — reviewer diligence is the only gate there.

---

### Ry statements don't accept bare-identifier expression statements

**Source**: #798/#799/#800 implementation (case/if expression unification)
**Tags**: parser, statement-grammar, expression-statement, if-block-expr

**Context**: Ry's `parseStatement` (`src/parser.cpp:499-664`) handles
non-identifier tokens as expression statements (`[1,2].map(f)`, `42`,
`"str"`, etc.) via the generic `parseConditional` fallback at line
499-501. But when a statement **starts with an identifier**, the parser
commits to the ident-dispatch path and requires one of `=`, `+=`, `[`,
`.`, `(`, `++`, `--` to follow. So `y` or `y + 1` as a bare statement is
rejected with `expected '=', '+=', ... after identifier`.

**Implication for `if` expression block form** (#798): the tail
expression of `if cond: body else: body` is parsed via `parseBlock` →
`parseStatement`, so the last line cannot be a bare identifier or an
unparenthesized identifier-starting binary expression. Users must wrap
such expressions in parens: `(y + 1)`, `(x)`. This is a **real
limitation** of the block form, not a bug.

**Rule**: When introducing new block-valued expression forms, remember
that Ry's statement grammar cannot parse identifier-starting pure
expression statements. Either (a) require parenthesized tail expressions
in user documentation, (b) use non-identifier expressions (literals,
calls), or (c) write a custom block parser that calls `parseConditional`
directly for the tail line.

### Identifier-trailing `!` tokenization must exclude every multi-char operator starting with `!`

**Source**: #1211 (2026-04-20, bug fix)
**Tags**: parser, lexer, identifier, trailing-bang, bangbang, ambiguity

**Context**: The lexer greedily absorbs a trailing `!` into an
identifier to support mutating method names (`sort!`, `reverse!`,
`append!`, `clear!`). The original guard only excluded `!=`, so `r!!`
tokenized as `r!` (Ident) + `!` (Error) and parse-failed as
`expected ')'` when used in expression position like `Ok(r!!)`. The
postfix error-propagation alias `!!` was thus broken for the
identifier-direct case — the documented equivalence with `?` was only
honored when the preceding token was not an identifier (e.g. after `)`).

**Rule**: The trailing-bang absorption in the identifier branch
(`src/lexer.cpp` identifier tokenization) must exclude **every**
multi-character operator token that begins with `!`, not just `!=`.
Currently that means `!=` and `!!`; any future operator starting with
`!` (hypothetical `!~`, `!?`, etc.) must be added to the exclusion set
at the same time it is introduced in the operator lexer branch.

**How to apply**: When adding a new operator whose first character is
`!`, update both (a) the operator dispatch in `Lexer::next` and
(b) the trailing-bang exclusion in the identifier tokenizer. A unit
test asserting `tokenize("r<op>")` yields `Ident("r")` + the new
operator prevents regression — `tokenize("<op>")` standalone does not
catch it because the bug is specific to the identifier-adjacent case.

---

## Runtime / Memory

### Runtime functions returning ARC-managed structs must use `arc_alloc`, not `checked_malloc`

**Source**: #1007, #1011 (2026-04-16, bug fixes), PR #997 (pattern established for ListHeader)
**Tags**: arc, runtime, memory-safety, iolistheader, listheader, mapheader, http, gotcha

**Rule**: Any runtime function that returns a heap-allocated struct (e.g.
`IOListHeader`, `ListHeader`, `MapHeader`) **to Ry code** must allocate the
struct with `arc_alloc`, not `checked_malloc`.

**Why**: `emitVarDecl` in the codegen emits a retain that reads
`header_ptr - ARC_HEADER_SIZE` (16 bytes) to increment the strong-count.
When the header was allocated with `checked_malloc` that region is malloc
metadata, not an ARC counter. Corrupting it and then calling
`arc_release`/`__ry_arc_free_counted` on scope exit causes a crash:

```text
malloc: *** error for object 0x...: pointer being freed was not allocated
```

ASan builds do not reproduce the crash because the allocator's internal
layout differs and the metadata region happens not to be misinterpreted as
zero.

**Affected sites in this codebase** (checked 2026-04-16):

| Function | File | Fixed by |
|---|---|---|
| `makeStringList`, `makeMatchList` | `include/ry/runtime_list.hpp` | PR #997 |
| `makeByteList` | `include/ry/runtime_io.hpp` | PR #1007 |
| `__ry_read_bytes` | `src/runtime_io.cpp` | PR #1007 |
| `makeEmptyIOList`, `__ry_tcp_receive` | `src/runtime_net.cpp` | PR #1007 |
| `__ry_tls_receive` | `src/runtime_tls.cpp` | PR #1007 |
| `build_str_map` | `src/runtime_http.cpp` | PR #1011 |

**Error-path pairing**: when an allocation succeeds with `arc_alloc` but
the function later bails out before returning to Ry, free with `arc_free`,
not `free`. Example: `__ry_tcp_receive` allocates the header with
`arc_alloc`, then on `recv()` error calls `free(header->data)` (plain `free`
because the data buffer uses `checked_malloc`) and `arc_free(header)`.

**Also**: C++ tests that call these runtime functions directly must also use
`arc_free` (not `free`) to release the returned struct.

### JIT-visible atomic counters must not expose `std::atomic<T>` storage via raw `T*`

**Source**: #1158 (2026-04-23, implementation)
**Tags**: runtime, atomic, jit, abi, layout, arc

**Rule**: If JIT-generated IR needs the address of a counter as a raw
`int64_t*` (or other plain-data pointer), back the storage with an
`alignas(T) T` object and access it through `__atomic_*` builtins. Do
not store the counter as `std::atomic<T>` and then return
`reinterpret_cast<T*>(&atomic_obj)` to the JIT.

**Why**: Ry's JIT emits `atomicrmw` directly against the address returned
by `__ry_arc_counter_address()`. C++ does not guarantee that
`std::atomic<int64_t>` has the same layout or address semantics as a
plain `int64_t`, so exposing its storage as `int64_t*` is a portability
bug even if it happens to work on one toolchain. `__atomic_fetch_add`,
`__atomic_fetch_sub`, and `__atomic_load_n` preserve the required
relaxed-atomic semantics without relying on `std::atomic` object layout.

### RWLock dispatch state must be thread-local, not guarded by a shared mutex

**Source**: #871 (2026-04-11, implementation; follow-up to #630 P1 audit)
**Tags**: rwlock, runtime-thread, concurrency, deadlock, thread-local, gotcha

**Rule**: In `src/runtime_thread.cpp`, the per-thread counter that tells
`__ry_rwlock_unlock` whether to call `unlock_shared()` vs `unlock()` on
the underlying `std::shared_mutex` lives in a
`thread_local std::unordered_map<RWLockHandle*, int>` at namespace
scope. Do NOT reintroduce a shared `std::unordered_map<std::thread::id, int>`
guarded by a separate `mode_mu`.

**Why**: The #630 audit suggested two fixes for the original two-step
race (where `mu.lock_shared()` was held before the tracking map was
updated). Option A — "hold mode_mu across lock_shared()" — **deadlocks**
under writer contention: a thread holding `mode_mu` while blocked
inside `lock_shared()` prevents any existing reader from taking
`mode_mu` to dispatch its own unlock, so the writer it is waiting on
can never make progress. Three-way deadlock: writer waits for existing
reader, existing reader waits for `mode_mu`, `mode_mu` owner waits
for writer. Thread-local tracking (Option B) sidesteps this entirely
because no shared data structure exists for the dispatch.

**Caveat**: unbalanced `rwlock_read_lock` without a matching
`rwlock_unlock` leaves a stale TLS entry that persists until thread
exit. If an RWLockHandle is then freed and a new one is allocated at
the same address, the stale count could be misinterpreted. The
previous map-based tracker had the same property — unbalanced lock
usage is a program bug, not something the runtime should engineer
around.

### Recursive `lock_shared()` on `std::shared_mutex` is undefined behavior in C++17

**Source**: #871 (2026-04-11, CodeRabbit review on PR #885)
**Tags**: rwlock, runtime-thread, std-shared-mutex, ub, gotcha

**Rule**: `__ry_rwlock_read_lock` must call `rwlock->mu.lock_shared()`
**only on the outermost read acquire**, gated by `tls_rwlock_read_counts`
being empty for that rwlock. Nested reads just bump the TLS counter.
`__ry_rwlock_unlock` mirrors this: the underlying `unlock_shared()`
only runs when the TLS counter drops from 1 to 0.

**Why**: Per [thread.sharedmutex.requirements] / cppreference
"If lock_shared is called by a thread that already owns the mutex in
any mode (exclusive or shared), the behavior is undefined." The TLS
counter exists solely to dispatch unlock correctly; it does not make
the underlying mutex reentrant. An earlier draft of the #871 fix
called `lock_shared()` unconditionally and still passed tests on
libc++ because the UB happened to be benign, but it would be a
time-bomb under a stricter standard library or instrumented build.
The regression is covered by
`tests/test_runtime_rwlock_stress.cpp::NestedReadLockPerThread`.

### RWLock stress tests for #871 must be C++ GoogleTests, not Ry spec files

**Source**: #871 (2026-04-11, implementation)
**Tags**: rwlock, testing, tsan, spec-loader, gotcha

**Rule**: When adding a TSan-gated stress test for
`src/runtime_thread.cpp` primitives, put it in a pure C++
GoogleTest under `tests/` (e.g.
`tests/test_runtime_rwlock_stress.cpp`), NOT in
`tests/spec/concurrency.test.ry`. Wire it into `ry_tests` via
`CMakeLists.txt` so it runs under the **required** `build-tsan/ry_tests`
step.

**Why**: The C++ test harness used by `CodeGenTest.ConcurrencySpecSuite`
is `runTestSource` in `tests/test_codegen_common.hpp`, which goes
`Lexer → Parser → CodeGen` directly and never invokes `ModuleLoader`.
Any `from thread import ...` statement in a spec run via that harness
fails with `unresolved import: thread (ModuleLoader should have
resolved this)`. Adding a stress test to `concurrency.test.ry`
therefore silently breaks the TSan-required gate. A pure C++ test
that calls `__ry_rwlock_*` directly via `include/ry/runtime_thread.hpp`
works under all sanitizers, runs in the required step, and is more
direct anyway — we are testing a runtime invariant, not a language
feature. See also the entry at the top of this "Testing" section for
the `runSource` / `runTestSource` limitation.

### `ListHeader` / `IOListHeader` returned to Ry code must be `arc_alloc`'d, not `checked_malloc`'d

**Source**: #997 (2026-04-16, fix)
**Tags**: runtime, arc, list, memory-safety, allocation

**Rule**: Every list header (`ListHeader*` or `IOListHeader*`) returned from a
C++ runtime function to Ry code must be allocated with `arc_alloc(sizeof(…))`,
not `checked_malloc(sizeof(…))`. The Ry ARC machinery reads/writes
`header_ptr - 16` (the ARC prefix) when retaining or releasing the list.
If the header is plain `checked_malloc`, this access lands in malloc metadata
→ use-after-free / bad-free at scope exit.

Applies to:
- `ListHeader` (string/match lists): use `makeStringList` / `makeMatchList` in
  `include/ry/runtime_list.hpp` — these already use `arc_alloc`.
- `IOListHeader` (byte lists): use `makeByteList` in `include/ry/runtime_io.hpp`;
  also `makeEmptyIOList` in `runtime_net.cpp` and inline allocations in
  `runtime_net.cpp` / `runtime_tls.cpp` / `runtime_io.cpp` — all converted in #997.

**C++ tests that wrap these headers**: Use `arc_free(header)` (from
`include/ry/runtime_arc.hpp`) instead of `free(header)`. The `data` array
inside the header is separately `checked_malloc`'d and must be freed with plain
`free`. Individual string elements created by `dupString` are also plain
`checked_malloc` and freed with plain `free`.

### `for` loop over a collection: snapshot via `emitArcRetain` to prevent buffer-pointer UAF

**Source**: #1021 (2026-04-16), #1041 (2026-04-17), #1091 (2026-04-17)
**Tags**: codegen, for-loop, list, set, map, arc, cow, use-after-free, iteration

**Rule**: Never cache a collection's internal buffer pointer (`data`, `keys`,
`vals`) in an SSA value that is read inside the loop body **when the iterable
carries an external alias** (`VariableExpr`, `FieldAccessExpr`, or `IndexExpr`). `append!`/`add`/map-insert grow the
collection; when the buffer is full, they `arc_alloc` a new buffer, copy, and
**`free` the old buffer** — leaving any pre-loop SSA pointer dangling (UAF).

**External-alias gate**: Apply retain+snapshot when
`iterableHasExternalAlias(*s->iterable)` returns true — i.e., the iterable is
`VariableExpr`, `FieldAccessExpr` (e.g. `obj.items`, `self.xs`), **or** `IndexExpr`
(e.g. `xs[i]`, `m["key"]`). For true temporaries (`range(100)`, result of a call
expression, `emitStringToCharList` output, list/set/map literals, etc.) there is no
external alias that can mutate the collection through CoW — pre-loop SSA values are safe.

**FieldAccessExpr / IndexExpr semantic difference**: `tryGetReceiverAlloca` returns `nullptr`
for `FieldAccessExpr` (only handles `VariableExpr`), so no CoW fork occurs on
mutation — `append!` mutates in-place. The retain still protects the snapshot:
the header's `strong_count` is bumped to ≥ 2, so the buffer is not freed. The
loop iterates the original length read from the snapshot before mutation.


**Fix**:

1. Before emitting the loop header, call `emitArcGetHeaderFromData(iterable)` →
   `emitArcRetain(arcHdr)` to bump `strong_count`.
2. Store the collection data pointer in a new scope-owned alloca (e.g.,
   `__for_iter_snap_N`). Register it with `setTypeMeta` (correct
   `ListElem`/`SetElem`/`MapKey`) and `markArcManaged` so `popScope()` releases
   it on normal exit, `return`, and `break`.
3. Load `len` once from the snapshot alloca right before `emitIndexedForLoop`.
   Inside the body lambda, load `data`/`keys`/`vals` from the snapshot alloca per
   iteration (the retain freezes the header; CoW on the source alias forks a new
   header without touching ours).
4. **Do NOT emit an explicit release.** `popScope()` / `emitScopeCleanupToDepth`
   walk `arc_managed_vars_` and call `emitArcReleaseVar` on every exit path.

**For non-alias iterables**: load `data`/`keys`/`vals`/`len` once before
`emitIndexedForLoop` as SSA values, capture them in the body lambda.

**Why retain works**: any mutation through the source alias inside the loop body
triggers `emitCowCheckSlot`, which sees `strong_count ≥ 2` and forks a new
header into the source alloca. Our snapshot alloca still points to the original
frozen header; the loop iterates the original content.

**Sequential-loop safety**: use a per-ForStmt counter for unique snapshot names
(`__for_iter_snap_0`, `__for_iter_snap_1`, …). `getOrCreateVar` looks up only
the **current scope**, so sequential loops in the same function scope cannot
accidentally share a snapshot alloca if the names differ.

**How to verify**: in `codegen_stmt_loop.cpp`, per-iteration buffer loads for
`iterableHasExternalAlias` paths should read from a `for_snap` alloca; for
non-alias paths, the captured SSA `dataPtr`/`keysPtr`/`valsPtr` values are used
directly.

---

### Thread / parallel-for thunk epilogue: `popScope()` before `CreateRetVoid()`

**Source**: #1090 (2026-04-17)
**Tags**: codegen, thread, parallel-for, arc, thunk, ir-verification

**Rule**: In any internally-generated thunk function (`__ry_thread.N`,
`__ry_parallel_for.N`), call `popScope()` **before** `builder_.CreateRetVoid()`.
`popScope()` → `emitScopeCleanupToDepth()` → `emitArcReleaseVar()` emits
`CreateLoad` + `CreateCondBr` diamond blocks. If `CreateRetVoid()` has already
terminated the current BB, those instructions land after the terminator, producing
malformed IR (multiple terminators / post-terminator instructions). The JIT
optimizer (`LowerExpectIntrinsicPass` for O2) crashes on such IR.

**Correct epilogue pattern** (from `emitParallelForRange`, `codegen_stmt_loop.cpp:765-766`):

```cpp
// body terminated by explicit return?  ReturnStmt already drained
// arc_managed_vars_; iterator_malloc_stack_ items also emitted; skip.
if (!builder_.GetInsertBlock()->getTerminator()) {
    popScope();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateRetVoid();
}
```

Note: if `iterator_malloc_stack_` at the current scope has items,
`emitScopeCleanupToDepth` emits `free` calls but does **not** clear the vector
(`codegen.cpp:384-390`). If `ReturnStmt` already fired `emitScopeCleanupToDepth(0)`,
those free calls were already emitted; an unconditional subsequent `popScope()` would
re-emit them into the (terminated) block. The `if (!terminated)` outer guard prevents
this double-free / post-terminator insertion.

**IR verification gap**: `thread_spawn` thunks now have `llvm::verifyFunction`
(`codegen_call_thread.cpp:284-288`). `emitParallelForRange` does not yet have it
(follow-up opportunity). Root cause of #1090 was the missing verify — the malformed
IR survived codegen and crashed only during JIT optimization.

---

### `~CodeGen()` glibc heap-check crashes are not destructor bugs

**Source**: #1161 / #1167 (2026-04-18)
**Tags**: codegen, arc, glibc, debugging, heap-corruption, gotcha

**Scope**: This entry covers crashes that **persist under ASan** (i.e. ASan itself reports
a heap-buffer-overflow or use-after-free). For crashes where all test cases pass and only
the teardown crashes — and the crash disappears on re-run — see the ORC JIT flake entry
([`LLVM ORC JIT intermittent crash`](#llvm-orc-jit-intermittent-crash-in-lljit--removeresourcetracker--codegen-linux--macos))
instead; those are pre-existing LLVM ORC flakiness, not user-code bugs.

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

---

## Regex Engine

### Thompson NFA stores per-thread state on NFAState itself — extending to capture slots requires a separate approach

**Source**: #829 (2026-04-14, implementation)
**Tags**: regex, nfa, capture-groups, design-decision

**Rule**: The `NFASimulator` in `src/runtime_regex.cpp` stores per-thread tracking data
directly on `NFAState` (`matchStartPos`). This is safe because only one value is tracked.
Extending the Thompson NFA to carry per-thread capture slot arrays (RE2 style) would require
moving to a `Thread = {NFAState*, vector<pair<int,int>>}` approach — a significant rewrite.

Instead, capture group extraction for `replace` uses a separate backtracking pass over the
NFA graph, anchored to the match boundaries already found by `findAll`. This:
1. Keeps the Thompson path completely unchanged (no performance impact for non-backreference patterns)
2. Is safe against catastrophic backtracking because the span is bounded by the match

As of #830, `find_all` also uses `CaptureBacktracker` to expose capture groups in `Match.groups`.
The same two-phase approach (NFA finds boundaries, CaptureBacktracker extracts groups) works
well without modifying the NFA simulator. Migrating to per-thread capture slot arrays would
only be necessary if performance profiling shows the per-match backtracking is a bottleneck.

### GroupOpen / GroupClose epsilon states are transparent to the Thompson NFA simulator

**Source**: #829 (2026-04-14, implementation)
**Tags**: regex, nfa, capture-groups, epsilon-states

**Rule**: `GroupOpen` and `GroupClose` are added to `NFAState::Kind` as epsilon states.
`addState()` treats them like `Split` — follows `out1` without consuming input, incrementing
no generation. This means the existing Thompson simulator ignores them at zero cost.
The `CaptureBacktracker` (used only when `$N` appears in the replacement) uses the same NFA
graph and reads these states explicitly.

### UFCS regex functions must dispatch to a dedicated `__ry_regex_<verb>` runtime symbol — never alias to the legacy `regex_*` form

**Source**: #1197 (2026-04-19, implementation)
**Tags**: regex, codegen, api-naming, ufcs, dispatch

**Rule**: Each unprefixed UFCS regex function (`is_match`, `search`, `replace`, `split`, `find_all`) must dispatch to a dedicated `__ry_regex_<verb>` runtime symbol whose semantics literally match the function's name. Never alias a UFCS form to the `regex_*` legacy form's runtime symbol — the two can have different semantics (e.g. `is_match` is partial/unanchored search, but `regex_match` is full-string match). When adding a new UFCS regex function, add a matching `__ry_regex_<verb>` C entry point in `src/runtime_regex.cpp` + `include/ry/runtime_regex.hpp` and cover it directly in `tests/test_regex_runtime.cpp` — do not rely on a shared symbol and a LLVM `Trunc` to paper over a semantic mismatch.

---

## Build / CI

### UBSan must disable `vptr` and `function` checks on this project

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

### TSan job — C++ tests required, Ry self-tests warn-only

**Source**: #868 (warn-only landed), #874 (split policy)
**Tags**: tsan, sanitizer, ci, concurrency

**Rule**: The C++ TSan step (`./build-tsan/ry_tests`) is required
and gates #630's P0 race fixes via `ConcurrencySpecSuite` (which
runs `tests/spec/concurrency.test.ry`). The Ry self-test TSan step
(`./build-tsan/ry test -p`) runs as `continue-on-error: true`
until upstream https://github.com/google/sanitizers/issues/1716
is fixed — see the separate `LargeMmapAllocator` entry. A PR that
introduces a new race must fix it in the same PR; warn-only is a
workaround for the TSan runtime bug, not tolerance for real races.
`parallel_for_depth_` only propagates to ARC ops emitted **inside**
the thunk body, not through helper calls; if a helper-alias race
surfaces, widen via #872 rather than blindly expanding the flag.

### LLVM ORC JIT intermittent crash in `~LLJIT()` / `removeResourceTracker` / `~CodeGen()` (Linux + macOS)

**Source**: #1022 (2026-04-16) CI run 24507853564; #1088 (2026-04-17) macOS Darwin 25.3.0; #1043 (2026-04-17) CI runs 24547110395 + 24547575356; #1187 (2026-04-19) macOS Darwin 25.3.0 residual ~16 % rate
**Tags**: llvm, orc, jit, ci, flaky, linux, macos, cleanup, parallel-test

**Symptom**: The Ry self-test (`ry test -p`) completes all `it` blocks
successfully, then crashes during JIT teardown.

- **Linux CI**: glibc heap consolidation crash (`cfree`) during LLVM teardown. The crash frame
  varies — most commonly `removeResourceTracker`, but `CodeGen::~CodeGen()` has also been observed
  ("corrupted size vs. prev_size while consolidating"). Same root cause; different destruction order.
- **macOS (non-ASan)**: intermittent `~40%` failure rate in parallel mode — worker subprocess
  exits with signal (`128+N`) silently; the parent counts `+1 total failures` with no red line.

On Linux (`removeResourceTracker` variant):
```text
135 passed, 0 failed
PLEASE submit a bug report to https://github.com/llvm/llvm-project/issues/...
#4  cfree (/lib/x86_64-linux-gnu/libc.so.6)
#5  llvm::orc::ExecutionSession::removeResourceTracker(...)
#7  scope_exit destructor in jit_runner.cpp
#8  runRySource(...)
```

On Linux (`~CodeGen()` variant):
```text
135 passed, 0 failed
corrupted size vs. prev_size while consolidating
#4  cfree (/lib/x86_64-linux-gnu/libc.so.6)
#5  CodeGen::~CodeGen()
```

**Discriminating evidence for flake vs. regression**: If all test cases in the failing file report
success (N passed, 0 failed) and only the teardown crashes, and a re-run on the same commit passes,
classify as flake. A genuine heap corruption from user code would fail a specific test case or fail
deterministically. If the crash **persists under ASan** (ASan reports a heap error on its own, not
just glibc's `cfree` check), it is user-code OOB/UAF — not an ORC flake. In that case, see the
[`~CodeGen() glibc heap-check crashes`](#codegen-glibc-heap-check-crashes-are-not-destructor-bugs)
entry in the Runtime / Memory section for the diagnostic checklist.

**Fix applied** (two-step suppression — both steps are required):

1. `(void)jit.release()` — guarded by `#if defined(__linux__) || defined(__APPLE__)`. Leaks the
   LLJIT so `~LLJIT()` never runs. Extended from Linux-only to macOS in #1088 (suppressed the
   `~LLJIT()` crash frame).
2. `rtCleanup.release()` — added immediately before `jit.release()` in the same `#if` block (#1187).
   Cancels the `scope_exit` destructor so `RT->remove()` never fires during stack unwind. This is
   necessary because `jit.release()` leaks but does NOT destroy the LLJIT; the leaked
   `ExecutionSession` remains alive, so `RT->remove()` → `handleRemoveResources` →
   `InProcessMemoryManager::deallocate` → `WrapperFunctionCall::runWithSPSRet` hits the same
   JITLink deallocation crash. Suppressing only the `~LLJIT()` frame left the
   `removeResourceTracker` frame, causing a residual ~16 % failure rate in `ry test -p`.

**Rule**: On Linux CI, trigger a re-run if this crash appears — it is pre-existing LLVM ORC
flakiness, not a regression. The `~CodeGen()` frame variant is the same flake family as
`removeResourceTracker`. On macOS, both workarounds together suppress the crash; if any failure
rate reappears after the fix, the root cause is broader than these two frames and needs fresh
investigation. Do not suppress the LLVM crash reporter or add `|| true` — a genuine double-free in
user code would produce the same frame.

### `@parallel for` captures must be retained AND ARC-backed inside the thunk

**Source**: #630 / #874
**Tags**: parallel-for, arc, cow, codegen, fnscope, gotcha

**Rule**: For `@parallel for` / `thread_spawn` captures: before
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

### Ry runtime panics bypass C++ exceptions — they `fprintf + exit(1)`

**Source**: #828
**Tags**: panic, runtime, exception, thread, gotcha

**Rule**: Every `CodeGen::emitRuntimeError` call site emits IR that
runs `fprintf(stderr, ...)` followed by `exit(1)` +
`CreateUnreachable` (`src/codegen_call_user.cpp:600-618`). Ry's user-level
panics (divide-by-zero, array OOB, range/contract violations,
integer overflow, etc.) therefore **terminate the entire process
immediately** — they do not propagate as C++ exceptions. Any
feature that plans to "catch a worker panic and report it as a
value" (e.g. `thread_join(t) -> Err` for a panicking worker,
`block_on(task) -> Err` for a panicking async body) cannot rely on
existing defensive `try { ... } catch (std::exception &)` blocks
inside runtime functions — those only fire for C++ exceptions
that escape from LLVM-generated code, which never happens for
user panics. The existing catch in `__ry_thread_spawn`
(`src/runtime_thread.cpp`) is dormant defensive code, not an
active error-propagation path. Fixing this requires refactoring
`emitRuntimeError` to `throw std::runtime_error(...)` + installing
a top-level catch in `ry run` / `ry test` — tracked in #880.
Before writing tests that trigger a panic and expect
`Err(error_msg)`, verify that the panic *actually* throws a C++
exception; the only current throw path from runtime code is
`__ry_task_join` double-join (`src/runtime_parallel.cpp:292`) and a
handful of compile-time lexer/loader errors.

### Custom-emitter native functions bypass argument type-checking

**Source**: #828
**Tags**: codegen, native-dispatch, type-checking, gotcha

**Rule**: Native functions registered with a `customEmitter` in
their `NativeDispatchEntry` go through the early-return at
`src/codegen_call_native.cpp:135-150` and **skip** the regular
argument type validation path (L165-203). That means the Ry
declaration in `share/std/<pkg>/<pkg>.ry` (e.g.
`fn thread_spawn(body: fn() -> Unit) -> Thread`)
constrains only what IDE/docs consumers see, not what the custom
emitter actually accepts. When extending a custom-emitter native
fn (e.g. broadening `thread_spawn` to accept non-Unit
lambdas, or `assert` to accept new argument shapes), you can
relax the effective input without touching the Ry declaration.
Do update the docs and declaration for hygiene, but do not
assume the parser/type-checker is gating you — your codegen is.

### TSan `LargeMmapAllocator` CHECK on Linux self-test runs

**Source**: #868 / #874
**Tags**: tsan, sanitizer, ci, ubuntu, gotcha

**Rule**: Keep `./build-tsan/ry test -p` as `continue-on-error: true`
until upstream https://github.com/google/sanitizers/issues/1716
is fixed: the self-test driver aborts inside TSan's
`LargeMmapAllocator::Deallocate` — a runtime bug, not a race in
ry code. Pinning `ubuntu-22.04` and lowering `vm.mmap_rnd_bits=28`
were both tried. `./build-tsan/ry_tests` (which includes
`ConcurrencySpecSuite`) runs cleanly on the same binary, so that
is the required gate for #630's race fixes. macOS is unaffected.

### LLVM mirror workflow and version-bump checklist

**Source**: #892 / #919 / #934 / #1246
**Tags**: llvm, ci, cache, mirror, version-bump, race-condition

**Rule**: CI fetches LLVM from a GitHub Releases mirror via
`.github/actions/setup-llvm/`. The mirror tarball is built by
`.github/workflows/mirror-llvm-toolchain.yml` (manual `workflow_dispatch`).

The mirror tarball includes `clang-tidy` (added in #934) and `scan-build` / `analyze-build` via `clang-tools-{MAJOR}` (added in #898). **FileCheck is NOT bundled** in the mirror tarball (#897) — the `filecheck` CI job installs it separately via `apt-get install llvm-{MAJOR}-tools` from `apt.llvm.org`. The
`setup-llvm` action accepts an optional `extra-packages` input for
the apt fallback path; the mirror/cache path already contains all
tools. If new tools are needed, add them to
`mirror-llvm-toolchain.yml`'s apt-get line, bump the cache key
version suffix (e.g. `v2` → `v3`), and re-dispatch the mirror workflow.
A follow-up issue tracks adding `llvm-{MAJOR}-tools` to the mirror
tarball to eliminate the extra apt install step.

**Mirror flow is non-destructive (#1246)**: Do NOT reintroduce
`gh release delete --cleanup-tag` in the mirror workflow. The flow is:

1. Each dispatch computes `next_rev = max(existing rev<N>) + 1` via
   `gh release list | jq` with an anchored regex
   `^llvm-toolchain-<V>-rev[0-9]+$` so malformed tags are skipped.
2. Creates an immutable `llvm-toolchain-<V>-rev<N>` release that is
   never deleted (audit trail + rollback source).
3. Promotes the stable pointer `llvm-toolchain-<V>` in place via
   `gh release upload --clobber`. This is NOT atomic — GitHub's API
   has no atomic replace, so `--clobber` is implemented as a
   sequential DELETE-then-POST per file. The missing-asset window
   per file is the POST duration: sub-second for the `.sha256`, but
   seconds to a few minutes for the LLVM tarball (300–500 MB). That
   is still orders of magnitude better than the previous 3–5 minute
   full-release gap produced by `gh release delete --cleanup-tag`.
4. A workflow-level `concurrency: group: mirror-llvm-${{ inputs.llvm_version }}`
   guard serialises dispatches for the same version so two racing
   runs cannot both compute the same `next_rev`.

The `force` input was removed — idempotent re-dispatches simply append
a new rev. Rollback is a manual GitHub UI operation: edit the stable
release and replace its assets with those of a prior `rev<N>` release.

**Known follow-up (setup-llvm hardening)**: During the tarball upload
window the stable release itself still exists, so
`.github/actions/setup-llvm/action.yml`'s `gh release view` check
succeeds and does not trigger the `apt.llvm.org` fallback — but the
subsequent `gh release download` gets a 404 and the job hard-fails.
The previous `--cleanup-tag` flow was actually softer here because
the release was fully gone, so `gh release view` failed and the apt
fallback kicked in. Because mirror dispatches are rare and manual,
this was accepted in #1246, but a follow-up issue should harden
`setup-llvm` to fall back on download failure too (e.g. retry the
`gh release view`/`download` pair, or unconditionally try apt on any
download failure). Do NOT re-introduce `--cleanup-tag` as a fix for
this — that would regress the window from seconds to minutes back
to 3–5 minutes.

Version bump checklist — update `env.LLVM_VERSION` (and
`env.LLVM_SHA256_SHORT` when non-empty) in:
- `.github/workflows/ci.yml`
- `.github/workflows/ci-scheduled.yml`
- `.github/workflows/codeql.yml`

Cache key format: `llvm-${VERSION}-linux-x86_64-v3-${SHA256_SHORT}`.
`restore-keys` is intentionally omitted: a partial cache hit would
restore a mismatched LLVM version, causing build failures or silent ABI
mismatches. An exact-match-only policy guarantees the correct toolchain.

`release.yml` still uses `apt.llvm.org` directly and is not yet
migrated — tracked as a separate follow-up from #892.

### Compiler warning flags require SYSTEM includes for third-party headers

**Source**: #895 (implementation)
**Tags**: build, cmake, warnings, llvm, googletest, system-include

**Context**: When `-Wall -Wextra -Wpedantic -Wconversion -Wshadow` are
enabled on internal targets, LLVM and GoogleTest headers produce hundreds
of warnings (implicit conversions, unused parameters, non-standard
extensions, etc.) that are not actionable.

**Rule**: LLVM include directories must be added via
`target_include_directories(... SYSTEM PUBLIC ${LLVM_INCLUDE_DIRS})`,
not via the global `include_directories(${LLVM_INCLUDE_DIRS})`. The
global form does not support SYSTEM and causes third-party warnings to
leak into the build output. GoogleTest uses the `SYSTEM` keyword in
`FetchContent_Declare` (requires cmake 3.25+). The project's own
`include/` directory must NOT be marked SYSTEM — warnings on our own
headers are intentional.

Warning flags are stored in the `RY_WARNING_FLAGS` CMake variable and
applied per-target via `target_compile_options(... PRIVATE ...)` so they
do not leak into FetchContent targets. The `add_ry_native_lib()` helper
applies them automatically to all native shared libraries.

### Clang-Tidy check selection and __ry_ naming convention

**Source**: #893 (implementation)
**Tags**: build, ci, clang-tidy, static-analysis, naming

**Context**: The `.clang-tidy` config disables several checks that
produce false positives in this project. Key decisions:

- `bugprone-reserved-identifier` / `cert-dcl37-c` / `cert-dcl51-cpp`:
  All runtime functions use `__ry_*` prefix (double underscore = reserved
  in C++). This naming is intentional for ABI and FFI reasons — do not
  rename them, disable the check instead.
- `cert-err58-cpp`: `RY_REGISTER_STDLIB_PACKAGE` macro creates static
  initializers. This is the standard pattern for self-registering
  stdlib packages.
- `cert-dcl50-cpp`: C-style variadic functions are used intentionally
  in runtime error formatting helpers.
- `performance-enum-size`: Internal enum base types are often
  intentional (ABI stability, JIT layout constraints).
- `performance-no-int-to-ptr`: Incompatible with LLVM IR builder
  patterns.
- `bugprone-multi-level-implicit-pointer-conversion`: C-style
  `free(ptr)` where `ptr` is `T**` is idiomatic in the runtime's
  manual memory management. This check fires on clang-tidy 21+ only.
- `cert-err33-c`: `snprintf`/`vsnprintf` return values are not
  meaningful in formatting-only contexts (error message buffers,
  string formatting). `std::atexit` failure is also not actionable.

**Rule**: Do not re-enable these checks without understanding why they
were disabled. If adding a new disabled check, document the reason in
the `.clang-tidy` comment header.

### Clang-Tidy: NOLINT patterns for intentional code

**Source**: #935 (implementation)
**Tags**: build, ci, clang-tidy, static-analysis

Some clang-tidy warnings are intentional patterns that should be
suppressed with `// NOLINT(...)` rather than refactored:

- **`performance-unnecessary-value-param`** on sink parameters
  (`std::string name` → `std::move(name)`): This is correct C++ sink
  idiom. Changing to `const std::string &` would break move semantics.
  Affected: `TypeNode::make*` factory methods in `ast.hpp`,
  `SourceManager::addSource` in `source_manager.hpp`.
- **`bugprone-empty-catch`** on shutdown/cleanup paths:
  `worker.join()` in `runtime_parallel.cpp` and stdlib-load try/catch
  in `jit_runner.cpp` intentionally swallow exceptions because join
  failure on thread shutdown is non-recoverable, and stdlib absence is
  expected. Add a brief justification comment alongside NOLINT.

**Rule**: When using NOLINT, always include the specific check name and
a one-line comment explaining why the suppression is justified.

### Cppcheck: suppression strategy and known false positives

**Source**: #894 (implementation)
**Tags**: build, ci, cppcheck, static-analysis

Cppcheck is run in the `lint` CI job without `compile_commands.json` (build-free,
fast). This means project macros defined in headers are not visible to Cppcheck,
causing `unknownMacro` false positives.

Known suppressions in `.cppcheck-suppressions`:

- **`unknownMacro`** (global): `RY_REGISTER_STDLIB_PACKAGE` and `DEFINE_LAST_ERROR`
  are project macros not resolved without `compile_commands.json`. Suppressed globally
  because running without the compilation database is intentional (avoids requiring a
  full build in the `lint` job).
- **`syntaxError:src/test_runtime.cpp`**: `__has_feature(...)` is a Clang compiler
  builtin predicate, not a function-like macro. Cppcheck cannot evaluate the
  `__has_feature(address_sanitizer)` idiom and emits a spurious `syntaxError`.

**Rule**: When adding a new suppression to `.cppcheck-suppressions`, always include
a comment explaining whether it is a false positive (and why) or a known acceptable
deviation. Use per-file suppressions (`id:src/file.cpp`) rather than global ones
where possible.

**Gotcha**: Cppcheck 2.13 (Ubuntu 24.04 package) does NOT support `#` comment lines
in `--suppressions-list` files. Comment syntax was added in 2.14. Keep
`.cppcheck-suppressions` comment-free to remain compatible with 2.13.

### LLVM 21 via apt.llvm.org requires zlib1g-dev and libzstd-dev in addition to LLVM packages

**Source**: #1165 Docker dev environment setup (2026-04-18)
**Tags**: build, llvm, cmake, docker, dependencies

**Rule**: When using LLVM 21 packages from `apt.llvm.org` on Ubuntu, the CMake package
`find_package(LLVM REQUIRED ...)` will fail at configure time unless `zlib1g-dev` and
`libzstd-dev` are installed — even if you never explicitly use zlib or zstd yourself.

**Why**: `LLVMExports.cmake` (generated when LLVM was compiled) lists `ZLIB::ZLIB` and
`zstd::libzstd_shared` in the `INTERFACE_LINK_LIBRARIES` of the `LLVMSupport` target.
CMake validates all link-interface targets at import time. If either target is missing,
you get:

```text
CMake Error at .../LLVMExports.cmake:73 (set_target_properties):
  The link interface of target "LLVMSupport" contains: ZLIB::ZLIB
  but the target was not found.
```

**How to apply**: Any Dockerfile or CI step that installs LLVM 21 via `apt.llvm.org`
must also `apt-get install -y zlib1g-dev libzstd-dev` BEFORE the `find_package(LLVM)`
CMake configure step. Add them to the same `apt-get install` layer as `cmake`, not after.

### ubuntu:24.04 archive signing key expires — bypass for dev Dockerfiles

**Source**: #1165 Docker dev environment setup (2026-04-18)
**Tags**: docker, ubuntu, gpg, apt, dev-environment

**Rule**: On Apple Silicon (arm64) the ubuntu:24.04 Docker image uses `ports.ubuntu.com`
which is signed with a key that can expire on systems whose clock is ahead of the key's
validity period. When `apt-get update` fails with "At least one invalid signature was
encountered", installing `ubuntu-keyring` will not help (it is already the newest version
in-image). The correct fix for a **dev-only** Dockerfile is to replace `Signed-By:` with
`Trusted: yes` in `/etc/apt/sources.list.d/ubuntu.sources` before the first
`apt-get update`:

```dockerfile
RUN sed -i 's|^Signed-By:.*|Trusted: yes|' /etc/apt/sources.list.d/ubuntu.sources \
  && apt-get update \
  && apt-get install -y ...
```

**Why**: ubuntu 24.04 uses the deb822 sources format at
`/etc/apt/sources.list.d/ubuntu.sources`. The `Signed-By:` directive points to the GPG
keyring. Replacing it with `Trusted: yes` bypasses signature verification entirely. Do
NOT use this workaround for production images.

**How to apply**: Add the `sed` step as the first line of the `RUN` block that installs
build dependencies. Keep it in the same `RUN` as the `apt-get install` so that the
`Trusted: yes` directive is baked in and future `apt-get update` calls within the build
also work.

---

### libFuzzer requires Clang, whole-archive ry_lib, and split -fsanitize=fuzzer flags

**Source**: #896 (2026-04-18)
**Tags**: libfuzzer, fuzzer, sanitizer, cmake, clang, llvm

**Rule**: libFuzzer has three non-obvious toolchain requirements that must all be satisfied:

1. **Clang-only**: `-fsanitize=fuzzer` is not supported by GCC or Apple Clang (Apple's fuzzer runtime is a stub). The `fuzz` CMake preset enforces this with a `FATAL_ERROR` check on `CMAKE_CXX_COMPILER_ID`. On macOS use `/opt/homebrew/opt/llvm@21/bin/clang++`; on Linux CI `/usr/local/llvm/bin/clang++`.

2. **Whole-archive link of ry_lib**: Fuzz harnesses link `ry_lib` with `-Wl,-force_load` (macOS) / `-Wl,--whole-archive` (Linux) — the same pattern as `ry` and `ry_tests`. Without this, JIT and runtime symbols (`__ry_set_last_error`, `checked_malloc`, etc.) needed by directly-compiled runtime sources (e.g. `runtime_json.cpp`) will be undefined.

3. **Split `-fsanitize=fuzzer-no-link` vs `-fsanitize=fuzzer`**: `ENABLE_FUZZER` adds `-fsanitize=fuzzer-no-link` globally (coverage-only; no `main` injection) and the `add_ry_fuzz_target` CMake helper adds `-fsanitize=fuzzer` at link time **only on fuzz executables**. If `-fsanitize=fuzzer` were applied globally it would inject a competing `main` into `ry` and `ry_tests`, causing link errors.

**macOS extra**: Homebrew LLVM Clang needs `SDKROOT=$(xcrun --show-sdk-path)` to find system C headers; without it the PCH compilation for `ry_lib` fails with libc++ `<cstdio>` not found. The `fuzz` preset does **not** hardcode this path (not portable); callers must set it as an env var.

**How to apply**: When adding a new fuzz harness target, use `add_ry_fuzz_target(name sources...)` in CMakeLists.txt (inside `if(ENABLE_FUZZER)`). Never apply `-fsanitize=fuzzer` globally. When building locally on macOS, always prepend `SDKROOT=$(xcrun --show-sdk-path) CC=<llvm-clang> CXX=<llvm-clang++>`.

---

### Regex parser calls exit(1) on malformed patterns — not fuzzable until refactored

**Source**: #896 (2026-04-18)
**Tags**: libfuzzer, regex, exit, gotcha

**Rule**: `RegexParser::parse()` in `src/runtime_regex_parser.cpp:13-21` calls `exit(1)` on unrecognised patterns. This terminates the libFuzzer process immediately, causing the fuzzer to report a crash and stop. The fuzz harness for the regex engine was therefore **excluded from #896** and tracked as follow-up issue #1176.

**How to apply**: Before adding a `fuzz_regex` harness, refactor `RegexParser::parse()` to throw `std::runtime_error` (or return `std::optional<CompiledRegex>`) instead of `exit(1)`. Also check `src/runtime_utf8.cpp`'s NUL-terminated variants (`__ry_utf8_char_at`, `__ry_utf8_char_index`) which also `exit(1)` on OOB — do not call these from fuzz harnesses; use the `_checked`/`_n` bounded variants instead.

---

### Fuzz harnesses must catch `std::exception`, not subtype specifics

**Source**: #1275 (2026-04-21)
**Tags**: libfuzzer, fuzzer, harness, exceptions, catch, parser

**Rule**: In `LLVMFuzzerTestOneInput`, wrap the target call in
`catch (const std::exception &)` — never catch only `std::runtime_error` or
a named derived type. `std::logic_error` (including `std::out_of_range`
and `std::invalid_argument`) is a **disjoint** subtree from
`std::runtime_error`, so a `runtime_error`-only catch silently lets future
parser/lexer throws escape to `libc++abi`, which the fuzzer reports as a
deadly signal and terminates the run.

**Context**: `tests/fuzz/fuzz_parser.cpp` originally caught
`ry::DiagnosticError` and `std::runtime_error` (the first is redundant —
`DiagnosticError` derives from `std::runtime_error` per
`include/ry/diagnostic.hpp:22`). This missed `std::out_of_range` thrown by
`parseFloatLiteral` / `parseIntLiteral` in `include/ry/parser.hpp:196,210`
for malformed literals like `0B1f32`, causing a libFuzzer crash despite
the CLI (`src/main.cpp:297-310`) handling the same input cleanly via its
top-level `catch (const std::exception &)`.

**How to apply**: When writing or reviewing a fuzz harness, match the
established top-level pattern used across `src/main.cpp:308`,
`src/cli.cpp:51,96`, `src/runtime_json.cpp:766`, `src/formatter.cpp:702,798`
— a single `catch (const std::exception &)` backstop. Preserve the
`// NOLINT(bugprone-empty-catch)` suppression so clang-tidy stays clean
under AGENTS.md §Clang-Tidy. Document the expected exception types in the
catch-block comment instead of splitting into multiple specific catches.

---

## Documentation

### Example code in docs must match the current Ry syntax

**Source**: #825 PR review (CodeRabbit)
**Tags**: documentation, syntax

**Context**: #825 docs used `val x: i32 = 1` — the `val` keyword does
not exist in Ry; the actual syntax is `x: i32 = 1`. The snippet was
written from memory (probably by analogy to Scala/Kotlin) and never
verified.

**Rule**: When writing doc examples, don't rely on memory. Either
(a) copy a working example from an existing `tests/spec/*.test.ry`, or
(b) verify the snippet with `printf '...\n' | ./build/ry -c` before committing.

**How to verify**: in `docs/reference/` PRs, grep for keywords that
don't exist in Ry's lexer:

```bash
grep -nE '\bval\b|\bvar\b|\bpublic\b|\bprivate\b' docs/reference/*.md
```

---

### `@native` declarations can silently drift from their dispatcher implementation

**Source**: #889 docs audit
**Tags**: stdlib, native, codegen, drift

**Context**: During a docs audit, `docs/reference/collections.md` was
suspected of being wrong because `share/std/list.ry` declared
`remove_at(...) -> Unit` while the docs described a return value.
Investigation showed the opposite was true: `CodeGen::emitCollOp_remove_at`
(`src/codegen_call_collection.cpp`) actually returns the removed element,
and `tests/spec/collections.test.ry` has long asserted on it
(`v = remove_at(xs, 1); expect(v).to_eq(2)`). The `-> Unit` declaration
in `list.ry` was the bug — it had been ignored by the custom dispatcher
for so long that nobody noticed.

**Rule**: The declared signature in `share/std/*.ry` is **not** the
source of truth for `@native` functions that go through a hand-written
codegen dispatcher (`src/codegen_call_*.cpp`). The dispatcher can —
and for historical reasons does — produce a different shape than the
declaration. Before treating a declaration as spec, cross-check the
dispatcher and the tests.

**How to apply**:
- When docs disagree with a `@native` signature, diff all three: the
  declaration file, the dispatcher in `src/codegen_call_*.cpp`, and an
  existing test in `tests/spec/`. The test's assertions reveal the
  actually observable return shape.
- If the declaration is wrong, fix the declaration rather than the
  docs or the tests; the dispatcher/test pair is the de-facto contract.
- Consider this whenever you audit or regenerate stdlib signatures.

**Follow-up audit (#890)**: A systematic sweep of every `@native`
declaration in `share/std/**/*.ry` against its dispatcher uncovered two
more drifts of the same shape and fixed all three:

| Declaration | Was | Correct |
|---|---|---|
| `share/std/map.ry` `items(Map<str, int>)` | `List<int>` | `List<(str, int)>` |
| `share/std/builtins.ry` `enumerate(List<int>)` | `List<int>` | `List<(int, int)>` |
| `share/std/builtins.ry` `zip(List<int>, List<int>)` | `List<int>` | `List<(int, int)>` |

In all three the dispatcher (`src/codegen_call.cpp` `emitBuiltinQuery`
for `enumerate`/`zip`, `src/codegen_call_collection.cpp:925`
`emitCollOp_items` for `items`) builds an `llvm::StructType::get(ctx,
{T1, T2})` tuple and calls
`setTypeMeta(TypeMeta::ListElem, newHeader, tupleTy)`. Type inference at
the call site consults the dispatcher's `ListElem` metadata, not the
declaration's return type — which is exactly why these drifts survived
so long: the compiler silently ignores `pairs: List<int> = enumerate(xs)`
and lets you call `pairs[0].0` on it without complaint. The declaration
is dormant documentation, not a gate. **Watch for this drift class in
any `emitBuiltin*` helper that calls `setTypeMeta(TypeMeta::ListElem,
..., tupleTy)` where `tupleTy` is a `StructType`.**

**Audit scope limits**: Pattern C (generic fallback, e.g. `base64`,
`filesystem`, `gc` — `emitGenericNativeCall` infers wrapping from the
declaration's return type) cannot drift by construction and was skipped.
Pattern A1 entries (table-driven with `ReturnWrapping` enum but no
`customEmitter`, e.g. `io`) are validated against the sig's return type
and got only a spot check. The audit focused on Pattern A2 (table
entries with `customEmitter`, which bypass type checking per
`codegen_call_native.cpp:135-149`) and Pattern B (hand-written
`emitBuiltin*` helpers that never consult the sig). Those are the two
places where drift is actually possible.

**Polymorphism constraint**: Ry declaration syntax has no generics /
`List<T>` / `Any`, so polymorphic collection ops (`list::filter`,
`list::map`, `map::keys`, etc.) will keep using monomorphic placeholder
element types like `List<int>` even when the dispatcher is
element-type-agnostic. Only drifts where the **structural shape** is
wrong (`List<int>` vs `List<tuple>`, `Unit` vs `int`) are actionable
until generics land. Don't re-chase placeholder-only inaccuracies as
drift.

---

### For heterogeneous return types on custom-emitter natives, use `any` as the declaration placeholder

**Source**: #1135
**Tags**: `stdlib, @native, codegen, type-declaration, hygiene`

When a custom-emitter native can return multiple concrete types at runtime
(e.g. `thread_spawn` / `thread_join` which return `Unit`, `int`, `float`,
or `bool` depending on the worker body), the declaration in
`share/std/**/*.ry` cannot use a true generic — several options fail:

| Option | Why it fails |
|---|---|
| `<T>` generic | `src/codegen_fn.cpp:471-481` diverts any function with `type_params` into `generic_fn_templates_` and returns early — no `NativeFnSignature` is registered, breaking dispatch |
| Bare `T` at top-level | `src/codegen_type.cpp:160-162`: `resolveType` fails for unbound identifier |
| `fn() -> T` nested | Parser free-pass (interior not validated), but the identifier is semantically meaningless — fragile |
| Overloads differing only in return type | `src/codegen_fn.cpp:531-545` dedupes on param types only; the second declaration is silently dropped |

**Rule**: Use `any` as the hygienic placeholder. `any` resolves cleanly via
`src/codegen_type.cpp:23` at both top-level and generic argument positions
(e.g. `Result<any, Error>`, `fn() -> any`). The custom emitter still
drives actual runtime type dispatch through `TypeMeta` metadata — `any` in
the declaration is never consulted at codegen time.

**Precedent for `any` at generic argument position**: `tests/spec/any.test.ry`
uses `Map<str, any>` and `List<any>` in 30+ places; `src/parser_decl.cpp:159`
uses `any` as the default element type for inferred collections. `Result<any, Error>`
was introduced by #1135 as a new pattern, but the surrounding type machinery
already handled `any` as a generic argument.

**Annotation in the `.ry` file**: Add a top-of-file comment explaining the
`any` ↔ `T` spelling gap, e.g.:
```ry
# `any` in thread_spawn / thread_join declarations below stands in for `T`
# in docs/reference/thread.md. The runtime narrows the actual type to
# Unit / int / float / bool via TypeMeta::ThreadResult metadata.
```
This prevents future readers from "fixing" `any` back to `Unit`.

---

### Ry has soft keywords that are NOT in the lexer `keyword_map`

**Source**: #1118 PR 1 docs audit
**Tags**: documentation, syntax, lexer

**Context**: Auditing `docs/reference/types.md` found `weak` described as "a keyword".
`src/lexer.cpp` keyword_map contains only hard keywords. `weak` is handled in
`parser_expr.cpp:463` and `parser_decl.cpp:726` as a contextual identifier
(`t.kind == TokenKind::Ident && t.value == "weak"`). Other examples:
`None`, `Some`, `Ok`, `Err` (variant constructors), and `_` (wildcard) are
similarly soft — they are legal variable names and only gain special meaning
in specific syntactic positions.

**Rule**: When writing or auditing docs, do **not** call soft keywords "keywords".
Use "contextual identifier" instead, or describe the syntax without labeling the
token class.

**How to verify**: Check `src/lexer.cpp`'s `keyword_map` (lines 35–66). If the
token is not there, it is not a reserved keyword — the parser must be checking
`t.value == "..."` explicitly to give it special meaning.

---

### Low-level integer types have different arithmetic semantics from `int`

**Source**: #1118 PR 1 docs audit
**Tags**: documentation, types, operators

**Context**: `operators.md` described `/` as "always float, IEEE 754". This is
correct for high-level `int`/`float` operands, but for low-level integer types
(`i8`..`i64`, `u8`..`u64`) `/` performs **integer division** and returns the
same type (`7i32 / 2i32` → `3i32`). Mixing a low-level type with `int` in the
same expression is a compile-time type error.

**Rule**: When auditing or writing docs for arithmetic operators, always
spot-check behavior with low-level integer types using `./build/ry -c`:

```bash
printf 'print(7i32 / 2i32)\nprint(type_of(7i32 / 2i32))' | ./build/ry -c -
```

The pattern extends to `+`, `-`, `*`, `%`, `//` — all produce the same type
without promotion when both operands are low-level integers.

---

### `docs/reference/` directory paths in prose can silently drift when source layout changes

**Source**: #1118 PR 2 docs audit
**Tags**: documentation, drift, stdlib

**Context**: `docs/reference/directives.md` referred to `core/*.ry` as the location of
`@native` prelude declarations (e.g. `core/builtins.ry`, `core/str.ry`). The actual
layout has been `share/std/` for a long time (e.g. `share/std/builtins.ry`). No `core/`
directory exists. The prose survived because it was never executed — only read.

**Rule**: When auditing stdlib-adjacent docs (PR 3–5 of #1118), grep for hardcoded
directory names in prose and verify them against the actual filesystem:

```bash
grep -rn 'core/' docs/reference/
ls share/std/           # verify actual layout
```

If a table lists filenames or paths, run `ls` on the claimed directory before trusting it.

---

### CLI flag value sets can drift between doc pages independently

**Source**: #1118 PR 2 docs audit
**Tags**: documentation, drift, CLI

**Context**: `project.md` global options table listed `--env=<env>` as accepting
`production|development|internal` (3 values). `packages.md` RY_ENV table had
5 values (`prod`, `dev`, `test`, `staging`, `internal`) plus their aliases. The
authoritative source is `src/dotenv.cpp:resolveRyEnv` + `src/cli.cpp` help text.

**Rule**: When two doc pages both describe the same CLI flag or env var value set,
verify they are consistent with each other AND with the implementation. Quick check:

```bash
# Find all pages that mention --env or RY_ENV
grep -rn 'env\|RY_ENV' docs/reference/ | grep -v '\.env\.'
# Check implementation's documented values
grep -n 'env.*=.*\|' src/cli.cpp | head -10
```

---

### Runtime errors in stdlib dispatchers must be mirrored in docs/reference/

**Source**: #1118 PR 3 docs audit
**Tags**: documentation, drift, stdlib

**Context**: During the PR 3 audit of `docs/reference/{builtins,collections,math}.md`, several
runtime (and compile-time) errors emitted inside `src/codegen_call_*.cpp` were found to be
completely absent from the corresponding reference pages. Examples found:

| Error message | Emitter location |
|---|---|
| `runtime error: reduce() on empty list` | `codegen_call_higher_order.cpp:237` |
| `runtime error: min() on empty list` | `codegen_call_higher_order.cpp:480` |
| `runtime error: max() on empty list` | `codegen_call_higher_order.cpp:480` |
| `runtime error: floor(): cannot convert %g to int` (also `ceil()`, `round()`, `trunc()`) | `codegen_call.cpp` math path via `emitCheckedFPToInt` |
| `flatten() requires a list of lists` (compile) | `codegen_call_collection.cpp` |
| `fold() initial value type must match function return type` (compile) | `codegen_call_higher_order.cpp:291-294` |

**Rule**: When auditing stdlib docs (PR 3–5 of #1118), grep all `codegenError()` and
`runtime error:` strings in `src/codegen_call_*.cpp` and `src/runtime_*.cpp`, then verify
each is mentioned (at least as a brief note) in the matching docs page:

```bash
grep -rn '"runtime error:' src/codegen_call_*.cpp src/runtime_*.cpp
grep -rn 'codegenError' src/codegen_call_*.cpp | grep '"' | grep -v '//'
```

Cross-check each hit against the corresponding `docs/reference/*.md` page. Missing ones are α drift.

---

### Cross-check operator-level specs before flagging per-file code examples as drift

**Source**: #1118 PR 4 docs audit
**Tags**: documentation, audit, drift, operators

**Context**: During PR 4 audit of `docs/reference/base64.md`, a code example using
`?` at the top level was initially flagged as drift ("top-level `?` is a compile error").
However, `docs/reference/operators.md:165-177` explicitly documents that `?` and `!!` work
at the top level (Err maps to exit 1 with error message). The implementation in
`src/codegen_expr.cpp:1861-1895` confirms this with a dedicated top-level `?` desugar path.
The false alarm arose because only per-package docs were consulted, not the canonical
operator spec.

**Rule**: When a per-file doc example uses a language operator (`?`, `!!`, `case`, `@`
directives, etc.), do NOT judge it solely by analogy with how that operator appears in
fn bodies. Always:
1. Check `docs/reference/operators.md` for the full spec (including top-level usage rules).
2. Cross-check `src/codegen_expr.cpp` or the relevant codegen for the operator's desugar
   behavior in different contexts (function body vs. top level vs. lambda).

**How to verify**: For `?` operator specifically, the top-level desugar constraint is
`src/codegen_expr.cpp:1873-1874`: the `Err` type must be `Error` exactly.

---

### Regenerate enumerated doc tables from implementation, not by hand

**Source**: #1118 PR 5 docs audit
**Tags**: documentation, audit, drift, http, enumerated-tables

**Context**: `__ry_http_reason_phrase` in `src/runtime_http.cpp:639-696` had 50 `case` statements, but `docs/reference/http.md` listed only 38 status codes — 12 were silently missing (402, 407, 412, 418, 421, 425, 428, 431, 451, 507, 508, 511).

**Rule**: When the implementation contains an enumerated list (status code switch, kind string registry, format specifier table) that is mirrored in a documentation table, **regenerate the doc table from the implementation** rather than hand-editing. For HTTP status codes specifically:

```bash
grep -nE '^\s*case [0-9]+:' src/runtime_http.cpp
```

gives the authoritative list; compare it against the `docs/reference/http.md` table before declaring clean. Apply the same pattern to any other enumerated registry (error kind strings, format specifiers, etc.).

**How to verify**: Count `case` statements in the switch vs. rows in the doc table. A mismatch always signals drift.

---

## Commands / Environment gotchas

This section records command/environment mistakes that were
non-obvious to diagnose. Add an entry whenever you iterate on a
command and the second invocation finally works — if the fix was not
trivial, record it so the same mistake isn't repeated.

### Record corrected command invocations (meta-rule)

**Source**: implementation experience (ongoing)
**Tags**: commands, environment, tooling, meta

**Rule**: When a command turns out to be wrong (bad flag, wrong path,
missing env var, outdated syntax) and you find the correct form, add
an entry below with a `Wrong → Correct → Why` triple. Examples of
what qualifies:

- forgetting `ASAN_OPTIONS=detect_container_overflow=0` on
  `build-asan/ry_tests`
- forgetting `RY_ENV=internal` when running a globally-installed `ry`
- calling `gh pr view` without `--repo` in a fork context
- using a wrong `cmake --preset` name
- heredoc / quoting / escaping mistakes in shell snippets

Skip: plain typos and mistakes that anyone would catch immediately.

**How to add a new entry**: every time you iterate on a command and
the second invocation works, ask "was the fix non-obvious?". If yes,
write a 3-line entry under this section with a descriptive subheading.

### Testing stdlib changes: run from the project root, not from /tmp/

**Source**: #1130 implementation (base64 `List<u8>` overloads)
**Tags**: commands, environment, stdlib, dev-stdlib, module-loader

**Wrong**: `printf '...\n' > /tmp/b64_smoke.ry && ./build/ry /tmp/b64_smoke.ry`
→ Error: `'encode_bytes' not found in package 'base64'`

**Correct**: `./build/ry test tests/spec/base64.test.ry` (or any `.ry` inside the repo)

**Why**: `./build/ry` resolves the stdlib path via `package.toml`'s hidden `[paths]._dev_stdlib` key, which requires a `package.toml` somewhere in the ancestor directory chain. Files under `/tmp/` have no such ancestor, so the module loader falls back to `~/.ry/share/std` (the globally-installed stdlib), which does not contain the newly added declarations. The trace event to look for: `"resolved_path":"/Users/.../.ry/share/std"` instead of `"resolved_path":"/Users/.../Workspace/ry-2/share/std"`.

### `gh issue edit --label` replaces all labels; use `--add-label` / `--remove-label`

**Source**: #1144 (2026-04-18)
**Tags**: gh, issue, label, wip, workflow

**Wrong**: `gh issue edit <n> --label wip`
→ **Replaces** the entire label set with `["wip"]`. All other labels (`bug`, `enhancement`, milestone-shadow labels, etc.) are silently deleted.

**Correct**:
- Add a label: `gh issue edit <n> --add-label wip`
- Remove a label: `gh issue edit <n> --remove-label wip`
- Create with labels (safe — no pre-existing labels): `gh issue create --label enhancement --label documentation`

**Why**: `--label` in `gh issue edit` is a *set* operation, not an *append*. The flag name is misleading because `gh issue create --label` is safe (empty initial state). The asymmetry bites every time you remember the create syntax and apply it to edit.

**How to apply**: Use `git-claim-issue` skill for `wip` attachment (enforces `--add-label` internally). Use `git-merge-pr` Step 5 for `wip` removal (enforces `--remove-label` internally). Never call `gh issue edit --label` directly for additive changes.

### bash `set -u` with empty array: use `"${arr[@]+"${arr[@]}"}"` not `"${arr[@]}"`

**Source**: #1165 Docker run.sh (2026-04-18)
**Tags**: commands, bash, shell, docker

**Wrong**: `docker run ... "${ENV_ARGS[@]}" ...` with `set -euo pipefail` and `ENV_ARGS=()`
→ Error: `ENV_ARGS[@]: unbound variable` when the array is empty

**Correct**: `docker run ... "${ENV_ARGS[@]+"${ENV_ARGS[@]}"}" ...`

**Why**: bash's `set -u` (nounset) treats an empty array expansion `"${arr[@]}"` as an
unbound variable. The idiom `"${arr[@]+"${arr[@]}"}"` uses parameter expansion with a
default — it expands to nothing when the array is empty, and to the full array contents
when non-empty. This is the standard POSIX-compatible workaround for `set -u` + optional
arrays in shell scripts.

### `ry -c` reads from stdin, not argv

**Source**: #1269 manual repro (2026-04-21)
**Tags**: commands, cli, ry, stdin

**Wrong**: `./build/ry -c 'print(1)'`
→ Silently prints nothing and exits 0. The positional argument after `-c` is ignored — the compiler reads an empty stdin, parses zero statements, and succeeds.

**Correct**: `printf 'print(1)\n' | ./build/ry -c` (or `echo 'print(1)' | ./build/ry -c`)

**Why**: `ry -c` follows a different convention from `python -c` / `sh -c`. It takes the source code on **stdin**, not as the next argv element. The `--help` output shows `echo '<code>' | ry -c` but this is easy to miss if you habitually reach for `-c 'snippet'` from shell/Python muscle memory. Particularly dangerous because the wrong form exits 0 with no output instead of erroring, so a failed manual repro looks like "compiler accepted the invalid program" when in fact no program was fed in at all.

**How to apply**: For one-off Ry snippets use a heredoc-to-pipe or write a scratch file under the project root (not `/tmp/` — see the `_dev_stdlib` gotcha above).

---

## Review feedback patterns

Meta-entries linking recurring reviewer comments to the specific
rules above. Useful for `grep` when triaging a new review comment —
if a reviewer's comment matches one of these patterns, jump to the
linked rule.

> **Format exception**: entries in this section are pointers into
> other sections, not standalone lessons, so they intentionally use a
> lighter structure (`Tags` + `Seen in` + `Points to`) instead of the
> full `Source` / `Tags` / `Context` / `Rule` layout. Each entry still
> has a `**Tags**:` line so the tag-search convention works uniformly.

### "Add a regression test for the new rejection path" (recurring)

**Tags**: meta-index, testing, rejection-path
**Seen in**: #841 (CodeRabbit)
**Points to**: [Testing → Every new rejection branch needs a test](#testing)

### "Handle the collapsed case from the new canonicalizer" (recurring)

**Tags**: meta-index, codegen, canonicalization
**Seen in**: #844 (CodeRabbit, critical)
**Points to**: [Codegen → Canonicalization that may collapse shape must be handled at all call sites](#codegen)

### "New primitive not wired into type inference / generics" (recurring)

**Tags**: meta-index, codegen, primitive-type, type-reflection
**Seen in**: #825 (CodeRabbit, 4 comments)
**Points to**: [Codegen → New primitive types must be wired into every type-reflection site](#codegen)

---

## Stdlib

### Bare `@native` vs `@native("pkg")` — NOT cosmetically equivalent

**Source**: #907 / PR #901 review (+ self-test regression during fix)
**Tags**: stdlib, @native, codegen, native-library, review-feedback-patterns

The two forms differ in a critical way beyond codegen dispatch:

- `@native("pkg")` sets `sig.library = "pkg"`, which populates
  `native_lib_index_`. At runtime the JIT attempts to **dlopen
  `libry_<pkg>.*`** based on this index.
- Bare `@native` leaves `sig.library` empty, so no library loading is
  triggered.

For dispatch key calculation (`effectivePackage`), both forms produce the
same key because `deriveNativePackage()` extracts the package name from
the file path regardless of the directive argument. So codegen dispatch
is identical.

**But**: if the package is **entirely** handled by a custom codegen
emitter (like `math`, whose functions are emitted as LLVM IR inline) and
has **no separate `libry_<pkg>.dylib`**, using `@native("pkg")` will
cause a runtime "native library not found" error.

**Rule**: Use bare `@native` (no argument) for stdlib packages whose
functions are all handled by custom codegen emitters and have no
separate shared library. Use `@native("pkg")` only when the package
has a corresponding `libry_<pkg>.*` shared library built via
`add_ry_native_lib()`.

### Bare builtins that call native-lib runtime symbols must register the library via `used_native_libraries_`

**Source**: #1261 (implementation of `input()` builtin)
**Tags**: stdlib, codegen, native-library, builtin, library-loading, @native

**Rule**: When adding a new bare builtin (dispatched via the `builtins_` map or
an inline branch in `emitBuiltinCore`) whose runtime implementation lives in a
native shared library (`libry_<pkg>.dylib`), the codegen emitter MUST call
`used_native_libraries_.insert("<pkg>")` before emitting the `CreateCall`.
Otherwise the JIT fails at runtime with
`JIT session error: Symbols not found: [ ___ry_<name> ]`, because no
`@native("pkg")` directive populated `native_lib_index_` automatically.

**Why**: Bare `@native` declarations in `share/std/builtins.ry` leave
`sig.library` empty (see "Bare `@native` vs `@native("pkg")` — NOT
cosmetically equivalent"), so library-load registration does NOT happen
through the declaration path. The burden shifts entirely to the codegen
dispatcher.

Most bare builtins (`print`, `length`, `range`, `arguments`, etc.) happen to
call symbols in `ry_lib` — the static library linked directly into the main
binary — so they resolve through `DynamicLibrarySearchGenerator::GetForCurrentProcess`
without any explicit registration. The trap surfaces only when the runtime
symbol lives in a **separate** `.dylib`. For `input()`, `__ry_read_line` and
`__ry_input_prompt` live in `libry_io.dylib`; without
`used_native_libraries_.insert("io")`, a user who writes `print(input())`
(with no `import` from `io`) hits the JIT error.

**How to apply**: When adding a bare builtin branch in `emitBuiltinCore`,
check which source file defines its runtime symbols:

- If in `src/runtime_<pkg>.cpp` (linked into `libry_<pkg>.dylib` via
  `add_ry_native_lib(<pkg> ...)` in `CMakeLists.txt`), add
  `used_native_libraries_.insert("<pkg>")`.
- If in `src/runtime_*.cpp` linked directly into `ry_lib` (e.g. `runtime_print.cpp`,
  `runtime_arc.cpp`), no registration is needed.

Regression test: exercise the builtin from a program that does NOT
`import` anything from the target package — this is exactly the usage
pattern users expect for a bare builtin, and it's the shape that
surfaces missing registration. Compile-only tests do not catch this
because the dispatcher succeeds at IR emission; the failure is deferred
to JIT symbol lookup.

### Builtin native-underscore wrappers must update the builtin dispatcher too

**Source**: #1277 (2026-04-23, implementation)
**Tags**: stdlib, builtin, wrapper, default-args, ufcs, codegen

**Rule**: When converting a bare builtin in `share/std/*.ry` from a direct
`@native fn name(...)` declaration to the native-underscore wrapper
pattern (`@native fn _name(...)` + `fn name(... = default)`),
update the C++ builtin dispatcher as well.

**Why**: Bare builtins like `split` are intercepted before ordinary Ry
fn resolution. Two separate changes may be required:

- Register the underscored alias (for example `"_split"`) in the relevant
  dispatch table (`emitBuiltinString`, `emitBuiltinCore`, etc.), otherwise the
  wrapper body itself fails with `undefined function: _name`.
- Mirror any new optional arity/default-argument behavior in the builtin
  emitter, because UFCS/direct builtin calls may still route through the C++
  handler instead of the Ry wrapper body.

**How to apply**: After introducing a builtin wrapper, test both forms:
ordinary call syntax (`name(...)`) and UFCS (`value.name(...)` when
supported). A passing spec for only one path is not enough.

### `Match` type is registered programmatically in codegen, not via a `record` declaration in regex.ry

**Source**: #830 (2026-04-14, implementation)
**Tags**: regex, record-type, codegen, stdlib-design

**Rule**: The `Match` type (`{full: str, groups: List<str>}`) used as the element type of
`find_all`'s return value is registered in `record_types_` inside the `CodeGen` constructor
(same pattern as `Error`), not declared as a `record` in `share/std/regex.ry`.

**Why**: If `Match` were declared in `regex.ry`, it would only be registered when a user
explicitly imports it (e.g., `from regex import Match, find_all`). Omitting `Match` from the
import would cause codegen to silently fail to find the type in `record_types_` when it tries
to tag the `find_all` result. By registering it unconditionally at construction time (same as
`Error`), users can write `from regex import find_all` without importing `Match` and still get
the fully-typed `List<Match>` result, including field access via `.full` and `.groups`.

**How to apply**: When a stdlib function returns a record type and that type must be available
regardless of explicit import, register it programmatically in the `CodeGen` constructor
alongside `Error`. If instead the type should only be available on explicit import (e.g., a
user-facing constructor), define it in the `.ry` file as a normal `record`.

### Rebuild metadata on values loaded from collection slots before recursive codegen

**Source**: #736 (2026-04-14, implementation)
**Tags**: codegen, metadata, equality, collection, emitComparisonOp

**Rule**: `ValueMetadata` is keyed by `llvm::Value*`. When you
`CreateLoad(elemTy, GEP(data, i))` to read an element from a `List` data pointer,
a `Map` values slot, or any similar array-of-elements, the produced SSA value is
fresh and has no entry in `value_metadata_`. Calls to helpers that dispatch on
metadata — `getListElementType`, `getMapValueType`, `getSetElementType`,
`isStringValue` — all return null/false, causing silent misclassification (e.g.,
pointer values treated as `str` and compared via `strcmp`).

**Why**: `getMeta` resolves `LoadInst → pointer operand` as a convenience, but a
`GEP`-produced pointer is itself a fresh SSA value with no entry; the resolution
stops there.

**How to apply**: Before passing a GEP-loaded pointer value to any helper that
dispatches on metadata, call
`propagateTypeMeta(outerMeta->list_elem_type_name, loaded)` (or the
`map_value_type_name` / `set_elem_type_name` equivalent) to reconstruct
`ValueMetadata` from the parent container's stored type name.

Guard the call with `!elemName.empty() && elemName != "str"` — `str` elements are
plain `char*` pointers that `isStringValue` already handles correctly without
metadata, and the type name may be empty for untyped string literals.

### ExtractValue on Option<Collection> or Result<Collection, E> inner field drops metadata — rebuild before recursive comparison

**Source**: #982 (2026-04-16, bugfix), #985 (2026-04-16, bugfix)
**Tags**: codegen, metadata, equality, option, result, collection, emitComparisonOp, ExtractValue

**Rule**: `builder_.CreateExtractValue(val, idx)` to read the inner value from an
`Option<T>` or `Result<T, E>` struct produces a fresh SSA value with no entry in `value_metadata_`,
exactly like a GEP-loaded pointer. For `Option<List<T>>`, `Option<Map<K,V>>`,
`Option<Set<T>>`, `Result<List<T>, E>`, `Result<Map<K,V>, E>`, and the symmetric
`Result<_, List<T>>` / `Result<_, Map<K,V>>` cases, the inner LLVM type is `ptrTy_`.
Without metadata, the recursive `emitComparisonOp` call falls through to the
`isStringValue` + `strcmp` path and produces false-positive equality.

**Why**: `buildSomeValue` and `buildOkValue` call `propagateMeta(inner, val)`, so the
outer `Option` / `Result` aggregate already carries `list_elem_type_name` /
`map_*_type_name` / `set_elem_type_name`. The extracted inner is an orphan: there is
no automatic metadata inheritance from the containing aggregate.
`buildErrValue` also calls `propagateMeta(inner, val)` (added in #985), so
`Result<_, Collection>` Err-payload metadata is likewise carried on the outer aggregate.

**How to apply**: After extracting the inner values, check whether the element type is `ptrTy_`.
If so, call `buildTypeNameFromMeta(lhs)` (or `rhs` as fallback) to snapshot the inner
type name, then `propagateTypeMeta(innerName, lhsInner)` + `propagateMeta(lhsInner, rhsInner)`
before the recursive comparison. Guard with `!innerName.empty() && innerName != "str"`.
Snapshot into a local `std::string` **before** calling `propagateTypeMeta` to avoid
rehash-invalidation of `ValueMetadata *` pointers (see #858 gotcha).

**`propagateTypeMeta` now handles `"Result<T,E>"` and `"Option<T>"` type name strings**
(added #985): When `propagateReturnTypeMeta` is called after a function call returning
`Result<List<int>, Error>`, `propagateTypeMeta("Result<List<int>, Error>", callResult)`
now extracts the Ok type "List<int>" and recurses. If the Ok type is not a collection,
it falls back to the Err type (covering the `Result<int, List<int>>` Err-payload case).
This ensures the alloca for `a = make_result_fn()` carries `list_elem` metadata even
without explicit type annotation, so `buildTypeNameFromMeta` can recover the type name
at compare time.

**ARC — collections inside Result/Option returned from functions** (#999, fixed):

### Outer `emitResultBranch` PHIs must re-propagate Ok-path metadata from inner Result values

**Source**: #1315 (2026-04-23, bugfix)
**Tags**: codegen, metadata, result, emitResultBranch, phi, http, resource

**Rule**: If a stdlib dispatcher first builds a metadata-carrying `Result<T, E>`
on the success path (for example `wrapPtrAsResult(...)` + `addResourceKind(...)`)
and then wraps that value in an **outer** `emitResultBranch(...)` for a guard such
as NUL validation, the outer merged PHI does **not** automatically inherit the Ok-path
metadata. Capture the successful inner Result in a local (e.g. `okIncoming`) and call
`propagateMeta(okIncoming, mergedResult)` after the outer `emitResultBranch(...)`.

**Why**: metadata lives in `value_metadata_` keyed by SSA value. The inner success
Result and the outer merged PHI are distinct SSA values, so the resource/collection
kind attached to the former is invisible to later consumers such as `Ok(resp)` case
bindings unless you copy it explicitly.

**Concrete manifestation**: `http_get` / `http_post` / `http_request` tagged the
inner `wrapPtrAsResult(...)` value as `HttpClientResponse`, but an outer NUL-check
`emitResultBranch(...)` returned a fresh PHI without that tag. `status(resp)` and
`body(resp)` inside `case http_get(...): Ok(resp): ...` were then rejected as
non-`HttpClientResponse` arguments.
`buildOkValue` / `buildErrValue` / `buildSomeValue` now call `tryRetainArcSource(inner)`
when `inner->getType() == ptrTy_`. This retains the collection before scope cleanup at
fn exit can release the local variable. `tryRetainArcSource` handles three cases:
1. `LoadInst` from an ARC-managed alloca (emits retain — the direct-param bugfix case)
2. `arc_owned_values_` (no-op — `Ok([1, 2])` inline construction stays unaffected)
3. `ExtractValueInst` (record/tuple field access via `CreateExtractValue`) — retains only
   when collection metadata (`list_elem` / `map_key` / `set_elem`) is set on the value;
   this guards against incorrectly retaining non-ARC `ptrTy_` values (closures, weak refs).
Note: `codegen_expr.cpp` previously had a standalone `tryRetainArcSource(errVal)` after
`buildErrValue` in the `?` operator error path. With Case 3 added, this became a
double-retain and was removed — `buildErrValue` now handles the retain internally.
Regression tests live in `tests/spec/result_option_arc_return.test.ry`.

**Limitation**: For `Result<List<T>, List<U>>` where both Ok and Err payloads are
collections of different types, the metadata on the outer aggregate reflects whichever
payload was inserted last (Ok wins in `buildOkValue` since it runs after `buildErrValue`).
Resolving this cleanly would require per-variant metadata slots on `ValueMetadata`.
This edge case is rare and out of scope; the common `Result<Collection, Error>` and
`Result<_, Collection>` cases are fully covered.

### Empty collection literal early-return in codegen_stmt.cpp does not propagate closure/nested-type metadata

**Source**: #958 (2026-04-15, implementation)
**Tags**: codegen, metadata, closure, set, collection, empty-literal

**Rule**: `codegen_stmt.cpp` has special early-return paths for empty collection
literals (e.g., `a: Set<fn> = {}`, `a: List<fn> = []`) that allocate the header and
`return` before reaching the general `if (newTy == ptrTy_)` metadata-propagation
block (L390+). Each such early-return path must explicitly set
`*_fn_type_info` and `*_elem_type_name` if the annotation's inner type is a closure
or nested collection — otherwise the metadata will be missing for the variable's
alloca, and downstream guards (e.g., the `set_elem_fn_type_info` equality rejection
at `codegen_expr.cpp`) will silently not fire.

**Why**: The empty-literal fast paths (e.g., L28-58 for Set, L102-151 for List)
were written to handle the common primitive/record case. The L102 List path
correctly sets `list_elem_fn_type_info` via an explicit `else if` branch. The L28
Set path was initially missing the equivalent, causing `Set<fn> == Set<fn>` to
compile without error instead of being rejected.

**How to apply**: Whenever you add or modify an early-return empty-literal path in
`codegen_stmt.cpp`, audit the block to ensure all of the following metadata fields
are populated as appropriate:
- `setTypeMeta(TypeMeta::SetElem / ListElem / MapKey / MapValue, ptr, elemTy)`
- `getOrCreateMeta(ptr).set_elem_type_name` (for nested collection element types)
- `getOrCreateMeta(ptr).set_elem_fn_type_info` (for closure element types)
- `getOrCreateMeta(ptr).map_key_type_name` / `map_key_fn_type_info` (for Map key side — added in #961; mirror of Set path)
- The `list_*` and remaining `map_value_*` counterparts for List/Map early paths

### `instantiateGenericEnum` omitted `variantOrder` population

**Source**: #959 (2026-04-15, implementation)
**Tags**: codegen, generic-enum, ADT, variantOrder, regression

**Rule**: `instantiateGenericEnum` in `src/codegen_fn_generic.cpp` builds `EnumInfo` for
concrete instantiations (e.g., `MyOpt<int>`). It populates `info.variants` (the
`unordered_map<name → tag>`) but historically did NOT populate `info.variantOrder` (the
ordered variant name vector). Any code that iterates `variantOrder` — such as the
payload-aware ADT equality comparison introduced in #959 — will silently iterate zero
elements, causing generic enum variants to fall through to the "no payload" fast path and
produce wrong results (tag-only comparison).

**Why**: The non-generic path in `codegen_stmt_misc.cpp` populates `variantOrder` inside
the same loop that adds entries to `variants`. The generic path duplicated most of the
loop body but missed this line.

**How to apply**: Any new code that reads `EnumInfo::variantOrder` for logic that must also
apply to generic enum instantiations must verify that `instantiateGenericEnum` keeps
`variantOrder` in sync with `variants`. When adding `EnumInfo` fields in the future,
update both `codegen_stmt_misc.cpp` and `codegen_fn_generic.cpp` together.

### Resolving generic enum types as fn param/return/let requires a two-layer fix

**Source**: #1203 (2026-04-19, implementation)
**Tags**: codegen, generic-enum, resolveType, type_param_scope_, ensureEnumInstantiated

**Rule**: `CodeGen::resolveType` alone is not sufficient to handle generic enum types in
type positions. Two independent mechanisms must be combined:

1. **Substitute type-param names inside `<...>` at `resolveType` entry.** The old code only
   did a bare-identifier lookup in `type_param_scope_`, so `resolveType("MyOpt<T>")` would
   fall through to the `unknown type` branch when called during generic-fn instantiation
   even though `T` was bound. The fix walks the type string recursively — bare name, `weak X`,
   `X?`, and `Base<Args...>` — substituting any bare identifier that appears in
   `type_param_scope_`, then re-calls `resolveType` with the rewritten name. See
   `substituteTypeParamsInName` in `src/codegen_type.cpp`.
2. **Call `ensureEnumInstantiated(typeName)` before the final `unknown type` error.** Even
   a fully-qualified instance like `MyOpt<int>` in a non-generic fn signature does not hit
   the instantiation path automatically — `resolveType` only queries `enum_types_` which is
   populated on construction, not on type-position mention. Adding the
   `ensureEnumInstantiated` fallback makes every type position uniform with expression
   positions.

**Why**: The two failure modes look similar (both say `unknown type: MyOpt<...>`) but have
different root causes. Fixing only one leaves half the issue.

**How to apply**: When adding a new type-position site (e.g., new let-binding form, new
field type, new generic argument shape), ensure both layers flow correctly. In
`instantiateGenericFn`, surface-name strings used for pattern-matching metadata
(`paramTypeNames`, `exposedReturnTypeName`) must also be passed through
`substituteTypeParamsInName` — substituting only the `llvm::Type*` is insufficient because
downstream code recovers the surface name from metadata to decide dispatch.

### Self-referential enum inline fields emit a wrapper-type diagnostic

**Source**: #1203 (2026-04-19, implementation)
**Tags**: codegen, enum, self-reference, diagnostic, user-experience

**Rule**: In `emitStmt(EnumStmt &s)` (`src/codegen_stmt_misc.cpp`), a pre-pass walks each
variant field's `TypeNode` AST recursively via the file-local helper
`containsInlineSelfReference`. It reports a self-reference when the enum's own name appears
in any of: a bare identifier (`Tree`), a generic application's base (`LList<T>`), the inner
of an optional (`Tree?`), a tuple element (`(int, Tree)`), an array element, a union
component, or inside an arbitrary non-indirection generic (`Option<Tree>`,
`Pair<Tree, Tree>`). The helper treats `List<_>`, `Map<_, _>`, `Set<_>`, `Task<_>`,
`Channel<_>`, `weak T`, and `fn(...)` as pointer-backed indirection and stops
descending — their payload is boxed, so the layout is finite. The recommendation message
points users to container indirections: `List<T>`, `Map<K, V>`, `Set<T>`, `Task<T>`, and
`Channel<T>` (#1351). It intentionally excludes `weak T` (weak requires an ARC-managed
payload, and a separate diagnostic already rejects `weak <ADT>`) and `fn(...)` (unusual
choice for data storage).

**Why**: An earlier substring-based check on the field's `toString()` only caught the bare
name and the generic base (`ftStr.substr(0, ftStr.find('<'))`). That missed `Tree?`,
`(int, Tree)`, and `Option<Tree>`, all of which then produced the cryptic
`unknown type: Tree` at field-type resolution time instead of the helpful diagnostic.
Switching to an AST walk fixes every wrapping shape uniformly and runs before the
generic-template save so `generic_enum_templates_` never stores a self-referential
template that would crash at instantiation time with `unknown type: T`. Full
recursive-enum support (auto-boxing) is a separate feature tracked as a follow-up issue.

**How to apply**: When adding a new indirection wrapper (e.g. `Rc<T>`, `Box<T>`) that
stores its payload behind a pointer, extend the indirection list in
`containsInlineSelfReference` *and* add it to the recommendation string. When adding a new
non-indirection wrapper (e.g. an inline SoA generic) nothing needs to change — the default
branch already traverses generic type-args. When considering supporting inline
self-reference in enums, this pre-pass must be removed (or relaxed to only reject when
auto-boxing cannot recover).

### Bare generic-enum name in type positions produces a dedicated diagnostic

**Source**: #1203 (2026-04-19, implementation)
**Tags**: codegen, generic-enum, resolveType, diagnostic, user-experience

**Rule**: `CodeGen::resolveType` distinguishes three failure modes for generic enums:

1. `MyOpt` (bare template name, no `<...>`) → `generic enum 'MyOpt' used without type
   arguments; write MyOpt<T>...` (echoes the template's actual first type-parameter name,
   not a hardcoded `T`).
2. `MyOpt<int>` (concrete) → on-demand `ensureEnumInstantiated` before the final `unknown
   type` error, so the enum is instantiated lazily at any type position.
3. `MyOpt<T>` with `T` bound in `type_param_scope_` → `substituteTypeParamsInName` rewrites
   to `MyOpt<int>`, then the case above kicks in.

**Why**: The user-facing regression in #1203 was that `fn f(opt: MyOpt, ...)` and
`fn f<T>(opt: MyOpt<T>, ...)` both produced `unknown type: ...` with no hint. Case 1
now guides the user to the correct syntax; cases 2 and 3 compile silently.

**How to apply**: When adding a new generic-template registry (alongside
`generic_enum_templates_`), mirror the bare-name check in `resolveType` so the user sees a
template-name-specific error message instead of falling through to the generic `unknown
type: <name>` branch. The first type-parameter name is read from the template record — do
not hardcode `T`.

### `github.ref_name` points to `main` in scheduled workflows, not the checked-out branch

**Source**: #970 (2026-04-15, implementation)
**Tags**: ci, github-actions, schedule, ccache, gotcha

**Rule**: In a GitHub Actions scheduled workflow (`on: schedule:`), `github.ref_name` is
always the **default branch** (`main`), regardless of which branch the job checks out via
`actions/checkout` with an explicit `ref:`. Expressions like
`github.ref_name == 'main' || startsWith(github.ref_name, 'v')` therefore always evaluate
to `true` or `false` based on the workflow file's source branch, not the checked-out branch.

**Why**: When designing `ci-scheduled.yml` (moving heavy jobs out of PR CI into a daily
schedule), the `ccache save:` condition copied from `ci.yml` was
`${{ github.ref_name == 'main' || startsWith(github.ref_name, 'v') }}`.
On a scheduled run this always evaluates to `true` (if the file lives on `main`) regardless
of `resolve-target`'s dynamic branch choice, but if the file ever ends up on a non-matching
branch the cache would silently never be saved. Relying on this expression in scheduled
workflows is fragile.

**How to apply**: In scheduled workflows that dynamically check out a branch, replace the
conditional `save:` expression with `save: true` (always save) or compute the condition from
the resolved `ref` output rather than `github.ref_name`.

### Tuple pattern in `case`: per-element metadata via `splitTypeArgs`

**Source**: #834 (2026-04-16, implementation)
**Tags**: codegen, pattern, tuple, metadata, case, splitTypeArgs

**Rule**: When emitting a `TuplePattern` in `emitPatternTest` / `emitPatternBindings`, the subject's type signature arrives as a `"(T1, T2, ...)"` string in `subjectEnumType`. Strip the outer parentheses and call `splitTypeArgs(inner)` to obtain per-element signatures. Pass each element's signature as the `subjectEnumType` argument in the recursive `emitPatternTest` / `emitPatternBindings` calls so that nested Option/Result/generic patterns can resolve their metadata correctly.

**Why**: `propagateTypeMeta` (KNOWLEDGE.md entry at ~L420) is deliberately single-purpose and does not decompose tuple type strings. The correct decomposition point is the `TuplePattern` codegen arm itself, mirroring the existing `emitTupleDestructure` helper in `codegen_stmt_loop.cpp`.

**How to apply**: When a future pattern type (e.g., positional record `Point(a, b)`) also needs per-element metadata, use the same pattern: strip the outer delimiters, split with `splitTypeArgs`, and feed each component signature into the recursive call. Do NOT extend `propagateTypeMeta` to handle composite type strings.

### Tuple pattern `parsePattern`: same ambiguity resolution as expression parser

**Source**: #834 (2026-04-16, implementation)
**Tags**: parser, pattern, tuple, grouping, ambiguity

**Rule**: The `parsePattern` `LParen` branch uses the same grouping-vs-tuple disambiguation as the expression parser (`src/parser_expr.cpp`):
- `()` → rejected ("zero-tuple pattern not supported")
- `(p)` (no comma) → grouping, returns the inner pattern unwrapped
- `(p,)` → 1-tuple `TuplePattern` with one element
- `(p1, p2, ...)` → N-tuple `TuplePattern`

**Why**: Keeping expression and pattern parsers consistent avoids user confusion (grouping `(p)` behaves the same in both contexts) and makes future record / enum-payload pattern parsers easier to cross-reference.

**How to apply**: When adding the next pattern type that starts with `(` (e.g., positional record `Point(a, b)` — note that starts with `Ident LParen`, not bare `LParen`), keep the bare-`LParen` branch as tuple-only and handle `Ident LParen` in a separate branch.

### `parseDirectives` must defer `LParen` when tuple-destructure LHS lookahead matches

**Source**: #1189 (2026-04-19, implementation)
**Tags**: parser, directive, tuple, destructure, lookahead, ambiguity

**Rule**: In `parseDirectives()`, before consuming `LParen` as the start of a directive argument list (`@name(arg, ...)`), check `!looksLikeParenthesizedTupleDestructure()`. If the lookahead matches, leave the `LParen` for `parseStatement()` to handle as the start of a tuple-destructure LHS (`@const (a, b) = expr`).

**Why**: Directive-arg syntax `@name(arg, ...)` and statement-level tuple-destructure LHS `(ident, ident, ...) =` both start with `@name LParen Ident`. The only disambiguator is the trailing `=` after `RParen`. Without this guard, `@const (a, b) = expr` is misparsed as `@const(a, b)` with positional args `a` and `b`, and the subsequent `=` trips the "directives are not supported on this statement" gate. `couldBeLambda`-style conservative lookahead (restored via `lex_.saveState()` / `restoreState()`) is the right tool — false negatives are fine because they fall through to the old behavior.

**How to apply**: When adding any new statement form that begins with `LParen` after directives (e.g., `@const (_ as Pattern) = expr`, hypothetical future destructuring variants), extend `looksLikeParenthesizedTupleDestructure` (or add a parallel predicate) and make `parseDirectives` defer to it in the same guard location. Never let `parseDirectives` unconditionally eat `LParen`.

### Formatter→parser roundtrip: `TupleDestructStmt` must not emit `: ` between pattern and `=`

**Source**: #1189 (2026-04-19, implementation)
**Tags**: formatter, parser, tuple, destructure, roundtrip, latent_bug

**Rule**: `formatTupleDestruct()` in `src/formatter_stmt.cpp` must emit only `<pattern> = <value>` (plus optional `@const` directive on a prior line). Do **not** emit a stray `: ` between the closing `)` of the pattern and the `=`. The immutability is conveyed by the `@const` directive emitted before the statement, not by a `:` suffix on the LHS.

**Why**: Until #1189 landed, the parser rejected all parenthesized tuple-destructure forms, so the formatter's output `(a, b):  = (1, 2)` never round-tripped through parse. Enabling the parenthesized parse branch exposed the latent `: ` bug — formatted output now fails `ry fmt` verification ("formatted output failed to re-parse"). Adding `FormatterTest.ParenTupleDestructRoundTrip` locks this in so future formatter edits cannot regress.

**How to apply**: When adding or modifying a formatter rule for a new statement shape, grep for a matching parser spec test and add a `verifyFormatting` / roundtrip assertion. Formatter output that fails to re-parse is a silent correctness bug during `ry fmt`; only the verification pass catches it.

### Record pattern in `case`: do NOT propagate primitive field type names as `subjectEnumType` in bindings

**Source**: #989 (2026-04-16, implementation)
**Tags**: codegen, pattern, record, metadata, case, enum_value_type, valueToString

**Rule**: In `emitPatternBindings` for `RecordPattern`, each field's Ry type string (`info.fields[i].type->toString()`) must be passed to `propagateTypeMeta` for collection/type metadata, but must **not** be passed blindly as `subjectEnumType` to the recursive `emitPatternBindings` call. Only pass it as `subjectEnumType` if the field type is an actual enum (`enum_types_.count(elemSig) > 0`). For primitives ("int", "float", "str", "bool") and collection types, pass empty string.

**Why**: The `VariablePattern` binding arm in `emitPatternBindings` does `getOrCreateMeta(varAlloca).enum_value_type = subjectEnumType` whenever `subjectEnumType` is non-empty. If `subjectEnumType` is "int", this stores `enum_value_type = "int"`. Then `valueToString` checks `enum_value_type`, looks up `enum_types_["int"]` (creates a zero-initialized default entry via `std::unordered_map::operator[]`), reads `nameArray` (null), and passes it to `CreateGEP` → LLVM asserts/crashes.

Records don't set `enum_value_type` on constructed values anywhere in the compiler (unlike enums which set it in `emitExprVariant(EnumExpr)` and `emitStructConstructor` in `codegen_call_dispatch.cpp`). So `subjectEnumType` for a record `case` subject is typically empty, and the per-field type from `struct_types_` is the authoritative source.

**How to apply**: Mirrored in `emitPatternTest` too: passing the full field type string as `subjectEnumType` in recursive `emitPatternTest` calls is safe (LiteralPattern and WildcardPattern ignore it; RecordPattern uses `pat->name` for lookup, not `subjectEnumType`). The dangerous place is **only in `emitPatternBindings`** where `VariablePattern` stores `subjectEnumType` directly into `enum_value_type` metadata.

### ConcurrencySpecSuite ASan DISABLED_ was stale after #630's atomic-ARC fix

**Source**: #872
**Tags**: asan, concurrency, parallel_for, arc, testing

**Rule**: `ConcurrencySpecSuite` was disabled under ASan in commit `fb010ea`
(2026-03-31) because non-atomic ARC retain/release ops in `@parallel for`
workers raced with ASan's shadow-memory interceptors, causing a deadlock
(SIGALRM, exit 142 after 300 s on Linux).  After #630's P0 fix made all ARC
ops inside `@parallel for` thunks use `atomicrmw seqcst`, the root cause
was removed.  The `DISABLED_` guard was removed in #872; on macOS the suite
runs in ~55 ms.  If a future change reintroduces non-atomic ARC inside a
`@parallel for` thunk, re-enable the guard and investigate the hang before
merging.

### `thread_spawn` captures do NOT emit ARC retain/release in the thunk

**Source**: #872
**Tags**: thread_spawn, arc, concurrency, codegen, gotcha

**Rule**: Unlike `@parallel for` — which explicitly emits atomic
`emitArcRetain(hdr, true)` at thunk entry and a matching `emitArcRelease` at
scope exit for each ARC-managed capture — `thread_spawn` thunks do NOT
retain/release their captures.  The capture is stored as a raw pointer copy
into the env struct; the thunk loads the pointer and calls the lambda, and
`FnScope` is destroyed at CODEGEN time without ever calling `popScope()`, so
no runtime release instruction is emitted.

Consequence: the main thread's original reference keeps the ARC object alive
for the worker's lifetime.  As long as `thread_join` is called before the
outer scope's variable goes out of scope, this is correct.  TSan does NOT
report a race for concurrent str captures via `thread_spawn` because no
concurrent ARC ops are emitted.

Correctness contract: the caller MUST ensure the captured ARC-managed value
outlives all spawned workers (i.e., call `thread_join` before the capture's
owning scope exits).  No retain/release means no race protection beyond that
lifetime ordering.  A future follow-up could add optional retain-at-capture /
release-at-thunk-exit for `thread_spawn` (distinct from #877, which tracks
ARC-managed **return types**, not capture semantics).

### `parallel_for_depth_` design decision — keep scope-counter for now

**Source**: #872
**Tags**: arc, parallel_for, design-decision, concurrency

**Rule**: The current mechanism (`parallel_for_depth_` counter in `CodeGen`
→ `isArcAtomic()` returns true → `atomicrmw seqcst` for all ARC ops inside
the `@parallel for` thunk body) is correct for values emitted **directly**
inside the thunk.  It does **not** propagate to helper functions the worker
calls; those emit non-atomic ARC ops.

Options considered for #872:
1. **Keep `parallel_for_depth_`** (current) — simple, no perf cost outside
   `@parallel for`.  Limitation: helper-function races are not covered.
2. **Whole-program "may cross threads" analysis** — correct but expensive;
   requires call-graph walk and is a significant compiler feature.
3. **User-visible `Send`-like marking** — expressive but a language design
   change with breaking implications.
4. **Unconditional atomic ARC everywhere** — correct and simple but adds
   measurable latency to every single-threaded ARC operation.

**Decision for #872**: Keep option 1.  The stress tests added in #872
(`ConcurrencySpecSuite`, `concurrency_stress.test.ry`,
`test_runtime_arc_contention_stress.cpp`) did NOT expose a helper-function
race.  Revisit only if future stress tests expose such a race.

### ADT enum constructor pattern: single TuplePattern binding must be "unwrapped"

**Source**: #990 (2026-04-16, implementation)
**Tags**: codegen, pattern, enum, tuple, ADT, emitPatternTest, emitPatternBindings

**Rule**: `Event::Click((0, 0))` is parsed as `EnumConstructorPattern` with **one** binding that is a `TuplePattern{elements: [0, 0]}`. If `emitPatternTest` / `emitPatternBindings` naively iterate `pat->bindings` (size 1) and try to match `fieldTypes[0]` ("int") against a `TuplePattern`, the recursive call to `emitPatternTest(TuplePattern{...}, int_val, "int")` will call `splitTupleSig("int")` → empty → crash ("tuple pattern applied to non-tuple subject").

The fix: before the field loop in both `emitPatternTest` and `emitPatternBindings`, detect the "single TuplePattern whose arity == variant's field count" case and redirect the loop to iterate over the TuplePattern's **elements** instead:

```cpp
const std::vector<Pattern> *fieldPats = &pat->bindings;
if (pat->bindings.size() == 1) {
    if (auto *tp = std::get_if<std::unique_ptr<TuplePattern>>(&pat->bindings[0])) {
        if ((*tp)->elements.size() == fit->second.fieldTypes.size())
            fieldPats = &(*tp)->elements;
    }
}
// Use (*fieldPats)[i] and fieldPats->size() in the loop.
```

**Why**: `(0, 0)` inside `Event::Click(...)` is grammatically a tuple pattern (two elements), not two separate arguments. The parser correctly emits one `TuplePattern`, but codegen must recognise this as syntactic sugar for "match the N fields individually". The unwrap only triggers when element count == field count; mismatched arities fall through to the normal path (which will produce a runtime type-check error, matching the behaviour of other arity mismatches).

**How to apply**: Mirrored changes are required in **both** `emitPatternTest` (for the test-phase) and `emitPatternBindings` (for the binding-phase). Missing either half causes the other phase to use the wrong pattern → incorrect runtime behaviour.

### ADT enum constructor pattern: payload tests must be gated by a tag-match branch

**Source**: #990 (2026-04-16, CodeRabbit review)
**Tags**: codegen, pattern, enum, ADT, emitPatternTest, PHI, LLVM, safety

**Rule**: In `emitPatternTest` for `EnumConstructorPattern`, do **not** use `CreateAnd` to combine the tag equality with payload sub-tests. `CreateAnd` does not short-circuit in LLVM IR, so payload loads execute unconditionally even when the tag does not match. Loading a `str` pointer field from a variant that actually stores an `int` payload (or vice versa) and then passing it to `strcmp` / pointer tests will crash at runtime.

**Fix**: Gate payload loads with a conditional branch (`CreateCondBr`) on the tag equality result, run the GEP/load/test loop in the `ecp.payload` basic block, then merge with a PHI node:

```cpp
llvm::BasicBlock *tagMatchBB = builder_.GetInsertBlock();
auto *payloadBB = llvm::BasicBlock::Create(*ctx_, "ecp.payload", fn);
auto *mergeBB   = llvm::BasicBlock::Create(*ctx_, "ecp.merge",   fn);
builder_.CreateCondBr(testResult, payloadBB, mergeBB);

builder_.SetInsertPoint(payloadBB);
// ... GEP / load / emitPatternTest loop builds fieldsMatch ...
llvm::BasicBlock *payloadEndBB = builder_.GetInsertBlock();
builder_.CreateBr(mergeBB);

builder_.SetInsertPoint(mergeBB);
llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, "ecp.final");
phi->addIncoming(llvm::ConstantInt::get(i1Ty_, 0), tagMatchBB); // tag missed → false
phi->addIncoming(fieldsMatch, payloadEndBB);                     // tag hit → payload result
testResult = phi;
```

**Why**: Tagged-union variants can have entirely different payload types (int vs str). Loading the wrong type's bytes as a pointer and dereferencing it is undefined behaviour that manifests as a crash. The `emitPatternTest` for `TuplePattern` uses `CreateExtractValue` (safe, always valid), but `EnumConstructorPattern` uses `CreateLoad` from raw GEP — inherently unsafe when the tag doesn't match.

**How to apply**: This pattern applies to any new pattern type that (1) lives inside a tagged union and (2) loads payload bytes via a GEP through the union's raw byte array. Always branch on the discriminant before loading.

### sema_return exhaustiveness: EnumConstructorPattern covers a variant only when payload is irrefutable

**Source**: #990 (2026-04-16, CodeRabbit review)
**Tags**: sema_return, exhaustiveness, pattern, enum, irrefutable, return-analysis

**Rule**: In `collectPatternInfo` (`sema_return.cpp`), only call `cov.coveredVariants.insert(variant_name)` for an `EnumConstructorPattern` when every binding in `pat->bindings` is irrefutable (i.e. `WildcardPattern`, `VariablePattern`, or recursively-irrefutable `TuplePattern`). An arm like `Event::Click((0, 0))` only matches a subset of `Click` values — adding `Click` to `coveredVariants` unconditionally would make `isExhaustiveMatch()` unsound and allow a non-returning `case` to pass return analysis.

**How to apply**: Use a small `isIrrefutable(const Pattern &)` recursive helper (see `src/sema_return.cpp`). Pattern types that are always refutable: `LiteralPattern`, `EnumPattern` (always a specific tag), `SomePattern`, `NonePattern`, etc. When adding any new pattern type to the language, update `isIrrefutable` accordingly. Pre-existing `EnumPattern` arms (without payload) are always irrefutable for their specific variant (the tag check is the only condition), so they continue to insert unconditionally.

### ErrPattern binding in codegen_match.cpp must propagateMeta to preserve collection element-type metadata

**Source**: #1001 (2026-04-16, implementation)
**Tags**: codegen_match, pattern, Result, Err, metadata, collection, propagateMeta

**Rule**: In `emitPatternBindings` (`codegen_match.cpp`), the `ErrPattern` arm must call `propagateMeta(subjectAlloca, varAlloca)` after storing the extracted Err payload — exactly like `OkPattern` (index 1) and `SomePattern` do. Without this call, the bound variable (e.g., `lst` in `Err(lst)`) loses the collection element-type metadata stored on the subject alloca. Any subsequent index access or collection-kind dispatch on the binding will then fail with "cannot determine list element type".

**Why**: `CreateExtractValue` produces a new LLVM `Value *` that does not inherit the custom metadata stored in the compiler's `value_metadata_` side-table. `propagateMeta` is the mechanism to copy that metadata to the new binding alloca. The same pattern was already applied to `OkPattern` and `SomePattern`, but the `ErrPattern` arm was accidentally left without it — there was no test that could trigger the gap before #1001 made `Err(collection)` construction possible.

**How to apply**: Whenever a new `xyzPattern` is added that extracts a sub-value from a subject alloca and introduces a binding variable, always add `propagateMeta(subjectAlloca, varAlloca)` after `CreateStore`. Review the neighbouring arms as a checklist.

### Post-hoc Result coercion: preferred over modifying Ok/Err constructors for annotation-driven type resolution

**Source**: #1001 (2026-04-16, design choice); updated #1111 (2026-04-17, `anyTy_` runtime branch + both-slot bailout fix); updated #1157 (2026-04-19, disc-static provenance gate)
**Tags**: codegen_stmt, coercion, Result, Ok, Err, annotation, emitVarDecl, anyTy_, type-inference

**Rule**: When `Err([...])` (or `Ok(...)`) yields a Result struct whose layout does not match the variable's type annotation, fix the mismatch in `emitVarDecl`'s post-hoc coercion chain (`coerceResultType`) rather than threading the annotation down into the Ok/Err constructor emitter in `codegen_call.cpp`.

**Why**: The Ok/Err constructors can be called from many contexts (function arguments, return values, inlined expressions) where the target type is unavailable or ambiguous. Post-hoc coercion at the declaration site is localised, mirrors the existing Option auto-wrap pattern.

**How to apply**: `coerceResultType(val, dstResTy)` in `codegen_stmt.cpp`:
- Compute `okNeedsRuntimeBranch` and `errNeedsRuntimeBranch` **before** the early bailout. These booleans check: `isAnyType(srcSlotTy) && !isAnyType(dstSlotTy) && dstSlotTy != i8Ty_ && canAnyHoldType(dstSlotTy)`. They must be available at the bailout call site (#1111 CodeRabbit review finding).
- **Bailout condition**: `srcOkTy != dstOkTy && srcErrTy != dstErrTy && (!okNeedsRuntimeBranch || !errNeedsRuntimeBranch)` → return `nullptr` (genuine type error). The original `both differ → nullptr` was wrong for `Result<any,any> → Result<T1,T2>`.
- Both slots differ AND both can be anyTy_-unwrapped → emit a **runtime disc branch** (disc=1 → Ok, disc=0 → Err), `unwrapFromAny` the any-typed payload to the dst concrete type in the active path, PHI the two rebuilt structs. A compile-time slot selection silently zeroes the active payload for the wrong disc value (#1111 Manifestation 1).
- **Single-slot mismatch — disc-static gate (#1157)**: Exactly one slot differs → **only** accept when `tryGetStaticResultDisc(val, &staticDisc)` returns true. This function walks the `InsertValueInst` chain (accounting for LLVM constant-folding of all-constant chains into `ConstantStruct`/`ConstantAggregateZero`) and recurses into `PHINode` incomings. Sources with a runtime disc (function call results, loads, mixed Ok/Err PHI) must return `nullptr` — they were silently miscompiling by zero-filling the active payload slot. Literal `Ok()`/`Err()` and if-exprs where all branches share the same disc (e.g., both Ok) remain accepted.
- **Placeholder types are NOT a safe signal at the LLVM level**: `Result<Unit,E>`, `Result<i8,E>`, and `Result<u8,E>` all lower to `{i1, i8, E}`. Checking `srcOkTy == i8Ty_` cannot distinguish a Unit dummy from a genuine i8 Ok payload. The disc value (static provenance) is the only reliable discriminant.
- The `Ok` emitter (`codegen_call.cpp`) must also unwrap `anyTy_` inner to the expected ok type from `fn_->getReturnType()` when building the Result struct — otherwise two branches of an if-expr (e.g., `Ok(x)` vs `Ok(0)`) produce different Result struct types and `validateBranchTypes` rejects them (#1111 Manifestation 2, fixed alongside `inferExprType`).
- Add the same coercion branch to variable reassignment handlers for consistency.

### `propagateTypeMeta`: handle both `Option<T>` prefix and `T?` suffix — they are the same type

**Source**: #1003 (2026-04-16, bugfix); extended by #1015 (2026-04-16, `extractGenericTypeArg` in `codegen_match.cpp`)
**Tags**: codegen, metadata, option, shorthand, list_elem, propagateTypeMeta, OptionalType, extractGenericTypeArg

**Rule**: Whenever you add or modify a code path that recognises `"Option<..."` as a prefix string, also handle the `"T?"` suffix form. `OptionalType::toString()` (`src/type_node.cpp:51-52`) emits the `"?"` suffix form, and `OverloadEntry::returnTypeName` stores it verbatim. So functions declared as `-> List<int>?` have `returnTypeName = "List<int>?"`, not `"Option<List<int>>"`. A branch that only checks `resolved.compare(0, 7, "Option<")` will silently skip `T?` returns.

**How to apply**: After the `Option<T>` branch, add an `else if (resolved.size() > 1 && resolved.back() == '?')` branch that strips the `?` and recurses: `propagateTypeMeta(resolved.substr(0, resolved.size() - 1), val)`. See `src/codegen_builtin.cpp:169-173` (added #1003) and the earlier precedent in `src/codegen_arc_gc.cpp:136-138`. The `resolveTypeAlias` function is a pure string map lookup and cannot produce `?`-suffixed strings for non-optional aliases (the lexer tokenises `?` as a standalone `Question` token, so identifiers cannot contain it), making the `resolved.back() == '?'` guard safe.

**Known parallel gaps**: `src/codegen_match.cpp` (`extractGenericTypeArg`) was resolved in #1015 — both `Option<T>` prefix and `T?` suffix are now handled, ensuring the typed ARC retain path (Path 2a in `emitPatternBindingArc`) is taken for `SomePattern`. The remaining gap is `src/codegen_stmt.cpp:430`, which checks `ann.substr(0, 7) == "Option<"` when propagating collection metadata from a variable's explicit type annotation. That gap only matters when the RHS value carries no metadata of its own (e.g., `x: List<int>? = None`). Since `None` contains no collection, any subsequent index on `x` would raise a runtime unwrap error before metadata is needed — so the practical impact is negligible. Track in a future issue if a concrete regression is found.

### `CreateStore(narrowVal, anyTyAlloca)` leaves the `data` field uninitialized

**Source**: #1020 (2026-04-16, bugfix — reduce with untyped lambda returns garbage)
**Tags**: codegen, any, alloca, store-width, reduce, fold

**Context**: `anyTy_` is a 16-byte struct `{i64 tag, [8 x i8] data}`.
Allocating an `any` slot then storing a raw `i64`/`f64`/`ptr` primitive into it writes only
8 bytes — the store's width is the value's width, not the alloca's width. The tag (offset 0)
ends up holding the raw value; the `data` field is stack garbage. Reloading as `anyTy_` then
passing to `__ry_any_*` dispatch routes on the garbage tag → silent wrong answer.

The `reduce` emitter (`src/codegen_call_higher_order.cpp`) hit this when its lambda had untyped
params (parser defaults them to `any`), making `info.returnType = anyTy_` while the first list
element is still raw `elemTy`. Loop-body `elem` values went through `coerceCallArgs` →
`wrapInAny` and were fine; only the accumulator **seed** skipped coercion.

**Rule**: Before `CreateStore(v, accSlot)` where `accSlot` has type `anyTy_`, coerce `v` via
`wrapInAny(v)` (or `coerceCallArgs`-style widening). Equivalently: never rely on a
`CreateStore` whose source type is narrower than the alloca's allocated type. Grep for
`CreateAlloca(info.returnType, ...)` / `CreateAlloca(anyTy_, ...)` followed by
`CreateStore(...)` — each such pair must guarantee the stored value matches the alloca's full
type.

**How to verify**: `tests/spec/collections.test.ry` has untyped-lambda cases for `reduce` on
int list (sum), 2/3-elem lists, and float list. These cases provide functional regression
coverage. For uninitialized-read detection specifically, use MemorySanitizer (MSan) rather
than ASan — ASan detects memory-access errors (buffer overflows, use-after-free) but does
not detect uninit reads.

### `fold()` rejects untyped lambda with type-check error (#1061)

**Source**: #1061 (2026-04-16, discovered during pre-verification for #1033; resolved in fix/1061-fold-untyped-lambda)
**Tags**: codegen, fold, untyped-lambda, type-check

**Context**: `fold(xs, 0, (a, b) => a + b)` produced a compile error:
`fold() initial value type must match function return type`. Unlike `reduce()` which was
patched in #1020 to accept untyped lambdas, `fold()` was not given the same treatment.
The type-check logic in `fold`'s emitter validates that the initial value type matches the
lambda's return type; when both params are untyped the lambda returns `any`, while `0` is
inferred as `int` — causing the mismatch.

**Fix** (`src/codegen_call_higher_order.cpp:287-290`): Insert `wrapInAny(initVal)` coercion
before the equality check, mirroring the reduce fix at L247-248:
```cpp
if (isAnyType(info.returnType) && !isAnyType(initVal->getType()))
    initVal = wrapInAny(initVal);
```
The equality check is preserved — it still rejects genuine typed-lambda mismatches such as
`fold([1,2,3], "hi", (a: int, b: int) => a + b)`.

**Rule**: When applying a fix for untyped-lambda compatibility to one higher-order function
(e.g., `reduce`), audit all sibling functions with similar emitters (`fold`, `scan`, etc.)
for the same gap. Do not assume a fix to one function automatically covers others.

**How to verify**: `tests/spec/collections.test.ry` — untyped-lambda fold cases (int seed,
float seed, string seed); `tests/test_codegen_fail.cpp::FoldTypedLambdaSeedTypeMismatch`
verifies the preserved rejection branch.

### Lambda / function return paths need bidirectional `any ↔ narrow` coercion (#1062)

**Source**: #1062 (2026-04-17, fixed in fix/1062-lambda-return-type-annot)
**Tags**: codegen, lambda, return-type-annotation, any-type, asymmetric-coercion, type-coercion

**Context**: `reduce([1,2,3,4,5], (a, b) -> int => a + b)` produced:
`lambda expression return type mismatch: expected 'int', found 'any'`. When lambda params
are untyped they default to `any`, so `a + b` is evaluated via `emitAnyBinaryOp` and returns
`any`. The `-> int` annotation resolves to `i64Ty_`. Only `any ← narrow` (via `wrapInAny`)
was handled; `narrow ← any` (`unwrapFromAny`) was missing in both return codegen sites.

**Fix**: Added inverse coercion guard at two sites, symmetric with the existing `wrapInAny` branch:
```cpp
// src/codegen_lambda.cpp (expression-body) and src/codegen_fn.cpp (block-body + regular fn return)
else if (isAnyType(val->getType()) && !isAnyType(retTy) && canAnyHoldType(retTy))
    val = unwrapFromAny(val, retTy);
```
`unwrapFromAny` emits a runtime tag check; type mismatches (e.g., float list with `-> int`)
become runtime errors, not compile errors. `canAnyHoldType` limits the guard to primitives
(`i64/f64/i1/ptr`); non-primitive return types (List, Result, Map) still produce a compile error.

**Rule**: Whenever you add a `wrapInAny` (narrow→any) branch at a return site, audit the
same site for the inverse `unwrapFromAny` (any→narrow) case. Both directions are required.
Omitting either direction causes asymmetric behavior between annotated and unannotated lambdas.

**How to verify**: `tests/spec/collections.test.ry` — `reduce`/`fold` tests with `-> int`
and `-> float` annotations on untyped params; block-body lambda variant via lambda variable.

### Seed-less reductions over a possibly-empty list must return `Option<T>`, not panic (#1209)

**Source**: #1209 (2026-04-20)
**Tags**: codegen, reduce, fold, higher-order, option, empty-list, api-design

**Context**: `reduce(list, fn)` (no seed) used to runtime-error on an empty list
(`runtime error: reduce() on empty list`). Python/JS users also reached for a
3-argument form `reduce(xs, init, fn)` and hit `reduce() takes exactly 2 arguments`
with no pointer to `fold`. Both pain points share a root cause: `reduce` and `fold`
are distinct functions with different safety profiles, but the API does not signal
the distinction.

**Rule**: Any reduction that does **not** take a seed (e.g., `reduce`, `min`, `max`)
must return `Option<T>` for empty inputs, **not** panic. Reductions that take a
seed (`fold`) are exempt — the seed makes the empty-list case naturally well-defined
and a plain `T` return is the right choice. Model the Option path on
`first`/`last` at `src/codegen_call.cpp:220-290`: empty-check → empty BB with
`buildNoneValue` → ok BB with reduction loop + `buildSomeValue` → PHI at merge.

**Rule**: When two near-identical builtins differ only in a parameter (e.g.,
`reduce(list, fn)` vs `fold(list, init, fn)`), detect the Python/JS-style miscall
`reduce(list, init, fn)` at the specific arg count and emit a targeted compile
error that names the sibling. Ad-hoc hint strings in the dispatch branch are the
existing precedent (see `src/codegen_call_user.cpp:17`); there is no "did you
mean" infrastructure to reuse. Scope the hint to exactly the wrong count that
maps onto the sibling — other wrong arities (0, 4+) should keep the generic
message.

**Sibling audit**: Although the audit rule at the prior `fold()` untyped-lambda
entry says "when fixing one higher-order function, audit siblings", the `fold`
empty-list case stays unchanged here because `fold` is inherently empty-safe via
its seed. Record the **reason** for asymmetry when it exists, so future
contributors do not conclude the asymmetry is an oversight.

**ARC gap (follow-up candidate)**: `buildSomeValue` retains the inner value only
when `inner->getType() == ptrTy_` AND the source pointer is registered as
ARC-managed. In `reduce`, the final accumulator is a `CreateLoad` from a local
alloca that is **not** registered as ARC-managed, so a `reduce` over a
`List<str>` where the lambda produces a string does not retain the result inside
the `Some`. Returning the `Option<str>` to a long-lived binding after the input
list is dropped can race with its refcount. Existing tests do not exercise this;
file a follow-up issue if exposing the gap.

**How to verify**: `tests/test_codegen_collection.cpp::ReduceEmptyListReturnsNone`,
`tests/test_codegen_collection.cpp::ReduceEmptyListUnwrapDefault`,
`tests/test_codegen_fail.cpp::ReduceThreeArgsSuggestsFold`,
`tests/test_codegen_fail.cpp::ReduceFourArgsUsesGenericError`,
`tests/spec/collections.test.ry` "should return None for reduce of empty list".

### Skill `allowed-tools` must cover all Bash commands the skill body prescribes

**Source**: #1045 (2026-04-16, CodeRabbit review)
**Tags**: skill, allowed-tools, claude-code, ci-investigate, review-feedback

**Rule**: Every Bash command that a SKILL.md step instructs the agent to run must be covered by an entry in `allowed-tools`. A common pitfall is listing only `gh pr:*`/`gh run:*`/`git branch:*` while the skill body also calls `cmake`, `clang-tidy`, `cppcheck`, `scan-build`, `find`, etc. At runtime the agent will be blocked from running those uncovered commands, silently breaking the step.

When the reproduction command set is open-ended (e.g. "run the CI job's corresponding local command"), use `Bash` (unrestricted) rather than a long enumeration of prefixes that will grow stale.

**How to verify**: grep the skill body for bare Bash commands not covered by the `allowed-tools` line.

**Note**: `.codex/skills/` mirrors intentionally omit the `allowed-tools` field — Codex CLI does not support this frontmatter field. Only `.claude/skills/` SKILL.md files use `allowed-tools`. Do not add `allowed-tools` to `.codex/` skill files.

### Quote `.codex/skills` frontmatter values when they contain `: `

**Source**: #1284 (2026-04-21, implementation)
**Tags**: skill, yaml, frontmatter, codex

**Rule**: In `.codex/skills/*/SKILL.md`, quote any frontmatter scalar that contains `: `, especially long `description` strings. Unquoted YAML plain scalars treat `: ` as a mapping boundary and can make the whole skill unloadable with `mapping values are not allowed in this context`.

**How to verify**: Parse the frontmatter as YAML or reload the skill list and confirm the skill is no longer skipped.

### `gh run list --branch` returns all runs on a branch, not just the PR head commit

**Source**: #1045 (2026-04-16, CodeRabbit review)
**Tags**: github-actions, gh-cli, ci-investigate, review-feedback, gotcha

**Rule**: `gh run list --branch <name>` includes runs from every commit on that branch. In a CI investigation or re-run tool, this causes reruns and log analysis for commits unrelated to the PR being investigated.

Always filter by the PR's `headRefOid` (head commit SHA) immediately after the `gh run list` call:

```bash
gh run list --branch <headRefName> --limit 20 \
  --json databaseId,headSha,name,status,conclusion,workflowName \
  | jq --arg sha "<headRefOid>" '[.[] | select(.headSha == $sha)]'
```

Alternatively, derive run IDs directly from `detailsUrl` in the `gh pr checks` output (`grep -oE '/runs/([0-9]+)' | grep -oE '[0-9]+'`).

### `inferExprType` / `inferExprTypeName` visitor must handle `IfExpr`/`IfBlockExpr` and ADT constructors (`Ok`/`Err`/`Some`/`None`/`Error`) to infer lambda return type correctly

**Source**: #1024 (2026-04-16, bugfix — lambda if-expr Result branch unification); #1043 (2026-04-17, bugfix — Option analog); #1111 (2026-04-17, bugfix — Error constructor + `anyTy_` IfExpr merge); #1115 (2026-04-18, bugfix — Option branch merge `preferConcrete` + `Some` emitter `anyTy_` unwrap); #1116 (2026-04-18, bugfix — Err emitter analog of #1111 Ok fix)
**Tags**: codegen, inference, visitor, lambda, Result, Option, IfExpr, anyTy_, Error

**Rule**: `inferExprType` and `inferExprTypeName` in `src/codegen_lambda.cpp` are the visitor functions that determine the return type of an expression-body lambda (`(x: int) => expr`). Both functions must have explicit `if constexpr` cases for every AST node that can appear as the outermost expression. Any unhandled node falls through to the default which returns `i64Ty_` (= `int`) — silently and without error.

Confirmed gaps (all fixed):
1. **`IfExpr` / `IfBlockExpr`** — neither visitor had a case for these nodes, so any lambda whose body is an `if` expression with mismatched-type branches (e.g., `Ok(T)` vs `Err(E)`) fell through to `int`, baking the wrong return type into the function signature before codegen. (#1024)
2. **`Ok` / `Err` in CallExpr** — the CallExpr branch had `Some` but not `Ok`/`Err`. Without these, the Ok/Err payload types could not be inferred, making the IfExpr merge logic unreachable even after adding it. (#1024)
3. **`Error` constructor in CallExpr** — `Err(Error("msg"))` must infer as `getResultType(i8Ty_, errorTy_)`. Without an explicit `if (c == "Error") return errorTy_` case, it falls through to `i64Ty_`, making the inferred branch type `{i1, i8, i64}` instead of `{i1, i8, errorTy_}`. The IfExpr merge then produces the wrong StructType pointer, and `validateBranchTypes` rejects the mismatch. (#1111)

**Result merge logic (IfExpr)**: When both branches of an `if` expression yield `isResultType`, the inferred types are `{i1, realOkTy, Unit}` and `{i1, Unit, realErrTy}` (each side uses `i8Ty_` as the placeholder for the missing payload). Also, unannotated lambda params yield `anyTy_` — which must lose to a concrete type but win over `i8Ty_`. Use a `preferConcrete` merge:
```cpp
// Priority: concrete > anyTy_ (unannotated param) > i8Ty_ (absent-payload placeholder)
auto preferConcrete = [&](llvm::Type *a, llvm::Type *b) -> llvm::Type * {
    if (a == i8Ty_) return b;
    if (!isAnyType(a)) return a;
    return (b != i8Ty_) ? b : a; // a=any: prefer concrete b, else keep any
};
return getResultType(preferConcrete(okA, okB), preferConcrete(erA, erB));
```
The old `(a == i8Ty_) ? b : a` rule was broken for `anyTy_`: `(any, i8) → i8` (wrong) instead of `any`. (#1111)

**`Ok` / `Err` emitters must unwrap `anyTy_` to the expected slot type**: When `fn_->getReturnType()` indicates a concrete ok/err type (e.g., `i64`) but `Ok(x)` / `Err(x)` emits with `x` of type `anyTy_` (unannotated param), the emitted Result struct contains `anyTy_` for that slot — different from the concrete type produced by the sibling branch. `validateBranchTypes` pointer-equality fails. Fix: in both the `Ok` and `Err` emitters (`codegen_call.cpp`), after deriving the expected slot type from `retStructTy->getElementType(1/2)`, call `unwrapFromAny(inner, expectedSlotTy)` when `isAnyType(inner->getType()) && canAnyHoldType(expectedSlotTy) && expectedSlotTy != i8Ty_`. `canAnyHoldType(errorTy_) = false`, so `Err(Error(...))` is unaffected — the gap is limited to `Result<T, primitive>` (int/float/bool/str) Err slots. (#1111 for Ok; #1116 for Err)

**Option merge logic (IfExpr — fixed in #1043, updated in #1115)**: Option layout is 2-slot `{i1, innerTy}` vs Result's 3-slot. Three gaps existed beyond the structural #1024 analog:
1. `Some` was missing from `inferExprType` (it was only in `inferExprTypeName`), causing `(x) => Some(x)` to fail.
2. `None()` (zero-arg CallExpr) had no emitter anywhere — it fell through to `findFunction("None")` and failed with `undefined function: None`.
3. `None` in `inferExprType` also needed a placeholder: `getOptionType(i8Ty_)`.

Option merge must use the same `preferConcrete` rule as Result (#1115 — the old `(a == i8Ty_) ? b : a` was broken for `anyTy_`):
```cpp
if (isOptionType(thenTy) && isOptionType(elseTy)) {
    llvm::Type *innerA = thenSt->getElementType(1);
    llvm::Type *innerB = elseSt->getElementType(1);
    // Priority: concrete > anyTy_ (unannotated param) > i8Ty_ (None placeholder)
    auto preferConcrete = [&](llvm::Type *a, llvm::Type *b) -> llvm::Type * {
        if (a == i8Ty_) return b;
        if (!isAnyType(a)) return a;
        return (b != i8Ty_) ? b : a;
    };
    return getOptionType(preferConcrete(innerA, innerB));
}
```
`None()` emitter in `codegen_call.cpp` reads `fn_->getReturnType()` to derive inner (like `Err` does for the Ok slot), so the emitted struct matches the inferred return type and `validateBranchTypes` pointer-equality succeeds.

**`Some` emitter must also unwrap `anyTy_` (mirrors `Ok` emitter, #1115)**: After the `preferConcrete` lambda-inference fix sets the function return type to `Option<int>`, the `Some(x)` call with unannotated `x` still emits `anyTy_`. Without an explicit unwrap, the produced struct is `{i1, anyTy_}` while `Some(0)` produces `{i1, int}` — `validateBranchTypes` pointer-equality fails. Fix: in the `Some` emitter (`codegen_call.cpp`), derive `expectedInnerTy` from `retStructTy->getElementType(1)`, then call `unwrapFromAny(inner, expectedInnerTy)` when `isAnyType(inner->getType()) && canAnyHoldType(expectedInnerTy)`. Both fixes must be bundled: `preferConcrete` alone converts a silent wrong value into a compile error without fixing correctness.

**Parser trap — block-form branches with `Some(...)`/`None()` tail**: In an `IfBlockExpr` (colon-indent body), a bare `Some(x)` or `None()` on the last line is parsed as a `CallStmt` (discarded statement), not as the block's return value. Wrap it in parentheses to produce an `ExprStmt` that is recognised as the tail expression:
```ry
f = (x: int) => if x > 0:
  (Some(x * 2))   # ← parens required; bare Some(x * 2) would be a discarded CallStmt
else:
  (None())
```
This is the same rule as the `IfBlockExpr` path tested in `tests/spec/option_lambda_if.test.ry`.

**How to check for new gaps**: When adding a new ADT constructor or a new expression-level control-flow node, grep for both `inferExprType` and `inferExprTypeName` and verify each has a case for the new node. A missing case is a silent wrong-type inference, not a compile error.

### `isNoneLiteral` must match all three None syntactic forms: `None`, `none`, `None()`

**Source**: #1099 (2026-04-17, bugfix — `None()` in let-decl and reassignment contexts)
**Tags**: codegen, Option, None, isNoneLiteral, let-decl, reassignment, emitVarDecl, CallExpr

**Rule**: `None`, `none`, and `None()` are semantically identical Option literals. `isNoneLiteral` in `src/codegen_type.cpp` must recognise all three:
- `NoneExpr` — the keyword `none`
- `VariableExpr{name:"None"}` — the bareword `None`
- `std::unique_ptr<CallExpr>{callee:"None", args.empty()}` — the call form `None()`

`None()` was introduced in #1043 for lambda if-expr unification. Its emitter in `codegen_call.cpp` uses `fn_->getReturnType()` to derive the inner type — correct for return-statement contexts, but wrong in let-decl and reassignment where the enclosing function is unrelated to the variable's annotation. Three sites that use `isNoneLiteral` to short-circuit into annotation-aware `buildNoneValue(annotTy)` were therefore bypassed:
- `src/codegen_stmt.cpp` `emitVarDecl` (line ≈204) — let-decl short-circuit (auto-fires after fix)
- `src/codegen_stmt.cpp` local reassignment (line ≈845) — previously had a raw `VariableExpr{"None"}` check
- `src/codegen_stmt.cpp` module-global reassignment (line ≈1020) — same raw check

**How to apply**: When adding a new syntactic form of `None` to the language, extend `isNoneLiteral` first. Then all three sites above automatically inherit the fix. The `None()` emitter in `codegen_call.cpp` is a separate fallback for contexts where no annotation is available (e.g., return-stmt); it is not a substitute for `isNoneLiteral`. Use `std::get_if<std::unique_ptr<CallExpr>>` (not `std::get_if<CallExpr>`) because `CallExpr` is stored as `unique_ptr` in the `ExprNode` variant.

### Hint channel for `None()`/`none` inner type in branch-merge and lambda-call argument positions

**Source**: #1154 (2026-04-18, bugfix — `None()` in if/case branch defaults to `Option<i8>`), #1179 (2026-04-19, bugfix — `None()` in lambda call arg defaults to wrong inner type), #1186 (2026-04-19, bugfix — `None()` in record/struct constructor field position defaults to wrong inner type)
**Tags**: codegen, Option, None, NoneExpr, branch-merge, IfExpr, CaseExpr, hint-channel, RAII, lambda-call, record-constructor

**Rule**: `None()` and `none` in branch-merge positions (e.g., `if cond => None() else Some(42)`) previously defaulted to `Option<i8>` or `Option<i64>` because the emitters only consulted `fn_->getReturnType()`. The fix introduces a two-field class-state hint channel:

- **`option_none_hint_inner_: llvm::Type*`** — arm hint, read directly by `None()` and `NoneExpr` emitters (nullptr = no hint).
- **`option_decl_annotation_inner_: llvm::Type*`** — declaration-context annotation inner, written by `emitVarDecl`/local-reassign/global-reassign via `DeclAnnotationInnerGuard`. **NOT read by None()/NoneExpr emitters directly** — only used as fallback in IfExpr/IfBlockExpr guards so it cannot leak into arbitrary sub-expressions (e.g., function arguments such as `foo(none)`).
- `OptionNoneHintGuard` RAII saves/restores `option_none_hint_inner_`; `DeclAnnotationInnerGuard` RAII saves/restores `option_decl_annotation_inner_`.
- `computeBranchOptionInnerHint(a, b)` in `codegen_lambda.cpp` runs `inferExprType` on both arms and picks the more-concrete inner via `preferConcrete`.
- **Annotation seed** at three `codegen_stmt.cpp` sites: `emitVarDecl`, local reassign, module-global reassign — writes `option_decl_annotation_inner_` (not `option_none_hint_inner_`) from the declared `Option<T>` inner before `emitExpr(RHS)`.
- **IfExpr / IfBlockExpr pre-scan** in `codegen_expr.cpp`: computes hint **before** condition emit, installs `OptionNoneHintGuard` **after** condition emit. The fallback for all-None arms uses `option_decl_annotation_inner_`. **Critical**: the guard must be installed after the condition because `none`/`None()` in the condition expression must not inherit the arm hint.
- **`NoneExpr` added to `inferExprType` and `inferExprTypeName`** in `codegen_lambda.cpp`: `inferExprType` returns `getOptionType(i8Ty_)` so `computeBranchOptionInnerHint` detects it as an Option placeholder; `inferExprTypeName` returns `"Option"`.
- **CaseExpr (pattern match) uses scrutinee-based hint** in `codegen_match.cpp::emitExprVariant(CaseExpr)`: `Some(v)` arm values reference pattern-bound variables not in scope at pre-scan time, so `computeBranchOptionInnerHint` cannot be applied. Instead, the scrutinee type (`subjectTy`) is used — if it is `Option<T>`, its inner `T` is extracted and installed as the `OptionNoneHintGuard` hint before any arm is emitted. Fallback is `option_decl_annotation_inner_`.
- **CaseCondExpr (`when cond => value`) uses pre-scan** in `codegen_expr.cpp::emitExprVariant(CaseCondExpr)`: no pattern variables are involved, so `computeBranchOptionInnerHint` on the first arm value and `else_expr` is safe. Guard is installed inside each value emit block (after the arm condition) to prevent hint leaking into condition expressions.
- **Lambda variable (indirect) call arguments** in `codegen_call_dispatch.cpp` (#1179): per-arg `OptionNoneHintGuard` with inner derived from the callee's `FnTypeInfo::paramTypes[i]` — only when that param is `isOptionType`. The hint source is the **callee signature** (not `option_decl_annotation_inner_`), so the invariant "decl annotation must not flow into arg positions" is preserved — the two channels remain disjoint by design. Non-Option params pass `nullptr` as inner (guard saves/restores nullptr, which is also a correct "clear" for nested contexts such as a call inside a branch-merge).
- **Record/struct constructor field positions** in `codegen_expr_literal.cpp::emitRecordConstructor` (#1186): per-field `OptionNoneHintGuard` with inner derived from `info.llvmType->getElementType(i)` — only when that field is `isOptionType`. The hint source is the **field's declared LLVM struct element type**, which covers inherited fields via `allFields` pre-flattening (parent fields occupy lower indices). Non-Option fields pass `nullptr` as inner (correctly clears any outer hint when emitting e.g. `Outer(Some(UserWithAvatar("carol", None())))` so the nested `None()` inherits `UserWithAvatar.avatar`'s `str` rather than `Outer.inner`'s `UserWithAvatar`).

**Design invariant**: `option_decl_annotation_inner_` does NOT flow directly into argument positions. Argument-position hints always come from the callee's declared parameter types (lambda variable call), the record's declared field types (record constructor), or are absent (regular `fn` calls, which use `resolveOverload`'s `isNoneLiteral` short-circuit). This prevents LHS declaration annotations from contaminating arbitrary sub-expressions.

**Priority invariant**: `isNoneLiteral` short-circuit at the three annotation-aware `codegen_stmt.cpp` sites must remain the *first* check (runs before the hint seed). It handles the trivial `x: Option<int> = None()` case without touching hint state and is faster.

**ADR #1111 compliance**: The hint is threaded via CodeGen class state + RAII guards, not via `emitExpr` signature changes.

### UnaryExpr fast-path covers bare int for INT64_MIN (`-9223372036854775808`)

**Source**: #1025 (2026-04-16)
**Tags**: codegen, numeric-literal, unary-minus, int64-min, ubsan

**Rule**: The `-<NumberExpr>` fast-path in `src/codegen_expr.cpp::emitExprVariant(UnaryExpr)`
accepts bare int (empty suffix) as equivalent to `i64` for the magnitude check
(`|INT64_MIN|` = 2^63). A standalone `9223372036854775808` without the unary minus must
still be rejected by the bare-`NumberExpr` path (`value < 0` check).

Magnitude > 2^63 on the bare path produces `"integer literal out of range for int: -<mag>"`
(matching the existing bare-int error wording); the signed-suffix path keeps the
`"<suffix> literal out of range: -<mag>"` wording.

**UB fix**: The original computation `static_cast<uint64_t>(-static_cast<int64_t>(mag))`
is signed-integer UB when `mag == 2^63` (negation of INT64_MIN overflows). Use
unsigned two's-complement arithmetic instead:
```cpp
const uint64_t negBits = static_cast<uint64_t>(0) - mag;  // well-defined, same bits
```
This also applies to the signed-suffix path — add `-9223372036854775808i64` to
`SignedSuffixMinAcceptedViaUnaryMinus` to keep it covered.

**How to apply**: If you touch this fast-path, keep the bare-int branch in sync with the
signed-low-level-suffix branch. Always use unsigned subtraction for `negBits`; never
negate a `uint64_t` value via `static_cast<int64_t>` when the magnitude may equal 2^(N-1).

### Collection element-access emitters must apply the canonical any-widening pattern

**Source**: #1029 (2026-04-16) / #1065 (2026-04-16)
**Tags**: codegen, any, widening, collections, Map, List, Set

**Rule**: Every site that reads or writes a value from/into a collection element slot must apply
the canonical 3-branch widening pattern:

Write sites (`Map` index-assign, `List` index-assign, `List.append!`, `List.appended`,
`List.insert`, `Set.add`):

```cpp
if (val->getType() != elemTy) {
    if (isAnyType(elemTy))
        val = wrapInAny(val);           // concrete → any
    else if (isAnyType(val->getType()) && canAnyHoldType(elemTy))
        val = unwrapFromAny(val, elemTy); // any → concrete (runtime-checked)
    else
        codegenError("... type mismatch");
}
```

Read sites (`in` / `not in` operator — Set, Map, List membership checks in
`src/codegen_expr.cpp`): same 3-branch pattern on the LHS element before calling
`emitSetElementLookup` / `emitMapKeyLookup` or entering the List linear search loop.

For `List` index-assign, the existing `tryEmitSubtypeCoerce` check must remain first.

**Additional rule for inline linear search loops with `anyTy_` elements**: The List `in`
operator uses an inline loop with a per-type comparison cascade. After widening, if
`listElemTy` is `anyTy_`, the `icmp eq` / `fcmp oeq` path is invalid (LLVM rejects `icmp eq`
on `{i64, [8 x i8]}` struct values — see entry `Set<any> element comparison requires
emitAnyBinaryOp`, below). Hoist scratch allocas for `__ry_any_eq` **outside** the loop
(mirroring `emitSetElementLookup` at `src/codegen_builtin.cpp:387-436` and
`emitMapKeyLookup` at `src/codegen_builtin.cpp:440-508`). Do **not** call `emitAnyBinaryOp`
inside the loop — it creates two allocas per call and does not hoist.

**Why**: Without widening, `"x" in s` on `Set<any>` (and the equivalent List / Map cases)
fails with "type mismatch" even though `any` accepts implicit conversion from all primitives.

**Files**: `src/codegen_stmt_misc.cpp` (map + list index-assign),
`src/codegen_call_collection.cpp` (add, append, appended, insert), and
`src/codegen_expr.cpp` (`in` / `not in` Set / Map / List branches).
When adding a new collection mutation or membership emitter, apply this pattern immediately.

### `Set<any>` element comparison requires `emitAnyBinaryOp`, not `emitComparisonOp`

**Source**: #1029 (2026-04-16)
**Tags**: codegen, any, Set, emitSetElementLookup, LLVM, icmp

**Rule**: `anyTy_` is a 16-byte struct `{i64 tag, [8 x i8] data}`. The `[8 x i8]`
array field cannot be used as an operand to `icmp eq` — LLVM IR verification rejects it.

In `emitSetElementLookup` (`src/codegen_builtin.cpp`), when `isAnyType(elemTy)`,
replace `emitComparisonOp("==", elem, cand, "", "")` with
`emitAnyBinaryOp("==", elem, cand)`, which calls `__ry_any_eq` at runtime.

The linear-scan path is taken automatically for `anyTy_` because it is a `StructType`
(`llvm::isa<llvm::StructType>(elemTy)` is true), so no additional predicate is needed.

---

### `low_level_type_name` mix check: convert empty metadata to "int" before comparing

**Source**: PR #1063 (CodeRabbit review). **Tags**: codegen, type-metadata, checked-arithmetic, mix-check

The three-way guard `!lhsName.empty() && !rhsName.empty() && lhsName != rhsName` is
insufficient when one side is bare `int` (empty metadata) and the other is a named low-level
type (e.g. `"i64"`): both conditions involving emptiness are false, so the mix silently passes.

**Why**: `getLowLevelTypeName` returns `""` for `int` (no `low_level_type_name` metadata). The
three-way check only fires when *both* names are non-empty.

**How to apply**: Map empty metadata to a display sentinel before comparing:

```cpp
const std::string &lhsLL = getLowLevelTypeName(lhs);
const std::string &rhsLL = getLowLevelTypeName(rhs);
const std::string lhsName = lhsLL.empty() ? "int" : lhsLL;
const std::string rhsName = rhsLL.empty() ? "int" : rhsLL;
if (lhsName != rhsName)
    codegenError(callee + "() cannot mix " + lhsName + " and " + rhsName);
```

Two bare `int` args (`"int" == "int"`) correctly pass; `int`+`i64` (`"int" != "i64"`) is caught.

**Corollary — testing int vs. low-level mix**: A bare literal `1` alongside `1i64` may be
coerced to `i64` by type inference, so `checked_add(1, 1i64)` appears to succeed. Use typed
variables (`a = 1; b: i64 = 2i64`) to force the mix in tests.

### `expect(str).to_eq("literal")` is NUL-truncating — use `expect(str == "literal").to_eq(true)` for NUL-containing strings

**Source**: PR #1048 and #1049 (CodeRabbit review). **Tags**: testing, NUL-safety, codegen_test

`to_eq` for string values emits a `strcmp` call (`codegen_test.cpp:784` via the `isStringValue` branch).
`strcmp` stops at the first `\0`, so `expect(substring("a\0b", 0, 3)).to_eq("a\0b")` passes even
when `substring` returns `"a"` — both C-strings compare equal as `""` / `"a"` depending on content.

**Why**: The `==` operator between two `str` values routes through `emitComparisonOp`
(`codegen_expr.cpp:1016`) → `__ry_str_cmp` (byte_len + memcmp), which is NUL-safe.
The `to_eq` matcher is a separate code path that does not reuse that logic.

**How to apply**: When the expected value contains an embedded `\0`, write the assertion as:
```ry
expect(expr == "a\0b").to_eq(true)   # NUL-safe: routes through __ry_str_cmp
# NOT:
expect(expr).to_eq("a\0b")           # NUL-truncating: strcmp stops at \0
```
Assertions whose expected value has no embedded NUL are safe to leave as `to_eq("literal")`.
Only `to_have_length` and `to_be_empty` are NUL-safe matchers besides `to_eq(bool)`.

### Byte-list APIs require `TypeMeta::ListElem == i8Ty_`; plain int list literals are rejected at compile time

**Source**: PR #1055. **Tags**: codegen, runtime, IOListHeader, ListHeader, bytes_to_str, write_bytes, send

**Rule**: Any native function that casts a `List` argument to `IOListHeader *` must set `NativeDispatchEntry::requireListU8Arg` to the 0-based index of that argument (or stamp `ListElemMeta::I8` for producers). Codegen enforces the gate via `CodeGen::emitTableDrivenNativeCall`. Do not add a new byte-list consumer without updating the audit table above and adding this field.

`IOListHeader` (`include/ry/runtime_io.hpp`) reads `data` with 1-byte stride (`int8_t *`).
Plain Ry list literals like `[97, 0, 98]` default element type to `int`, which codegen stores
with 8-byte (`i64`) stride. Passing such a list to a byte-list consumer produces silent memory
corruption (reads 8× too much data, interprets padding as bytes).

**Why rejected at compile time**: `IOListHeader` and `ListHeader` share the same LLVM struct
shape (`{i64 len, i64 cap, ptr data}`), so no runtime flag can distinguish them without an ABI
break. A compile-time gate is the only safe option.

**Four byte-list consumers** (all gated to require `TypeMeta::ListElem == i8Ty_`):

| Consumer | Gate location |
|---|---|
| `bytes_to_str` | `src/codegen_call_io.cpp` table, `requireListU8Arg = 0` |
| `write_bytes` | `src/codegen_call_io.cpp` table, `requireListU8Arg = 1` |
| `tcp_send` / `tls_send` | `src/codegen_call.cpp:577-595` (inline guard) |

**Four byte-list producers** (all stamp `TypeMeta::ListElem = i8Ty_`):

| Producer | Location |
|---|---|
| `to_bytes` | `src/codegen_call_io.cpp` table, `ListElemMeta::I8` |
| `read_bytes` | `src/codegen_call_io.cpp` table, `ListElemMeta::I8` |
| `tcp_receive` / `tls_receive` | `src/codegen_call.cpp:616` |
| HTTP `body_bytes` | `src/codegen_call_io.cpp:337` |

**Gate mechanism**: `NativeDispatchEntry::requireListU8Arg` (field in
`include/ry/codegen.hpp`) holds the 0-based arg index to check; `-1` means no check.
Enforced in `CodeGen::emitTableDrivenNativeCall` (`src/codegen_call_native.cpp`).

**How to apply**: When adding a new byte-list consumer that casts its argument to
`IOListHeader *`, set `requireListU8Arg` in its table entry and add the function to the
audit table above. When adding a new byte-list producer, set `ListElemMeta::I8` in its
entry or call `setTypeMeta(TypeMeta::ListElem, result, i8Ty_)` post-call.

**Annotation-driven element suffix (#1079, #1085, #1102)**: `bs: List<u8> = [97, 0, 98]`,
`bs = [99, 100, 101]`, and `bs += [99]` all work. `injectLowLevelSuffix` is shallow — it
does not descend into `ListExpr` elements on its own. `emitVarDecl`, `AssignStmt` (plain `=`
and compound `+=`), and `emitModuleGlobalWriteThrough` (both variants) propagate `T` into
each element AST node before calling `emitExpr` via `injectListExprElemSuffixes`
(`include/ry/ast.hpp`). The fix must happen before `emitExpr`; post-emit metadata rescue
cannot repair the stride because `getListElementType(val)` returns i64 (truthy) and the
annotation-fallback branch is gated on `!elemTy`. AssignStmt recovers the element type name
from `ValueMetadata::list_elem_type_name` (now populated for low-level int types at
declaration time) to preserve source-level signedness ("i8" vs "u8"); `reverseResolveTypeName`
is the fallback but is lossy (always returns "u8" for `i8Ty_`). Compound-op branches also
require `propagateTypeMeta("List<" + elemTypeName + ">", currentVal)` after `CreateLoad` so
that `getListElementType(currentVal)` in `emitListConcat` returns the correct element type
(without this, the loaded SSA value has no ListElem metadata and dispatch falls through to
`emitArithmeticOp`, triggering "cannot mix types"). Scope: `let`/`var` declarations, plain
`=` reassignment, and compound ops (`+=`) — inline call-argument paths are not covered.

---

### `List<str>` / `Map<K,str>` / `Set<str>` destructor specialization (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, collection, destructor, List, Map, Set, memory-leak

**Rule**: `getOrCreateCollectionDestructor` uses a `(CollectionKind, elemSig, valSig)` tuple cache key, not `CollectionKind` alone. Caching by kind only would produce a single generic destructor that does not release str elements, causing leaks for any `List<str>`, `Map<K,str>`, or `Set<str>`.

**Why**: When `elemSig == "str"` (or `valSig == "str"` for Map values), the destructor must iterate the dense element array and call `emitArcRelease(emitStrGetHeaderFromData(elem))` for each element before freeing the buffer. The generic destructor (cache key with empty sigs) only frees the data buffer.

**How to apply**: `resolveCollectionDestructor(alloca)` reads `list_elem_type_name` / `map_key_type_name` / `map_value_type_name` from the alloca's `ValueMeta` and calls `resolveTypeAlias` before passing to `getOrCreateCollectionDestructor`. Always ensure the alloca carries the correct type name metadata before this call.

**Extension (#1108)**: The same requirement applies to `emitArcReleaseLoadedElement`. When an ARC-managed element (e.g. `List<str>`) is stored in a slot (`List<List<str>>` element, `Map<K, List<str>>` value, record field), overwriting the slot must call `getOrCreateCollectionDestructor` with the **inner** elem/val signatures derived from the element's type name, not the generic (empty-sig) version. Fix: `emitArcReleaseLoadedElement` now accepts `elemTypeName` and calls `resolveTypeAlias` + `splitGenericTypeName` internally to derive `innerElemSig`/`innerValSig`. Callers must pass the declared element type name; passing `""` falls back to the generic destructor (leaks inner str). Rule: any new ARC release helper that calls `getOrCreateCollectionDestructor` must pass the full element type name, not just the `CollectionKind`.

---

### CoW copy of `List<str>` must retain str elements (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, CoW, copy-on-write, double-free

**Rule**: When CoW-copying a `List<str>` (or `Map<K,str>`, `Set<str>`), str elements **must** be retained immediately after the copy, regardless of whether the `retainElements` parameter is `true`. In `emitCowCheckSlot`, `doElemRetain` is forced `true` when `elemArcKind == CollectionKind::Str`.

**Why**: Before #1046 the `List<str>` destructor did not release str elements (only freed the buffer), so no retention was needed during CoW. After #1046 the destructor releases elements — without retention the two lists (old + CoW copy) both try to release the same str elements, causing double-free.

**How to apply**: In `emitCowRetainArcElements`, pass `elemArcKind` and use `emitStrGetHeaderFromData(elem)` (offset −24) when `elemArcKind == CollectionKind::Str`, otherwise `emitArcGetHeaderFromData(elem)` (offset −16). The wrong offset silently corrupts `weak_count` or `byte_len` instead of `strong_count`.

---

### `memcpy` of an element buffer must be paired with ARC retain (#1204)

**Source**: #1204 (2026-04-19)
**Tags**: codegen, arc, collections, slice, memcpy, retain, memory-safety

**Rule**: Any codegen path that duplicates a container's element buffer with `memcpy` — `emitListSlice`, `emitCollOp_take_impl`, `emitListConcat`, future helpers — must follow the `memcpy` with a retain loop when the source element type is ARC-managed (`List<str>`, `List<List<T>>`, `List<Map<K,V>>`, closures, …). The canonical pattern is:

```cpp
CollectionKind elemArcKind = CollectionKind::List;
if (elementTypeIsArcManaged(srcListPtr, CollectionKind::List, &elemArcKind)) {
    emitCowRetainArcElements(newData, count, "<tag>", elemArcKind);
}
```

**Why**: `memcpy` copies raw pointers — it does not bump the embedded ARC strong_count. When the source list is later released (scope exit, reassignment, destructor), its destructor walks the element buffer and decrements each element's refcount. Without retention the new buffer's elements are freed out from under it, producing a dangling pointer the holder will eventually dereference (heap-use-after-free on subsequent read, double-free on subsequent release).

**How to apply**: `elementTypeIsArcManaged(containerPtr, kind, &outElemKind)` reads `list_elem_type_name` / `map_*_type_name` from the *source* container's metadata — make sure the source pointer (not the newly-allocated header) is passed. Scalar element types (`List<int>`, `List<f64>`) naturally take the false branch, so the scalar path is unchanged. `emitCowRetainArcElements` handles str offset (−24) vs collection offset (−16) internally based on `elemArcKind`, so the caller does not need to distinguish. `emitCowClone` in `src/codegen_arc_cow.cpp` is the reference implementation — it performs the same memcpy+retain sequence for the CoW path.

**Related**: #1046 (CoW str retain), #855 (slot overwrite release), #1184 (slice helper extracted from `emitCollOp_slice`), #1235 (fixed for `emitCollOp_take_impl`), #1236 (same defect in `emitListConcat`).

---

### `List<str>` literals can have empty `list_elem_type_name` — UAF tests on retain paths are silently neutralized

**Source**: #1235 (2026-04-20, observation during ARC retain fix)
**Tags**: codegen, arc, metadata, list_elem_type_name, testing, symmetric-omission

**Rule**: A `List<str>` constructed purely from a list literal (e.g. `xs: List<str> = ["a" + "b", "c" + "d"]`) typically has `list_elem_type_name = ""` rather than `"str"`. Tests that assign `xs = ["dropped"]` after a `take` / `slice` / `concat` and then read the returned list expect to exercise the retain path — but on `List<str>`, the *release* path of the source also skips str destruction (because the destructor only emits `emitStrElemLoop` when `elemSig == "str"`). Retain omission + release omission = leak, not UAF, so the test passes whether or not the retain was implemented. Do NOT rely on `List<str>` UAF tests alone to prove a retain fix is wired up; add a `List<List<int>>` or `List<Map<K,V>>` case (where `list_elem_type_name` *is* populated — see `codegen_stmt.cpp` annotation branch at ~647) or inspect IR for the `cow_*_elem_loop` retain block.

**Why**: `codegen_expr_literal.cpp`'s list-literal path sets `list_elem_type_name = inferCollectionTypeName(vals[0])`, which returns `""` for string values. The annotation branch in `codegen_stmt.cpp:emitVarDecl` (~647-672) fills in `List<Map<…>>`, `List<Set<…>>`, `List<List<…>>`, `List<fn(…)>`, `List<Tuple<…>>`, `List<int>` etc., but has no arm that writes `"str"`. The only path that stamps `list_elem_type_name = "str"` is `emitStringToCharList` for `__ry_split_chars` in `codegen_builtin.cpp:225-234`. `elementTypeIsArcManaged` reads this field, so the retain is skipped for plain `List<str>`. Symmetrically, `getOrCreateCollectionDestructor` in `codegen_arc.cpp:843+` only emits str-element release when `elemSig == "str"`, so the element isn't released either — the heap string leaks (small) but no dangling pointer is produced.

**How to apply**: When writing UAF regression tests for collection-element retain paths, prefer `List<List<int>>` / `List<Map<str,int>>` / `List<fn(…) -> …>` over `List<str>`, or use both. When reviewing such a PR, verify the test discriminates — a test that passes before the fix indicates the test (or the surrounding metadata pipeline) is not catching what it claims to catch. Also note: #1204's own `list_range_index.test.ry` has the same `List<str>` blind spot — any future fix that addresses the missing `list_elem_type_name = "str"` annotation (possibly a follow-up issue) should re-verify #1204's tests then fail pre-fix.

**Related**: #1204 (slice retain fix — same test pattern, same blind spot), #1235 (take retain fix — discovered property), #1046 (str ARC dispatch via side-table, independent of `list_elem_type_name`).

---

### `arc_str_managed_vars_` side-table for str ARC dispatch (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, side-table, offset, emitStrGetHeaderFromData

**Rule**: Whenever an `AllocaInst` holds a `str` value that participates in ARC (i.e., was retained on assignment), insert it into `arc_str_managed_vars_`. This side-table is the canonical way to select `emitStrGetHeaderFromData` vs `emitArcGetHeaderFromData` at release time in `emitArcReleaseVar`.

**Why**: LLVM IR has no type-level distinction between a `str` handle (`ptr` at offset −24) and a collection handle (`ptr` at offset −16). Without the side-table, every release would use the wrong offset for one of the types, silently corrupting the ARC header.

**How to apply**: After inserting into `arc_managed_vars_` (via `markArcManaged`) for a str-typed alloca, also insert into `arc_str_managed_vars_`. Intermediary tmp allocas created in RecordPattern / EnumConstructorPattern must be added too, so that downstream VariablePattern bindings loaded from them inherit the correct offset. Do NOT add allocas to `arc_backed_vars_` (the CoW set) for str — CoW uses `emitArcGetHeaderFromData` offset and would corrupt the str header.

---

### `emitTableDrivenNativeCall` variadic path must use raw `ptr` type for `ResultPtr` entries

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: codegen, ResultPtr, variadic, emitTableDrivenNativeCall, nul-safety

**Rule**: In `src/codegen_call_native.cpp`, the fixed-arity path of `emitTableDrivenNativeCall`
already handled `ReturnWrapping::ResultPtr` (uses `ptrTy_` for the C ABI return, then wraps via
`wrapPtrAsResult`). The variadic-arity path (for functions like `path.join` with 2/3/4 overloads)
did **not** — it called `resolveType(matchedSig->returnTypeName)` to determine the C return type.
After upgrading `.ry` signatures to `Result<str, Error>`, `matchedSig->returnTypeName` becomes
`"Result<str, Error>"`, which `resolveType` maps to an LLVM struct type instead of `ptr` — causing
an ABI mismatch crash (the runtime still returns `char *`).

**Fix pattern**:
```cpp
// In the variadic path, before CreateCall:
llvm::Type *cRetTyVariadic = (entry->wrapping == ReturnWrapping::ResultPtr)
    ? ptrTy_
    : resolveType(matchedSig->returnTypeName);
auto *fnTy = llvm::FunctionType::get(cRetTyVariadic, argTypes, false);
// ... CreateCall, then:
if (entry->wrapping == ReturnWrapping::ResultPtr) {
    std::string errFn = getErrFnName();
    return wrapPtrAsResult(callResult, errFn.c_str());
}
return callResult;
```

**How to apply**: Any time a function with multiple overloads at different arities (variadic
dispatch path) is upgraded from bare `-> str` to `-> Result<str, Error>`, both the
`NativeDispatchEntry::wrapping` field AND the variadic code path in `emitTableDrivenNativeCall`
need updating. The fixed-arity path handles it automatically once `wrapping = ResultPtr` is set;
the variadic path requires explicit `ResultPtr` detection before `resolveType`.

### NUL-safety at C API boundaries: `stringByteLen`, `hasEmbeddedNul`, and propagating nullptr

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: nul-safety, c-api-boundary, stringByteLen, hasEmbeddedNul, Result

**Rule**: PR #1022 introduced `StringHeader` with an explicit `byte_len` field, allowing Ry `str`
to contain embedded NUL bytes. C library APIs (`realpath`, `stat`, `mkdir`, `getaddrinfo`, etc.)
assume NUL-terminated strings and silently truncate at the first `\0`. Apply the following three
patterns consistently when implementing or auditing runtime functions that pass Ry string handles
to POSIX/libc:

**1. Use `stringByteLen(handle)` instead of `strlen(handle)`**:
```cpp
// Wrong (silently truncates at \0):
size_t n = strlen(handle);
// Right:
size_t n = static_cast<size_t>(stringByteLen(handle));
```

**2. Validate with `hasEmbeddedNul` before passing to NUL-sensitive C APIs**:
```cpp
// Defined in include/ry/runtime_string.hpp:
inline bool hasEmbeddedNul(const char *handle) {
    if (!handle) return false;
    size_t n = static_cast<size_t>(stringByteLen(handle));
    return n > 0 && memchr(handle, '\0', n) != nullptr;
}

// Usage in runtime functions:
if (hasEmbeddedNul(p)) {
    setLastError("path.basename: argument contains an embedded NUL byte");
    return nullptr;  // caller wraps as Result<str, Error> via ResultPtr
}
```

**3. Propagate `nullptr` through chained join-style helpers**:
When a helper like `join2_impl(a, b)` returns nullptr on NUL, callers must guard and propagate:
```cpp
extern "C" const char *__ry_path_join3(const char *a, const char *b, const char *c) {
    char *ab = join2_impl(a, b);
    if (!ab) return nullptr;  // without this, join2_impl(nullptr, c) silently returns c
    char *result = join2_impl(ab, c);
    freeStringSlot(ab);
    return result;
}
```
Without the `if (!ab)` guard, `join2_impl(nullptr, c)` treats nullptr as empty string and returns
a copy of `c` — silently succeeding when it should propagate the error.

**How to apply**:
- Binary-safe consumers (HTTP body, multipart file values): use `stringByteLen` for length; do NOT
  reject NUL — these legitimately carry binary payloads.
- Text/path consumers (URLs, host names, header names, file paths): reject NUL with `hasEmbeddedNul`
  and return `Err` / nullptr.
- Upgrade bare `-> str` runtime functions that pass user strings to NUL-sensitive C APIs to
  `-> Result<str, Error>` (return nullptr + `setLastError`; set `ReturnWrapping::ResultPtr` in
  the dispatch table).
- **Audit scope**: PR #1054 covered libc-sensitive modules (path, net, http, http_client,
  filesystem) but not binary-safe stdlib (e.g. base64). When adding a new stdlib package, grep
  its runtime for `strlen(input)` and replace with `stringByteLen(input)` for any function that
  receives a Ry `str` handle. `base64` was fixed as a post-#1054 follow-up in #1129.

### Ry case arms: `()` unit expression and `if <var>:` as sole body cause parse failures

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: testing, case, parser, unit, if

**Rule**: Two patterns in case arm bodies trigger parser errors that manifest as a confusing
"unexpected token ')'" at the nearest enclosing `describe(..., ():` or `it(..., ():` line:

**Pattern 1: `()` as a case arm body**
```ry
# Fails — parser sees ')' and thinks it closes the outer describe/it closure:
case is_dir(d):
  Ok(v): remove_all(d)
  Err(_): ()         # <-- breaks parser

# Fix: use a real expression, or discard the conditional entirely:
remove_all(d)        # discard Result; Ok = removed, Err = didn't exist, both fine for setup
```

**Pattern 2: `if <bool>:` as the sole statement in an inline case arm body**
```ry
# Fails — single-arm if expression at end of inline case arm body breaks parser:
case is_dir(d):
  Ok(v): if v: remove_all(d)   # <-- breaks parser
  Err(_): 0

# Fix: use unconditional call (discard result) or move inside a multiline body with a trailing statement:
remove_all(d)          # simplest fix for setup guards
```

**Pattern 3: `Ok(true)` / `Ok(false)` literal patterns are NOT supported**
```ry
# Fails — nested literal matching inside constructor patterns:
case is_dir(d):
  Ok(true): remove_all(d)   # <-- parser error, not a type error
  _: ()

# Fix: bind to variable, then check:
case is_dir(d):
  Ok(v): expect(v).to_be_true()
  Err(e): fail("is_dir failed: " + e.message)
```

**How to apply**: In test files, always use `Err(e): fail(...)` (not `Err(_): ()`) for error arms.
For setup guards ("if dir exists, remove it"), prefer unconditional `remove_all(d)` — the returned
`Result<Unit, Error>` is discarded if unused. For `Result<bool, Error>` predicates, use
`Ok(v): expect(v).to_be_true()` / `Ok(v): expect(v).to_be_false()` patterns.

### `hasEmbeddedNul` / `stringByteLen` are only safe on Ry string handles — never on raw C strings

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: nul-safety, stringheader, c-api-boundary, runtime

**Rule**: Both `hasEmbeddedNul(handle)` and `stringByteLen(handle)` read a `StringHeader` struct
immediately *before* the pointer to obtain `byte_len`. This is only valid when `handle` points
into a Ry string slot (i.e., the pointer came from `makeString` or was read from a Ry `str` handle).

**It is undefined behaviour** (reads garbage memory) when applied to:
- C string literals: `hasEmbeddedNul("POST")` — no `StringHeader` prefix
- `std::string::c_str()` results — allocated by the STL without a `StringHeader`
- `strdup`/`malloc`-allocated strings (e.g. map keys built by C++ tests) — no `StringHeader` prefix
- Any other raw `const char *` that did not originate from a Ry string slot

**Symptoms**:
- `hasEmbeddedNul` false positive: reads random bytes as `byte_len`, `memchr` finds NUL → returns
  `true` unexpectedly, causing the runtime function to return nullptr and HTTP mock server tests
  to hang (server `::accept` waits for a client that never connects).
- `stringByteLen` garbage value: random bytes interpreted as body/data length → `Content-Length`
  header wildly wrong → HTTP server reads wrong number of bytes → test hangs.

**How to apply**: Only call `hasEmbeddedNul` inside codegen emitters (where arguments are known
Ry handles) or inside runtime functions whose callers pass exclusively Ry handles. If a runtime
fn is also called from C++ unit tests with `.c_str()` or with C literals, move the NUL
check to the codegen emitter.

### NUL checks belong in the codegen emitter, not the runtime, for multi-context functions

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: nul-safety, codegen, runtime, c-api-boundary

**Context**: `__ry_http_client_request(method, url, headers, body)` is invoked from three contexts:
1. User Ry code → codegen emitter → all arguments are Ry handles (StringHeader present)
2. C++ unit tests calling the function directly with `url.c_str()` or C string literals
3. Internal runtime functions (`__ry_http_post` calls it with the literal `"POST"`)

Applying `hasEmbeddedNul` inside the runtime covers case 1 but produces false positives for
cases 2 and 3 (reads garbage memory before the pointer).

**Rule**: If a runtime function is called from Ry codegen *and* from C++ or internal C code,
put NUL checks in the codegen emitter only. Leave the runtime function unchecked (treat it as
an internal C API). Add a comment in the runtime explaining the split:

```cpp
// NUL checks for method and url are done in codegen (emitHttpClientCall) for
// the user-facing Ry paths. This function is also called directly from C++
// tests and internally with plain C strings that carry no Ry StringHeader —
// applying hasEmbeddedNul here would read garbage before those pointers.
```

### Thread-local HTTP error buffer is shared across tests in the same process

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: nul-safety, testing, http, thread-local

**Rule**: `http_last_error_buf` in `runtime_http_error.cpp` is `thread_local` and persists for
the lifetime of the thread. If test A sets an error message (e.g. "url contains embedded NUL")
and test B then performs an operation that fails but does *not* write to `http_last_error_buf`
(e.g. a connection refused), test B's `e.message` will still contain test A's stale message.

**How to apply**: Do not write spec tests that assert error message contents after a network
failure that may or may not set the buffer. For NUL-safety tests, assert only that the result
is `Err` and that `e.message` contains the expected string. Never assert the message is
*absent* across test boundaries.

### Do not call `setLastError` in bool-return runtime functions — thread-local `last_error_buf` pollution

**Source**: #1128 (2026-04-18). **Tags**: nul-safety, setLastError, thread-local, runtime, bool-return

**Rule**: `setLastError` writes to a thread-local buffer (`last_error_buf` in `runtime_error.cpp`). In runtime functions whose Ry return type has **no error channel** (e.g. `bool`-returning `__ry_file_exists`), calling `setLastError` when rejecting NUL would leave stale error text in the buffer. The next Result-returning call on the same thread that fails an unrelated check reads the buffer for `e.message`, and would silently report the stale NUL rejection message instead of its own.

**How to apply**:
- Bool-return-only functions (e.g. `exists`) that detect an invalid argument: return the safe conservative value (`false` / `0`) silently — do **not** call `setLastError`.
- Result-returning functions: call `setLastError("fn: argument contains an embedded NUL byte")` before returning `nullptr` / `1`.
- This matches the split used in `src/runtime_io.cpp` after #1128: `__ry_file_exists` has `if (hasEmbeddedNul(path)) return 0;` with no `setLastError`, while all other path-taking functions call it.

### Ry expect matchers: use `to_not_contain`, not `not_to_contain`

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: testing, ry-syntax, matchers

**Rule**: The correct negating form for string containment in Ry expect matchers is
`to_not_contain("...")`, not `not_to_contain("...")`. Using `not_to_contain` compiles but calls
a non-existent method at runtime, producing an "undefined method" error.

```ry
# ❌ Wrong — runtime error
expect(e.message).not_to_contain("NUL")

# ✅ Correct
expect(e.message).to_not_contain("NUL")
```

---

### `@parallel for` str captures must use `emitStrGetHeaderFromData` (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, parallel_for, concurrency, offset, segfault

**Rule**: In `emitParallelForRange` (`src/codegen_stmt_loop.cpp`), when a captured variable is a `str` (i.e., it is in `arc_str_managed_vars_`), the thunk entry retain and the `arc_str_managed_vars_` side-table insertion for the thunk-local dst alloca must both use `emitStrGetHeaderFromData` (offset −24), not `emitArcGetHeaderFromData` (offset −16).

**Why**: Before #1046, str was never ARC-managed, so no str captures reached the retain path. After #1046, a `str` variable captured by `@parallel for` entered the retain path with the wrong offset: `emitArcGetHeaderFromData` pointed to `weak_count` (not `strong_count`). Each worker retained/released `weak_count` instead. When the last worker's release decremented `weak_count` to 0, the destructor called `free()` on a literal global's static allocation → segfault.

**How to apply**: Snapshot `capIsArcStr[i] = arc_str_managed_vars_.count(src) > 0` alongside `capIsArcManaged` and `capIsArcBacked` in the parent context (before `FnScope` clears the side-tables). In the thunk: after `markArcManaged(dst)`, if `capIsArcStr[i]` insert `dst` into `arc_str_managed_vars_`; at the retain site use `capIsArcStr[i] ? emitStrGetHeaderFromData(dataPtr) : emitArcGetHeaderFromData(dataPtr)`. The release via `popScope()→emitArcReleaseVar()` will then automatically select the correct offset via `arc_str_managed_vars_`.

---

### `emitPatternBindingArc` path 2b must propagate `arc_str_managed_vars_` from source (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, pattern-binding, emitPatternBindingArc, arc_str_managed_vars_, segfault

**Rule**: In `emitPatternBindingArc` path 2b ("no type signature — probe heuristically"), when `tryRetainArcSource(val)` returns true and `val` is a `LoadInst` from a source alloca in `arc_str_managed_vars_`, the destination `bindAlloca` must also be inserted into `arc_str_managed_vars_` — **not** into `arc_backed_vars_`. The check must mirror the full set hierarchy: closure → str → arc_backed.

**Why**: `emitArcReleaseVar` dispatches on the alloca's set membership to pick the header offset. `arc_backed_vars_` members use `emitArcGetHeaderFromData` (offset −16). For a `str` value, offset −16 is the `weak_count` field, not `strong_count`. When the `strong_count` check reads `weak_count = 0` instead of `ARC_IMMORTAL`, the immortal guard is bypassed and the code tries to atomically decrement `*(handle − 16)`. Since Ry string literal globals are compiled as `isConstant=true` LLVM globals (mapped read-only by the JIT), any write to them causes a SIGSEGV.

**How to apply**:
```cpp
// Correct propagation in emitPatternBindingArc path 2b:
if (auto *ld = llvm::dyn_cast<llvm::LoadInst>(val)) {
    auto *src = llvm::dyn_cast<llvm::AllocaInst>(ld->getPointerOperand());
    if (src && closure_managed_vars_.count(src))
        closure_managed_vars_.insert(bindAlloca);
    else if (src && arc_str_managed_vars_.count(src))   // ← must check str before arc_backed
        arc_str_managed_vars_.insert(bindAlloca);
    else
        arc_backed_vars_.insert(bindAlloca);
} else if (arc_str_owned_values_.count(val)) {          // non-LoadInst str source
    arc_str_managed_vars_.insert(bindAlloca);
} else {
    arc_backed_vars_.insert(bindAlloca);
}
```

This matters any time a `str` is extracted from an enum field via `EnumConstructorPattern` with a `VariablePattern` binding: the tmp alloca for the field is in `arc_str_managed_vars_`, and when `emitPatternBindings` recurses into `VariablePattern`, the resulting binding alloca inherits the str classification.

---

### Str literal globals are `isConstant=true` — wrong offset → SIGSEGV, not SIGABRT

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, StringHeader, isConstant, LLVM, offset, segfault

**Rule**: In `cachedGlobalString` (`src/codegen.cpp`), the StringHeader global for string literals is created with `isConstant=true`. The JIT maps such globals as read-only pages. Any code that uses `emitArcGetHeaderFromData` (offset −16) instead of `emitStrGetHeaderFromData` (offset −24) on a literal `str` will bypass the `ARC_IMMORTAL` guard (because `weak_count=0 ≠ INT64_MAX`), then attempt an atomic write to the read-only page → SIGSEGV (exit 139), not SIGABRT.

This makes wrong-offset bugs on str literals almost always fatal immediately, which is useful for diagnosis but means the stack trace will point into JIT code at an unusual address. Cross-reference with `arc_str_managed_vars_` membership at the crashing alloca's declaration site.

---

### Canonical-source pattern for deduplicating reference doc sections

**Source**: #1125 (2026-04-18)
**Tags**: documentation, dedup, drift, builtins, collections

**Rule**: When two reference pages redundantly describe the same function with full sections (signature + description + examples), pick one as canonical and reduce the other to a stub (`heading + signature + "See also" link`), not parallel bodies. Link stubs to per-function anchors where they exist; fall back to the parent section anchor when heading punctuation (e.g. `!`) would collide with an existing anchor after GitHub slugification (GitHub strips `!`, so `## sort!` → `#sort` which collides with `## sort`). Precedent: `docs/reference/functions.md:778`.

**Context**: `builtins.md` and `collections.md` both fully described `filter`, `map`, `sort`, `sort!`, `reverse!`, `appended`, `append!` (~200 lines of drift-prone parallel content). After #1125, `collections.md` is canonical; `builtins.md` sections are stubs. The quick-reference table in `builtins.md` stays (it is a useful at-a-glance index) with an umbrella note under the section header pointing to the canonical page.

**Anchor collision detail**: Bang variants (`sort!`, `reverse!`, `append!`) must link to the parent section (e.g. `#in-place-mutating-variants`) rather than a per-function anchor, because GitHub slugifies `### sort!` to `#sort` — identical to the slug for `### sort`. Stubs for these functions must not link to `#sort!` or similar; the parent section anchor is the correct fallback.

---

### Mutating collection operations belong in `codegen_call_higher_order.cpp`, not `codegen_call_string.cpp`

**Source**: #1124 (2026-04-18)
**Tags**: codegen, dispatch, placement, list, string

**Rule**: `sort!` and `reverse!` (and any future in-place list mutation) must be registered in `emitBuiltinHigherOrder` (`src/codegen_call_higher_order.cpp`), not in the String dispatcher table in `emitBuiltinString` (`src/codegen_call_string.cpp`). The String dispatcher is tried before HigherOrder in the dispatch chain (`codegen_call_dispatch.cpp`), so a stray entry in the String dispatcher will unconditionally claim the call for every type — including lists — before HigherOrder gets a chance. This caused `reverse!` to silently route list calls through a misleadingly-named `emitStrOp_reverse_mut` function, and to produce the confusing error "requires a list" when called on a string.

**How to apply**: When adding a new mutating built-in that applies to lists, check whether `sort!` already lives in HigherOrder and mirror that placement. If a built-in should error on strings with a user-facing message, add the `getListElementType() == nullptr` guard in the HigherOrder handler and emit a clear diagnostic (e.g. "requires a list; strings are immutable, use reverse() instead").

---

### `emitStrGetDataPtr` must insert into `arc_str_owned_values_`, not `arc_owned_values_`

**Source**: PR #1148 review (2026-04-18)
**Tags**: codegen, str, ARC, arc_str_owned_values_, emitStrGetDataPtr, StringHeader

**Rule**: `emitStrGetDataPtr` recovers the str data handle from a StringHeader pointer (GEP by `+STRING_HEADER_SIZE`). The result must go into `arc_str_owned_values_` (not `arc_owned_values_`), because downstream retain/release for str uses `emitStrGetHeaderFromData` (offset −24) while collection ARC uses `emitArcGetHeaderFromData` (offset −16). Putting a str handle in `arc_owned_values_` causes every subsequent retain/release to corrupt memory by reading the wrong 8 bytes.

**How to apply**: Search for `arc_owned_values_.insert` at call sites that received their value from `emitStrGetDataPtr` or from a `GEP(i8*, strHeaderPtr, STRING_HEADER_SIZE)` — these should be `arc_str_owned_values_.insert` instead.

---

### Map CoW must retain str keys independently of value retention

**Source**: PR #1148 review (2026-04-18)
**Tags**: codegen, CoW, Map, str, ARC, emitCowClone, elementTypeIsArcManaged

**Rule**: `elementTypeIsArcManaged(containerPtr, CollectionKind::Map, &kind)` only checks `map_value_type_name`, not `map_key_type_name`. Therefore the CoW clone loop (`emitCowClone`) retains values but never keys. For `Map<str, V>`, str keys are released by the destructor but never retained after the clone — use-after-free. After the value-retention block, emit a separate key-retention loop if `meta->map_key_type_name` resolves to an ARC-managed type (check via `fieldTypeIsArcManaged`). The Map destructor already handles str key release (L964 in `codegen_arc.cpp`: `if (elemSig == "str") emitStrElemLoop(keys, len, "mkey")`), so the missing piece is only on the retain side.

---

### Map literal `{k: v}` with str keys/values needs side-table retain + explicit str stamp

**Source**: #1347 (2026-04-24)
**Tags**: codegen, Map, literal, str, ARC, inferCollectionTypeName, retainArcValue, map_key_type_name

**Rule**: In `emitExprVariant(MapExpr)` (`src/codegen_expr_literal.cpp`), the existing retain gate `keyTypeName = inferCollectionTypeName(keyVals[0])` returns `""` for plain str values, so the gate short-circuits at `!keyTypeName.empty()` = `false`. A parallel `isStrHandle(v)` side-table check (`arc_str_owned_values_.count(v) > 0 || LoadInst from arc_str_managed_vars_ alloca`) is required; then call `retainArcValue(keyVals[i])` unconditionally on str handles. `retainArcValue → tryRetainArcSource` Case 1 emits retain for bound `LoadInst`; Case 2b is a **transfer-semantic no-op** for fresh `+1` `makeString` values (returns `true` without emitting retain), so the unconditional call doesn't leak on `{"foo": "bar"}`.

The retain side alone is insufficient for Map literals: the non-empty-literal VarDecl path in `codegen_stmt.cpp` only propagates `map_value_type_name` from `val`'s meta, not `map_key_type_name`. The str annotation at `L269-271` only fires for the empty-literal branch. Add symmetric `map_key_type_name` meta propagation from `valMeta` / `loadMeta` / annotation in the non-empty branch (mirror of the existing `mvtn` block), AND stamp `map_key_type_name = "str"` / `map_value_type_name = "str"` on the literal's returned `headerPtr` when `isStrHandle` detects str keys/values. Without those stamps, `resolveCollectionDestructor` dispatches to `__ry_arc_dtor_map` with empty sigs, so `emitInnerReleaseLoop` returns false for the keys array and the str keys never get released — swapping a UAF for a leak.

Why this differs from #1346 SetItem: `m: Map<str, str> = {}` (empty literal, then `m[k] = v`) hits the annotation-driven L269-282 path that stamps both `map_key_type_name` and `map_value_type_name`, so the #1346 fix only had to lift the `!= CollectionKind::Str` gate. `m: Map<str, str> = {k: v}` (non-empty literal) never hits L269-282 and has a fundamentally different root cause (`inferCollectionTypeName` returns empty for str) even though the user-visible symptom ("map key not found") is identical.

**Scope note**: List<str> / Set<str> literal variants were deferred when #1347 landed and are resolved in #1354 by the same side-table retain + explicit stamp pattern. The #1266 carve-out's "no stamp without retain" rule was narrowed (see the #1266 entry): stamp-with-retain is safe because the per-element retain cancels the `makeString`/`__ry_arc_alloc_counted` counter asymmetry at insert time, so the str-aware destructor can run without the large-negative `arc_live_count` delta that originally motivated the carve-out.

---

### List/Set literal `[s]` / `{s}` with str elements needs side-table retain + explicit str stamp (#1354)

**Source**: #1354 (2026-04-24)
**Tags**: codegen, List, Set, literal, str, ARC, inferCollectionTypeName, retainArcValue, list_elem_type_name, set_elem_type_name

**Rule**: In `emitExprVariant(ListExpr)` and `emitExprVariant(SetExpr)` (`src/codegen_expr_literal.cpp`), mirror the PR #1353 Map fix exactly: add an `isStrHandle` lambda that consults both `arc_str_owned_values_` (fresh `+1` `makeString` values) and `arc_str_managed_vars_` (borrowed `LoadInst` sources), apply `retainArcValue` per-entry when `elemTy == ptrTy_ && isStrHandle(v)`, track `anyElemIsStr` for a post-loop stamp of `list_elem_type_name = "str"` / `set_elem_type_name = "str"` on the `headerPtr`. For Set the retain must fire inside the `insertBB` branch (not the main loop body) so that duplicate insertions skipped by dedup do not double-retain. Then in `codegen_stmt.cpp` List tracking, propagate the literal's `list_elem_type_name = "str"` stamp into the indexer-facing side-channel `list_elem_is_str = true` via a single `if (letn == "str") inner_is_str = true;`.

**Why**: `inferCollectionTypeName` returns `""` for plain str values (same root cause as #1347 Map case), so the existing retain gate `element_needs_retain(elemTy)` / `setElemNeedsRetain` / `elemNeedsRetain` short-circuits. Without the side-table retain, `List<str> = [s]` / `Set<str> = {s}` with locally-constructed str elements produces a dangling pointer once the source var goes out of scope. Without the explicit `"str"` stamp on the literal header, `resolveCollectionDestructor` picks the non-str variant and the str elements silently leak. The per-entry retain + post-loop stamp pattern (not "first-entry-only" heuristic) is required because mixed-origin literals like `[bound_s, "literal_s"]` alternate between `LoadInst` and `ExtractValue` origins that escape first-entry detection.

**Chosen resolution framing — "third option"**: The issue body listed two options: (1) reconcile the counter conventions by making `makeString` increment by +1 to match `__ry_arc_alloc_counted`, or (2) provide a parallel dispatch path for str elements. This PR adopts neither, and instead mirrors PR #1353: side-table retain + explicit stamp is the pragmatic path that (a) fits a v0.0.13 hotfix scope, (b) matches the empirical delta = +1 per function call baseline already observed for `Map<int,int>`, and (c) leaves Option 1 (global counter reconciliation) for v0.0.14+ where `include/ry/ry_layout.hpp:29-32` already flags str as "not fully ARC-managed by codegen yet".

**How to apply**: When you see `element_needs_retain(elemTy)` (or its Map/Set analogues) gating a retain on an `inferCollectionTypeName`-returned name, and the element type is `ptrTy_`, audit whether str values can flow through that site with locally-constructed origins. If yes, add the `isStrHandle` side-table check as an OR condition and stamp the corresponding `*_type_name = "str"` on the literal header so the str-aware destructor matches the insert-side retain.

---

### Record ARC reassignment: use `!CallInst && !InvokeInst` not `LoadInst || ExtractValueInst`

**Source**: PR #1148 review (2026-04-18)
**Tags**: codegen, ARC, record, reassignment, InsertValueInst

**Rule**: The guard in `codegen_stmt.cpp` (`AssignStmt` record-with-ARC-fields path) that determines whether to call `emitRecordArcFieldsRetain` must be `!isa<CallInst>(val) && !isa<InvokeInst>(val)` — **not** `isa<LoadInst>(val) || isa<ExtractValueInst>(val)`. Fresh constructions (CallInst = direct call, InvokeInst = call with unwind) are sole owners and must not be retained. Every other value — including `InsertValueInst` chains like `r2 = { r.field, new_val }` — is a view of existing state and must be retained. The original allowlist of two cases was too narrow and caused use-after-free for `InsertValueInst` records.

---

### FileCheck golden authoring conventions

**Source**: #897 (2026-04-18)
**Tags**: filecheck, codegen, ir, testing, ci

**Rule**: FileCheck goldens live in `tests/filecheck/*.ry`. Each file is both valid Ry source and a FileCheck script — Ry uses `#` line comments, and `# CHECK:` lines work because FileCheck searches for the `CHECK:` substring regardless of prefix. **Do not use `//`** — that is not a Ry comment and causes a parse error.

Key constraints:

1. **Ry comment syntax is `#`**: Write `# CHECK:`, `# CHECK-NEXT:`, `# CHECK-NOT:`, `# CHECK-DAG:`, `# CHECK-LABEL:`. Never `// CHECK:`.
2. **Unoptimized IR only**: `ry --emit-llvm-ir` emits codegen output before any LLVM optimization passes. `alloca`/`store`/`load` patterns for every function argument are always present. `mem2reg` has not run.
3. **Opaque pointers (LLVM 17+)**: All pointer types are `ptr`; never write `i64*`, `i8*`, etc. in CHECK patterns.
4. **ARC retain/release visibility**: `@ry_retain` / `@ry_release` BasicBlocks only appear in CoW clone paths, lambda captures, and `@parallel for` patterns. They do not appear in simple scalar or string identity functions — choose goldens accordingly.
5. **Result type layout**: `%Result = type { i1, i64, ptr }` — `i1` is the `is_ok` flag; `Err` uses constant aggregate `{ i1 false, ... }`, `Ok` uses `insertvalue %Result { i1 true, ... }`.
6. **LLVM version bumps**: Goldens are LLVM-version-sensitive. After any LLVM version bump, re-run `ctest -L filecheck` and update patterns if IR structure changed.
7. **FileCheck installation**: Mirror tarball does NOT include FileCheck (#897). CI installs it via `apt-get install llvm-{MAJOR}-tools` from `apt.llvm.org`. macOS: `brew install llvm@{MAJOR}` → `/opt/homebrew/opt/llvm@{MAJOR}/bin/FileCheck`.

---

### Skill SKILL.md: keep `owner` and `repo` as separate variables when downstream steps use `{owner}`/`{repo}` individually

**Source**: PR #1148 CodeRabbit review / issue #1152 (2026-04-19)
**Tags**: skill, gh-cli, review-feedback

**Rule**: Using `gh repo view --json owner,name --jq '.owner.login + "/" + .name'` and storing the result as both `owner` and `repo` is correct only when the downstream code treats the combined value as a single placeholder (e.g. `repos/$FULL/...`). When downstream steps separately substitute `{owner}` and `{repo}` (REST paths like `repos/{owner}/{repo}/pulls/{PR}/...` or GraphQL `repository(owner: "<owner>", name: "<repo>")`), the combined string causes doubled path segments (e.g. `repos/t0k0sh1/ry/ry/pulls/...`) or an incorrect GraphQL `owner` argument. In that case, fetch them separately: `OWNER=$(gh repo view --json owner --jq '.owner.login')` / `REPO=$(gh repo view --json name --jq '.name')`. When writing or reviewing a skill step that stores repository coordinates, verify whether downstream uses the value as one unit or as two — they require different fetch forms.

---

### IndexExpr first index is scalar only — RangeExpr routes to emitListSlice

**Source**: #1184 (2026-04-19, fix)
**Tags**: codegen, IndexExpr, RangeExpr, slice, index-type-guard, emitExprVariant, emitListSlice

**Rule**: `emitExprVariant(IndexExpr)` (`src/codegen_expr_literal.cpp`) has always assumed that `emitExpr(*e->indices[0])` produces an i64 (or map key type). `lst[a..b]` is parsed as `IndexExpr{ object: lst, indices: [RangeExpr{a,b}] }` (via `parseConditional` → `parseRange` in `src/parser_expr.cpp`), which means without a type-guard the `RangeExpr` emitter (`src/codegen_expr_cast.cpp`) materialises an ARC-allocated `List<i64>` header (`ptr`). That `ptr` then flows into `emitBoundsCheck` / GEP, producing `ICmp ptr vs i64` type-mismatch errors in the IR verifier.

**Fix (since #1184)**: Before the `indexValues` emit loop, check:
```cpp
if (e->indices.size() == 1 &&
    std::holds_alternative<std::unique_ptr<RangeExpr>>(e->indices[0]->data)) {
    // reject str / map; get list elemTy; emitExpr start/end directly (i64);
    // emitNegativeIndexWrap each bound; endExcl = end + 1; emitListSlice(...)
}
```
This avoids materialising the intermediate `List<i64>` entirely.

**Semantics**:
- `lst[a..b]` is **inclusive** on both ends (matches `..` operator semantics per `docs/reference/operators.md`)
- Negative bounds are **wrapped** against list length (matches `lst[-1]` behaviour via `emitNegativeIndexWrap`)
- Out-of-bounds bounds are **clamped** (matches `slice()` behaviour)
- Equivalent to `slice(lst, a, b + 1)` — the `+ 1` conversion from inclusive to exclusive is done at the call site, not inside `emitListSlice`
- Non-list receivers (`str`, maps, fixed-length arrays) emit `codegenError` in the new guard

**Related helpers**:
- `emitListSlice(listPtr, startVal, endExclVal, elemTy)` (`src/codegen_call_collection.cpp`) — shared between `slice()` builtin and `lst[a..b]`. Clamps internally.
- `emitNegativeIndexWrap(idx, len, prefix)` (`src/codegen_call_user.cpp:645`) — use prefix `"ri_start"` / `"ri_end"` to avoid label collision with scalar-index prefix `"index"`.
- **#1198**: `emitCollOp_slice` was missing the pre-wrap step required by this contract (negatives were passed raw to `emitListSlice`, which clamps to `[0, len]` → `-3` collapsed to `0`). Fixed by applying `emitNegativeIndexWrap(start, len, "sl_start")` / `emitNegativeIndexWrap(end, len, "sl_end")` before the `emitListSlice` call, mirroring the range-index route. The `emitListSlice` invariant ("caller pre-wraps, I clamp") now applies to both call sites.
- **#1199**: `emitStrOp_substring` had the same 0-clamp bug on strings. Fixed by mirroring the slice pattern, with one extra step: `wrapBase` must be the UTF-8 codepoint count via `__ry_utf8_len_n(s, byteLen)`, not `emitStringByteLen(s)` (`"あいうえお"` is 5 chars / 15 bytes — using byte length wraps against the wrong base). See the sibling entry "String negative-index wrap uses UTF-8 char count, not byte length" below.

---

### String negative-index wrap uses UTF-8 char count, not byte length

**Source**: PR #1199 fix for `emitStrOp_substring` (2026-04-20)
**Tags**: codegen, substring, emitNegativeIndexWrap, UTF-8, char-count, __ry_utf8_len_n

**Rule**: When applying `emitNegativeIndexWrap(idx, wrapBase, prefix)` to string (char) indices, `wrapBase` MUST be the UTF-8 codepoint count obtained via `__ry_utf8_len_n(s, byteLen)`, not `emitStringByteLen(s)`. For a 5-char, 15-byte string like `"あいうえお"`, using byte length as wrap base gives `-1 + 15 = 14` (a garbage index into a 5-char string); using char count gives `-1 + 5 = 4` (correct last-char index). The `_n` variant is NUL-safe (counts embedded `\0` as one codepoint).

**Pattern** (copy from `src/codegen_call_string.cpp:218-245`):
```cpp
llvm::Value *byteLen = emitStringByteLen(s);    // hoisted: reused for runtime call
auto utf8LenFn = getRuntimeFn("__ry_utf8_len_n", i64Ty_, {ptrTy_, i64Ty_});
llvm::Value *charLen = builder_.CreateCall(utf8LenFn, {s, byteLen}, "charlen");
llvm::Value *wrapped = emitNegativeIndexWrap(idx, charLen, "prefix");
// ... then zero-clamp (wrap of very negative idx can still be negative), then runtime call
```

Always hoist `emitStringByteLen(s)` into a local so the same byte length feeds both `__ry_utf8_len_n` and the downstream string-indexed runtime call (e.g. `__ry_utf8_substring`). The zero-clamp after wrap is still required: `-100 + 5 = -95` needs to be clamped to `0`.

**Why this differs from `emitCollOp_slice`**: `emitCollOp_slice` uses list length (element count = storage length), so there is no "byte vs codepoint" distinction. String call sites must remember this extra indirection.

---

## #1156: split match subject type into enum-only vs broad source name

**Source**: PR #1156 fix for `codegen_match.cpp` subjectEnumType wrong channel
**Tags**: pattern, match, ARC, codegen, Option, Result, tuple, subjectEnumType

**Rule**: `emitPatternTest`, `emitPatternBindings`, and `checkMatchExhaustiveness`
take **two** source-name parameters:

- `subjectEnumName` — narrow channel (`ValueMetadata::enum_value_type`).
  Only set for enum/ADT subjects (from `resolveEnumType()`). Used by
  `EnumPattern`/`EnumConstructorPattern` generic-instantiated lookup, by
  `Some`/`Ok`/`Err` binding `extractGenericTypeArg`, and by `VariablePattern`
  binding's `enum_value_type` write (still guarded by
  `enum_types_.count(resolveTypeAlias(name))` per the KNOWLEDGE L2946 rule).
- `subjectSourceTypeName` — broad channel (`resolveSubjectSourceTypeName()`).
  Reconstructs `Option<T>` / `Result<T, E>` / `(T1, T2, ...)` from the LLVM
  subject type when no enum annotation exists. Used by `TuplePattern` /
  `RecordPattern` structural verification and by `emitPatternBindingArc`
  Path 2a from `VariablePattern` only.

**Reconstruction lossiness**: `reverseResolveTypeName(ptrTy_)` returns `"str"`;
unknown structs return `"any"`. So `Option<List<int>>` reconstructs as
`"Option<str>"`. The reconstructed string is therefore **only** safe for
structural checks ("is this a tuple struct?" / "does this match record name X?").
Do NOT re-feed it into `extractGenericTypeArg` for ARC payload classification —
`Option<List<int>>` would be misclassified as `Option<str>` (wrong header
offset: str uses -24, List uses -16), causing heap corruption under ASan.

**Defense-in-depth on TuplePattern**: the test arm rejects via
`!sTy || !isTupleStructType(sTy)` regardless of any source name. This fires
even when both names are empty — closing the path where Option's `{i1, T}`
2-element struct silently passed the 2-arity check and crashed with ICmp
type mismatch (original #1156 crash vector).

**Why split rather than unify**: the previous single `subjectEnumType`
parameter was overloaded with non-enum names via the new broad helper
would force every consumer to add an `enum_types_.count(name)` guard.
The split signature makes "enum-only" vs "broader subject type" a
compile-time-enforced distinction. The guard at VariablePattern binding
(`KNOWLEDGE L2946`) remains necessary but is now purely defensive
(subjectEnumName is already enum-only).

**Follow-up**: Add a lossless `source_type_name` field to `ValueMetadata` so
`Option<List<int>>` reconstruction is accurate, enabling ARC Path 2a for nested
generics (currently handled by Path 2b heuristic via `propagateMeta`).
- Pre-existing defect in `emitListSlice`: ARC retain omitted for reference-typed elements (#1204) — tracked separately.

### All str handles passed to `emitStringByteLen` must be StringHeader-backed (#1159)

**Source**: Issue #1159 / PR fix/1159-fstring-enum-unknown-stringheader. **Tags**: codegen, strings, stringheader, fstring, arc, emitStringByteLen, cachedGlobalString

**Rule**: `emitStringByteLen(handle)` reads `handle - 8` (STRING_BYTELEN_OFFSET) to obtain `byte_len`. This is only safe when `handle` points into a `StringHeader`-prefixed allocation — i.e., the pointer was produced by `cachedGlobalString`, `buildArcGlobal`, or a Ry ARC runtime that calls `makeString`/`makeStringUninit`. `IRBuilder::CreateGlobalString` produces a plain `[N x i8]*` global **without** a StringHeader prefix; passing such a pointer to `emitStringByteLen` reads the 8 bytes immediately before the global (typically unrelated global data or relocation metadata) and interprets them as `byte_len`, causing truncation, garbage output, or UB.

**Root cause**: `src/codegen_tostring.cpp` — `hasExplicitValues` enum branch — had a default fallback BB that used `builder_.CreateGlobalString("?", ".enum_unknown")`. The PHI on that branch collected the raw pointer, which `emitStringByteLen` later misread. The ADT/union default on the same file already used `cachedGlobalString` correctly; the explicit-value enum default was the outlier.

**How to apply**: When adding a new `str` constant in codegen, always use `cachedGlobalString`. When reviewing existing codegen, grep for `CreateGlobalString(` and verify that the result never flows into `emitStringByteLen`, `emitStringLen`, or f-string concat helpers. The only safe uses of raw `CreateGlobalString` are LLVM format strings for `printf`-style calls that are never read back as Ry `str` handles.

### `contains()` on Map must be intercepted in `emitStrOp_contains`, not via `@native`

**Source**: #1185 (2026-04-19, fix)
**Tags**: codegen, collections, map, dispatch-order, contains

`emitBuiltinString` is invoked before `emitBuiltinCollection` in
`src/codegen_call_dispatch.cpp`. Therefore every `contains(x, y)` call reaches
`emitStrOp_contains` first. Since all collection pointers share `ptrTy_` with
strings under the opaque pointer model, the Set and Map intercept blocks must
live inside `emitStrOp_contains` — before the str fall-through logic — rather
than in `emitBuiltinCollection`.

**Rule**: If a new collection type needs `contains()` semantics, add a
`getXxxType(s)` intercept block immediately after the Set block in
`emitStrOp_contains` (`src/codegen_call_string.cpp`). Do NOT declare a
`@native` `contains` overload in the stdlib package file; that would route
through the Pattern-A dispatch table and interact unpredictably with the
string handler.

The Map intercept mirrors `has_key` (`src/codegen_call.cpp`) exactly:
```cpp
if (llvm::Type *mapKeyTy = getMapKeyType(s)) {
    if (e.args.size() != 2)
        codegenError("Map.contains() takes exactly 1 argument");
    llvm::Value *key = emitExpr(*e.args[1]);
    if (key->getType() != mapKeyTy)
        codegenError("contains() key type mismatch");
    llvm::Value *idx = emitMapKeyLookup(s, key, mapKeyTy);
    return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "map_contains");
}
```

Note: `emitMapKeyLookup` is called without threading `keyName` here, matching
the existing `has_key` implementation. Structural-key correctness (pointer-typed
map keys) is a separate issue — see the L1106 entry.

### `@native` dispatch must run exact-match Pass 1 before widening Pass 2

**Source**: #1193 (2026-04-19, `@native` int→float widening fix)
**Tags**: codegen, stdlib, overload, widening, coercion, native-dispatch, resolveOverload

**Context**: User-defined Ry functions accept implicit `int → float` widening
via `CodeGen::resolveOverload` (`src/codegen_call_user.cpp`). Before #1193,
`@native` dispatch used strict `args[i]->getType() != expectedTy` — so
`sqrt(4)` errored even though `takes_float(4)` worked. All three `@native`
dispatch sites in `src/codegen_call_native.cpp` — `emitTableDrivenNativeCall`
fixed-arity path, `emitTableDrivenNativeCall` variadic path, and
`emitGenericNativeCall` — now run a **two-pass** resolution:

1. Pass 1: exact type match (unchanged behavior).
2. Pass 2 (only if Pass 1 is empty): accept args via
   `isWideningConversion` / `emitWideningConversion` (`src/codegen.cpp:761-789`).

**Rule**: Any new `@native` dispatch path — or any edit to the three existing
ones — must keep the two passes in this exact order. Exact matches must win
across the entire overload set before any widening is considered, so that
`pow(2, 3)` continues to dispatch to the `(int, int) -> int` overload rather
than silently promoting to `(float, float) -> float`. Reuse the public
helpers `CodeGen::isWideningConversion` and `CodeGen::emitWideningConversion`;
do not re-implement `SIToFP` inline.

**Why**: Collapsing the passes into a single scored loop (like
`resolveOverload` does) is tempting but changes precedence semantics when an
exact match and a widening-eligible match both exist; the two-pass split
makes the invariant explicit and localises the change.

**How to apply**: For each overload candidate, track a
`std::vector<bool> needsWidening` alongside the existing `paramLLVMTypes`
vector. Before emitting `CreateCall`, loop over the matched args and call
`emitWideningConversion` wherever `needsWidening[i]` is true. The error path
for "no matching overload" is unchanged — falling through Pass 1 and Pass 2
both lands in the original `"argument N requires <type>"` message.

Custom emitters must replicate this pattern locally: immediately after
`emitExpr` of each arg, call `isWideningConversion` + `emitWideningConversion`
before the hardcoded `x->getType() != cg.f64Ty_` guard. For emitters that
already have two exact-type branches (`emitMathPow`'s `(f64, f64)` and
`(i64, i64)`), append the widening branch *after* both exact branches so the
precedence invariant (`pow(2, 3) → int 8`) is preserved by branch ordering.
See `emitMathFloorCeilRound` / `emitMathLog` / `emitMathPow` in
`src/codegen_call.cpp` for the canonical examples.

**Resolved follow-up**: Math custom emitters (`emitMathFloorCeilRound`,
`emitMathLog`, `emitMathPow` mixed-type) now also accept widened `int` args
via the local pattern above (#1230, 2026-04-21). `emitMathAbs` was already
correct because both `abs(int)` and `abs(float)` overloads are declared in
`share/std/math/math.ry` and the emitter exact-matches on both LLVM types.

### scan-build: mirror tarball uses Debian-patched path for FindClang

**Source**: #1247 (2026-04-20)
**Tags**: ci, scan-build, llvm, mirror, static-analysis

**Rule**: The Debian-patched `scan-build` Perl script bundled in the LLVM
mirror tarball (via `clang-tools-{MAJOR}`) has a hard-coded fallback path
in `FindClang()` (line 1508) that points at `/usr/lib/llvm-{MAJOR}/bin/clang`.
The mirror tarball extracts to `/usr/local/llvm`, so that Debian path does
not exist on the runner. Always pass `--use-analyzer=/usr/local/llvm/bin/clang`
explicitly to `scan-build` — `--use-cc` controls only the build-time compiler
and is ignored by the analyzer-clang lookup.

**Why**: Before the mirror tarball was introduced, `setup-llvm` fell back
to `apt.llvm.org`, which populates `/usr/lib/llvm-{MAJOR}/`, so the
hard-coded Debian path resolved accidentally and `scan-build` worked
without `--use-analyzer`. Once the mirror took over, the path no longer
exists and `FindClang()` exhausts all three lookup candidates
(`$RealBin/bin/clang`, `/usr/lib/llvm-{MAJOR}/bin/clang`, Xcode toolchain)
and leaves `$Clang` undefined, producing
`Use of uninitialized value $Clang in concatenation` and
`scan-build: error: Cannot find an executable 'clang' relative to scan-build`.

**How to apply**: In every CI `scan-build` invocation (`.github/workflows/ci.yml`,
`.github/workflows/ci-scheduled.yml`) and every local-execution example in
`AGENTS.md`, include `--use-analyzer=/usr/local/llvm/bin/clang` alongside
`--use-cc` / `--use-c++`. macOS Homebrew `scan-build` does not have the
Debian patch, so the flag is redundant there — but keeping it uniform
prevents drift between local and CI invocations.

**How to verify**: `grep -n 'scan-build' .github/workflows/*.yml AGENTS.md`
and confirm `--use-analyzer` accompanies every invocation. In CI logs, the
`Use of uninitialized value $Clang` warning must be absent.

---

### Destructor recursion into inner ARC elements forces retain-on-store at every write site

**Source**: #1242 (2026-04-21). **Tags**: ARC, destructor, retain, list-literal, map-literal, set-literal, appended, insert, merge, uaf, regression

**Rule**: `getOrCreateCollectionDestructor` for List/Map/Set now walks the element buffer and calls `emitArcRelease` on each ARC-managed inner element (List/Map/Set), not just `free(dataBuf)`. Every codegen site that stores an ARC pointer into a collection slot — literal construction, `appended`, `insert`, set `add`, `append(!)`, map `merge`, IndexAssignStmt Map insert — must emit a matching retain, or the destructor will free shared elements out from under other holders.

The retain discipline is **symmetric** with the destructor:
- Destructor releases ⇒ source must retain on store (literal + container op).
- Destructor does not release (str element slot because `list_elem_type_name` is never stamped "str" per #1266) ⇒ retain is skipped.

**Why**: Pre-#1242 the destructor only freed the buffer, so the "raw-pointer aliasing" bug between a collection and its inner elements was invisible — inner elements leaked forever and stayed dereferenceable. The destructor fix flips that: the freed memory is now reachable from any dropped alias, producing UAF on the first rebind. `xs: List<List<int>> = [[0]]; while i < N: xs = [[1,2],[3,4]]` showed delta = 3*N+2 pre-fix (pure leak). Once the destructor recurses, the same rebind frees the previous inner lists — safe if and only if every site that duplicated those pointers retained.

**How to apply**: When adding a new collection write site, ask "does the destructor now release this slot?" — if yes (List/Map/Set element type resolves via `fieldTypeIsArcManaged` with `CollectionKind != Str`), emit `retainArcValue(val)` on freshly-loaded values and `emitCowRetainArcElements(newBuf, len, tag, elemArcKind)` on memcpy'd ranges before the store. For update-in-place patterns (Map insert-or-update, IndexAssignStmt update branch), release the overwritten element first. Callers must read `list_elem_type_name` / `map_*_type_name` / `set_elem_type_name` from the *source* container's metadata, not the newly-allocated header, since the new one is still being populated. The canonical sites fixed in #1242: `codegen_expr_literal.cpp` (List/Map/Set literal store loops), `codegen_call_collection.cpp::emitCollOp_add` / `_append` / `_appended` / `_insert` / `emitMapMergeCore`, `codegen_stmt_misc.cpp::IndexAssignStmt` Map insert path.

**Str carve-out preserved**: The destructor extension covers only List/Map/Set inner elements. The str element destructor path (`if (elemSig == "str") emitStrElemLoop(...)` in `getOrCreateCollectionDestructor`) is unchanged per #1266 — `AssignStmt` never stamps `list_elem_type_name = "str"` because flipping the destructor exposes a counter asymmetry between `__ry_arc_alloc_counted` (+1) and `makeString` (no-op), producing a large negative `arc_live_count()` delta in `arc_split_chars.test.ry`. The side-channel `ValueMetadata::list_elem_is_str` scoped to the indexer (#1266) remains the correct way to propagate str-elementness for read paths. A full str-destructor-switch belongs to a future issue.

**Related**: #1204 (slice retain), #1235 (take retain), #1046 (CoW str retain + original destructor extension for str), #1108 (emitArcReleaseLoadedElement inner-sig propagation), #855 (slot overwrite release discipline), #1266 (str destructor carve-out + `list_elem_is_str` side-channel).

---

### `appended` / `insert` / `merge` duplicate pointers without retain — symmetric to `slice`/`take`

**Source**: #1242 (2026-04-21, discovered during destructor extension). **Tags**: ARC, appended, insert, merge, memcpy, retain-on-store, uaf

**Rule**: Functions that construct a *new* collection whose element buffer is produced by memcpy from a source (`emitListSlice`, `emitCollOp_take_impl`, `emitCollOp_appended`, `emitMapMergeCore`'s m1 copy) must retain each ARC-managed element after the memcpy. Functions that store an individual ARC value into an existing or new collection (`emitCollOp_add`, `emitCollOp_append`, `emitCollOp_insert`, map merge's m2 insert/update) must retain the value before the store. Update-in-place also needs to release the overwritten element.

**Why**: Same defect class as #1204 (slice) / #1235 (take). Pre-#1242 the defect was latent: the source's destructor did not release inner elements, so the memcpy'd pointers stayed valid even after source release. Post-#1242 the destructor does release, so the second holder of the memcpy'd pointer is left with a dangling pointer. Rebind of either side triggers UAF.

**How to apply**: A repro is `m1 = {"a": a}; merged = merge(m1, m2); m1 = {"x": [99]}; print(merged["a"][0])` — pre-fix on a post-destructor-extension build this reads freed memory. Tests that rebind the source AND run an allocation-churning loop before reading the new collection reliably surface the UAF (without the churn, the freed memory still holds the old value and the read looks correct). See `tests/spec/arc_release_on_rebind.test.ry` "merge(Map<str,List<int>>, ...) survives source rebind" and "appended(List<List<int>>, ...) survives source rebind" for the canonical pattern.

**Related**: #1204, #1235, #1046 (original CoW retain), #1242.

---

### `List<T>` annotations must preserve named pointer-backed inner types for index-load metadata rebuilds

**Source**: #1320 (2026-04-22, implementation). **Tags**: codegen, metadata, list, index, annotation, JsonValue, resource

**Rule**: When recording `list_elem_type_name` from a `List<T>` annotation, preserve every non-primitive inner type name except `str` (which must keep using the `list_elem_is_str` side-channel from #1266). Limiting the stored name to nested collections/functions misses pointer-backed named types like `JsonValue` and stdlib resources (`Lock`, `RWLock`, etc.), so `xs[i]` cannot rebuild the loaded element's metadata via `propagateTypeMeta()`.

**How to apply**: In `emitVarDecl`, after handling `str` and function-typed inners, assign `list_elem_type_name = inner` for any inner type other than `int` / `float` / `bool`. This keeps index loads, loop bindings, and other metadata-reconstruction sites able to restore resource kinds or JSON dispatch metadata from the annotation even when the loaded LLVM type is just `ptr`.

---

### Lambda return-type inference must consult `@native` signatures, not just user-function overloads

**Source**: #1318 (2026-04-23, implementation). **Tags**: codegen, lambda, type-inference, native, stdlib, resource, JsonValue

**Rule**: `inferExprType()` / `inferExprTypeName()` for `CallExpr` must check the `@native` signature registry (`native_fn_sigs_` + `native_lib_index_`) when `findFunction()` misses. Many stdlib calls such as `lock_new()` and `json.parse()` are registered only as `@native` signatures, so falling straight to the scalar fallback (`i64Ty_` / `"int"`) makes lambda return-type checking report nonsense like `expected 'int'` for `fn() -> Lock` or `fn() -> Result<JsonValue, Error>`.

**How to apply**: Keep the lambda inference path in sync with native-call dispatch: match `@native` overloads by arity and inferred argument types, allow the same implicit widening tier as normal call resolution, and return the matched signature's declared Ry return type name. Without the name-level lookup, metadata-gated returns (`Result<JsonValue, Error>`, resource handles, function-typed returns) may still compile to the right LLVM type later but fail or lose metadata during lambda pre-checking.

---

### `inferNativeCallReturnTypeName` must narrow overloads by source-level type name, not LLVM type

**Source**: #1349 (2026-04-24, implementation). **Tags**: codegen, lambda, type-inference, native, overload, ptr-collapse, captured-vars

**Rule**: When narrowing `@native` overloads inside lambda return-type inference, compare `sig.params[i].typeName` against `inferExprTypeName(arg)` (source-level Ry names), not `argTypes[i] == resolveType(sig.params[i].typeName)` (LLVM types). Ptr-backed types (`str`, `List`, `Map`, `Set`) all resolve to the same `ptr` under opaque pointers, so every overload appears to match and the ambiguity guard fires. The companion bug is that captured variables in lambda / nested-fn bodies also need their source-level collection metadata (`list_elem_type_name` / `map_key_type_name` / `set_elem_type_name` / `union_value_type` / `enum_value_type`) propagated from the outer alloca — only `fn_type_info` was being forwarded, so dispatch inside the body misrouted `length(xs)` to the str runtime even after the type-inference fix landed.

**How to apply**: In `inferNativeCallReturnTypeName` (`src/codegen_lambda.cpp`), build `argTypeNames` via `inferExprTypeName()` alongside `argTypes`, match by `resolveTypeAlias(argTypeNames[i]) == resolveTypeAlias(sig.params[i].typeName)` first, and fall back to the LLVM-type equality path only when the source-level name is empty. Keep `isInferenceWidening` — its `isLowLevelTypeName` gate means it won't misfire on ptr-backed types. Thread a `paramTypeNameMap` default parameter so `inferExprTypeName::CallExpr` can forward the outer name map into the check. For captured variables, save the outer `llvm::AllocaInst*` in `CaptureAnalysisResult::capturedSrcAllocas` during `analyzeFreeVariables`, and in both lambda (`emitExprVariant<LambdaExpr>`) and nested-fn (`emitStmt<FnStmt>`) captured-param setup call `propagateMeta(srcAlloca, newAlloca)` after `CreateStore` — this copies the full metadata packet (collection types, type-name strings, `fn_type_info`, resource kinds, ARC-managed flag) in one shot.

---

### Higher-order Result/lambda boundaries must rehydrate ptr-backed metadata from type names or wrappers

**Source**: #1319 (2026-04-23, implementation). **Tags**: codegen, metadata, lambda, Result, JsonValue, resource, higher-order

**Rule**: Any higher-order boundary that unwraps a `Result<T, E>` / `Option<T>` payload or calls an indirect lambda must restore metadata for pointer-backed named types. `CreateExtractValue` drops the payload's `ValueMetadata`, and indirect calls return a fresh SSA value with no metadata unless `FnTypeInfo.returnTypeName` is re-applied.

**How to apply**: Mirror the `?` operator path in combinators: after extracting `ok_val` / `some_val`, call `propagateMeta(wrapper, payload)` before passing the value into the callback. In `emitLambdaCall`, always stamp the call result from `FnTypeInfo.returnTypeName` (and `returnFnTypeInfo` for function-typed returns). For lambda return-name inference on `VariableExpr`, prefer `buildTypeNameFromMeta()` over raw `reverseResolveTypeName()` so resource / `JsonValue` names survive opaque-pointer lowering.

---

### For-loop multi-variable destructure and `Set<X>` annotation propagation both trailed the List path until #1350

**Source**: #1350 (2026-04-24, implementation). **Tags**: codegen, for-loop, destructure, set, tuple, metadata, set_elem_type_name

**Rule**: Two asymmetries between the Set and List codepaths were latent until the multi-variable destructure case surfaced them:

1. **Multi-variable for-loop binding path** (`src/codegen_stmt_loop.cpp` multi-var branch) treated "not a map" as "must be a list" and emitted `"for loop destructuring requires a list of tuples"` for any `Set<(T, U)>`. The single-variable path (same file, single-var branch) had already mirrored the Set-first / List-fallback shape via `getSetElementType` + `setHeaderTy_` with `getListElementType` + `listHeaderTy_` as fallback. Multi-var must use the exact same shape — switch the local `headerTy` variable between `setHeaderTy_` and `listHeaderTy_`, set snapshot metadata to `TypeMeta::SetElem` vs `TypeMeta::ListElem`, and prefer `set_elem_type_name` over `list_elem_type_name` when reading the tuple source name for `emitForBindingPattern`. `List` / `Set` headers share field 0 (len) and field 2 (data ptr), so existing GEP arithmetic is reusable.

2. **`Set<X>` annotation → `set_elem_type_name` propagation** in `codegen_stmt.cpp`'s `emitVarDecl` narrowed the inner type to `{List|Map|Set|FnType}`, so named non-primitive tuple/enum/record/alias inners were dropped. The sibling `List<X>` path (#1320) had already switched to a denylist (`inner != "str" && inner != "int" && inner != "float" && inner != "bool"`). Mirror that denylist on Set, otherwise `Set<(str, List<int>)>` annotations never carry `"(str, List<int>)"` down to the for-loop, and `sum(v)` in the destructured body fails with "sum() requires a list" even after the codegen_stmt_loop fix is in place.

**Why**: These are the same class of asymmetry: every time a List-side enhancement lands (destructure dispatch, annotation denylist widening), the Set-side equivalent should be ported in the same PR unless there is a concrete reason to diverge. The List/Set iteration machinery deliberately shares header layout, so divergent codepaths are a code smell.

**How to apply**:

- Before declaring a List-specific fix complete, grep for sibling `getSetElementType` / `setHeaderTy_` / `set_elem_type_name` sites and check whether the same logic belongs there.
- The single-variable for-loop path (L324-407) is the canonical Set-first / List-fallback template; new iteration features should mirror both simultaneously.
- Test both collection-destructure shapes (`for a, b in listOfTuples`, `for a, b in setOfTuples`) whenever a binding-related for-loop change lands.
- Follow-up candidate: extract the shared `(elemTy, headerTy, typeName, headerMetaKind)` lookup into a helper so multi-var and single-var paths stop growing copy-paste in parallel. Not blocking for v0.0.13, but worth filing if another divergence shows up.

---
