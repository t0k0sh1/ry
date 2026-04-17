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

- Smoke-verify the error via `./build/ry -c '...'` during development
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

**How to verify before opening a PR**:

```bash
grep -nE 'CreateLoad.*elem|CreateLoad.*valVal' src/codegen_tostring.cpp
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
parameter type *is* a bare type variable — so `function identity<T>(x: T)`
worked but `function first_of<T>(xs: List<T>)` silently produced no
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
a custom emitter, (1) add the `@native function` declaration, (2)
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
5. If `typeSig` is `"str"` or another non-collection non-closure pointer type: do not retain (see **Do not retain `str`** below). Return.
6. Otherwise (no `typeSig`): fall through to `tryRetainArcSource(val)` (source-alloca heuristic); then check `arc_owned_values_`; then check collection-type metadata set by `propagateTypeMeta`.

**Do not retain `str`**: `str` values are not ARC-managed in PR #1022. Dynamic `str` values use a `StringHeader` layout (`{strong_count, weak_count, byte_len, data[], '\0'}`), where the handle points to `alloc + STRING_HEADER_SIZE (24)`. The ARC header is at `handle - 24`, but `emitArcGetHeaderFromData` uses offset `ARC_HEADER_SIZE (16)`, so retaining a `str` would point to the middle of the header (16 bytes instead of 24) → corrupts the `byte_len` field → crash. The correct approach is: only retain collections (`List<T>`, `Map<K,V>`, `Set<T>`), never bare `str`. Full ARC management of `str` (with the correct `-24` offset) is deferred to a follow-up issue after #1022.

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

**`markArcManaged(tmp)` pre-mark must be guarded by `fieldTypeIsArcManaged`** (Source: #1016):
TuplePattern / RecordPattern / EnumConstructorPattern pre-mark a temporary alloca
(`tmp`) as ARC-managed so the recursive leaf `VariablePattern` binding can emit a
single retain via `tryRetainArcSource` Case 1. Without the guard, _any_ `ptrTy_`
field (including `str`, bare fn-ptr, and resource ptrs) is incorrectly marked,
causing `tryRetainArcSource` to call `emitArcGetHeaderFromData(val)` — which points
16 bytes before a plain `checked_malloc` pointer, corrupting malloc metadata. ASan
detects this as "attempting free on address which was not malloc()-ed".
Fix: replace `if (elemTy == ptrTy_) markArcManaged(tmp)` with
`if (elemTy == ptrTy_ && fieldTypeIsArcManaged(elemSig, nullptr)) markArcManaged(tmp)`
at all three sites (`src/codegen_match.cpp`). `fieldTypeIsArcManaged` resolves
aliases and returns true only for non-weak List/Map/Set types. Capturing closure in
tuple/record/enum fields is intentionally excluded: `fn_type_info` metadata is not
propagated by `propagateTypeMeta` onto `ExtractValue` intermediates, so closure
detection is impossible here; this was also the pre-#1008 behaviour.

### `str` does not support `[]` index syntax — guard with `isStringValue` before list/map dispatch

**Source**: #1026 (2026-04-16, diagnostic improvement)
**Tags**: codegen, diagnostics, str, index-access, emitExprVariant, IndexExpr, IndexAssignStmt

**Rule**: In `emitExprVariant(IndexExpr)` (`src/codegen_expr_literal.cpp`) and the `IndexAssignStmt` emitter (`src/codegen_stmt_misc.cpp`), `str` values are `ptrTy_` pointers and pass the "must be ptr" gate. Without an explicit guard they fall through the Map check (no map metadata) and then either (a) hit the List fallthrough and emit the misleading `"cannot determine list element type for index access"` error or (b) reach `emitCowCheck(objPtr, ..., CollectionKind::List)` which may corrupt state.

**Fix**: After the `"index operator requires list or map"` gate and before the Map dispatch, check `isStringValue(objPtr)` and emit a targeted diagnostic:
- Read path: `"str does not support index access; use char_at(s, i) instead"`
- Write path: `"str does not support index assignment; strings are immutable"` — and critically, this guard must come **before** the `emitCowCheck(objPtr, receiverAlloca, CollectionKind::List)` call; passing a `str` pointer with `CollectionKind::List` to `emitCowCheck` is unsafe.

**Why `isStringValue` is the right predicate**: `isStringValue(val)` (`src/codegen_any.cpp:28`) returns true when `val->getType() == ptrTy_` and `isNonStrPointer(val)` is false (i.e., no collection/resource metadata). Lists, Maps, Sets, and resource handles all have metadata (`hasAnyMeta() == true`), so only `str` triggers the new guard. This is the canonical predicate used throughout codegen to distinguish `str` from other `ptrTy_` values.

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

### Use depth-tracking for nested delimiters, never naive find()

**Source**: #801 PR
**Tags**: parser, lexer, delimiters

**Context**: `parseFnTypeAnnotation()` originally used
`string::find(')')` to locate the closing paren, which broke on
nested function-typed parameters like
`function((int) -> int) -> int`.

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

### LLVM ORC JIT intermittent crash in `~LLJIT()` / `removeResourceTracker` (Linux + macOS)

**Source**: #1022 (2026-04-16) CI run 24507853564; #1088 (2026-04-17) macOS Darwin 25.3.0
**Tags**: llvm, orc, jit, ci, flaky, linux, macos, cleanup, parallel-test

**Symptom**: The Ry self-test (`ry test -p`) completes all `it` blocks
successfully, then crashes during JIT teardown.

- **Linux CI**: `cfree` → `removeResourceTracker` LLVM abort message after the test summary.
- **macOS (non-ASan)**: intermittent `~40%` failure rate in parallel mode — worker subprocess
  exits with signal (`128+N`) silently; the parent counts `+1 total failures` with no red line.

On Linux:
```text
135 passed, 0 failed
PLEASE submit a bug report to https://github.com/llvm/llvm-project/issues/...
#4  cfree (/lib/x86_64-linux-gnu/libc.so.6)
#5  llvm::orc::ExecutionSession::removeResourceTracker(...)
#7  scope_exit destructor in jit_runner.cpp
#8  runRySource(...)
```

**Fix applied**: `src/jit_runner.cpp` — `(void)jit.release()` workaround is now guarded by
`#if defined(__linux__) || defined(__APPLE__)` (previously Linux-only). The LLJIT is intentionally
leaked so `~LLJIT()` never runs; the OS reclaims memory on process exit. Tracked in #742.

**Rule**: On Linux CI, trigger a re-run if this crash appears — it is pre-existing LLVM ORC
flakiness, not a regression. On macOS, the workaround suppresses the crash; if the `~40%` failure
rate reappears after the fix, the root cause has changed and needs fresh investigation. Do not
suppress the LLVM crash reporter or add `|| true` — a genuine double-free in user code would
produce the same frame.

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
`function thread_spawn(body: function() -> Unit) -> Thread`)
constrains only what IDE/docs consumers see, not what the custom
emitter actually accepts. When extending a custom-emitter native
function (e.g. broadening `thread_spawn` to accept non-Unit
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

**Source**: #892 / #919 / #934
**Tags**: llvm, ci, cache, mirror, version-bump

**Rule**: CI fetches LLVM from a GitHub Releases mirror via
`.github/actions/setup-llvm/`. The mirror tarball is built by
`.github/workflows/mirror-llvm-toolchain.yml` (manual `workflow_dispatch`).

The mirror tarball includes `clang-tidy` (added in #934) and `scan-build` / `analyze-build` via `clang-tools-{MAJOR}` (added in #898). The
`setup-llvm` action accepts an optional `extra-packages` input for
the apt fallback path; the mirror/cache path already contains all
tools. If new tools are needed, add them to
`mirror-llvm-toolchain.yml`'s apt-get line, bump the cache key
version suffix (e.g. `v2` → `v3`), and re-dispatch the mirror workflow
with `force=true`.

Version bump checklist — update `env.LLVM_VERSION` (and
`env.LLVM_SHA256_SHORT` when non-empty) in:
- `.github/workflows/ci.yml`
- `.github/workflows/codeql.yml`

Cache key format: `llvm-${VERSION}-linux-x86_64-v2-${SHA256_SHORT}`.
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
(b) verify the snippet with `./build/ry -c '...'` before committing.

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

*(specific entries to be added as they are encountered)*

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
`buildOkValue` / `buildErrValue` / `buildSomeValue` now call `tryRetainArcSource(inner)`
when `inner->getType() == ptrTy_`. This retains the collection before scope cleanup at
function exit can release the local variable. `tryRetainArcSource` handles three cases:
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

**Source**: #1001 (2026-04-16, design choice)
**Tags**: codegen_stmt, coercion, Result, Ok, Err, annotation, emitVarDecl

**Rule**: When `Err([...])` (or `Ok(...)`) yields a Result struct whose layout does not match the variable's type annotation, fix the mismatch in `emitVarDecl`'s post-hoc coercion chain (`coerceResultType`) rather than threading the annotation down into the Ok/Err constructor emitter in `codegen_call.cpp`.

**Why**: The Ok/Err constructors can be called from many contexts (function arguments, return values, inlined expressions) where the target type is unavailable or ambiguous. Post-hoc coercion at the declaration site is localised, mirrors the existing Option auto-wrap pattern, and is safe because the inactive field of a Result is always zero (never read through the discriminant).

**How to apply**: `coerceResultType(val, dstResTy)` in `codegen_stmt.cpp` — extract discriminant and the matching payload, zero the non-matching payload with `getNullValue`, rebuild via `CreateInsertValue`, then `propagateMeta(val, coerced)`. Return `nullptr` if both payload types differ (genuine type error). Add the same coercion branch to variable reassignment handlers for consistency.

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

### Skill `allowed-tools` must cover all Bash commands the skill body prescribes

**Source**: #1045 (2026-04-16, CodeRabbit review)
**Tags**: skill, allowed-tools, claude-code, ci-investigate, review-feedback

**Rule**: Every Bash command that a SKILL.md step instructs the agent to run must be covered by an entry in `allowed-tools`. A common pitfall is listing only `gh pr:*`/`gh run:*`/`git branch:*` while the skill body also calls `cmake`, `clang-tidy`, `cppcheck`, `scan-build`, `find`, etc. At runtime the agent will be blocked from running those uncovered commands, silently breaking the step.

When the reproduction command set is open-ended (e.g. "run the CI job's corresponding local command"), use `Bash` (unrestricted) rather than a long enumeration of prefixes that will grow stale.

**How to verify**: grep the skill body for bare Bash commands not covered by the `allowed-tools` line.

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

### `inferExprType` / `inferExprTypeName` visitor must handle `IfExpr`/`IfBlockExpr` and ADT constructors (`Ok`/`Err`/`Some`) to infer lambda return type correctly

**Source**: #1024 (2026-04-16, bugfix — lambda if-expr Result branch unification)
**Tags**: codegen, inference, visitor, lambda, Result, IfExpr

**Rule**: `inferExprType` and `inferExprTypeName` in `src/codegen_lambda.cpp` are the visitor functions that determine the return type of an expression-body lambda (`(x: int) => expr`). Both functions must have explicit `if constexpr` cases for every AST node that can appear as the outermost expression. Any unhandled node falls through to the default which returns `i64Ty_` (= `int`) — silently and without error.

Two gaps were confirmed in #1024:
1. **`IfExpr` / `IfBlockExpr`** — neither visitor had a case for these nodes, so any lambda whose body is an `if` expression with mismatched-type branches (e.g., `Ok(T)` vs `Err(E)`) fell through to `int`, baking the wrong return type into the function signature before codegen.
2. **`Ok` / `Err` in CallExpr** — the CallExpr branch already had a `Some` case but not `Ok`/`Err`. Without these, the Ok/Err payload types could not be inferred, making the IfExpr merge logic unreachable even after adding it.

**Result merge logic (IfExpr)**: When both branches of an `if` expression yield `isResultType`, the inferred types are `{i1, realOkTy, Unit}` and `{i1, Unit, realErrTy}` (each side uses `i8Ty_` as the placeholder for the missing payload). Merge by preferring the non-`i8Ty_` element for each of Ok and Err:
```cpp
llvm::Type *mergedOk = (okA == i8Ty_) ? okB : okA;
llvm::Type *mergedEr = (erA == i8Ty_) ? erB : erA;
return getResultType(mergedOk, mergedEr);
```
This produces the correct unified `Result<T, Error>` StructType that both `Ok` and `Err` emitters in `codegen_call.cpp` will reuse via `fn_->getReturnType()`.

**Analogous gap for Option**: `Some`/`None()` in an `if`-expr body has the same structural problem (#1043). The Option merge logic mirrors the Result one but must handle the Option StructType layout instead of `getResultType`.

**How to check for new gaps**: When adding a new ADT constructor or a new expression-level control-flow node, grep for both `inferExprType` and `inferExprTypeName` and verify each has a case for the new node. A missing case is a silent wrong-type inference, not a compile error.

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

**Annotation-driven element suffix (#1079)**: `bs: List<u8> = [97, 0, 98]` now works.
`injectLowLevelSuffix` is shallow — it does not descend into `ListExpr` elements on its own.
When the variable annotation is `List<T>` and `T` is a low-level integer type, the parent
`emitVarDecl` loop in `src/codegen_stmt.cpp` propagates `T` into each element AST node
before calling `emitExpr`. This mirrors the fixed-size array path at line 247-293. The fix
must happen before `emitExpr`; post-emit metadata rescue (line 444-493) cannot repair the
stride because `getListElementType(val)` returns i64 (truthy) and the annotation-fallback
branch is gated on `!elemTy`. Scope: `let`/`var` declarations only — `AssignStmt`
reassignment and inline call-argument paths are not covered.
