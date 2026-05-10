---
paths:
  - "src/codegen_fn*.cpp"
  - "src/codegen_lambda*.cpp"
  - "src/codegen_fn_generic.cpp"
  - "src/codegen_call_higher_order.cpp"
  - "src/codegen_stmt.cpp"
  - "include/ry/codegen.hpp"
---

# Codegen Functions, Generics, and Higher-Order

### `inferExprType` / `inferExprTypeName` visitor must handle `IfExpr`/`IfBlockExpr` and ADT constructors (`Ok`/`Err`/`Some`/`None`/`Error`) to infer lambda return type correctly

**Source**: #1024 / #1043 / #1111 / #1115 / #1116 (2026-04-16 .. 2026-04-18)
**Tags**: codegen, inference, visitor, lambda, Result, Option, IfExpr, anyTy_, Error

**Rule**: `inferExprType` and `inferExprTypeName` in `src/codegen_lambda.cpp` determine an expression-body lambda's return type (`(x: int) => expr`). Both must have explicit `if constexpr` cases for every AST node that can be the outermost expression. An unhandled node falls through to the default `i64Ty_` (= `int`) — silently and without error.

Confirmed gaps (all fixed):
1. **`IfExpr` / `IfBlockExpr`** — neither visitor had a case, so a lambda whose body is `if` with mismatched-type branches (`Ok(T)` vs `Err(E)`) fell through to `int`, baking the wrong return type before codegen. (#1024)
2. **`Ok` / `Err` in CallExpr** — the CallExpr branch had `Some` but not `Ok`/`Err`, so Ok/Err payload types could not be inferred. (#1024)
3. **`Error` constructor in CallExpr** — `Err(Error("msg"))` must infer as `getResultType(i8Ty_, errorTy_)`. Without an explicit `if (c == "Error") return errorTy_` case, the inferred branch type is `{i1, i8, i64}` instead of `{i1, i8, errorTy_}`, and `validateBranchTypes` rejects the StructType pointer mismatch. (#1111)

**Result merge logic (IfExpr)**: When both branches yield `isResultType`, the inferred types are `{i1, realOkTy, Unit}` and `{i1, Unit, realErrTy}` (each side uses `i8Ty_` as the missing-payload placeholder). Unannotated lambda params yield `anyTy_` — must lose to a concrete type but win over `i8Ty_`. Use `preferConcrete`:

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

**`Ok` / `Err` emitters must unwrap `anyTy_` to the expected slot type**: When `fn_->getReturnType()` indicates a concrete ok/err type but `Ok(x)` / `Err(x)` emits with `x` of `anyTy_` (unannotated param), the Result struct contains `anyTy_` for that slot — different from the sibling branch's concrete type, and `validateBranchTypes` pointer-equality fails. Fix: in both emitters (`codegen_call.cpp`), derive expected slot type from `retStructTy->getElementType(1/2)`, then call `unwrapFromAny(inner, expectedSlotTy)` when `isAnyType(inner->getType()) && canAnyHoldType(expectedSlotTy) && expectedSlotTy != i8Ty_`. `canAnyHoldType(errorTy_) = false`, so `Err(Error(...))` is unaffected — gap is limited to `Result<T, primitive>` Err slots. (#1111 Ok; #1116 Err)

**Option merge logic (IfExpr — fixed in #1043, updated in #1115)**: 2-slot `{i1, innerTy}` vs Result's 3-slot. Three gaps beyond the structural #1024 analog:
1. `Some` was missing from `inferExprType` (only in `inferExprTypeName`), so `(x) => Some(x)` failed.
2. `None()` (zero-arg CallExpr) had no emitter — fell through to `findFunction("None")` and failed with `undefined function: None`.
3. `None` in `inferExprType` needed placeholder `getOptionType(i8Ty_)`.

Option merge uses the **same `preferConcrete` rule** as Result (#1115 — `(a == i8Ty_) ? b : a` was broken for `anyTy_`); apply to `getOptionType(preferConcrete(innerA, innerB))`. `None()` emitter in `codegen_call.cpp` reads `fn_->getReturnType()` to derive inner (like `Err` does for the Ok slot).

**`Some` emitter must also unwrap `anyTy_` (mirrors `Ok` emitter, #1115)**: After `preferConcrete` sets the function return type to `Option<int>`, `Some(x)` with unannotated `x` still emits `anyTy_` → `{i1, anyTy_}` while `Some(0)` produces `{i1, int}`. Fix: derive `expectedInnerTy` from `retStructTy->getElementType(1)`, then `unwrapFromAny(inner, expectedInnerTy)` when `isAnyType(inner->getType()) && canAnyHoldType(expectedInnerTy)`. Both fixes must bundle: `preferConcrete` alone converts a silent wrong value into a compile error without fixing correctness.

**Parser trap — block-form branches with `Some(...)`/`None()` tail**: In `IfBlockExpr` (colon-indent body), bare `Some(x)` / `None()` on the last line parses as `CallStmt` (discarded), not return value. Wrap in parens to produce an `ExprStmt`:

```ry
f = (x: int) => if x > 0:
  (Some(x * 2))   # ← parens required; bare Some(x * 2) would be a discarded CallStmt
else:
  (None())
```

Same rule as `IfBlockExpr` tested in `tests/spec/option_lambda_if.test.ry`.

**How to check for new gaps**: When adding a new ADT constructor or expression-level control-flow node, grep both `inferExprType` and `inferExprTypeName` and verify each has a case. A missing case is a silent wrong-type inference, not a compile error.

### `isNoneLiteral` must match all three None syntactic forms: `None`, `none`, `None()`

**Source**: #1099 (2026-04-17)
**Tags**: codegen, Option, None, isNoneLiteral, let-decl, reassignment, emitVarDecl, CallExpr

**Rule**: `None`, `none`, and `None()` are semantically identical Option literals. `isNoneLiteral` in `src/codegen_type.cpp` must recognise all three: `NoneExpr` (keyword `none`), `VariableExpr{name:"None"}` (bareword), `std::unique_ptr<CallExpr>{callee:"None", args.empty()}` (call form).

`None()` was introduced in #1043 for lambda if-expr unification. Its emitter uses `fn_->getReturnType()` — correct in return-stmt contexts, wrong in let-decl/reassignment where the enclosing function is unrelated to the variable's annotation. Three sites use `isNoneLiteral` to short-circuit into annotation-aware `buildNoneValue(annotTy)`:
- `src/codegen_stmt.cpp` `emitVarDecl` (line ≈204) — let-decl short-circuit
- `src/codegen_stmt.cpp` local reassignment (line ≈845)
- `src/codegen_stmt.cpp` module-global reassignment (line ≈1020)

**How to apply**: When adding a new syntactic form of `None`, extend `isNoneLiteral` first — all three sites inherit the fix. The `None()` emitter in `codegen_call.cpp` is a fallback for contexts without annotation, not a substitute for `isNoneLiteral`. Use `std::get_if<std::unique_ptr<CallExpr>>` (not `std::get_if<CallExpr>`) because `CallExpr` is stored as `unique_ptr` in `ExprNode`.

### Hint channel for `None()`/`none` inner type in branch-merge and lambda-call argument positions

**Source**: #1154 / #1179 / #1186 (2026-04-18 .. 2026-04-19)
**Tags**: codegen, Option, None, NoneExpr, branch-merge, IfExpr, CaseExpr, hint-channel, RAII, lambda-call, record-constructor

**Rule**: `None()` / `none` in branch-merge positions (`if cond => None() else Some(42)`) previously defaulted to `Option<i8>` / `Option<i64>` because emitters only consulted `fn_->getReturnType()`. The fix is a two-field class-state hint channel:

- **`option_none_hint_inner_: llvm::Type*`** — arm hint, read directly by `None()` and `NoneExpr` emitters (nullptr = no hint).
- **`option_decl_annotation_inner_: llvm::Type*`** — declaration-context annotation inner, written by `emitVarDecl`/local-reassign/global-reassign via `DeclAnnotationInnerGuard`. **NOT read by None()/NoneExpr emitters directly** — only used as fallback in IfExpr/IfBlockExpr guards so it cannot leak into arbitrary sub-expressions (e.g., `foo(none)`).
- `OptionNoneHintGuard` / `DeclAnnotationInnerGuard` RAII save/restore the respective fields.
- `computeBranchOptionInnerHint(a, b)` in `codegen_lambda.cpp` runs `inferExprType` on both arms and picks the more-concrete inner via `preferConcrete`.
- **Annotation seed** at three `codegen_stmt.cpp` sites (`emitVarDecl`, local reassign, module-global reassign): writes `option_decl_annotation_inner_` from the declared `Option<T>` inner before `emitExpr(RHS)`.
- **IfExpr / IfBlockExpr pre-scan** in `codegen_expr.cpp`: computes hint **before** condition emit, installs `OptionNoneHintGuard` **after** condition emit. Fallback uses `option_decl_annotation_inner_`. **Critical**: the guard must install after the condition because `none`/`None()` in the condition must not inherit the arm hint.
- **`NoneExpr` added to `inferExprType` and `inferExprTypeName`**: returns `getOptionType(i8Ty_)` / `"Option"` so `computeBranchOptionInnerHint` detects it as Option placeholder.
- **CaseExpr (pattern match) uses scrutinee-based hint** in `codegen_match.cpp`: `Some(v)` arm values reference pattern-bound variables not in scope at pre-scan time, so `computeBranchOptionInnerHint` cannot apply. Use scrutinee type — if `Option<T>`, install inner `T` as `OptionNoneHintGuard` before any arm is emitted. Fallback `option_decl_annotation_inner_`.
- **CaseCondExpr (`when cond => value`) uses pre-scan**: no pattern variables, so `computeBranchOptionInnerHint` on first arm + `else_expr` is safe. Guard installed inside each value emit block to prevent hint leaking into condition.
- **Lambda variable (indirect) call arguments** in `codegen_call_dispatch.cpp` (#1179): per-arg `OptionNoneHintGuard` with inner from `FnTypeInfo::paramTypes[i]` — only when `isOptionType`. Hint source is the **callee signature** (not `option_decl_annotation_inner_`), preserving the disjoint-channels invariant. Non-Option params pass `nullptr`.
- **Record/struct constructor field positions** in `codegen_expr_literal.cpp::emitRecordConstructor` (#1186): per-field `OptionNoneHintGuard` from `info.llvmType->getElementType(i)` — only for Option fields. Covers inherited fields via `allFields` pre-flattening (parent fields at lower indices). Non-Option fields pass `nullptr` (correctly clears outer hint when emitting e.g. `Outer(Some(UserWithAvatar("carol", None())))` so nested `None()` inherits `UserWithAvatar.avatar`'s `str`, not `Outer.inner`'s `UserWithAvatar`).

**Design invariant**: `option_decl_annotation_inner_` does NOT flow directly into argument positions. Argument-position hints come from callee-declared param types (lambda call), record-declared field types (constructor), or are absent (regular `fn` calls use `resolveOverload`'s `isNoneLiteral` short-circuit). This prevents LHS declaration annotations from contaminating arbitrary sub-expressions.

**Priority invariant**: `isNoneLiteral` short-circuit at the three annotation-aware sites must remain the *first* check (before hint seed). It handles trivial `x: Option<int> = None()` without touching hint state.

**ADR #1111 compliance**: hint is threaded via CodeGen class state + RAII guards, not via `emitExpr` signature changes.

### `instantiateGenericEnum` omitted `variantOrder` population

**Source**: #959 (2026-04-15)
**Tags**: codegen, generic-enum, ADT, variantOrder, regression

**Rule**: `instantiateGenericEnum` in `src/codegen_fn_generic.cpp` builds `EnumInfo` for concrete instantiations (e.g., `MyOpt<int>`). It populates `info.variants` (`unordered_map<name → tag>`) but historically did NOT populate `info.variantOrder` (the ordered name vector). Code that iterates `variantOrder` — such as the payload-aware ADT equality comparison from #959 — silently iterates zero elements, falling through to the "no payload" fast path with wrong results (tag-only comparison).

**Why**: The non-generic path in `codegen_stmt_misc.cpp` populates `variantOrder` inside the same loop as `variants`. The generic path duplicated most of the loop body but missed this line.

**How to apply**: Any new code reading `EnumInfo::variantOrder` for logic that must apply to generic instantiations must verify `instantiateGenericEnum` keeps `variantOrder` in sync with `variants`. When adding `EnumInfo` fields, update both `codegen_stmt_misc.cpp` and `codegen_fn_generic.cpp` together.

### Resolving generic enum types as fn param/return/let requires a two-layer fix

**Source**: #1203 (2026-04-19)
**Tags**: codegen, generic-enum, resolveType, type_param_scope_, ensureEnumInstantiated

**Rule**: `CodeGen::resolveType` alone is not sufficient for generic enum types in type positions. Two independent mechanisms must combine:

1. **Substitute type-param names inside `<...>` at `resolveType` entry.** Old code did bare-identifier lookup in `type_param_scope_`, so `resolveType("MyOpt<T>")` fell through to `unknown type` during generic-fn instantiation even with `T` bound. Fix: walk the type string recursively — bare name, `weak X`, `X?`, `Base<Args...>` — substituting any bare identifier in `type_param_scope_`, then re-call `resolveType`. See `substituteTypeParamsInName` in `src/codegen_type.cpp`.
2. **Call `ensureEnumInstantiated(typeName)` before the final `unknown type` error.** Even fully-qualified `MyOpt<int>` in a non-generic fn signature does not hit instantiation automatically — `resolveType` only queries `enum_types_`, populated on construction not on type-position mention. The `ensureEnumInstantiated` fallback makes type positions uniform with expression positions.

The two failure modes look similar (both `unknown type: MyOpt<...>`) but have different root causes — fixing only one leaves half the issue.

**How to apply**: When adding a new type-position site, ensure both layers flow correctly. In `instantiateGenericFn`, surface-name strings used for pattern-matching metadata (`paramTypeNames`, `exposedReturnTypeName`) must also pass through `substituteTypeParamsInName` — substituting only `llvm::Type*` is insufficient because downstream code recovers surface name from metadata to decide dispatch.

### Lambda / function return paths need bidirectional `any ↔ narrow` coercion (#1062)

**Source**: #1062 (2026-04-17)
**Tags**: codegen, lambda, return-type-annotation, any-type, asymmetric-coercion, type-coercion

**Rule**: `reduce([1,2,3,4,5], (a, b) -> int => a + b)` produced `lambda return type mismatch: expected 'int', found 'any'`. Untyped lambda params default to `any`, so `a + b` evaluates via `emitAnyBinaryOp` and returns `any`. The `-> int` annotation resolves to `i64Ty_`. Only `any ← narrow` (via `wrapInAny`) was handled; `narrow ← any` (`unwrapFromAny`) was missing in both return codegen sites.

Fix: inverse coercion guard at two sites, symmetric with the existing `wrapInAny` branch:
```cpp
// src/codegen_lambda.cpp (expression-body) and src/codegen_fn.cpp (block-body + regular fn return)
else if (isAnyType(val->getType()) && !isAnyType(retTy) && canAnyHoldType(retTy))
    val = unwrapFromAny(val, retTy);
```
`unwrapFromAny` emits a runtime tag check; type mismatches (e.g., float list with `-> int`) become runtime errors. `canAnyHoldType` limits to primitives (`i64/f64/i1/ptr`); non-primitive return types still produce a compile error.

Whenever you add a `wrapInAny` (narrow→any) at a return site, audit for the inverse `unwrapFromAny` (any→narrow). Both directions are required — omitting either causes asymmetric behavior between annotated and unannotated lambdas.

**How to verify**: `tests/spec/collections.test.ry` — `reduce`/`fold` with `-> int` and `-> float` annotations on untyped params; block-body lambda variant via lambda variable.

### `fold()` rejects untyped lambda with type-check error (#1061)

**Source**: #1061 (2026-04-16)
**Tags**: codegen, fold, untyped-lambda, type-check

**Rule**: `fold(xs, 0, (a, b) => a + b)` produced `fold() initial value type must match function return type`. Unlike `reduce()` (patched in #1020), `fold()` was not given the same treatment. The type-check validates initial-value type against lambda return type; both untyped params → lambda returns `any`, while `0` is `int`.

Fix (`src/codegen_call_higher_order.cpp:287-290`): insert `wrapInAny(initVal)` before the equality check, mirroring the reduce fix at L247-248:
```cpp
if (isAnyType(info.returnType) && !isAnyType(initVal->getType()))
    initVal = wrapInAny(initVal);
```
The equality check still rejects genuine typed-lambda mismatches like `fold([1,2,3], "hi", (a: int, b: int) => a + b)`.

When applying an untyped-lambda compatibility fix to one higher-order function (e.g., `reduce`), audit all sibling functions with similar emitters (`fold`, `scan`, etc.) for the same gap. Do not assume a fix to one function automatically covers others.

**How to verify**: `tests/spec/collections.test.ry` untyped-lambda fold cases (int seed, float seed, string seed); `tests/test_codegen_fail.cpp::FoldTypedLambdaSeedTypeMismatch` verifies the preserved rejection branch.

### Seed-less reductions over a possibly-empty list must return `Option<T>`, not panic (#1209)

**Source**: #1209 (2026-04-20)
**Tags**: codegen, reduce, fold, higher-order, option, empty-list, api-design

**Rule**: Any reduction without a seed (`reduce`, `min`, `max`) must return `Option<T>` for empty inputs, **not** panic. Reductions with a seed (`fold`) are exempt — the seed makes the empty-list case naturally well-defined and a plain `T` return is correct. Model the Option path on `first`/`last` at `src/codegen_call.cpp:220-290`: empty-check → empty BB with `buildNoneValue` → ok BB with reduction loop + `buildSomeValue` → PHI at merge.

**Rule**: When two near-identical builtins differ only in a parameter (e.g., `reduce(list, fn)` vs `fold(list, init, fn)`), detect the Python/JS-style miscall `reduce(list, init, fn)` at the specific arg count and emit a targeted compile error naming the sibling. Ad-hoc hint strings in dispatch are the existing precedent (`src/codegen_call_user.cpp:17`); there is no "did you mean" infrastructure to reuse. Scope the hint to exactly the wrong count that maps onto the sibling — other arities (0, 4+) keep the generic message.

### `CreateStore(narrowVal, anyTyAlloca)` leaves the `data` field uninitialized

**Source**: #1020 (2026-04-16)
**Tags**: codegen, any, alloca, store-width, reduce, fold

**Rule**: `anyTy_` is a 16-byte struct `{i64 tag, [8 x i8] data}`. Storing a raw `i64`/`f64`/`ptr` primitive into an `any` slot writes only 8 bytes — the store's width is the value's width, not the alloca's. The tag (offset 0) ends up holding the raw value; `data` is stack garbage. Reloading as `anyTy_` then dispatching to `__ry_any_*` routes on the garbage tag → silent wrong answer.

The `reduce` emitter hit this when its lambda had untyped params (parser defaults to `any`), making `info.returnType = anyTy_` while the first list element is still raw `elemTy`. Loop-body `elem` values went through `coerceCallArgs` → `wrapInAny` and were fine; only the **accumulator seed** skipped coercion.

Before `CreateStore(v, accSlot)` where `accSlot` has type `anyTy_`, coerce `v` via `wrapInAny(v)` (or `coerceCallArgs`-style widening). Equivalently: never rely on `CreateStore` whose source type is narrower than the alloca's allocated type. Grep `CreateAlloca(info.returnType, ...)` / `CreateAlloca(anyTy_, ...)` followed by `CreateStore(...)` — each pair must guarantee the stored value matches the alloca's full type.

**How to verify**: `tests/spec/collections.test.ry` has untyped-lambda cases for `reduce` on int list (sum), 2/3-elem lists, and float list. For uninitialized-read detection specifically, use MemorySanitizer (MSan) — ASan detects memory-access errors but not uninit reads.

### `@native` dispatch must run exact-match Pass 1 before widening Pass 2

**Source**: #1193 (2026-04-19)
**Tags**: codegen, stdlib, overload, widening, coercion, native-dispatch, resolveOverload

**Rule**: User-defined Ry functions accept implicit `int → float` widening via `CodeGen::resolveOverload`. Pre-#1193, `@native` dispatch used strict `args[i]->getType() != expectedTy` — so `sqrt(4)` errored even though `takesFloat(4)` worked. All three `@native` dispatch sites in `src/codegen_call_native.cpp` (`emitTableDrivenNativeCall` fixed-arity + variadic, `emitGenericNativeCall`) now run two-pass:

1. Pass 1: exact type match (unchanged behavior).
2. Pass 2 (only if Pass 1 is empty): accept args via `isWideningConversion` / `emitWideningConversion` (`src/codegen.cpp:761-789`).

Any new `@native` dispatch path — or any edit to the three existing ones — must keep the two passes in this exact order. Exact matches must win across the entire overload set before any widening, so `pow(2, 3)` continues to dispatch to `(int, int) -> int` rather than silently promoting to `(float, float) -> float`. Reuse `CodeGen::isWideningConversion` / `CodeGen::emitWideningConversion`; do not re-implement `SIToFP` inline.

**Rejected alternative**: collapsing the passes into a single scored loop (like `resolveOverload`) changes precedence semantics when an exact match and a widening-eligible match coexist; the two-pass split makes the invariant explicit and localises the change.

**How to apply**: For each candidate, track `std::vector<bool> needsWidening` alongside `paramLLVMTypes`. Before `CreateCall`, loop over matched args and call `emitWideningConversion` where `needsWidening[i]` is true. The "no matching overload" error path is unchanged — falling through both passes lands in the original `"argument N requires <type>"` message.

Custom emitters must replicate locally: immediately after `emitExpr` of each arg, call `isWideningConversion` + `emitWideningConversion` before the hardcoded `x->getType() != cg.f64Ty_` guard. For emitters with two exact-type branches (`emitMathPow`'s `(f64, f64)` and `(i64, i64)`), append the widening branch *after* both exact branches so `pow(2, 3) → int 8` is preserved. See `emitMathFloorCeilRound` / `emitMathLog` / `emitMathPow` in `src/codegen_call.cpp`.

**Resolved follow-up**: Math custom emitters (`emitMathFloorCeilRound`, `emitMathLog`, `emitMathPow` mixed-type) accept widened `int` args via the local pattern (#1230, 2026-04-21). `emitMathAbs` was already correct — both `abs(int)` and `abs(float)` overloads are declared in `share/std/math/math.ry` and the emitter exact-matches on both LLVM types.

### IndexExpr first index is scalar only — RangeExpr routes to emitListSlice

**Source**: #1184 (2026-04-19)
**Tags**: codegen, IndexExpr, RangeExpr, slice, index-type-guard, emitExprVariant, emitListSlice

**Rule**: `emitExprVariant(IndexExpr)` (`src/codegen_expr_literal.cpp`) assumes `emitExpr(*e->indices[0])` produces an i64 (or map key type). `lst[a..b]` parses as `IndexExpr{ object: lst, indices: [RangeExpr{a,b}] }` (via `parseConditional` → `parseRange`) — without a type-guard the `RangeExpr` emitter materialises an ARC-allocated `List<i64>` header (`ptr`), which flows into `emitBoundsCheck` / GEP, producing `ICmp ptr vs i64` IR verifier errors.

Fix: before `indexValues` emit loop, check:
```cpp
if (e->indices.size() == 1 &&
    std::holds_alternative<std::unique_ptr<RangeExpr>>(e->indices[0]->data)) {
    // reject str / map; get list elemTy; emitExpr start/end directly (i64);
    // emitNegativeIndexWrap each bound; endExcl = end + 1; emitListSlice(...)
}
```

**Semantics**:
- `lst[a..b]` is **inclusive** on both ends (matches `..` operator semantics per `docs/reference/operators.md`)
- Negative bounds **wrapped** against list length (matches `lst[-1]` behaviour via `emitNegativeIndexWrap`)
- Out-of-bounds bounds **clamped** (matches `slice()` behaviour)
- Equivalent to `slice(lst, a, b + 1)` — the inclusive→exclusive `+ 1` is at the call site, not inside `emitListSlice`
- Non-list receivers (`str`, maps, fixed-length arrays) emit `codegenError` in the new guard

**Related helpers**:
- `emitListSlice(listPtr, startVal, endExclVal, elemTy)` (`src/codegen_call_collection.cpp`) — shared between `slice()` builtin and `lst[a..b]`. Clamps internally.
- `emitNegativeIndexWrap(idx, len, prefix)` (`src/codegen_call_user.cpp:645`) — use prefix `"ri_start"` / `"ri_end"` to avoid label collision with scalar `"index"`.
- **#1198**: `emitCollOp_slice` was missing pre-wrap (negatives passed raw to `emitListSlice`, which clamps to `[0, len]` → `-3` collapsed to `0`). Fixed by `emitNegativeIndexWrap(start, len, "sl_start")` / `emitNegativeIndexWrap(end, len, "sl_end")` before the call. Invariant: caller pre-wraps, callee clamps.
- **#1199**: `emitStrOp_substring` had the same 0-clamp bug on strings. Fixed by mirroring slice, with one extra: `wrapBase` must be UTF-8 codepoint count via `__ry_utf8_len_n(s, byteLen)`, not `emitStringByteLen(s)` (`"あいうえお"` is 5 chars / 15 bytes — byte length wraps wrong). See "String negative-index wrap uses UTF-8 char count, not byte length" entry below.

### Self-referential enum inline fields emit a wrapper-type diagnostic

**Source**: #1203 (2026-04-19)
**Tags**: codegen, enum, self-reference, diagnostic, user-experience

**Rule**: In `emitStmt(EnumStmt &s)` (`src/codegen_stmt_misc.cpp`), a pre-pass walks each variant field's `TypeNode` AST recursively via the file-local helper `containsInlineSelfReference`. Reports self-reference when the enum's own name appears in any of: bare identifier (`Tree`), generic application's base (`LList<T>`), inner of optional (`Tree?`), tuple element (`(int, Tree)`), array element, union component, arbitrary non-indirection generic (`Option<Tree>`, `Pair<Tree, Tree>`). The helper treats `List<_>`, `Map<_, _>`, `Set<_>`, `Task<_>`, `Channel<_>`, `weak T`, `fn(...)` as pointer-backed indirection and stops descending — payload is boxed, layout is finite. Recommendation message points to container indirections: `List<T>`, `Map<K, V>`, `Set<T>`, `Task<T>`, `Channel<T>` (#1351). Excludes `weak T` (requires ARC-managed payload; separate diagnostic rejects `weak <ADT>`) and `fn(...)` (unusual data storage choice).

**Why**: An earlier substring check on the field's `toString()` only caught the bare name and generic base (`ftStr.substr(0, ftStr.find('<'))`), missing `Tree?`, `(int, Tree)`, `Option<Tree>` — these produced cryptic `unknown type: Tree` at field-type resolution instead of the helpful diagnostic. The AST walk fixes every wrapping shape uniformly and runs before generic-template save so `generic_enum_templates_` never stores a self-referential template that would crash at instantiation with `unknown type: T`. Full recursive-enum support (auto-boxing) is a separate follow-up.

**How to apply**: When adding a new indirection wrapper (e.g. `Rc<T>`, `Box<T>`) that stores payload behind a pointer, extend the indirection list in `containsInlineSelfReference` *and* the recommendation string. New non-indirection wrapper: nothing needs to change — default branch traverses generic type-args. Supporting inline self-reference would require removing or relaxing this pre-pass.

### Bare generic-enum name in type positions produces a dedicated diagnostic

**Source**: #1203 (2026-04-19)
**Tags**: codegen, generic-enum, resolveType, diagnostic, user-experience

**Rule**: `CodeGen::resolveType` distinguishes three failure modes for generic enums:

1. `MyOpt` (bare template, no `<...>`) → `generic enum 'MyOpt' used without type arguments; write MyOpt<T>...` (echoes the template's actual first type-parameter name, not hardcoded `T`).
2. `MyOpt<int>` (concrete) → on-demand `ensureEnumInstantiated` before the final `unknown type` error, so the enum is instantiated lazily at any type position.
3. `MyOpt<T>` with `T` bound in `type_param_scope_` → `substituteTypeParamsInName` rewrites to `MyOpt<int>`, then case 2 kicks in.

Pre-#1203 both `fn f(opt: MyOpt, ...)` and `fn f<T>(opt: MyOpt<T>, ...)` produced `unknown type: ...` with no hint. Case 1 now guides the user; cases 2 and 3 compile silently.

**How to apply**: When adding a new generic-template registry (alongside `generic_enum_templates_`), mirror the bare-name check in `resolveType` so users see a template-name-specific error instead of falling through to generic `unknown type: <name>`. The first type-parameter name is read from the template record — do not hardcode `T`.

### Lambda return-type inference must consult `@native` signatures, not just user-function overloads

**Source**: #1318 (2026-04-23)
**Tags**: codegen, lambda, type-inference, native, stdlib, resource, JsonValue

**Rule**: `inferExprType()` / `inferExprTypeName()` for `CallExpr` must check the `@native` signature registry (`native_fn_sigs_` + `native_lib_index_`) when `findFunction()` misses. Many stdlib calls (`lockNew()`, `json.parse()`) are registered only as `@native` signatures, so falling straight to scalar fallback (`i64Ty_` / `"int"`) makes lambda return-type checking report nonsense like `expected 'int'` for `fn() -> Lock` or `fn() -> Result<JsonValue, Error>`.

**How to apply**: Keep lambda inference in sync with native-call dispatch — match `@native` overloads by arity and inferred argument types, allow the same implicit widening tier as normal call resolution, and return the matched signature's declared Ry return type name. Without name-level lookup, metadata-gated returns (`Result<JsonValue, Error>`, resource handles, function-typed returns) may compile to the right LLVM type later but fail or lose metadata during lambda pre-checking.

### `inferNativeCallReturnTypeName` must narrow overloads by source-level type name, not LLVM type

**Source**: #1349 (2026-04-24)
**Tags**: codegen, lambda, type-inference, native, overload, ptr-collapse, captured-vars

**Rule**: When narrowing `@native` overloads inside lambda return-type inference, compare `sig.params[i].typeName` against `inferExprTypeName(arg)` (source-level Ry names), not `argTypes[i] == resolveType(sig.params[i].typeName)` (LLVM types). Ptr-backed types (`str`, `List`, `Map`, `Set`) all resolve to the same `ptr` under opaque pointers, so every overload appears to match and the ambiguity guard fires. Companion bug: captured variables in lambda / nested-fn bodies need source-level collection metadata (`list_elem_type_name` / `map_key_type_name` / `set_elem_type_name` / `union_value_type` / `enum_value_type`) propagated from the outer alloca — only `fn_type_info` was being forwarded, so dispatch inside the body misrouted `length(xs)` to the str runtime even after the type-inference fix landed.

**How to apply**: In `inferNativeCallReturnTypeName` (`src/codegen_lambda.cpp`), build `argTypeNames` via `inferExprTypeName()` alongside `argTypes`, match by `resolveTypeAlias(argTypeNames[i]) == resolveTypeAlias(sig.params[i].typeName)` first, fall back to LLVM-type equality only when source-level name is empty. Keep `isInferenceWidening` — its `isLowLevelTypeName` gate prevents misfire on ptr-backed types. Thread a `paramTypeNameMap` default parameter so `inferExprTypeName::CallExpr` can forward the outer name map. For captured variables: save outer `llvm::AllocaInst*` in `CaptureAnalysisResult::capturedSrcAllocas` during `analyzeFreeVariables`, and in both `emitExprVariant<LambdaExpr>` and `emitStmt<FnStmt>` captured-param setup call `propagateMeta(srcAlloca, newAlloca)` after `CreateStore` — this copies the full metadata packet (collection types, type-name strings, `fn_type_info`, resource kinds, ARC-managed flag) in one shot.

### Higher-order Result/lambda boundaries must rehydrate ptr-backed metadata from type names or wrappers

**Source**: #1319 (2026-04-23)
**Tags**: codegen, metadata, lambda, Result, JsonValue, resource, higher-order

**Rule**: Any higher-order boundary that unwraps a `Result<T, E>` / `Option<T>` payload or calls an indirect lambda must restore metadata for pointer-backed named types. `CreateExtractValue` drops the payload's `ValueMetadata`, and indirect calls return a fresh SSA value with no metadata unless `FnTypeInfo.returnTypeName` is re-applied.

**How to apply**: Mirror the `?` operator path in combinators: after extracting `ok_val` / `some_val`, call `propagateMeta(wrapper, payload)` before passing into the callback. In `emitLambdaCall`, always stamp the call result from `FnTypeInfo.returnTypeName` (and `returnFnTypeInfo` for function-typed returns). For lambda return-name inference on `VariableExpr`, prefer `buildTypeNameFromMeta()` over raw `reverseResolveTypeName()` so resource / `JsonValue` names survive opaque-pointer lowering.

### `@native` first-class materialization: synthesize a thunk that re-enters the call dispatcher

**Source**: #1569 (2026-05-04)
**Tags**: codegen, native, first-class, thunk, FnScope, multi-overload, default-args

**Rule**: To make `@native` stdlib functions usable as first-class values (`let f = toInt`, `xs.map(toInt)`), `emitExprVariant<VariableExpr>` must — *immediately after the user-fn lookup branch and before the `undefined variable` error* — call `materializeNativeThunk(name)`. The helper:
- (a) collects candidate `NativeFnSignature` entries from `native_fn_sigs_` matching the bare name or any `<package>::name` suffix
- (b) rejects multi-overload names with `ambiguous reference to @native function 'X': multiple overloads exist; wrap in a lambda to select one`
- (c) emits an internal LLVM Function `__ry_native_thunk_<name>` whose body is a synthetic `CallExpr{callee=name, args=VariableExpr{p.name}}` routed back through `emitExpr` so existing native dispatch (Pattern A/B/generic + `@native("libname")` dlopen) is reused without duplication
- (d) caches the thunk by name so repeated references share one Function and recursive references resolve mid-emission

**How to apply**:
- **`FnScope` is mandatory** around body emission. The thunk is synthesized lazily from inside the caller's IR builder cursor; without `FnScope`, the synthetic CallExpr's IR lands in the caller's BB and corrupts the surrounding instruction stream. `FnScope` saves/clears/restores `fn_`, `current_function_`, and `IRBuilder` state, then `pushScope()` opens a fresh symbol table.
- **Cache before body emission** (`native_thunk_cache_[name] = thunk;` *before* the synthetic CallExpr) so any self-referential resolution returns the same Function instead of recursing infinitely.
- **Set `FnTypeInfo` metadata via `getOrCreateMeta(thunk).fn_type_info = info;`** with `info.paramTypeNames` from `sig.params[i].typeName` — `lookupFnTypeInfo()` and `emitLambdaCall()` consult these source-level names for higher-order dispatch and Result/JsonValue metadata rehydration. If `resolveTypeAlias(sig.returnTypeName)` is itself a function type, populate `info.returnFnTypeInfo` via `parseFnTypeAnnotation`.
- **Default arguments materialize at full arity.** The thunk's LLVM signature mirrors `sig.params` 1:1; the default-omission shortcut only works on the original direct `CallExpr` path. Document in user-facing docs.
- **Resolution order matters.** The new path activates only when user-fn lookup misses, so user-defined `fn` declarations continue to shadow `@native` imports of the same name without behavior change.

**Why this design**:
- Routing through `emitExpr(CallExpr)` rather than re-implementing dispatch means both bare `@native` and `@native("libname")` work identically — the synthetic call hits the same Pattern A/B/generic chain that handles direct calls. No special-casing for dlopen wiring is needed.
- Multi-overload reject mirrors user-fn behavior at `codegen_expr.cpp:194` (the existing "ambiguous reference" branch); same error class keeps the diagnostic surface consistent.
- Custom-emitter natives (`math.abs`, `math.pow`, `math.round`, `math.log`, etc.) are all multi-overload by construction, so the single-overload rule transparently rejects them as first-class values without an extra "custom emitter" carve-out. Users who need first-class versions wrap them in a lambda (`f = (x) => abs(x)`).
