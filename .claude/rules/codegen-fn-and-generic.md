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

### Lambda return-type inference must consult `@native` signatures, not just user-function overloads

**Source**: #1318 (2026-04-23, implementation). **Tags**: codegen, lambda, type-inference, native, stdlib, resource, JsonValue

**Rule**: `inferExprType()` / `inferExprTypeName()` for `CallExpr` must check the `@native` signature registry (`native_fn_sigs_` + `native_lib_index_`) when `findFunction()` misses. Many stdlib calls such as `lockNew()` and `json.parse()` are registered only as `@native` signatures, so falling straight to the scalar fallback (`i64Ty_` / `"int"`) makes lambda return-type checking report nonsense like `expected 'int'` for `fn() -> Lock` or `fn() -> Result<JsonValue, Error>`.

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

