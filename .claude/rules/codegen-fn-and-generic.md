---
paths:
  - "src/codegen_fn*.cpp"
  - "src/codegen_lambda*.cpp"
  - "src/codegen_call_higher_order.cpp"
  - "src/codegen_stmt.cpp"
---

# Codegen Functions, Generics, and Higher-Order

### inferExprType / inferExprTypeName visitors must handle every expression node, including IfExpr/IfBlockExpr and ADT constructors

Any unhandled node falls through to `i64Ty_` (= `int`) silently, baking the wrong return type into the function signature.

### None has three syntactic forms — isNoneLiteral must match all three

`None` (bareword `VariableExpr`), `none` (`NoneExpr`), and `None()` (zero-arg `CallExpr`) are semantically identical. Missing the `None()` form causes `emitVarDecl` / local-reassign sites to fall back to `fn_->getReturnType()` — wrong when the enclosing function's return type is unrelated.

### instantiateGenericEnum must keep variantOrder in sync with variants

The generic-enum instantiation path missed populating `info.variantOrder`. Code reading `variantOrder` silently iterates zero elements for generic enums and falls through to the tag-only fast path, returning wrong results. When adding new `EnumInfo` fields, update both the non-generic path (`codegen_stmt_misc.cpp`) and the generic path (`codegen_fn_generic.cpp`).

### Generic enum types in type positions need substituteTypeParamsInName plus ensureEnumInstantiated

Both layers are required — fixing only one leaves half the issue. Surface-name strings used for metadata-driven dispatch must also flow through `substituteTypeParamsInName`.

### Lambda / function return paths need bidirectional any ↔ narrow coercion

Untyped lambda params default to `any`, so `(a, b) -> int => a + b` evaluates `a + b` via `emitAnyBinaryOp` (returns `any`) but the annotation says `int`. Apply at both expression-body and block-body return sites: `if (isAnyType(val->getType()) && !isAnyType(retTy) && canAnyHoldType(retTy)) val = unwrapFromAny(val, retTy);`

### Higher-order untyped lambdas: accumulator anyTy_ alloca needs wrapInAny on seed

`anyTy_` is a 16-byte struct. `CreateStore(narrowVal, anyTyAlloca)` writes only 8 bytes, leaving `data` uninitialized; the tag holds the raw value and dispatch routes on garbage. When fixing one higher-order fn (`reduce`), audit all siblings (`fold`, `scan`). Use MSan, not ASan, to detect this regression — ASan does not flag uninitialized reads.

### Seed-less reductions on possibly-empty lists must return Option, not panic

`reduce(list, fn)` (no seed), `min`, `max`, and similar must return `Option<T>` and emit `buildNoneValue` on the empty branch / `buildSomeValue` on the populated branch with a PHI at merge.

### @native dispatch: exact-match Pass 1 before widening Pass 2

All three `@native` dispatch sites run two-pass overload resolution: Pass 1 exact type match, Pass 2 widening. Order matters — `pow(2, 3)` must dispatch to `(int, int) -> int` rather than silently promoting to `(float, float) -> float`. User-defined generic fns follow the same two-pass shape. Widening is honored only at top-level `BasicType` concrete leaves; recursive inner positions (`List<int>` does NOT accept `List<u8>`) always disable the flag.

### Lambda return-type inference must consult @native signatures and use source-level type names; propagate full captured-var metadata

`inferExprType` / `inferExprTypeName` for `CallExpr` must check `native_fn_sigs_` + `native_lib_index_` when `findFunction()` misses. Overload narrowing must compare source-level Ry names, NOT LLVM types — under opaque pointers `str` / `List` / `Map` / `Set` all resolve to `ptr` and LLVM-type comparison declares every overload ambiguous.

### Named-fn capture analysis gates on `!isTopLevelContext()`, not `fn_nesting_depth_ > 0`; lambdas keep by-value capture of top-level bindings

**Tags**: codegen, closure, capture, named-fn, lambda, module-global, top-level, isTopLevelContext, scope_stack_, asymmetry

The module-global skip in `analyzeFreeVariables` uses identity comparison (`findModuleGlobal(name)->original_alloca == alloca`), not name comparison — a real local shadowing a module-global name is still captured correctly. The asymmetry is intentional and documented: `skipModuleGlobals=true` for named fns (write-through via `__ry_modvar_<name>` trampoline), default `false` for lambdas (by-value capture semantics per `docs/reference/functions.md`). Do NOT unify — `CodeGenTest.CapturedVarReassignError` / `OuterReassignAfterCaptureOK` / `CapturedRecordFieldAssignOK` cover the lambda side; erasing the distinction breaks write-through for named fns inside loops.
