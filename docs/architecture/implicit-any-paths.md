# Implicit `any` Creation and Propagation — Path Inventory

This document inventories every place where the Ry compiler currently creates or propagates a value of type `any` **without an explicit user decision**, and classifies each path as **keep / warn / deprecate / remove**. It is the v0.0.30 design deliverable of issue #2320 and the survey that the follow-up migration issues (#2321 implicit-unwrap deprecation, #2322 strict-any default flip, #2323 unannotated-param deprecation, #2324 stdlib-docs migration) consume.

The inventory is restricted to **implicit** paths — i.e. cases where the user did not write `any` or call an API whose signature returns `any`. Explicit `any` sources (`x: any = ...`, `from ry.json import load`, FFI return types, heterogeneous-return `@native` declarations using `any` as the placeholder per `.claude/rules/docs-reference-conventions.md`) are out of scope: those are legitimate dynamic-boundary uses and are addressed by #2324 if they appear in stdlib examples.

## Classification summary

| Class      | Paths             | Meaning |
|------------|-------------------|---------|
| keep       | 2, 3, 4, 5, 6, 7, 8 | Behavior is correct as-is; no migration planned. |
| warn       | —                 | Empty: any path whose removal is committed is classified **deprecate** instead. No path is intended to remain warn-only. |
| deprecate  | 1, 9, 10          | Currently warned (or partially warned); slated for removal via a tracked follow-up issue. |
| remove     | —                 | Empty: removals happen in #2321 / #2322 / #2323, not in this inventory. |

## Path table

| # | Path | Main source site(s) | Current behavior | Class | Follow-up | Tests |
|---|---|---|---|---|---|---|
| 1 | Unannotated function parameter | `src/parser/parser_decl.cpp:174` defaults to `TypeNode::makeBasic("any")` with `has_explicit_type=false`. Lint Pattern 3 in `src/codegen_fn.cpp:755-766`. | Warns under `enableAnyUsageLint` (i.e. `ry run`/`ry test`) with `"... has no type annotation and defaults to 'any'"`. Silent for non-user files and for in-process C++ unit tests. | **deprecate** | #2323 | `tests/test_any_usage_warnings.cpp` `Pattern3*`; `tests/spec/any.test.ry:43` "should accept int as param" block. |
| 2 | Unannotated lambda parameter | `src/parser/parser_expr.cpp` paren-form and bare-form lambda parsers — both default to `TypeNode::makeBasic("any")`. | Silent. Explicitly suppressed by design (see `tests/test_any_usage_warnings.cpp` `Pattern3LambdaUnannotatedSuppressed` — "Unannotated lambda params are idiomatic"). | **keep** | — | `tests/test_any_usage_warnings.cpp` `Pattern3LambdaUnannotatedSuppressed`; `tests/spec/any.test.ry:39-42` "should accept lambda without type annotation". |
| 3 | Omitted return type annotation | `src/codegen_fn.cpp:815-839` body-scan inference via `collectReturnTypes` + `deduceReturnType` (`src/codegen_lambda.cpp`). Lambda equivalent in `src/codegen_lambda.cpp`. | Inference: `Unit` for no-return, the unique LLVM type for homogeneous, a union type for heterogeneous. Silent throughout. | **keep** | — | `tests/spec/any.test.ry:368-374` "any default return type"; `tests/spec/any_fn_return.test.ry`. |
| 4 | Heterogeneous list literal `[1, "x", true]` | `src/codegen_expr_literal.cpp:459` same-type check. | Hard `codegenError("list elements must all have the same type")` whenever the literal-any hint is not set — i.e. when there is no LHS annotation, or when the LHS annotation is a concrete-typed collection. The check is bypassed only by an explicit `List<any>` / `Map<str, any>` / `Set<any>` annotation. | **keep** | — | The rejection is exercised via the typed-annotation form (`xs: List<int> = [1, "x"]`) in `tests/test_codegen_stmt.cpp:3172` `ListElementTypeMismatchUnderTypedAnnotationRejected`; the positive `List<any>` auto-wrap is exercised in `tests/spec/any.test.ry:562` "should accept mixed element types in List<any> literal". |
| 5 | `List<any>` literal element auto-wrap | `src/codegen_expr_literal.cpp` — `computeLiteralAnyHintFromAnnot` sets `list_elem_any`, then each element flows through `wrapInAny`. | Silent wrap. Engaged only when LHS annotation is `List<any>`. | **keep** | — | `tests/spec/any.test.ry:562` "should accept mixed element types in List<any> literal". |
| 6 | `Map<str, any>` literal value auto-wrap | Same mechanism: `map_value_any` hint. Map key `any` is intentionally rejected. | Silent wrap on values; compile error on `Map<any, V>` mixed keys. | **keep** | — | `tests/spec/any.test.ry:468-470` "should accept mixed value types in Map<str, any> literal"; key-side guard in `tests/test_codegen_stmt.cpp:3187`. |
| 7 | `Set<any>` literal element auto-wrap | Same mechanism: `set_elem_any` hint. | Silent wrap, engaged only on explicit `Set<any>` annotation. | **keep** | — | `tests/spec/any.test.ry:641` `s: Set<any> = {1, "x", true}`. |
| 8 | Overload resolution fallback | `src/codegen_call_user.cpp` `RankedCandidate` scoring with `anyMatches` as the lowest-priority tiebreaker. | No fallback. Concrete overloads always win over `any`-typed overloads. If no overload matches, the compiler raises `codegenErrorNoMatchingOverload`. | **keep** | — | `tests/spec/any.test.ry:234-242` "overload resolution: concrete preferred over any". |
| 9 | Implicit `any` → concrete unwrap | Variable decl: `src/codegen_stmt.cpp` `emitImplicitUnwrapDiag` (`[[noreturn]]`). Function-call arg (incl. default-value path): `src/codegen_call_user.cpp:192-194`, `:220-222`. Lambda-call arg: `src/codegen_call_dispatch.cpp` `coerceCallArgs`. `Ok(any)`/`Err(any)`/`Some(any)` into typed `Result`/`Option` slots: `src/codegen_call.cpp`. | All four sub-cases reject as `[strict-any/any-implicit-unwrap]` hard errors via `CodeGen::strictAnyError`. The runtime tag check inside `unwrapFromAny` is still reached by other entry points (return-type coercion, Result/Option case destructure) but is no longer entered from the four Path 9 sites. | **rejected** | #2321 (warn + strict reject) → #2322 (default flip, opt-in removed) | `tests/test_strict_any.cpp::AnyImplicitUnwrap*RejectedByDefault` pins each sub-case. |
| 10 | Direct operators on `any` | `src/codegen_expr.cpp:1745-1758` binary path; `src/codegen_expr.cpp:311-322` unary `-`. Rule covers arithmetic plus ordering comparisons via `CodeGen::isAnyArithOp` (`src/codegen_any.cpp:17-25`). | Arithmetic `+ - * / % // **`, unary `-`, and ordering `< <= > >=` are `[strict-any/any-arithmetic]` hard errors. `==` / `!=` remain allowed (`__ry_any_eq` returns 0 on type mismatch). | **rejected** | #2316 (warn) → #2322 (default flip, ordering ops added to the rule) | `tests/test_strict_any.cpp::Any{Arith,Ordering*,UnaryAnyArith,CompoundAnyArith}RejectedByDefault`; the legacy `any arithmetic operators` describe block in `tests/spec/any.test.ry` was rewritten to the `asType[T]` recovery pattern in #2322. |

## Notes

### Path 9 spec coverage after #2322

The original `tests/spec/any.test.ry` blocks at lines 207–272 ("any unwrap in function call / lambda / Result slot") and at lines 416–454 ("any to concrete conversion") tested the compat-mode runtime unwrap. After #2322 promoted strict-any to the compiler default, those compat-mode entries are unreachable from canonical source. The spec file now exercises the canonical `case asType[T](v): Ok(x): ...` recovery pattern instead. Per-site rejection is pinned by `tests/test_strict_any.cpp::AnyImplicitUnwrap*RejectedByDefault`; no separate CI mode is needed.

### Path 3 defensive fallback

`src/codegen_lambda.cpp` contains a `reverseResolveTypeName` fall-through that returns `"any"` for LLVM types it cannot name. This is a defensive branch — current Ry surface syntax does not produce types that reach it (tuples and closures are already handled by named arms). The inventory documents the site but does not add a test, since constructing an unnameable type from the user-facing language is not currently possible. If a future feature exposes such a type to a return-position inference site, a test should accompany it.

### Explicit `any` sources (out of scope)

The following return-`any` patterns are **explicit** API choices, not implicit creation, and therefore fall outside this inventory:

- `from ry.json import load` and `from ry.json5 import load` — typed APIs whose declared return type is `Result<any, Error>`. The user chose the dynamic-data boundary by importing them.
- FFI / `@extern` return types declared as `any`.
- Heterogeneous-return `@native` functions (e.g. `threadSpawn` / `threadJoin`) that use `any` as the declaration placeholder per `.claude/rules/docs-reference-conventions.md`. The actual dispatch is driven by the custom emitter; the `any` in the declaration is a hygienic stand-in for an unbound `T`, not a true type-erasure.

These are classed **keep** by category — they represent the intended use of `any` as a dynamic boundary. Migration of stdlib examples and user-facing docs away from gratuitous broad-`any` exposure is the scope of #2324.

## Cross-reference

The deprecation rollout is staged through the strict-any rule set. See [`docs/reference/strict-any.md`](../reference/strict-any.md) §"v0.0.30 issue ladder" for the issue history and per-rule status. Paths 1, 9, and 10 in this document map directly onto rows in that table.
