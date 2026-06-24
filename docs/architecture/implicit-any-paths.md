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
| 9 | Implicit `any` → concrete unwrap | Variable decl: `src/codegen_stmt.cpp` Pattern 4 lint + `unwrapFromAny`. Function-call arg (incl. default-value path): `src/codegen_call_user.cpp:192-194`, `:220-222`. Lambda-call arg: `src/codegen_call_dispatch.cpp` `coerceCallArgs`. `Ok(any)`/`Err(any)`/`Some(any)` into typed `Result`/`Option` slots: `src/codegen_call.cpp`. | Compat mode: all four sub-cases (including `Some(any) → Option<T>`) warn under `enableAnyUsageLint` with the Pattern 4 template, gated by `shouldEmitAnyLintAt` so stdlib / cross-package imports stay silent. Strict mode: all four reject as `[strict-any/any-implicit-unwrap]` hard errors via `CodeGen::strictAnyError`. The runtime tag check in `unwrapFromAny` remains as the compat-mode fallback. | **deprecate** | #2321 (warn + strict reject shipped) → #2322 (default flip) | Variable decl: `tests/test_any_usage_warnings.cpp` `Pattern4*`. Named-call arg: `tests/spec/any.test.ry:207-232` "any unwrap in function call" + `Pattern4bCallArgFiresWarning`. Lambda-call arg: `tests/spec/any.test.ry` "any unwrap in lambda call" + `Pattern4cLambdaCallArgFiresWarning`. `Ok/Err/Some` slot: `tests/spec/any.test.ry` "any unwrap into Result slot" + `Pattern4d{Ok,Err,Some}SlotFiresWarning`. Strict-mode rejection: `tests/test_strict_any.cpp` `AnyImplicitUnwrap*RejectedInStrict` pairs. |
| 10 | Direct operators on `any` | `src/codegen_expr.cpp:1744-1756` binary path; `src/codegen_expr.cpp:311-319` unary `-`. Deprecation warning in `src/codegen_any.cpp` `warnAnyOpDeprecated` (deduped per op key). Strict-any guard in the same site. | Without `--strict-any`: warns once per arithmetic/ordering operator. With `--strict-any`: arithmetic `+ - * / % // **` and unary `-` are `[strict-any/any-arithmetic]` hard errors. `==` / `!=` remain allowed in both modes. | **deprecate** | #2316, #2322 | `tests/test_any_op_warnings.cpp`; `tests/test_strict_any.cpp`; `tests/spec/any.test.ry:82-150` "any arithmetic operators". |

## Notes

### Path 9 spec blocks — runtime-correctness only

The spec blocks at `tests/spec/any.test.ry:207-272` pin only the **runtime-correctness half** of Path 9's sub-cases (the unwrap succeeds and the unwrapped value matches expectations); they do not assert absence of diagnostics. #2321 added compat-mode warnings at the three previously-silent sub-cases and a `[strict-any/any-implicit-unwrap]` rejection in strict mode — those diagnostics are pinned separately by `tests/test_any_usage_warnings.cpp` (compat) and `tests/test_strict_any.cpp` (strict). Running the spec suite under `RY_STRICT_ANY=1` is not a supported CI configuration: the Path 9 spec blocks expect the compat-mode unwrap to succeed and will fail under strict mode, which is the rule's intended behavior.

### Path 3 defensive fallback

`src/codegen_lambda.cpp` contains a `reverseResolveTypeName` fall-through that returns `"any"` for LLVM types it cannot name. This is a defensive branch — current Ry surface syntax does not produce types that reach it (tuples and closures are already handled by named arms). The inventory documents the site but does not add a test, since constructing an unnameable type from the user-facing language is not currently possible. If a future feature exposes such a type to a return-position inference site, a test should accompany it.

### Explicit `any` sources (out of scope)

The following return-`any` patterns are **explicit** API choices, not implicit creation, and therefore fall outside this inventory:

- `from ry.json import load` and `from ry.json5 import load` — typed APIs whose declared return type is `Result<any, Error>`. The user chose the dynamic-data boundary by importing them.
- FFI / `@extern` return types declared as `any`.
- Heterogeneous-return `@native` functions (e.g. `threadSpawn` / `threadJoin`) that use `any` as the declaration placeholder per `.claude/rules/docs-reference-conventions.md`. The actual dispatch is driven by the custom emitter; the `any` in the declaration is a hygienic stand-in for an unbound `T`, not a true type-erasure.

These are classed **keep** by category — they represent the intended use of `any` as a dynamic boundary. Migration of stdlib examples and user-facing docs away from gratuitous broad-`any` exposure is the scope of #2324.

## Cross-reference

The deprecation rollout is staged through the strict-any mode. See [`docs/reference/strict-any.md`](../reference/strict-any.md) §"Relationship to upcoming changes" for the issue ladder and per-rule status. Paths 1, 9, and 10 in this document map directly onto rows in that table.
