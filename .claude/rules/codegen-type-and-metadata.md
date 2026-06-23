---
paths:
  - "src/codegen_type.cpp"
  - "src/codegen_builtin.cpp"
  - "src/codegen_metadata.cpp"
  - "src/codegen_any.cpp"
  - "src/codegen_expr_literal.cpp"
---

# Codegen: Type System and Metadata

### Type registry cross-check + alias resolution before lookup; new primitives wired into every reflection site

Always route a user-supplied name through `resolveTypeAlias()` before registry lookup or string equality — `"MyStr" == "str"` false-negative breaks `emitWeakUpgrade` header offset and `validateTypeBounds` alias bound check. New primitives must be wired into `reverseResolveType`, `inferExprType` / `inferExprTypeName`, `inferTypeArgs`, `resolveType`, `wrapInAny` / `unwrapFromAny` slot tag tables — these sites are invisible from any single file.

### Canonicalization that may collapse type category must be guarded at every call site

`flattenUnionWithAliases("A | int")` where `type A = int` collapses to `"int"`; downstream `wrapInUnion` indexes `union_type_info_` by the original category and dereferences `end()`.

### `propagateTypeMeta` is single-value; composites decompose at the caller; tuple sigs use `source_type_name` as boundary channel

`propagateTypeMeta(name, val)` rebuilds `TypeMeta::*` slots on a single SSA value. Do NOT add a `tuple_elem_type_names: vector<string>` slot — decomposition belongs at the caller via `splitTypeArgs`. `source_type_name` is the lossless channel for type names that `propagateTypeMeta` cannot carry. `TupleExpr` literal codegen must also stamp `source_type_name` because anonymous LLVM `StructType` collapses str / List / Map / closure / etc. to `ptr` — downstream consumers cannot distinguish per-element types from LLVM types alone.

Higher-order combinator element loads (`filter` / `map` / `reduce` / `fold` etc. in `src/codegen_call_higher_order.cpp`) historically gated `propagateTypeMeta(snap, elem)` on `elem->getType() == ptrTy_`, which falsely excluded tuple-typed elements. The file-local helper `shouldPropagateElemMeta` now accepts both ptr-typed and `StructType` elements when `snap` is a tuple sig. Other StructType cases (records, `anyTy_`, ADT enum payloads) deliberately stay rejected. Iterator-header element metadata cannot use the higher-order combinator pattern because the next-fn is an independent toplevel function; `ValueMetadata` is keyed by `llvm::Value*` so a `propagateTypeMeta` inside the next-fn body stamps a fresh SSA value the consumer never sees — use `iterator_elem_type_name` on the IteratorHeader instead.

### Metadata rebuilds: every load / extract / PHI / branch-merge needs explicit propagation

`ValueMetadata` is keyed by `llvm::Value*`. `CreateLoad`, `CreateExtractValue`, BinOp results, and PHI-merged values are fresh SSA with no metadata. Snapshot the inner type name into a local `std::string` BEFORE calling `propagateTypeMeta` — the helper may rehash and invalidate `getMeta` results. `reduce_first` and any seed-loading site that calls `wrapInAny`: propagate must run before the conditional `wrapInAny`; otherwise `getAnyTypeTagForValue` tags a `ptrTy_` collection element as `Str=3`. Do NOT call `propagateTypeMeta("str", elem)` to flag str container elements — the helper does not set `str_elem` and re-introduces the `-24` / `-16` offset confusion.

### `wrapInUnion` disambiguates same-LLVM-type variants by metadata, not LLVM type equality

Under opaque pointers, `List<int>` / `Map` / `Set` / `str` / closure all resolve to `ptr`. Picking the first LLVM-matching variant produces `Map` wrapped with tag=0 (List) and corrupts dispatch. Resolve in three passes: (1) full source-level name via `buildTypeNameFromMeta` + exact match; (2) coarse kind match via `isListTypeName` / `isMapTypeName` etc.; (3) LLVM-type equality fallback.

### Numeric coercion: `/` always float, `//` / `%` integer-error-on-zero, narrowing via `emitCheckedFPToInt`, mix-check uses "int" sentinel

Never call `CreateFPToSI` / `CreateFPToUI` directly — use `emitCheckedFPToInt` which inserts a NaN / ±inf / range guard. Signed boundary is `[−2^(W-1), 2^(W-1))`; do NOT use symmetric `fabs(x) >= 2^(W-1)` which spuriously rejects `INT64_MIN`. `low_level_type_name` mix-check between operands must map empty → `"int"` sentinel before comparing — else `1 + getFromMap(m, k)` silently mixes int with `1i64`.

### `any` collection / record / enum wrap dispatches by metadata, not LLVM type; element-type erased on wrap

**Tags**: codegen, any, metadata-erasure, collection, record, enum, type-dispatch, descriptor

Under opaque pointers, `List<int>`, `Map<str,int>`, `Set<bool>`, `str`, and record / enum data ptrs all collapse to `ptrTy_` — `getAnyTypeTag(val->getType())` cannot pick the right tag. Use `getAnyTypeTagForValue(val)` (NOT `getAnyTypeTag(val->getType())`) when the input may be a collection — the `ForValue` variant inspects collection metadata before falling back to type-based dispatch. Do NOT widen `canAnyHoldType(ty)` — it returns true only for `{i64, f64, i1, ptr}`. The rejection is `isNonStrPointer(val) && !isCollection && !isRecord && !isEnum`.

### `Option<T>` / `Result<T, E>` alloca decls must stamp `source_type_name` so wrapInAny recovers the full generic parameterization across LoadInst

**Tags**: codegen, any, enum, option, result, source-type-name, generic, alloca, load, descriptor-cache-key

When a `Result<T, E>` / `Option<T>` value is stored into an alloca and later loaded for `wrapInAny`, `findEnumLikeTypeNameForBoxing` cannot synthesize the full source-level name from the LLVM struct alone — under opaque pointers ARC-managed inner slots collapse to `ptr`, `reverseResolveTypeName(ptrTy_)` returns `"str"`, and the descriptor cache is keyed as `"Result<str, str>"` while the unwrap site queries `"Result<List<int>, str>"` — descriptor-pointer equality fails at runtime.

### Collection element-access any-widening; `Set<any>` needs `emitAnyBinaryOp`; post-hoc Result coercion

`anyTy_` is `{i64 tag, [8 x i8] data}`; LLVM `icmp eq` rejects the array data — for `Set<any>` / `Map<any>` element comparison, use `emitAnyBinaryOp("==", ...)` not `emitComparisonOp`. Result/Option type coercion is post-hoc in `emitVarDecl` (`coerceResultType`), NOT threaded into Ok/Err constructors. Single-slot mismatch: only accept when `tryGetStaticResultDisc(val, &staticDisc)` returns true; runtime-disc sources must return `nullptr` else silent miscompile zero-fills the active payload. Do NOT widen `canAnyHoldType` — the enum branch is dedicated and parallel.

### `any` → typed collection unwrap at `emitVarDecl` needs a `source_type_name` gate; `"any"` is ambiguous, not a concrete mismatch

**Tags**: codegen, any, var-decl, source_type_name, typed-collection, json-load, segfault

`json.load(text)` returns `Result<any, Error>` whose `Ok(v)` arm binds `v: any` with no collection element metadata. Without a `source_type_name` gate, `xs: List<str> = v` routes into `unwrapFromAny(val, ptrTy_, "List<str>")` without verifying the runtime tag — `List<str>` / `List<float>` segfault (8-byte stride walked off the 16-byte `RyAny` payload); `List<int>` silently corrupts. A runtime check is not possible: `ListHeader` layout has no stride / element-type field. Do NOT widen `canAnyHoldType(ty)` — the gate is dynamic-metadata-driven at the call site. When `source_type_name == "any"`, defer to runtime tag check (ambiguous, not a concrete mismatch — the motivating case is `fn identityAny(v: any) -> any`).

### `tryUnwrapFromAny` is the Result-returning sibling of `unwrapFromAny`; emitResultBranch loses metadata so callers re-stamp via propagateTypeMeta

**Tags**: codegen, any, json, load, loadAs, result, propagateTypeMeta, error-message-prefix, recursive-unwrap

Do NOT retrofit `unwrapFromAny` to take a "trap on mismatch" flag — `_Exit` paths and Result paths have structurally different error-message shapes. `emitResultBranch` builds the result via a PHI on two SSA values, which erases all `getMeta()` metadata — the caller must re-stamp via `propagateTypeMeta` after the call, or downstream `case Ok(xs):` binding reaches `xs` with empty `list_elem_type_name`.

### `Map<any, V>` is out of scope for the literal `any`-hint path; key-side must keep the strict same-type gate

**Tags**: codegen, any, map, literal, hint, rehash, hash-table, scope, strict-type-gate

Force `hint.map_key_any = false` even when the annotation says `Map<any, V>`. The hash-table rehash dispatch (`__ry_ht_rehash_i64` / `__ry_ht_rehash_f64` / `__ry_ht_rehash_str`) has no 16-byte struct (`anyTy_`) variant, and ARC for `anyTy_` keys carrying str / List / Map / Set / record / enum payloads is unaddressed.
