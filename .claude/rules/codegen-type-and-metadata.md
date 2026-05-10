---
paths:
  - "include/ry/codegen.hpp"
  - "src/codegen_type.cpp"
  - "src/codegen_builtin.cpp"
  - "src/codegen_metadata.cpp"
  - "src/codegen_expr_literal.cpp"
---

# Codegen: Type System and Metadata

### Adding a new type kind requires cross-checking every other type registry

**Source**: #815 (2026-04-11), extended by #850
**Tags**: codegen, types, registry, duplicate-check, naming

**Rule**: When introducing a new type kind (or touching an existing `emitStmt(<Decl>Stmt &)` path), reject the name if it collides with **any other user-name type registry**. Type definitions in `include/ry/codegen.hpp` use four user-name registries — `record_types_`, `enum_types_` (concrete), `generic_enum_templates_`, `type_aliases_` — that share one user-facing name space. Each `emitStmt(<Decl>Stmt &)` path (Record, Enum concrete, Enum generic, TypeAlias) must (a) reject same-kind duplicates with a kind-specific message, and (b) call `rejectIfTypeNameTakenByOtherKind()` (`src/codegen_stmt_misc.cpp`) for cross-kind collisions.

`union_type_info_` is **not** a user-name registry — it's an anonymous cache populated by `resolveType()` (`src/codegen_type.cpp`) when an inline union like `int | str` is encountered (key: flattened type string). Named unions like `type Foo = int | str` go through `TypeAliasStmt` and land in `type_aliases_`, so a cross-kind check against `union_type_info_` is unnecessary and would reject legitimate anonymous cache hits.

**How to verify**: grep `std::unordered_map<std::string, .*(?:Info|Template|Type)>` in the codegen header to find every type registry, then confirm each `emitStmt` for a type declaration consults all user-name registries before inserting. Add `EXPECT_THROW` regression tests (with `runSource`) per cross-category pair in both orders — legal cases alone won't catch a missing guard.

### Canonicalization that may collapse shape must be handled at all call sites

**Source**: #844 PR review (CodeRabbit, critical)
**Tags**: codegen, canonicalization, union, types

**Rule**: When introducing a normalization/canonicalization step that can change a type's **category** (union → scalar, variadic → single, tuple → element, alias → target), audit every caller that assumes the original category. Add an explicit early-return for the collapsed case.

**Why**: `flattenUnionWithAliases("A | int")` where `type A = int` collapses to `"int"`. Downstream `wrapInUnion` then did `union_type_info_.find("int")`, missed, and dereferenced `end()`. The fix was to early-return when the flattened result is no longer a non-literal union.

**How to verify**: `grep -n '<normalizer_name>' src/`, then for each hit ask "what if the result is no longer a union/variadic/tuple?". If the caller indexes a map keyed on the original category, it needs a guard.

### valueToString element loads must propagate metadata via propagateTypeMeta

**Source**: #811 / #836 / #810 sweep (2026-04-10)
**Tags**: codegen, formatter, metadata, collection, nested

**Rule**: Whenever you add a new outer container type to `valueToString()` (`src/codegen_tostring.cpp`), the element-load block MUST:

1. Snapshot both the element type name and any `*_fn_type_info` field from the outer container's metadata **before** calling `getOrCreateMeta()` (which may rehash `value_metadata_` and invalidate pointers).
2. Call `propagateTypeMeta(elemTypeName, elem)` to rebuild the element's collection metadata (`list_elem`, `map_value_type_name`, etc.).
3. If the snapshot had `*_fn_type_info`, store it on the element's metadata so closures format as `<closure>`.

The union-variant branch needs the same pattern (`codegen_tostring.cpp:200-230`, post-#836). The **ADT variant field-load path** in `getOrCreateADTToStringFn()` (same file) has the same requirement — without propagation, fields like `List<Tree>` inside `Tree::Node(int, List<Tree>)` print as `""`, enums print as raw tag integers, `Option<int>` prints as `Some(Some(0))` (#1238). Call `propagateTypeMeta(fieldTypeName, fieldVal)` on each field load instead of only propagating `low_level_type_name`.

**Why**: `List` / `Map` / `Set` element loads from container data arrays produce fresh SSA `Value*` with no metadata. If the element type is itself a collection (e.g. `Map<str, List<int>>`), calling `valueToString(elem)` without propagating the inner type name falls through to the raw string-pointer path and prints empty strings or garbage. The `List` branch had this propagation pre-#811; `Map` and `Set` did not.

**Codegen-time recursion**: ADT formatting MUST go through a per-type helper function (`getOrCreateADTToStringFn`) rather than inlining the switch body at every `valueToString` call site. Inlining recurses forever at codegen time for self-referential ADTs (Tree → List<Tree> → Tree → ...). The helper caches the emitted function *before* it emits the body, so subsequent references resolve to an ordinary IR `call`. Do not revert to an inline body.

**How to verify**:

```bash
grep -nE 'CreateLoad.*elem|CreateLoad.*valVal|CreateLoad.*fval' src/codegen_tostring.cpp
```

For each hit, confirm a metadata snapshot + `propagateTypeMeta` call follows within ~5 lines, or that the outer container has no corresponding `*_type_name` metadata slot (e.g. fixed-length arrays).

### wrapInUnion must disambiguate same-LLVM-type variants by metadata

**Source**: #836 sweep
**Tags**: codegen, union, variant-dispatch, collection, metadata

**Rule**: When matching a value to a union component, resolve in three passes: (1) reconstruct the full source-level type name from the value's metadata via `buildTypeNameFromMeta()` and match against `componentNames` exactly — handles same-kind variants like `List<int> | List<str>`; (2) fall back to coarse kind match (`isListTypeName` / `isMapTypeName` / `isSetTypeName` / `isFunctionTypeName`) against the same metadata fields — handles cases where canonical name couldn't be built (literals without annotations, aliases); (3) fall back to LLVM-type equality. The value's `CodeGen::ValueMetadata` fields (`list_elem`, `list_elem_type_name`, `map_key` / `map_value` / `map_value_type_name`, `set_elem` / `set_elem_type_name`, `fn_type_info`) are the source of truth.

**Why**: `wrapInUnion` originally picked the first variant whose LLVM type matched. For unions like `List<int> | Map<str, int>` this always picks the first because both are `ptrTy_`, so a `Map` value gets wrapped with tag=0 (List) and is then interpreted as a List at dispatch time, producing garbage. Invisible before #836 because the formatter rejected collection variants entirely.

**How to apply**: Any future dispatch using LLVM type equality to route between ptr-backed Ry types must first consult metadata. `src/codegen_match.cpp::wrapInUnion()` and `buildTypeNameFromMeta()` in `src/codegen_builtin.cpp` are the canonical pattern.

### Float formatter uses std::to_chars shortest round-trip, NOT %.17g

**Source**: #808 sweep (2026-04-10), superseded by #1031 (2026-04-16)
**Tags**: formatter, float, precision, tests

**Rule**: `__ry_any_fmt_float` (`src/runtime_any.cpp`) uses `std::to_chars(buf, end, x)` overload 3 (no format arg, C++17), then appends `".0"` when the result has no decimal point, no exponent, and is not `nan`/`inf`. A parallel `__ry_any_fmt_f32` handles `f32` values with `float`-typed `to_chars` (avoids FPExt changing the shortest representation).

**Why this approach**: two tempting alternatives are wrong:
- `%g` (= `%.6g`): rounds `0.30000000000000004` to `"0.3"`, so `print` output contradicts equality (`0.1 + 0.2 == 0.3` → `false` despite `print(0.1 + 0.2)` showing `"0.3"`). Original approach, fixed in #1031.
- `%.17g`: always 17 digits, turns `3.14` into `"3.1400000000000001"`, breaking many existing tests. Rejected in the #808 plan.

`std::to_chars` overload 3 gives the *shortest* decimal that round-trips exactly — `"3.14"` for `3.14`, `"0.30000000000000004"` for `0.1 + 0.2`. No existing literal tests break.

**How to apply**: Do NOT switch to `%.17g` (breaks `"3.14"` assertions). Do NOT revert to `%g` (loses precision). Use `std::to_chars` overload 3 (no `chars_format` arg) for both `double` and `float` separately. Keep the `".0"` suffix logic — it detects `.`/`e`/`E`/`n`/`N`/`i`/`I` to skip correction for nan, inf, and already-fractional values.

### Diagnose type / control-flow mismatches at the frontend, never at LLVM verify

**Source**: #805 / #818 / #821 sweep (2026-04-10)
**Tags**: codegen, diagnostics, ir-verify, frontend

**Rule**: Whenever codegen is about to emit IR that depends on a type or control-flow invariant (argument type, terminator absence, block validity), check the invariant in C++ and call `codegenError(...)` with a user-facing message **before** the invalid IR is written. Never treat LLVM verification as a user-visible error surface.

**Why**: Three bugs surfaced as cryptic LLVM IR verify errors because codegen emitted invalid IR and relied on the verifier:
- #805: `Result<T, E>` routed to `T`-expecting runtime call because `isJsonValue()` trusted a metadata flag without checking `ptrTy_`.
- #818: `toBool(ptr)` fell through to `ICmpNE(v, ConstantInt::get(ptrTy_, 0))` which LLVM printed as `icmp ne ptr, i0 0`.
- #821: `emitExit()` created an `unreachable` terminator but did not switch insertion blocks, so trailing statements emitted into a terminated block.

**How to verify**:

```bash
git diff origin/<base> -- 'src/**' | grep -nE 'ConstantInt::get\(.*->getType\(\)'
git diff origin/<base> -- 'src/**' | grep -nE 'CreateUnreachable|CreateRet|CreateBr'
```

Every `ConstantInt::get(v->getType(), ...)` site must be guarded by an `isIntegerTy()` check. Every terminator-emitting call that's not the last in its basic block must either be followed by a new block creation + `SetInsertPoint`, or the caller loop must break on `GetInsertBlock()->getTerminator()`.

### propagateTypeMeta is single-value; callers decompose tuples

**Source**: #820 + #813 sweep (2026-04-11)
**Tags**: codegen, metadata, propagation, tuple, destructure, enum

**Rule**: `propagateTypeMeta(name, val)` fills metadata for a single value. Composite types (tuples, future records) that need to split across multiple downstream values must be parsed by the caller using `splitTypeArgs`, which already understands nested `<>` and `()`.

**How to apply**: When a tuple type string arrives at a destructure / unpacking site (for-loop, `let (a, b) = ...`), parse the components via `splitTypeArgs(inner)` after stripping outer parens and call `propagateTypeMeta(component[i], boundVar[i])` per bound variable. Do **not** extend `propagateTypeMeta` itself to write to a `tuple_elem_type_names` slot — keep the helper single-purpose.

**Why**: Fixing function-return / for-loop destructure metadata propagation (#820 / #813) required teaching `propagateTypeMeta` to recognize enum type names but deliberately *not* tuple type strings like `"(int, List<int>)"`. Tuples are not a single metadata carrier — components are stored in separate bound variables during destructure, so decomposition must happen at the call site that owns those variables (`emitTupleDestructure` in `src/codegen_stmt_loop.cpp`). The rejected alternative — adding a `tuple_elem_type_names: std::vector<std::string>` field to `ValueMetadata` — would require updating `propagateMeta`, `hasAnyMeta`, and every metadata-copy site with no benefit over parsing the string once at the caller (compile-time O(n)).

### IndexExpr → FieldAccessExpr tuple metadata uses `source_type_name` as the boundary channel

**Source**: #1664 (2026-05-09)
**Tags**: codegen, metadata, tuple, IndexExpr, FieldAccessExpr, source_type_name, splitTupleSig, enumerate, zip

**Rule**: When a tuple is loaded from a `List<(K, V)>` (the only non-destructure boundary fixed by #1664), stamp the full tuple sig onto the loaded value via `getOrCreateMeta(val).source_type_name = "(K, V)"`. The downstream FieldAccessExpr numeric-index arm reads `source_type_name`, calls `splitTupleSig` to decompose, and propagates the matching component's name onto the extracted field. This mirrors the `Result<T, E>` / `Option<T>` precedent (#1638) — `source_type_name` is the lossless channel for type names that `propagateTypeMeta` cannot carry.

Other tuple-producing boundaries (function returns of `(K, V)` outside a `List`, tuple-typed record fields) are NOT covered by #1664 — extending this pattern to them is a future follow-up.

**Why**: `enumerate(xs)` / `zip(xs, ys)` produce `List<(K, V)>` with header `list_elem_type_name = "(K, V)"`. Reading via `xs[0]` routes through `propagateTypeMeta` which deliberately ignores tuple sigs (see "single-value; callers decompose tuples" entry above). Without `source_type_name` the loaded tuple reaches FieldAccessExpr's numeric-index arm with no metadata, defaults to `str` dispatch, and `xs[0].1[0]` raises "str does not support index access". The for-loop destructure site solves the same shape via `emitForBindingPattern` + `splitTupleSig`, but the expression site has no named-variable boundary — just a tuple SSA value flowing through `.0` / `.1`.

**How to apply**:

1. **Stamp side** (IndexExpr List path, function return, future tuple-producing boundaries): guard with `elemTypeName.size() >= 2 && elemTypeName.front() == '(' && elemTypeName.back() == ')'` and stamp `source_type_name = elemTypeName`. Snapshot `elemTypeName` into a local `std::string` before calling `propagateTypeMeta` (#858 rehash gotcha) — reusing the existing snapshot block's local is sufficient.
2. **Decompose side** (FieldAccessExpr numeric-index arm): after `CreateExtractValue`, read `getMeta(obj)->source_type_name` into a local `std::string`, guard with `front() == '(' && back() == ')'`, call `splitTupleSig`, and call `propagateTypeMeta(components[idx], fieldVal)` when `idx < components.size()` and the component name is non-empty.

**Why `source_type_name` not a new slot**: same reasoning as #820 — tuples have N downstream targets, not one. Adding `tuple_elem_type_names: std::vector<std::string>` was rejected at #820. The `source_type_name` channel already exists for `Result` / `Option`, so reusing it adds no new fields.

**Scope coverage**: `enumerate` / `zip` / `items` all work end-to-end because their codegen sites stamp `list_elem_type_name = "(int, T)"` / `"(T, U)"` / `"(K, V)"` (`src/codegen_call.cpp` for enumerate/zip; #1659 added `items(m)`).

### Enum metadata propagation: list literals of enum values need a direct check

**Source**: #820 sweep (2026-04-11)
**Tags**: codegen, list, enum, metadata, propagation

**Rule**: When a container literal is built over a non-pointer element type that still carries *source-level* metadata (enums today, potentially small-struct values in future work), copy the element's `enum_value_type` (or equivalent) into the container's `list_elem_type_name` explicitly, **outside** the `elemTy == ptrTy_` guard. The guard exists for nested-collection tracking, not for generic metadata propagation.

**Why**: `emitExprVariant(ListExpr)` in `src/codegen_expr_literal.cpp` guards its `inferCollectionTypeName` fallback behind `if (elemTy == ptrTy_)` because list-of-list, list-of-map, and list-of-set all use pointer-typed elements. Simple enums are `i64` and ADT enums are LLVM struct values — neither is a pointer type — so the pointer-guarded path skipped them entirely and `list_elem_type_name` was never set. Consequently `valueToString`'s list branch (which uses `propagateTypeMeta(list_elem_type_name, loaded_elem)` to rebuild enum metadata) received an empty string and printed the raw i64 bit pattern.

**How to verify**: grep `if (elemTy == ptrTy_)` in container literal paths (`codegen_expr_literal.cpp`) and confirm each handles non-pointer elements whose metadata still needs to reach the container's element-type-name slot.

### List literals of `Result<T, E>` / `Option<T>` struct elements need an explicit `list_elem_type_name` stamp

**Source**: #1570 (2026-05-04)
**Tags**: codegen, list, result, option, metadata, propagation, higher-order

**Rule**: For list literals whose element type is `Result<...>` or `Option<...>`, stamp `list_elem_type_name = "Result<inner, err>"` / `"Option<inner>"` on the header explicitly, **outside** the `elemTy == ptrTy_` guard. Recover `inner` via `buildTypeNameFromMeta(vals[0])` (which reads the metadata `propagateMeta` attached at `buildOkValue` / `buildSomeValue`), and fall back to `reverseResolveTypeName(innerTy)` for primitive payloads. Recover `err` via `reverseResolveTypeName(errTy)` only — the Err arm metadata path (#982 / #985) is unreliable for the literal-construction case where most elements are Ok.

**Why**: `Result` and `Option` are LLVM struct values (`{i1, T, E}` / `{i1, T}`), not pointers, so the pre-existing `inferCollectionTypeName` path inside `if (elemTy == ptrTy_)` never reaches them. Harmless until #1570 — `valueToString` reads `enum_value_type` (set per-element) rather than `list_elem_type_name`, so printed output was correct. The gap surfaced when `sequence`'s custom emitter needed to reverse-engineer the Ok / Some payload type at codegen time to stamp the *output* list's `list_elem_type_name` (so subsequent collection ops on `Ok(outList)` dispatch correctly). With an empty source name there's no way to recover `T` from the LLVM struct alone when `T` is a ptr-typed collection.

**How to apply**: Mirror the canonical site at `codegen_expr_literal.cpp` in `emitExprVariant(ListExpr)`, immediately after the enum-value-type stamp block. Future higher-order combinators on `List<Result<T,E>>` / `List<Option<T>>` (e.g. `traverse(f)`, `partition`) constructing output collections wrapping `T` will rely on this stamp.

### `inferCollectionTypeName` List branch must prefer stored `list_elem_type_name` over `reverseResolveTypeName`

**Source**: #1094 + #1095 (2026-04-17)
**Tags**: codegen, metadata, list, inferCollectionTypeName, reverseResolveTypeName, nested-list, tuple

**Rule**: In `inferCollectionTypeName` (`src/codegen_builtin.cpp`), check `meta->list_elem_type_name` before falling back to `reverseResolveTypeName`. Additionally, return `""` for unnamed `StructType` elements (tuples) so callers (`emitVarDecl`'s annotation fallback, `codegen_stmt.cpp:491-512`) can derive the correct name from the source annotation. Do **not** return `""` for `ptrTy_` unconditionally — plain `List<str>` lists set no `list_elem_type_name` (string literals have no collection metadata), so returning `""` for them would break nested-list display (e.g. `toStr([["a","b"],["c"]])` prints `["",""]` instead of `[["a","b"],["c"]]`).

**Why**: The List branch originally called `reverseResolveTypeName(elemTy)` unconditionally, which is lossy:
- `ptrTy_` → `"str"` — `List<List<int>>` whose middle list correctly stored `list_elem_type_name = "List<int>"` had that info discarded; outer list got annotated `"List<str>"` instead of `"List<List<int>>"` (#1095: for-loop over `outer[0][0]` ran 0 times).
- Unnamed `StructType` (tuples) → fall-through to `"any"` — `List<List<(int,int)>>` annotated as `"List<any>"`, bypassing `emitVarDecl`'s annotation fallback, giving the second destructured variable wrong metadata (#1094).

The Map branch already preferred stored names via `meta->map_key_type_name` / `meta->map_value_type_name`; the List branch wasn't updated.

**How to apply**: When extending `inferCollectionTypeName` for Set or other container kinds, mirror the pattern: (1) stored name first, (2) return `""` only for types with no canonical LLVM-to-name mapping (currently unnamed StructType), (3) fall back to `reverseResolveTypeName` for everything else.

### Helper-function returns need both declared-type metadata and dynamic return metadata

**Source**: #1317 (2026-04-22)
**Tags**: codegen, metadata, function-return, resource, thread

**Rule**: Rebuild canonical metadata from the declared return type at the call site, but store dynamic return metadata in dedicated per-function slots — not by copying the full `ValueMetadata` onto `llvm::Function`. Full-copying is not branch-safe: a function with multiple `return` sites can overwrite or merge single-valued metadata like `thread_result` / `task_result` / `fn_type_info` and replay the wrong snapshot at every call site. Instead, record only the needed dynamic fields (`ThreadResult`, `TaskResult`, function-return `FnTypeInfo`) and reject inconsistent values across return branches with a compile-time error.

**Why**: Reapplying only `propagateTypeMeta(entry->returnTypeName, callResult)` at user-function call sites is not enough for values whose runtime metadata extends beyond the declared type name. `Thread` helper returns need the resource kind so `threadJoin(t)` / `lockAcquire(lock)` type checks pass, but they may also carry dynamic metadata such as `thread_result` from `threadSpawn(() => 7)`. With only the declared type restored, helper returns compiled but `threadJoin(mkThread())` silently fell back to the Unit join path and produced `0` instead of `7`.

### New primitive types must be wired into every type-reflection site

**Source**: #825 PR review (CodeRabbit, 4 comments)
**Tags**: codegen, primitive-type, type-reflection, generics

**Rule**: When adding a new primitive type, grep for existing primitives and update every site. Common sites: `reverseResolveType`, `inferExprType`, `inferTypeArgs`, `resolveType`, pretty printers, docs type tables.

**Why**: #825 added `Type` primitive + `typeOf` builtin. Main codegen path worked but `reverseResolveType()`, `inferTypeArgs()` (`src/codegen_fn_generic.cpp`), and `inferExprType()` (lambda call-site inference) were not updated. Lambdas like `(x: int) => typeOf(x)` inferred return as `i64`, and generic inference collapsed `Type` to `any`.

**How to verify**:

```bash
grep -nE 'i64Ty_|boolTy_|strTy_|f64Ty_' src/ include/
```

The new type should appear wherever these existing primitives do (justify any exceptions explicitly).

### Generic function type inference must walk TypeNode structurally, not string-match `toString()`

**Source**: #823 (2026-04-11)
**Tags**: codegen, generics, type-inference, unification

**Rule**: Inference must recurse over the `TypeNode` variant (`BasicType` / `GenericType` / `TupleType` / `FnType` / `OptionalType`) and unify each type variable against the matching slice of the argument's inferred type name (`inferExprTypeName`, which already produces shaped strings like `"List<int>"`). Use `unifyTypeParam` in `src/codegen_fn_generic.cpp` as the reference walker, with `splitGenericTypeName` / `splitTupleTypeName` / `splitFunctionTypeName` helpers doing string-side splitting at top-level commas while respecting `<`, `>`, `>>`, `>>>`, parens, and brackets. Keep the LLVM-type fallback (alloca type → `reverseResolveTypeName`) for the bare-variable + scalar-argument case so existing test coverage keeps working.

**Why**: The original `inferTypeArgs()` compared the whole parameter type `toString()` against the type-parameter names set (`if (typeParamSet.count(paramType))`). That check only fires when the parameter type *is* a bare type variable — `fn identity<T>(x: T)` worked but `fn firstOf<T>(xs: List<T>)` silently produced no binding ("could not infer type parameter 'T'"). Every container, tuple, and function-type parameter was broken the same way because `"List<T>" != "T"`.

**How to verify**: `tests/spec/generic_infer_container.test.ry` covers `List`, `Map`, `Set`, tuple, nested, named-local, and cross-parameter unification cases. `tests/test_codegen_fail.cpp` covers the two error paths (empty-container "could not infer" and conflicting-bindings).

### `inferExprTypeName` must read alloca metadata for container locals, not just primitives

**Source**: #823 (2026-04-11)
**Tags**: codegen, type-inference, value-metadata, generics

**Rule**: In `inferExprTypeName` for `VariableExpr`, look up the alloca with `findVar(name)` and check `getTypeMeta(TypeMeta::ListElem|MapKey|MapValue|SetElem, alloca)` FIRST to build shaped names like `"List<int>"`. Only fall back to `reverseResolveTypeName(alloca->getAllocatedType())` for structs and primitives. The struct-local fallback must use `getAllocatedType()`, not the alloca's pointer type, or bounded generic calls like `get_name<T: Animal>(dog_local)` will regress to inferring `T = str`.

**Why**: For a local `xs: List<int> = [1, 2, 3]`, the alloca carries `TypeMeta::ListElem = i64Ty_` but **not** the string field `list_elem_type_name` — that string is populated only for nested / non-primitive inner types (`List<Map<str, int>>`). When `inferExprTypeName(VariableExpr)` fell through to `reverseResolveTypeName(inferExprType(...))`, it got `ptrTy_` (the opaque-pointer alloca type in LLVM 15+) and returned `"str"`, which caused generic inference to bind `T = str` for a list argument.

### `inferReturnType` and `inferReturnTypeName` must be maintained in lockstep

**Source**: #790 (2026-04-11, follow-up to #788)
**Tags**: codegen, lambda, returnTypeName, inferReturnType, type-inference

**Rule**: Any inference helper producing an `llvm::Type*` for a lambda/function return (`inferReturnType` + `collectReturnTypes`) must have a sibling that produces the shaped type-name string (`inferReturnTypeName` + `collectReturnTypeNames` in `src/codegen_lambda.cpp`). When you touch one, update the other. The call site in `emitExprVariant(LambdaExpr)` must populate `returnTypeName` on every inferred-return branch (expression-body AND block-body), not just one.

**Why**: #788 added `inferExprTypeName` + `returnTypeName` propagation for the **expression-bodied** lambda branch. The symmetric **block-bodied** branch only called `inferReturnType` (which loses `List<int>` / `Map<K,V>` shape), leaving `returnTypeName` empty. Downstream `propagateTypeMeta()` is gated on `if (!info.returnTypeName.empty())`, so collection metadata silently never reached the call-site alloca — breaking `result.len()` / `result[i]` on values returned by block lambdas with literal-collection returns.

**How to verify**: `tests/spec/lambda_collection_return.test.ry` covers block-bodied lambdas returning list / map / set literals and branched returns with consistent element types. Known limitation: returning a **local** collection variable from a block lambda still loses shape (#883).

### Lambda return-name inference must thread declared param type NAMES, not just `llvm::Type*`

**Source**: #886 (2026-04-11, follow-up to #788 / #790 / #884)
**Tags**: codegen, lambda, returnTypeName, inferExprTypeName, paramType, type-inference

**Rule**: Whenever `paramTypeMap` (name → `llvm::Type*`) is built and passed to `inferExprTypeName` / `inferReturnTypeName` / `collectReturnTypeNames`, build a parallel `paramTypeNameMap` (name → resolved Ry type-name string via `resolveTypeAlias(p.type->toString())`) and thread it through the same three helpers. In the `VariableExpr` case, consult `paramTypeNameMap` BEFORE falling back to the `inferExprType` path. For nested `LambdaExpr` inside a lambda body, extend BOTH maps symmetrically. This is the parameter-side analog of "`inferReturnType` and `inferReturnTypeName` must be maintained in lockstep" — the string path needs its own data source because `llvm::Type*` is lossy for opaque-pointer collection types.

**Why**: Lambda parameters are NOT yet in `scope_stack_` during return-type inference in `emitExprVariant(LambdaExpr)` — their allocas are created only after inference runs. When a lambda body returned one of its own parameters (`(xs: List<int>) => xs`, or block-body equivalent), `findVar` missed and the fallback `reverseResolveTypeName(inferExprType(expr, paramTypeMap))` collapsed `ptrTy_` to `"str"`. The lambda's `returnTypeName` came out wrong, downstream `propagateTypeMeta()` skipped populating `list_elem_type_name` / `map_*_type_name` / `set_elem_type_name` on the call-site alloca, breaking `result.len()` / indexing on the returned collection.

**How to verify**: `tests/spec/lambda_collection_return.test.ry` "lambda returning its collection parameter" covers expression-body and block-body returns of `List<int>`, `Map<str, int>`, `Set<int>`, nested `List<List<int>>`, plus a branched block-body case exercising `collectReturnTypeNames` → `IfStmt` walking.

### Resolve type aliases before registry lookup or type-name equality

**Source**: #1060 (2026-04-17), #1155 (2026-04-19)
**Tags**: codegen, type-alias, resolveTypeAlias, registry-lookup, generics, bounds

**Rule**: Before looking up a user-supplied type name in `record_types_` / `enum_types_` / `type_aliases_`, or comparing it by string equality against another type name, always route it through `resolveTypeAlias()`. Always keep the user-written original in `codegenError` messages — rewriting those to the resolved name degrades the diagnostic.

**Canonical examples**:
- `RecordPattern` case (`codegen_match.cpp:381-393`) — correct template.
- `validateTypeBounds` (`codegen_fn_generic.cpp`) — #1155 regression: alias bound / concrete were looked up and compared un-resolved, producing a false negative.
- `emitWeakUpgrade` (`codegen_arc.cpp`) — #1060 regression: `"MyStr" == "str"` returned false and the wrong header offset was chosen.

**Why**: `type A = B` registers `A` only in `type_aliases_`, never in the canonical registries. A `count(name)` or `name == "str"` without alias resolution yields a false negative whenever the user writes an alias, silently breaking the language-level promise that aliases are transparent. Reviews of any diff that contains `count(...)` / `== "<name>"` against a user-supplied type name should always ask "is this resolved?".

### `weak T` header-offset dispatch requires alias resolution

**Source**: #1060 (2026-04-17)
**Tags**: codegen, weak, str, StringHeader, type-alias, resolveTypeAlias, emitWeakUpgrade

**Rule**: `weakInnerTypeName()` only strips the `"weak "` prefix — it does **not** resolve type aliases. Before comparing the result to `"str"` (to pick `STRING_HEADER_SIZE` vs `ARC_HEADER_SIZE`), callers must feed the result through `resolveTypeAlias`. Preferred pattern: store the resolved name at the single write site (`codegen_stmt.cpp LetStmt`) so all downstream readers get the canonical form:

```cpp
// codegen_stmt.cpp — initial let (resolve at the write site)
std::string innerName = resolveTypeAlias(weakInnerTypeName(*annot));
// stored into weak_inner_type_names_[ptr]

// codegen_arc.cpp — emitWeakUpgrade (defensive resolve inside the emitter)
const std::string resolvedInner = resolveTypeAlias(innerTypeName);
// use resolvedInner == "str" (not innerTypeName) for the two data-pointer dispatch branches
```

**Why**: `type MyStr = str; w: weak MyStr = weak s` passes `"weak MyStr"` to `weakInnerTypeName`, yielding `"MyStr"`. Without `resolveTypeAlias`, `"MyStr" == "str"` is false → `ARC_HEADER_SIZE` (16) is used instead of `STRING_HEADER_SIZE` (24) → `emitWeakRetain`'s atomic add hits `byte_len` (at `handle−8`) instead of `strong_count` → corrupts `byte_len` → UB / wrong comparison result. Keep `weakInnerTypeName` as a pure static string-slice helper so error messages preserve the user-written alias name.

### Rebuild metadata on values loaded from collection slots before recursive codegen

**Source**: #736 (2026-04-14)
**Tags**: codegen, metadata, equality, collection, emitComparisonOp

**Rule**: `ValueMetadata` is keyed by `llvm::Value*`. When you `CreateLoad(elemTy, GEP(data, i))` to read an element from a `List` data pointer, `Map` values slot, or any array-of-elements, the produced SSA value is fresh and has no entry in `value_metadata_`. Calls to helpers that dispatch on metadata — `getListElementType`, `getMapValueType`, `getSetElementType`, `isStringValue` — return null/false, causing silent misclassification (e.g. pointer values treated as `str` and compared via `strcmp`).

**How to apply**: Before passing a GEP-loaded pointer value to any helper that dispatches on metadata, call `propagateTypeMeta(outerMeta->list_elem_type_name, loaded)` (or `map_value_type_name` / `set_elem_type_name` equivalent) to reconstruct `ValueMetadata` from the parent container's stored type name. Guard with `!elemName.empty() && elemName != "str"` — `str` elements are plain `char*` pointers that `isStringValue` already handles correctly without metadata, and the type name may be empty for untyped string literals.

**Why**: `getMeta` resolves `LoadInst → pointer operand` as a convenience, but a `GEP`-produced pointer is itself a fresh SSA value with no entry; resolution stops there.

### Same-element-type collection helpers must pair `setTypeMeta` with `propagateMeta`

**Source**: #1205 (2026-04-19); #961 precedent (map merge)
**Tags**: codegen, metadata, ListHeader, MapHeader, propagateMeta, setTypeMeta, slice, appended, take, distinct

**Rule**: Helpers that allocate a new collection header and copy elements verbatim from a source collection of the **same element type** (slice, take, drop, appended, distinct, ...) must call BOTH:

1. `setTypeMeta(TypeMeta::ListElem, newHeader, elemTy)` — LLVM type
2. `propagateMeta(src, newHeader)` — string names, `list_elem_fn_type_info`, `nested_list_elem`, `map_value_type_name`, `resource_kinds`, etc.

`propagateMeta` is rehash-safe by construction (`src/codegen_metadata.cpp:125-131` calls `getOrCreateMeta(dst)` first, then re-fetches `getMeta(src)`), so no local snapshot is required at the call site — unlike `propagateTypeMeta(name, val)` which IS subject to the #858 rehash gotcha.

**Why**: `setTypeMeta` only records the LLVM-level element type. Source-level string metadata (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`, `map_value_type_name`, etc.) is NOT touched. Without it, downstream code needing source-level type names (nested indexing on the result, `valueToString` formatting, generic dispatch) silently falls through to defaults — `slice(xs, 0, 1)[0][0]` where `xs: List<List<int>>` raises confusing "str does not support index access".

**Canonical references**: map merge (#961), `emitListSlice` (#1205), list `+` concat (#1648), `filter` / `sort` / `reverse` List branch (#1651), all set ops (#1651, `emitSetUnionCore` / intersection / difference / symmetric_difference). The redundant `set_elem_type_name` copy block at each set-op site was deleted because `propagateMeta` already copies that field (`src/codegen_metadata.cpp:158`).

**How to apply**: When adding or reviewing a collection helper returning `List<SameT>` / `Map<SameK, SameV>` / `Set<SameT>`, confirm the trailing `setTypeMeta` block is followed by `propagateMeta(src, newHeader)`. Helpers that change element type (`flatten`, `enumerate`, `zip`, `map`) need a different approach — see the `enumerate` / `zip` precedent in `src/codegen_call.cpp:366-443` that manually constructs `list_elem_type_name = "(T1, T2)"`.

### `propagateTypeMeta`: handle both `Option<T>` prefix and `T?` suffix — they are the same type

**Source**: #1003 (2026-04-16); extended by #1015 (`extractGenericTypeArg` in `codegen_match.cpp`)
**Tags**: codegen, metadata, option, shorthand, list_elem, propagateTypeMeta, OptionalType, extractGenericTypeArg

**Rule**: Whenever you add or modify a code path that recognises `"Option<..."` as a prefix string, also handle the `"T?"` suffix form. `OptionalType::toString()` (`src/type_node.cpp:51-52`) emits the `?` suffix form, and `OverloadEntry::returnTypeName` stores it verbatim. Functions declared as `-> List<int>?` have `returnTypeName = "List<int>?"`, not `"Option<List<int>>"`. A branch that only checks `resolved.compare(0, 7, "Option<")` will silently skip `T?` returns.

**How to apply**: After the `Option<T>` branch, add `else if (resolved.size() > 1 && resolved.back() == '?')` that strips the `?` and recurses: `propagateTypeMeta(resolved.substr(0, resolved.size() - 1), val)`. See `src/codegen_builtin.cpp:169-173` (#1003) and earlier precedent `src/codegen_arc_gc.cpp:136-138`. `resolveTypeAlias` is a pure string map lookup and cannot produce `?`-suffixed strings (the lexer tokenises `?` as a standalone `Question` token, so identifiers cannot contain it), making the `resolved.back() == '?'` guard safe.

**Known parallel gaps**: `extractGenericTypeArg` in `codegen_match.cpp` resolved in #1015 (both forms handled, ensuring the typed ARC retain path — Path 2a in `emitPatternBindingArc` — is taken for `SomePattern`). `src/codegen_stmt.cpp:430` checks `ann.substr(0, 7) == "Option<"` when propagating collection metadata from a variable's annotation — gap only matters when RHS carries no metadata (e.g. `x: List<int>? = None`); since `None` contains no collection, any subsequent index raises a runtime unwrap error before metadata is needed. Track in a future issue if a concrete regression is found.

### `low_level_type_name` mix check: convert empty metadata to "int" before comparing

**Source**: PR #1063 (CodeRabbit review)
**Tags**: codegen, type-metadata, checked-arithmetic, mix-check

**Rule**: Map empty metadata to a display sentinel before comparing low-level type names:

```cpp
const std::string &lhsLL = getLowLevelTypeName(lhs);
const std::string &rhsLL = getLowLevelTypeName(rhs);
const std::string lhsName = lhsLL.empty() ? "int" : lhsLL;
const std::string rhsName = rhsLL.empty() ? "int" : rhsLL;
if (lhsName != rhsName)
    codegenError(callee + "() cannot mix " + lhsName + " and " + rhsName);
```

**Why**: `getLowLevelTypeName` returns `""` for `int` (no `low_level_type_name` metadata). The three-way guard `!lhsName.empty() && !rhsName.empty() && lhsName != rhsName` only fires when both names are non-empty — so `int` (empty) + `i64` ("i64") silently passes. Two bare `int` args (`"int" == "int"`) correctly pass; `int` + `i64` (`"int" != "i64"`) is caught.

**Corollary — testing int vs. low-level mix**: A bare literal `1` alongside `1i64` may be coerced to `i64` by type inference, so `checkedAdd(1, 1i64)` appears to succeed. Use typed variables (`a = 1; b: i64 = 2i64`) to force the mix in tests.

### `/` is always float + IEEE 754; `//` and `%` are integer and error on zero

**Source**: #1023 (2026-04-16)
**Tags**: codegen, operators, ieee754, division, floor-div, modulo

**Rule**: `/` always returns `float` and follows IEEE 754 for zero divisors — `x / 0` → `±inf` (sign follows dividend), `0 / 0` → `nan`. Do NOT add an integer zero-division guard to the `/` branch of `emitArithmeticOp` (`src/codegen_expr.cpp:1333`) or the `(Int, Int)` branch of `__ry_any_div` (`src/runtime_any.cpp:229`).

The integer-semantics operators `//` and `%` DO error on zero divisor (`src/codegen_expr.cpp:1318,1364` and `__ry_any_floordiv` / `__ry_any_mod`). Low-level native-width integer `/` and `%` (`i32`, `u8`, …) also error (`src/codegen_expr.cpp:1170-1178`) because they are true integer division, not float-promoted.

Issue #754 previously added an int-specific guard to `/` that contradicted the "always float" spec; #1023 reverted that for `/` while keeping integer semantics (and the guards) for `//` and `%`.

### Implicit int↔float coercion is annotation-driven and limited to assignment-like sites

**Source**: #1192 (2026-04-19), extended by #1195 (return values), #1232 (FPToInt)
**Tags**: codegen, type-coercion, narrowing, widening, int, float, coerceToLowLevelType, applyCompoundOp

**Rule**: High-level `int` and `float` variables accept cross-type values implicitly at **var decl**, **reassign**, and **compound assign** sites. The fix lives in two places — `coerceToLowLevelType` (`src/codegen_call_user.cpp`) adds f64→i64 / i64→f64 branches for var decl and reassign; `applyCompoundOp` (`src/codegen_stmt_misc.cpp`) adds the same pair for compound assign, propagating to all 8 call sites (scalar local/global + 3 field-assign branches + array/map/list element). Narrowing uses `CreateFPToSI` (truncation toward zero, same as `x as int`).

**Why**: `**` and `/` always return `f64` (see #1023), so `x: int = 2 ** 3` and `x **= 2` failed pre-#1192. Forcing users to write `(2 ** 3) as int` everywhere was unergonomic, so coerce at the receiver based on annotation instead of changing the operator's return type.

**How to apply**:
- The gate is `!isLowLevelTypeName(typeName)` in `coerceToLowLevelType` and `getLowLevelTypeName(currentVal).empty()` in `applyCompoundOp`. Low-level types (`i64`, `f32`, `u8`, ...) never participate — they still require explicit `as` casts.
- **Do NOT** extend this to function arguments, if-expr branch unification, or struct field init — those still reject narrowing; users must write `as int`. Return values **are** coerced implicitly as of #1195 — same `coerceToLowLevelType` call inserted in both `emitStmt(ReturnStmt)` (`src/codegen_fn.cpp`, named fns + block-body lambdas) and the lambda expression-body path (`src/codegen_lambda.cpp`). Rejection tests in `tests/test_codegen.cpp::IntFloatCoercionBoundaryRejections` (args) and `IntFloatCoercionReturnLowLevelStillRejected` (low-level return types) guard remaining boundaries.
- Low-level compound assign mixing (e.g. `x: i64 = 5i64; x **= 2`) is rejected upstream in `emitArithmeticOp` via `checkLowLevelTypeMix` before reaching `applyCompoundOp`, **provided the loaded slot value carries its container's low-level metadata**. Fixed-size arrays (`buf: i64[1]`) require an explicit `propagateTypeMeta(array_elem_type_names_[ai], oldVal)` before `applyCompoundOp`, mirroring list/map element paths. Without that propagation, the f64→i64 branch silently truncates. Canary `IntFloatCoercionCanaryLowLevelStillRejected` exercises `x: i64 = 5i64; x **= 2` and `buf: i64[1] = [5i64]; buf[0] **= 2`.
- `fptosi` / `fptoui` on NaN, ±inf, or out-of-range goes through the unified `CodeGen::emitCheckedFPToInt` (see "Checked float→int narrowing" entry below). NaN / ±inf / out-of-range raise a runtime error and exit with status 1 rather than producing poison.

### Checked float→int narrowing: always go through `emitCheckedFPToInt`

**Source**: #1232 (2026-04-20)
**Tags**: codegen, conversion, runtime-check, fptosi, fptoui

**Rule**: New code MUST NOT call `builder_.CreateFPToSI` or `builder_.CreateFPToUI` directly on a user-visible value. Use `CodeGen::emitCheckedFPToInt(val, targetTy, typeName, bbPrefix, siteLabel)` instead. The helper inserts a NaN / ±inf / range guard before the narrowing LLVM op and branches to `emitRuntimeError` on failure, matching the project-wide convention (integer overflow, division by zero, bounds checks) of exiting with status 1 instead of producing poison.

**Why**: LLVM `fptosi` / `fptoui` return poison (LangRef) when the source is NaN, ±inf, or outside the target's representable range. Poison propagates silently through downstream arithmetic and is true UB. #1192 surfaced this in implicit `float → int` coercion; the fix sweeps every existing FPToSI / FPToUI site (`src/codegen_expr_cast.cpp`, `src/codegen_call_user.cpp`, `src/codegen_stmt_misc.cpp`, math rounding path in `src/codegen_call.cpp`) to share one helper.

**How to apply**:
- `typeName` drives signedness (via `isUnsignedLowLevelName` → `fptoui`) and the error message. Pass `"int"`, `"i32"`, `"u8"`, etc.
- `bbPrefix` names the fail/ok basic blocks and the error global; keep it unique per call site (`"cast_i"`, `"cast_u8"`, `"floor_i"`, ...).
- `siteLabel` (optional) prefixes the error with call-site context, e.g. `"floor()"` → `runtime error: floor(): cannot convert %g to int`. Leave empty for direct `as` casts.
- Helper accepts both f32 and f64: f32 is silently promoted to f64 for the range check; final narrowing uses the original `val` so no extra precision is lost.
- **Boundary semantics**: signed W-bit uses the half-open range `[-2^(W-1), 2^(W-1))`, unsigned uses `[0, 2^W)`. Correctly accepts `INT64_MIN` (exactly representable in f64) while rejecting `2^63` (which f64 rounds from `INT64_MAX`). Do NOT fall back to symmetric `fabs(x) >= 2^(W-1)` — that spuriously rejects `INT64_MIN`.
- When adapting math-style rounding (`floor` / `ceil` / `round` / `trunc`), pass the **result** of the runtime call — not the input — because `ceil(9.22e+18)` can round up past `INT64_MAX` even though the input was representable.

**How to verify**: `grep -nE 'CreateFPTo(SI|UI)' src/ include/` should match only the single call inside `emitCheckedFPToInt` itself.

**Related**: The inverse direction (`SIToFP` / `UIToFP`) is total — every integer is representable in f64 modulo IEEE-754 rounding, with no NaN / inf / out-of-range failure mode — so no runtime check needed. Do not reopen as a separate issue.

### ExtractValue on Option<Collection> or Result<Collection, E> inner field drops metadata — rebuild before recursive comparison

**Source**: #982, #985 (2026-04-16)
**Tags**: codegen, metadata, equality, option, result, collection, emitComparisonOp, ExtractValue

**Rule**: `builder_.CreateExtractValue(val, idx)` on `Option<T>` / `Result<T, E>` produces a fresh SSA value with no entry in `value_metadata_`, like a GEP-loaded pointer. For `Option<Collection>`, `Result<Collection, E>`, and symmetric `Result<_, Collection>` cases, the inner LLVM type is `ptrTy_` — without metadata, the recursive `emitComparisonOp` call falls through to the `isStringValue` + `strcmp` path and produces false-positive equality.

**How to apply**: After extracting inner values, check whether element type is `ptrTy_`. If so, call `buildTypeNameFromMeta(lhs)` (or `rhs` as fallback) to snapshot the inner type name, then `propagateTypeMeta(innerName, lhsInner)` + `propagateMeta(lhsInner, rhsInner)` before the recursive comparison. Guard with `!innerName.empty() && innerName != "str"`. Snapshot into a local `std::string` **before** calling `propagateTypeMeta` (#858 rehash gotcha).

**Why metadata is on the outer aggregate**: `buildSomeValue` and `buildOkValue` call `propagateMeta(inner, val)`, so the outer `Option` / `Result` aggregate carries `list_elem_type_name` / `map_*_type_name` / `set_elem_type_name`. The extracted inner is an orphan: no automatic inheritance from the containing aggregate. `buildErrValue` also calls `propagateMeta(inner, val)` (#985), so `Result<_, Collection>` Err-payload metadata is likewise on the outer aggregate.

**`propagateTypeMeta` handles `"Result<T,E>"` and `"Option<T>"` strings** (#985): When `propagateReturnTypeMeta` is called after a function returning `Result<List<int>, Error>`, `propagateTypeMeta("Result<List<int>, Error>", callResult)` extracts the Ok type "List<int>" and recurses. If Ok is not a collection, it falls back to Err type (covers `Result<int, List<int>>` Err-payload). The alloca for `a = makeResultFn()` thus carries `list_elem` metadata even without explicit annotation, so `buildTypeNameFromMeta` can recover the type name at compare time.

### Outer `emitResultBranch` PHIs must re-propagate Ok-path metadata from inner Result values

**Source**: #1315 (2026-04-23); inline retain mechanism added by #999
**Tags**: codegen, metadata, result, emitResultBranch, phi, http, resource

**Rule**: If a stdlib dispatcher first builds a metadata-carrying `Result<T, E>` on the success path (e.g. `wrapPtrAsResult(...)` + `addResourceKind(...)`) and then wraps it in an **outer** `emitResultBranch(...)` for a guard such as NUL validation, the outer merged PHI does **not** automatically inherit Ok-path metadata. Capture the successful inner Result in a local (e.g. `okIncoming`) and call `propagateMeta(okIncoming, mergedResult)` after the outer `emitResultBranch(...)`.

**Why**: metadata lives in `value_metadata_` keyed by SSA value. The inner success Result and the outer merged PHI are distinct SSA values, so the resource/collection kind on the former is invisible to later consumers (e.g. `Ok(resp)` case bindings) unless explicitly copied. Concrete manifestation: `httpGet` / `httpPost` tagged the inner `wrapPtrAsResult(...)` value as `HttpClientResponse`, but an outer NUL-check `emitResultBranch(...)` returned a fresh PHI without the tag — `status(resp)` / `body(resp)` inside `case Ok(resp): ...` were rejected as non-`HttpClientResponse` arguments.

**Inline retain in `buildOkValue` / `buildErrValue` / `buildSomeValue`** (#999): these now call `tryRetainArcSource(inner)` when `inner->getType() == ptrTy_`, retaining the collection before scope cleanup at fn exit can release the local. `tryRetainArcSource` handles three cases:
1. `LoadInst` from an ARC-managed alloca (emits retain — the direct-param bugfix case)
2. `arc_owned_values_` (no-op — `Ok([1, 2])` inline construction stays unaffected)
3. `ExtractValueInst` (record/tuple field access via `CreateExtractValue`) — retains only when collection metadata (`list_elem` / `map_key` / `set_elem`) is set on the value; guards against incorrectly retaining non-ARC `ptrTy_` values (closures, weak refs).

`codegen_expr.cpp` previously had a standalone `tryRetainArcSource(errVal)` after `buildErrValue` in the `?` operator error path. With Case 3 added, this became a double-retain and was removed — `buildErrValue` now handles the retain internally. Regression tests: `tests/spec/result_option_arc_return.test.ry`.

**Limitation**: For `Result<List<T>, List<U>>` where Ok and Err payloads are different-typed collections, the metadata on the outer aggregate reflects whichever payload was inserted last (Ok wins in `buildOkValue` since it runs after `buildErrValue`). Resolving cleanly would require per-variant metadata slots on `ValueMetadata`. Rare edge case, out of scope.

### Empty collection literal early-return in codegen_stmt.cpp does not propagate closure/nested-type metadata

**Source**: #958 (2026-04-15)
**Tags**: codegen, metadata, closure, set, collection, empty-literal

**Rule**: `codegen_stmt.cpp` has special early-return paths for empty collection literals (`a: Set<fn> = {}`, `a: List<fn> = []`) that allocate the header and `return` before reaching the general `if (newTy == ptrTy_)` metadata-propagation block (L390+). Each such early-return path must explicitly set `*_fn_type_info` and `*_elem_type_name` if the annotation's inner type is a closure or nested collection — otherwise metadata is missing for the variable's alloca, and downstream guards (e.g. `set_elem_fn_type_info` equality rejection at `codegen_expr.cpp`) silently won't fire.

**How to apply**: Whenever you add or modify an early-return empty-literal path in `codegen_stmt.cpp`, audit the block to ensure all of the following metadata fields are populated as appropriate:
- `setTypeMeta(TypeMeta::SetElem / ListElem / MapKey / MapValue, ptr, elemTy)`
- `getOrCreateMeta(ptr).set_elem_type_name` (nested collection element types)
- `getOrCreateMeta(ptr).set_elem_fn_type_info` (closure element types)
- `getOrCreateMeta(ptr).map_key_type_name` / `map_key_fn_type_info` (Map key side, #961, mirror of Set path)
- `list_*` and remaining `map_value_*` counterparts for List/Map early paths

**Why**: empty-literal fast paths (L28-58 for Set, L102-151 for List) were written for the common primitive/record case. The L102 List path correctly sets `list_elem_fn_type_info` via an explicit `else if`. The L28 Set path was initially missing the equivalent — `Set<fn> == Set<fn>` compiled without error instead of being rejected.

### Post-hoc Result coercion: preferred over modifying Ok/Err constructors for annotation-driven type resolution

**Source**: #1001 (2026-04-16, design choice); updated #1111 (`anyTy_` runtime branch + both-slot bailout fix); updated #1157 (disc-static provenance gate)
**Tags**: codegen_stmt, coercion, Result, Ok, Err, annotation, emitVarDecl, anyTy_, type-inference

**Rule**: When `Err([...])` (or `Ok(...)`) yields a Result struct whose layout doesn't match the variable's type annotation, fix the mismatch in `emitVarDecl`'s post-hoc coercion chain (`coerceResultType`) rather than threading the annotation down into the Ok/Err constructor emitter in `codegen_call.cpp`.

**Why**: The Ok/Err constructors can be called from many contexts (function arguments, return values, inlined expressions) where the target type is unavailable or ambiguous. Post-hoc coercion at the declaration site is localised, mirrors the existing Option auto-wrap pattern.

**How to apply**: `coerceResultType(val, dstResTy)` in `codegen_stmt.cpp`:
- Compute `okNeedsRuntimeBranch` and `errNeedsRuntimeBranch` **before** the early bailout. These check: `isAnyType(srcSlotTy) && !isAnyType(dstSlotTy) && dstSlotTy != i8Ty_ && canAnyHoldType(dstSlotTy)`. They must be available at the bailout call site (#1111 CodeRabbit review finding).
- **Bailout condition**: `srcOkTy != dstOkTy && srcErrTy != dstErrTy && (!okNeedsRuntimeBranch || !errNeedsRuntimeBranch)` → return `nullptr` (genuine type error). The original `both differ → nullptr` was wrong for `Result<any,any> → Result<T1,T2>`.
- Both slots differ AND both can be anyTy_-unwrapped → emit a **runtime disc branch** (disc=1 → Ok, disc=0 → Err), `unwrapFromAny` the any-typed payload to the dst concrete type in the active path, PHI the two rebuilt structs. A compile-time slot selection silently zeroes the active payload for the wrong disc value (#1111 Manifestation 1).
- **Single-slot mismatch — disc-static gate (#1157)**: Exactly one slot differs → **only** accept when `tryGetStaticResultDisc(val, &staticDisc)` returns true. This walks the `InsertValueInst` chain (accounting for LLVM constant-folding into `ConstantStruct`/`ConstantAggregateZero`) and recurses into `PHINode` incomings. Sources with a runtime disc (function call results, loads, mixed Ok/Err PHI) must return `nullptr` — they were silently miscompiling by zero-filling the active payload slot. Literal `Ok()`/`Err()` and if-exprs where all branches share the same disc remain accepted.
- **Placeholder types are NOT a safe signal at the LLVM level**: `Result<Unit,E>`, `Result<i8,E>`, and `Result<u8,E>` all lower to `{i1, i8, E}`. Checking `srcOkTy == i8Ty_` cannot distinguish a Unit dummy from a genuine i8 Ok payload. The disc value (static provenance) is the only reliable discriminant.
- The `Ok` emitter (`codegen_call.cpp`) must also unwrap `anyTy_` inner to the expected ok type from `fn_->getReturnType()` when building the Result struct — otherwise two branches of an if-expr (`Ok(x)` vs `Ok(0)`) produce different Result struct types and `validateBranchTypes` rejects them (#1111 Manifestation 2).
- Add the same coercion branch to variable reassignment handlers for consistency.

### Collection element-access emitters must apply the canonical any-widening pattern

**Source**: #1029 (2026-04-16) / #1065 (2026-04-16)
**Tags**: codegen, any, widening, collections, Map, List, Set

**Rule**: Every site reading or writing a value from/into a collection element slot must apply the canonical 3-branch widening pattern. Without widening, `"x" in s` on `Set<any>` (and equivalent List / Map cases) fails with "type mismatch" even though `any` accepts implicit conversion from all primitives.

Write sites (`Map` index-assign, `List` index-assign, `List.append!` / `appended` / `insert`, `Set.add`):

```cpp
if (val->getType() != elemTy) {
    if (isAnyType(elemTy))
        val = wrapInAny(val);              // concrete → any
    else if (isAnyType(val->getType()) && canAnyHoldType(elemTy))
        val = unwrapFromAny(val, elemTy);  // any → concrete (runtime-checked)
    else
        codegenError("... type mismatch");
}
```

Read sites (`in` / `not in` operator — Set, Map, List membership in `src/codegen_expr.cpp`): same 3-branch pattern on the LHS element before calling `emitSetElementLookup` / `emitMapKeyLookup` or entering the List linear search loop.

For `List` index-assign, the existing `tryEmitSubtypeCoerce` check must remain first.

**Additional rule for inline linear search loops with `anyTy_` elements**: List `in` uses an inline loop with a per-type comparison cascade. After widening, if `listElemTy` is `anyTy_`, the `icmp eq` / `fcmp oeq` path is invalid (LLVM rejects `icmp eq` on `{i64, [8 x i8]}` struct values — see "Set<any> element comparison" entry below). Hoist scratch allocas for `__ry_any_eq` **outside** the loop (mirroring `emitSetElementLookup` at `src/codegen_builtin.cpp:387-436` and `emitMapKeyLookup` at `:440-508`). Do **not** call `emitAnyBinaryOp` inside the loop — it creates two allocas per call and does not hoist.

**Files**: `src/codegen_stmt_misc.cpp` (map + list index-assign), `src/codegen_call_collection.cpp` (add, append, appended, insert), `src/codegen_expr.cpp` (`in` / `not in` Set / Map / List branches). When adding a new collection mutation or membership emitter, apply this pattern immediately.

### `Set<any>` element comparison requires `emitAnyBinaryOp`, not `emitComparisonOp`

**Source**: #1029 (2026-04-16)
**Tags**: codegen, any, Set, emitSetElementLookup, LLVM, icmp

**Rule**: `anyTy_` is a 16-byte struct `{i64 tag, [8 x i8] data}`. The `[8 x i8]` array field cannot be used as an operand to `icmp eq` — LLVM IR verification rejects it. In `emitSetElementLookup` (`src/codegen_builtin.cpp`), when `isAnyType(elemTy)`, replace `emitComparisonOp("==", elem, cand, "", "")` with `emitAnyBinaryOp("==", elem, cand)`, which calls `__ry_any_eq` at runtime.

The linear-scan path is taken automatically for `anyTy_` because it is a `StructType` (`llvm::isa<llvm::StructType>(elemTy)` is true), so no additional predicate is needed.

### `List<T>` annotations must preserve named pointer-backed inner types for index-load metadata rebuilds

**Source**: #1320 (2026-04-22)
**Tags**: codegen, metadata, list, index, annotation, JsonValue, resource

**Rule**: When recording `list_elem_type_name` from a `List<T>` annotation, preserve every non-primitive inner type name except `str` (which must keep using the `list_elem_is_str` side-channel from #1266). Limiting the stored name to nested collections/functions misses pointer-backed named types like `JsonValue` and stdlib resources (`Lock`, `RWLock`, etc.), so `xs[i]` cannot rebuild the loaded element's metadata via `propagateTypeMeta()`.

**How to apply**: In `emitVarDecl`, after handling `str` and function-typed inners, assign `list_elem_type_name = inner` for any inner type other than `int` / `float` / `bool`. This keeps index loads, loop bindings, and other metadata-reconstruction sites able to restore resource kinds or JSON dispatch metadata from the annotation even when the loaded LLVM type is just `ptr`.

### For-loop multi-variable destructure and `Set<X>` annotation propagation both trailed the List path until #1350

**Source**: #1350 (2026-04-24)
**Tags**: codegen, for-loop, destructure, set, tuple, metadata, set_elem_type_name

**Rule**: Two asymmetries between Set and List codepaths must be kept in sync:

1. **Multi-variable for-loop binding** (`src/codegen_stmt_loop.cpp` multi-var branch): use the same Set-first / List-fallback shape as the single-var path — switch local `headerTy` between `setHeaderTy_` and `listHeaderTy_`, set snapshot metadata to `TypeMeta::SetElem` vs `TypeMeta::ListElem`, prefer `set_elem_type_name` over `list_elem_type_name` when reading the tuple source name for `emitForBindingPattern`. List/Set headers share field 0 (len) and field 2 (data ptr), so existing GEP arithmetic is reusable. Pre-#1350 the multi-var branch treated "not a map" as "must be a list" and emitted `"for loop destructuring requires a list of tuples"` for any `Set<(T, U)>`.

2. **`Set<X>` annotation → `set_elem_type_name` propagation** in `codegen_stmt.cpp`'s `emitVarDecl`: use the denylist (`inner != "str" && inner != "int" && inner != "float" && inner != "bool"`) — same as List (#1320). Pre-#1350 this narrowed inner type to `{List|Map|Set|FnType}`, dropping named non-primitive tuple/enum/record/alias inners. Without the fix, `Set<(str, List<int>)>` annotations never carry `"(str, List<int>)"` down to the for-loop, and `sum(v)` in the destructured body fails with "sum() requires a list" even with codegen_stmt_loop fixed.

**Why**: Same class of asymmetry — every time a List-side enhancement lands (destructure dispatch, annotation denylist widening), the Set-side equivalent should be ported in the same PR unless there's a concrete reason to diverge. List/Set iteration machinery deliberately shares header layout, so divergent codepaths are a code smell.

**How to apply**:
- Before declaring a List-specific fix complete, grep sibling `getSetElementType` / `setHeaderTy_` / `set_elem_type_name` sites and check whether the same logic belongs there.
- The single-variable for-loop path (L324-407) is the canonical Set-first / List-fallback template; mirror both simultaneously for new iteration features.
- Test both shapes (`for a, b in listOfTuples`, `for a, b in setOfTuples`) whenever a binding-related for-loop change lands.
