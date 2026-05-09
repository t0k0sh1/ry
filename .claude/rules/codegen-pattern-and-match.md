---
paths:
  - "src/codegen_match.cpp"
  - "src/sema_return.cpp"
  - "include/ry/codegen.hpp"
---

# Codegen Pattern Matching

### Match subject types use two channels: subjectEnumName (narrow) vs subjectSourceTypeName (broad)

`emitPatternTest` / `emitPatternBindings` / `checkMatchExhaustiveness` take both names. `subjectEnumName` is enum/ADT only (used for `EnumPattern`, `Some`/`Ok`/`Err` extraction, `VariablePattern` `enum_value_type` write). `subjectSourceTypeName` reconstructs `Option<T>` / `Result<T,E>` / tuple from LLVM types but is lossy (`reverseResolveTypeName(ptr)` returns `"str"`, unknown structs return `"any"`), so it is safe only for structural checks. Never re-feed it into `extractGenericTypeArg` for ARC payload classification — `Option<List<int>>` reconstructs as `Option<str>` and uses the wrong header offset (-24 instead of -16), corrupting the heap. Use the lossless `source_type_name` metadata field (stamped by `propagateTypeMeta` at Result/Option branch entries) for ARC routing. Keep TuplePattern's `!sTy || !isTupleStructType(sTy)` defense-in-depth check so empty source names cannot let Option's 2-element struct silently pass arity checks.

### TuplePattern: per-element metadata via splitTypeArgs

In `emitPatternTest` / `emitPatternBindings`, when the subject's type signature is `"(T1, T2, ...)"`, strip outer parens, call `splitTypeArgs(inner)`, and recurse with each element's signature as `subjectEnumType`. Do NOT extend `propagateTypeMeta` to handle composite type strings — decomposition belongs in the pattern arm itself, mirroring `emitTupleDestructure`.

### RecordPattern bindings: do not pass primitive field type names as subjectEnumType

In `emitPatternBindings` for `RecordPattern`, only pass a field's Ry type as `subjectEnumType` when it is an actual enum (`enum_types_.count(elemSig) > 0`). For primitives and collections, pass empty string. Otherwise `VariablePattern` writes `enum_value_type = "int"` into metadata, `valueToString` then looks up `enum_types_["int"]` (creates a zero-initialized default entry via `operator[]`), reads its null `nameArray`, and crashes in `CreateGEP`. The danger is only in `emitPatternBindings`; `emitPatternTest` is safe because Literal/Wildcard ignore the field name and Record uses `pat->name` for lookup.

### Ok/Err/Some bindings: typeSig-driven propagateTypeMeta, not bulk propagateMeta

In the `OkPattern` / `ErrPattern` / `SomePattern` arms of `emitPatternBindings`, derive `innerSig` from the subject's `source_type_name` metadata via `extractGenericTypeArg("Result<", N)` (N=0 for Ok, 1 for Err) or `extractGenericTypeArg("Option<", 0)`, then call `propagateTypeMeta(innerSig, varAlloca)`. Do NOT bulk-copy with `propagateMeta(subjectAlloca, varAlloca)` — that leaks metadata from the *other* slot of the tagged union, e.g. `Result<List<int>, str>`'s Ok-side `list_elem_type_name = "int"` leaks into an Err binding whose actual runtime type is `str`, breaking `isStringValue` (compile error on `"prefix:" + e`) and crashing `valueToString` (reads list metadata off a str pointer). VariablePattern (whole-subject binding) keeps `propagateMeta` — it should inherit all subject metadata.

### ADT enum constructor pattern: unwrap single TuplePattern, gate payload tests by tag-match

`Event::Click((0, 0))` parses as `EnumConstructorPattern{bindings:[TuplePattern{0,0}]}` (one binding). When `bindings.size() == 1` and that binding is a `TuplePattern` whose arity equals the variant's field count, redirect the loop in BOTH `emitPatternTest` and `emitPatternBindings` to iterate the TuplePattern's elements instead (one-sided fix produces wrong runtime behaviour). Separately, never combine the tag equality with payload sub-tests via `CreateAnd` — it does not short-circuit, so payload loads execute even when the tag mismatches and dereferencing a `str` pointer slot from an `int`-tagged variant crashes. Gate payload loads with `CreateCondBr` on the tag-equality result; run the GEP/load/test loop in a dedicated payload BB; merge with a PHI: false from the tag-match block, `fieldsMatch` from the payload-end block.

### sema_return: EnumConstructorPattern covers a variant only when its payload is irrefutable

In `collectPatternInfo` (`sema_return.cpp`), only call `coveredVariants.insert(variant_name)` for an `EnumConstructorPattern` when every binding is irrefutable (Wildcard / Variable / recursively-irrefutable Tuple). `Event::Click((0, 0))` matches a strict subset of `Click` values; treating it as full coverage makes `isExhaustiveMatch` unsound and lets non-returning `case`s pass return analysis. Update `isIrrefutable` whenever a new pattern type is introduced; Literal / Enum / Some / None are always refutable.

### Case subject Result/Option struct allocas need arc_tagged_union_vars_, not arc_managed_vars_

When `CaseStmt` / `CaseExpr` materializes its subject into a `{i1, T, E}` (Result) or `{i1, T}` (Option) struct alloca and at least one slot is ARC-managed, register the alloca in `arc_tagged_union_vars_` AND in the case-level `scope_stack_` frame (synthetic key `$match_subject_<counter>`). `emitTaggedUnionRelease` extracts the `i1` tag and CondBr-dispatches to release only the active slot via `emitArcReleaseLoadedElement`. Do NOT register in `arc_managed_vars_` / `arc_str_managed_vars_` / `arc_field_record_vars_` — those expect a single ARC pointer at the alloca's address, not a struct. Without this side-table the per-arm payload retain leaks once per `case` eval per ARC slot. `T?` shorthand must be handled explicitly because `splitGenericTypeName` only recognizes `Option<T>`. Scope-stack registration also makes early-exit cleanup fire via `emitScopeCleanupToDepth`.
