---
paths:
  - "src/codegen_match.cpp"
  - "src/sema/sema_return.cpp"
---

# Codegen Pattern Matching

### Match subject types use two channels: subjectEnumName (narrow) vs subjectSourceTypeName (broad)

`subjectSourceTypeName` is lossy (`reverseResolveTypeName(ptr)` returns `"str"`). Never re-feed it into `extractGenericTypeArg` for ARC payload classification — `Option<List<int>>` reconstructs as `Option<str>` and uses the wrong header offset (-24 instead of -16), corrupting the heap. Use `source_type_name` metadata (stamped by `propagateTypeMeta` at Result/Option branch entries) for ARC routing.

### RecordPattern bindings: do not pass primitive field type names as subjectEnumType

Only pass a field's Ry type as `subjectEnumType` when it is an actual enum (`enum_types_.count(elemSig) > 0`). For primitives and collections, pass empty string. Otherwise `VariablePattern` writes `enum_value_type = "int"` into metadata, `valueToString` looks up `enum_types_["int"]` (creates a zero-initialized default entry via `operator[]`), reads its null `nameArray`, and crashes in `CreateGEP`.

### Ok/Err/Some bindings: typeSig-driven propagateTypeMeta, not bulk propagateMeta

Do NOT bulk-copy with `propagateMeta(subjectAlloca, varAlloca)` in `OkPattern` / `ErrPattern` / `SomePattern` arms — that leaks metadata from the other slot of the tagged union, e.g. `Result<List<int>, str>`'s Ok-side `list_elem_type_name = "int"` leaks into an Err binding whose actual runtime type is `str`, breaking `isStringValue` (compile error on `"prefix:" + e`) and crashing `valueToString`. `VariablePattern` (whole-subject binding) keeps `propagateMeta` — it should inherit all subject metadata.

### ADT enum constructor pattern: unwrap single TuplePattern, gate payload tests by tag-match

Never combine the tag equality with payload sub-tests via `CreateAnd` — it does not short-circuit, so payload loads execute even when the tag mismatches and dereferencing a `str` pointer slot from an `int`-tagged variant crashes. Gate payload loads with `CreateCondBr` on the tag-equality result.

### sema_return: EnumConstructorPattern covers a variant only when its payload is irrefutable

`Event::Click((0, 0))` matches a strict subset of `Click` values; treating it as full coverage makes `isExhaustiveMatch` unsound and lets non-returning `case`s pass return analysis.

### Case subject Result/Option struct allocas need arc_tagged_union_vars_, not arc_managed_vars_

Do NOT register in `arc_managed_vars_` / `arc_str_managed_vars_` / `arc_field_record_vars_` — those expect a single ARC pointer at the alloca's address, not a struct. Without `arc_tagged_union_vars_` registration, the per-arm payload retain leaks once per `case` eval per ARC slot.
