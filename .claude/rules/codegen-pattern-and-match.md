---
paths:
  - "src/codegen_match.cpp"
  - "src/sema_return.cpp"
  - "include/ry/codegen.hpp"
---

# Codegen Pattern Matching

### ## #1156: split match subject type into enum-only vs broad source name

**Source**: PR #1156 fix for `codegen_match.cpp` subjectEnumType wrong channel
**Tags**: pattern, match, ARC, codegen, Option, Result, tuple, subjectEnumType

**Rule**: `emitPatternTest`, `emitPatternBindings`, and `checkMatchExhaustiveness`
take **two** source-name parameters:

- `subjectEnumName` — narrow channel (`ValueMetadata::enum_value_type`).
  Only set for enum/ADT subjects (from `resolveEnumType()`). Used by
  `EnumPattern`/`EnumConstructorPattern` generic-instantiated lookup, by
  `Some`/`Ok`/`Err` binding `extractGenericTypeArg`, and by `VariablePattern`
  binding's `enum_value_type` write (still guarded by
  `enum_types_.count(resolveTypeAlias(name))` per the KNOWLEDGE L2946 rule).
- `subjectSourceTypeName` — broad channel (`resolveSubjectSourceTypeName()`).
  Reconstructs `Option<T>` / `Result<T, E>` / `(T1, T2, ...)` from the LLVM
  subject type when no enum annotation exists. Used by `TuplePattern` /
  `RecordPattern` structural verification and by `emitPatternBindingArc`
  Path 2a from `VariablePattern` only.

**Reconstruction lossiness**: `reverseResolveTypeName(ptrTy_)` returns `"str"`;
unknown structs return `"any"`. So `Option<List<int>>` reconstructs as
`"Option<str>"`. The reconstructed string is therefore **only** safe for
structural checks ("is this a tuple struct?" / "does this match record name X?").
Do NOT re-feed it into `extractGenericTypeArg` for ARC payload classification —
`Option<List<int>>` would be misclassified as `Option<str>` (wrong header
offset: str uses -24, List uses -16), causing heap corruption under ASan.

**Defense-in-depth on TuplePattern**: the test arm rejects via
`!sTy || !isTupleStructType(sTy)` regardless of any source name. This fires
even when both names are empty — closing the path where Option's `{i1, T}`
2-element struct silently passed the 2-arity check and crashed with ICmp
type mismatch (original #1156 crash vector).

**Why split rather than unify**: the previous single `subjectEnumType`
parameter was overloaded with non-enum names via the new broad helper
would force every consumer to add an `enum_types_.count(name)` guard.
The split signature makes "enum-only" vs "broader subject type" a
compile-time-enforced distinction. The guard at VariablePattern binding
(`KNOWLEDGE L2946`) remains necessary but is now purely defensive
(subjectEnumName is already enum-only).

**Follow-up**: Add a lossless `source_type_name` field to `ValueMetadata` so
`Option<List<int>>` reconstruction is accurate, enabling ARC Path 2a for nested
generics (currently handled by Path 2b heuristic via `propagateMeta`).
- Pre-existing defect in `emitListSlice`: ARC retain omitted for reference-typed elements (#1204) — tracked separately.

### Tuple pattern in `case`: per-element metadata via `splitTypeArgs`

**Source**: #834 (2026-04-16, implementation)
**Tags**: codegen, pattern, tuple, metadata, case, splitTypeArgs

**Rule**: When emitting a `TuplePattern` in `emitPatternTest` / `emitPatternBindings`, the subject's type signature arrives as a `"(T1, T2, ...)"` string in `subjectEnumType`. Strip the outer parentheses and call `splitTypeArgs(inner)` to obtain per-element signatures. Pass each element's signature as the `subjectEnumType` argument in the recursive `emitPatternTest` / `emitPatternBindings` calls so that nested Option/Result/generic patterns can resolve their metadata correctly.

**Why**: `propagateTypeMeta` (see `codegen-type-and-metadata.md` → "propagateTypeMeta is single-value; callers decompose tuples") is deliberately single-purpose and does not decompose tuple type strings. The correct decomposition point is the `TuplePattern` codegen arm itself, mirroring the existing `emitTupleDestructure` helper in `codegen_stmt_loop.cpp`.

**How to apply**: When a future pattern type (e.g., positional record `Point(a, b)`) also needs per-element metadata, use the same pattern: strip the outer delimiters, split with `splitTypeArgs`, and feed each component signature into the recursive call. Do NOT extend `propagateTypeMeta` to handle composite type strings.

### Record pattern in `case`: do NOT propagate primitive field type names as `subjectEnumType` in bindings

**Source**: #989 (2026-04-16, implementation)
**Tags**: codegen, pattern, record, metadata, case, enum_value_type, valueToString

**Rule**: In `emitPatternBindings` for `RecordPattern`, each field's Ry type string (`info.fields[i].type->toString()`) must be passed to `propagateTypeMeta` for collection/type metadata, but must **not** be passed blindly as `subjectEnumType` to the recursive `emitPatternBindings` call. Only pass it as `subjectEnumType` if the field type is an actual enum (`enum_types_.count(elemSig) > 0`). For primitives ("int", "float", "str", "bool") and collection types, pass empty string.

**Why**: The `VariablePattern` binding arm in `emitPatternBindings` does `getOrCreateMeta(varAlloca).enum_value_type = subjectEnumType` whenever `subjectEnumType` is non-empty. If `subjectEnumType` is "int", this stores `enum_value_type = "int"`. Then `valueToString` checks `enum_value_type`, looks up `enum_types_["int"]` (creates a zero-initialized default entry via `std::unordered_map::operator[]`), reads `nameArray` (null), and passes it to `CreateGEP` → LLVM asserts/crashes.

Records don't set `enum_value_type` on constructed values anywhere in the compiler (unlike enums which set it in `emitExprVariant(EnumExpr)` and `emitStructConstructor` in `codegen_call_dispatch.cpp`). So `subjectEnumType` for a record `case` subject is typically empty, and the per-field type from `struct_types_` is the authoritative source.

**How to apply**: Mirrored in `emitPatternTest` too: passing the full field type string as `subjectEnumType` in recursive `emitPatternTest` calls is safe (LiteralPattern and WildcardPattern ignore it; RecordPattern uses `pat->name` for lookup, not `subjectEnumType`). The dangerous place is **only in `emitPatternBindings`** where `VariablePattern` stores `subjectEnumType` directly into `enum_value_type` metadata.

### ErrPattern binding in codegen_match.cpp must propagateMeta to preserve collection element-type metadata

**Source**: #1001 (2026-04-16, implementation)
**Tags**: codegen_match, pattern, Result, Err, metadata, collection, propagateMeta

**Rule**: In `emitPatternBindings` (`codegen_match.cpp`), the `ErrPattern` arm must call `propagateMeta(subjectAlloca, varAlloca)` after storing the extracted Err payload — exactly like `OkPattern` (index 1) and `SomePattern` do. Without this call, the bound variable (e.g., `lst` in `Err(lst)`) loses the collection element-type metadata stored on the subject alloca. Any subsequent index access or collection-kind dispatch on the binding will then fail with "cannot determine list element type".

**Why**: `CreateExtractValue` produces a new LLVM `Value *` that does not inherit the custom metadata stored in the compiler's `value_metadata_` side-table. `propagateMeta` is the mechanism to copy that metadata to the new binding alloca. The same pattern was already applied to `OkPattern` and `SomePattern`, but the `ErrPattern` arm was accidentally left without it — there was no test that could trigger the gap before #1001 made `Err(collection)` construction possible.

**How to apply**: Whenever a new `xyzPattern` is added that extracts a sub-value from a subject alloca and introduces a binding variable, always add `propagateMeta(subjectAlloca, varAlloca)` after `CreateStore`. Review the neighbouring arms as a checklist.

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
