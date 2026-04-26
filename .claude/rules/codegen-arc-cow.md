---
paths:
  - "src/codegen_arc*.cpp"
  - "src/codegen_arc.cpp"
  - "src/codegen_arc_cow.cpp"
  - "src/codegen_stmt_misc.cpp"
  - "src/codegen_stmt_loop.cpp"
  - "src/codegen_match.cpp"
  - "src/codegen_expr_literal.cpp"
  - "include/ry/codegen.hpp"
---

# Codegen ARC and Copy-on-Write

### Element-slot writes must release the overwritten ARC pointer

**Source**: #855 (2026-04-11)
**Tags**: codegen, arc, collections, index-assign, slot-overwrite

**Context**: A list/map/set slot whose element type is itself an ARC-managed
collection holds the sole owning ref to the inner allocation — `append` /
collection-literal construction insert without retain, and the container's
`emitArcReleaseVar` only releases the *container's* header on scope exit
(its destructor walks elements, but only after the container itself dies).
Therefore overwriting a slot via `list[i] = v`, `m[k] = v`, or their
compound forms MUST load the previously-stored pointer and `emitArcRelease`
it before storing the new one, otherwise the prior inner allocation is
orphaned. The fix mirrors the canonical pattern in
`src/codegen_stmt.cpp::emitStmt(AssignStmt&)` ARC branch (the retain-then-
release-with-null-check sequence) but operates on a heap slot rather than
an alloca-backed variable.

`tryRetainArcSource` handles the retain only for two cases: a `LoadInst`
whose pointer operand is an ARC-managed alloca, and a value already in
`arc_owned_values_` (literals / fresh allocations). It does NOT retain for
a `LoadInst` from a GEP (cross-slot copy `xs[i] = ys[j]`), so the slot-
overwrite path manually retains via `emitArcGetHeaderFromData` +
`emitArcRetain` when `tryRetainArcSource` returns false. Order matters:
**retain new before releasing old** so self-assignment `xs[i] = xs[i]`
cannot transiently drop the refcount to zero.

**Rule**: When adding a write into any collection slot whose element is
pointer-typed and ARC-managed, follow this sequence:
1. Determine "is the slot's element type ARC-managed?" via
   `elementTypeIsArcManaged(containerPtr, kind, &elemKind)` — this checks
   the type *name*, not the LLVM type. Strings, weak refs, closures, and
   records are intentionally excluded.
2. For the **plain form**, retain the new value first
   (`tryRetainArcSource`, falling back to a manual `emitArcRetain` when
   the rhs is a Load from a GEP).
3. Load the old slot value with a null check.
4. Release the old value via `emitArcReleaseLoadedElement(...)` (the
   helper handles null guard + CFG branching).
5. Store the new value.
6. For the **compound form** (`xs[i] += v`), reuse the `oldVal` already
   loaded for `applyCompoundOp` rather than re-loading from the slot —
   preserves the "evaluate index/slot exactly once" rule from #812. The
   compound op produces a fresh ARC allocation that never aliases the
   slot, so no retain of the new value is needed.

**Verification gap** (resolved by #859): The ARC balance counter
(`runtime_internal.arc_live_count()`) is now available for delta-based
leak assertions — see the dedicated KNOWLEDGE entry below. ASan/LSan CI
coverage remains tracked separately.

**Follow-up landed**: Records and record fields with ARC fields
(`rec.arcField = newList`) share the same root cause but live in the
`FieldAssignStmt` write path (`InsertValue`/`store` of whole structs).
Fixed in #857 by mirroring the retain-then-release-old protocol in all
three `FieldAssignStmt` branches (VariableExpr / FieldAccessExpr /
IndexExpr) and introducing a string-based `fieldTypeIsArcManaged`
predicate that consults the AST `FieldDef.type->toString()` rather than
container metadata — record fields are typed by declaration, not by
runtime value metadata.

**Follow-up landed** (#854): Deep-chain middle-hop ARC ownership —
intermediate record hops that are shallow-copied through
`writeBackFieldChain`, and more generally any chained LHS whose root is
not a direct `VariableExpr` — is now handled by path CoW
(`emitPathCowForChain` in `src/codegen_arc_cow.cpp`), which drives
`emitCowCheckSlot` at each hop. Record-to-record assignment retains each
ARC field (`emitRecordArcFieldsRetain`) so path CoW can observe
strong_count > 1 at the record-field slot; scope exit releases the
fields (`emitRecordArcFieldsRelease`). See "Path copy-on-write isolates
chained writes through nested collections" earlier in this file for
the full protocol and unsupported shapes.

### `tryRetainArcSource` LoadInst cases must be metadata-gated

**Source**: #1266 (2026-04-21, fix)
**Tags**: codegen, arc, retain, tryRetainArcSource, container-element, GEP-load, memory-safety

**Context**: `tryRetainArcSource` is the central retain-side helper for AssignStmt
(new variable + reassignment), function return, call-site argument pass, match
binding, type coercion, and lambda capture. Until #1266 it covered only
LoadInst-from-alloca (Case 1), arc_owned values (Case 2), and ExtractValue with
container metadata (Case 3) — leaving GEP-loaded container elements (`xs[i]`,
`m[k]`, `for e in set { ... }`) without a retain. Once #1242 lands the
destructor-side recursive release, this gap immediately becomes a UAF: 
`inner = xs[0]; xs = []` would drop the inner list while `inner` still holds it.

**Rule**: When adding a new LoadInst case to `tryRetainArcSource`, gate on
container-element metadata exactly like Case 3:

```cpp
if (llvm::isa<llvm::LoadInst>(val) && val->getType() == ptrTy_) {
    auto *meta = getMeta(val);
    if (meta && (meta->list_elem || meta->map_key ||
                 meta->map_value || meta->set_elem)) { /* retain */ }
}
```

A bare `ptrTy_` type check is **not** sufficient: weak refs, capturing closures,
bare function pointers, and resource handles are all `ptrTy_` and would receive
spurious retains that crash on `−16` offset GEPs into header memory that does
not belong to ArcHeader. The metadata gate is the same protection #999 added to
Case 3 for ExtractValueInst.

**Why not switch all callers to `retainArcValue` instead?**: `retainArcValue`'s
fallback (`emitArcGetHeaderFromData(val)` + `emitArcRetain(...)`) is unconditional.
That works for IndexAssignStmt / FieldAssignStmt (#1108 / #857) where the
caller has already proven the slot is ARC-managed, but it is unsafe for
`tryRetainArcSource`'s callers — `return 42` (i64), `f(closure_arg)` (closure
ptr), or `let x = weak_ref` (weak ref) all reach these sites without an upstream
type guarantee. Adding the case inside `tryRetainArcSource` keeps the metadata
gate as the single audit point.

**`map_value` belongs in the gate too**: For `Map<K,V>`, `propagateTypeMeta` sets
both `MapKey` and `MapValue` flags — `m["a"]` returns the value side, so a gate
that omits `map_value` would miss it. Case 3 currently uses
`list_elem || map_key || set_elem`; if you ever add a Case-3-style retain on a
new instruction kind, include `map_value` (and double-check whether Case 3
itself is missing it for some scenario). Verified for Case 4 in #1266; not
audited for Case 3 yet.

**str elements need a separate flag and a separate header offset**: A str
container element (`xs: List<str>`, `m: Map<K,str>`) uses
`StringHeader` at offset −24, not `ArcHeader` at −16. Case 4 discriminates via
a dedicated `ValueMetadata::str_elem` bool and dispatches to
`emitStrGetHeaderFromData` instead of `emitArcGetHeaderFromData`.

**`str_elem` must ONLY be stamped in the List<str> / Map<K,str> / Set<str>
indexer paths** — `codegen_expr_literal.cpp` (List indexer reads
`list_elem_is_str`) and wherever a map/set str value is loaded. Do NOT stamp
it from a shared helper like `propagateTypeMeta("str", ...)`: that helper is
called from ~50 sites (pattern bindings, enum variant fields, Result/Option
payload, function return meta, tuple destructure, …) and many of those are
str values that are **not** container-element borrows. Stamping `str_elem`
there makes Case 4 dispatch through `emitStrGetHeaderFromData(val)` —
`val - 24` — on a pointer that does not own a `StringHeader`, corrupting
adjacent heap state. The corruption is often invisible under macOS libSystem
malloc but crashes under glibc (tested in CI Linux on `enum_generic_param_type.test.ry`'s
`MyOpt<str>::MySome(v)` pattern binding, SIGSEGV / exit 139). Do not be
tempted to reuse `low_level_type_name == "str"` either — `isLowLevelTypeName`
only matches numeric types (i8–i64, u8–u64, f32), so the branch would never
fire, and extending it to include `"str"` pollutes the arith/cast/tostring
paths that switch on `low_level_type_name`.

**Do NOT stamp `list_elem_type_name = "str"` as the signal for List<str>**:
That field is also read by `resolveCollectionDestructor`, and setting it to
`"str"` flips the destructor to the str-aware variant which iterates the
element buffer and calls `emitArcRelease` on each element. That is the right
destructor — but it exposes a pre-existing ARC live-count asymmetry: list
headers are allocated via `__ry_arc_alloc_counted` (counter +1) while str
handles go through `makeString` (counter no-op). Every str element release
then subtracts 1 from a counter it never contributed to, breaking
`arc_split_chars.test.ry` with a large negative delta. The destructor switch
belongs to #1242; #1266 only needs to reach the indexer. Use the side-channel
`ValueMetadata::list_elem_is_str` set by the AssignStmt annotation parser
(`codegen_stmt.cpp`) and read only by the list indexer
(`codegen_expr_literal.cpp` in `emitExprVariant(IndexExpr)` list branch) to
stamp `str_elem` on the loaded element without touching
`list_elem_type_name`. Map<K,str> does not need this side-channel because
`map_value_type_name = "str"` is already wired correctly on the destructor
side (Map values are released symmetric with the str allocation in
`makeStringKeyedMap`-style callers).

**Function parameters use a different path and do not need the side-channel**:
`applyParamTypeMeta` in `codegen_fn.cpp` calls `propagateTypeMeta(ptype,
alloca)` directly, so a `xs: List<str>` parameter alloca picks up
`list_elem_type_name = "str"` via the existing path. The list indexer then
sees that name, calls `propagateTypeMeta("str", elem)`, and `str_elem` lands
on the loaded element. This is safe for parameters because the parameter
alloca is a borrowed view — destructor selection happens at the **caller's
allocation site**, not at the parameter slot. Only AssignStmt (which IS the
allocation site for new variables) must avoid `list_elem_type_name = "str"`.

**Narrowing (#1353, #1354)**: The above "never stamp on the allocation path"
rule has since been narrowed to "never stamp **without** a matching insert-side
retain". PR #1353 (Map literals) and this PR's sibling fix for List/Set
literals (#1354) do stamp `map_key_type_name = "str"` /
`map_value_type_name = "str"` / `list_elem_type_name = "str"` /
`set_elem_type_name = "str"` on the literal's `headerPtr`, but only after the
literal path emits an explicit `retainArcValue` for each str handle via the
`isStrHandle` side-table check. The retain pre-increments the ARC counter by
+1 per element, which the str-aware destructor then decrements symmetrically —
the `makeString`/`__ry_arc_alloc_counted` counter asymmetry is compensated at
insert time rather than suppressed at dispatch time. The rule "no stamp
without retain" still governs the pure-annotation AssignStmt path (`xs:
List<str> = some_call()`): valMeta inheritance propagates the stamp set by
the literal / returned map / returned set, but the annotation fallback in
`codegen_stmt.cpp` still avoids stamping `list_elem_type_name = "str"` for
non-literal sources because those sources may not have paid the insert-side
retain (in practice they did, via their own literal path, but the
annotation-fallback branch runs only when valMeta is empty and cannot verify
this). Use `inner_is_str` side-channel there instead.

**Verification pattern**: Behavioural ARC tests using
`runtime_internal.arc_live_count()` cannot detect missed retains directly —
the counter tracks live allocations, not refcount. Use a FileCheck IR golden
under `tests/filecheck/` that asserts the `arc.retain:` / `arc.retain.done:`
basic-block labels appear after the container-element load, and that the
`getelementptr i8, ptr %elem, i64 -16` (ArcHeader) or `i64 -24` (StringHeader)
offset matches the element's actual header layout. See
`tests/filecheck/arc_retain_list_elem_*.ry`,
`tests/filecheck/arc_retain_map_value_new_var.ry`,
`tests/filecheck/arc_retain_list_str_elem.ry`, and
`tests/filecheck/arc_retain_map_str_value.ry` for canonical patterns.

### Pattern binding ARC and `emitPatternBindingArc`

**Source**: #997 (2026-04-16, fix)
**Tags**: codegen, arc, pattern, match, retain, memory-safety

**Rule**: Every pattern binding arm that extracts a sub-value (via
`ExtractValue` or `Load`) must call `emitPatternBindingArc(val, bindAlloca,
typeSig)` after storing the extracted value. Without this, scoped ARC release
will call `free(ptr - 16)` on a pointer that was never ARC-retained, causing
a crash or refcount underflow.

Affected patterns and what typeSig to pass:
- **SomePattern** / **OkPattern**: `innerSig = extractGenericTypeArg(subjectEnumType, "Option<"/"Result<", 0)`
- **ErrPattern**: `innerSig = extractGenericTypeArg(subjectEnumType, "Result<", 1)`
- **EnumConstructorPattern**: `fit->second.fieldTypeNames[bi]` for each field
- **VariablePattern** / **TuplePattern** / **RecordPattern**: pass `""` (fall through to source-alloca heuristic)

**What `emitPatternBindingArc` does**:
1. If `val` is a record struct (`llvm::StructType`) with ARC fields: retain the ARC fields for `LoadInst`/`ExtractValueInst` sources via `emitRecordArcFieldsRetain`, and insert `bindAlloca` into `arc_field_record_vars_`. Return.
2. If `val->getType() != ptrTy_` (scalar, non-ARC) → return.
3. If `typeSig` resolves to a collection type (`isCollectionTypeName`): retain + mark ARC-managed + insert into `arc_backed_vars_`. Return.
4. If `typeSig` is a function type (`isFunctionTypeName`): retain and mark ARC-managed **only for capturing/uniform closures** (detected via `fn_type_info` metadata on the source value). Bare function pointers are skipped. Return.
5. If `typeSig` is `"str"`: retain via `emitStrGetHeaderFromData` (offset −24), mark ARC-managed, insert into `arc_str_managed_vars_`. Return. (see **`str` is ARC-managed** below). Other non-collection non-closure pointer types: do not retain. Return.
6. Otherwise (no `typeSig`): fall through to `tryRetainArcSource(val)` (source-alloca heuristic); then check `arc_owned_values_`; then check collection-type metadata set by `propagateTypeMeta`.

**`str` is ARC-managed (#1046, updated #1105)**: `str` values are fully ARC-managed since PR #1046. Use `emitStrGetHeaderFromData(handle)` (offset −24, `STRING_HEADER_SIZE`) — **not** `emitArcGetHeaderFromData` (offset −16, `ARC_HEADER_SIZE`) — to get the ARC header for a `str`. Dispatch via `resolveTypeAlias(typeName) == "str"` or check `arc_str_managed_vars_.count(alloca) > 0` before calling retain/release helpers. The `arc_str_managed_vars_` side-table records all AllocaInsts whose type is `str`, enabling correct offset selection in `emitArcReleaseVar`. `fieldTypeIsArcManaged("str")` returns true with `outKind = CollectionKind::Str`. Collection destructors for `List<str>`, `Map<K,str>`, `Set<str>` iterate elements and call `emitArcRelease(emitStrGetHeaderFromData(elem))` before freeing the data buffer.

**ARC header offset dispatch — use helpers, not inline `emitArcGetHeaderFromData` (#1105)**: Never call `emitArcGetHeaderFromData` inline at retain/release sites that may handle `str` handles. Use these helpers instead:

- **`emitArcHeaderForAlloca(handle, alloca)`** — checks `arc_str_managed_vars_.count(alloca)` and dispatches to `emitStrGetHeaderFromData` / `emitArcGetHeaderFromData`. Use at all alloca-origin sites (e.g., `AssignStmt` old-value release).
- **`CapturedArcKind::Str`** enum variant — returned by `detectCapturedArcKind` when the captured alloca is in `arc_str_managed_vars_`. Closure construction retain (`codegen_lambda.cpp`) and closure destructor release (`codegen_arc_cow.cpp`) switch on this variant to call `emitStrGetHeaderFromData` instead of the default `−16` path.
- **`arc_str_owned_values_` guard in ExprStmt** — bare `str` temporaries (e.g., `"foo".to_upper()`) are in `arc_str_owned_values_`, not `arc_owned_values_`. `emitStmt(ExprStmt)` checks both sets and dispatches correctly.

**Why inline `emitArcGetHeaderFromData` is dangerous**: Adding a new ARC-managed type with a non-standard header offset (like `str`'s +8 byte `byte_len` field) requires updating every inline callsite. An inline call that misses the update silently corrupts memory — `−16` into a `StringHeader` lands on `weak_count`, so the atomic add increments the wrong field. Use the helpers above to make future additions opt-in-correct rather than opt-in-wrong. The `@parallel for` capture site in `codegen_stmt_loop.cpp` uses a local `capIsArcStr[i]` vector instead of `emitArcHeaderForAlloca` because the thunk's `FnScope` clears `arc_str_managed_vars_` before the thunk body executes; do not change that pattern.

### StringHeader layout — `str` memory representation (#1022)

**Source**: #1022 (2026-04-16)
**Tags**: str, StringHeader, ARC, memory-layout, NUL, byte_len, makeString

**Rule**: Every Ry `str` value is a pointer to the *data* region of a `StringHeader` block. Never treat a Ry `str` handle as a raw `char*` allocated with `malloc`/`strdup`; always use `makeString`/`makeStringUninit` to create them and `freeStringSlot` to free them.

**Layout** (defined in `include/ry/ry_layout.hpp` and `include/ry/runtime_string.hpp`):

```text
Offset from handle:  Field
  handle - 24       strong_count : i64  (ARC_IMMORTAL for literals; 1 for dynamic)
  handle - 16       weak_count   : i64  (0 always in PR #1022)
  handle - 8        byte_len     : i64  (STRING_BYTELEN_OFFSET = 8)
  handle + 0 ..     data bytes          ← the char* passed to Ry / codegen
  handle + byte_len '\0'                (null terminator for libc compat)
```

Key constants:
- `ARC_HEADER_SIZE = 16` (unchanged — collection headers keep this offset)
- `STRING_HEADER_SIZE = 24` (= ARC_HEADER_SIZE + 8 for the byte_len field)
- `STRING_BYTELEN_OFFSET = 8`

**How to get byte length in C++**: `stringByteLen(handle)` — reads `*(int64_t*)(handle - 8)`.  
**How to get byte length in LLVM IR**: `emitStringByteLen(handle)` — emits a GEP + load.  
**How to free a dynamic str**: `freeStringSlot(handle)` — calls `free(handle - STRING_HEADER_SIZE)`.  
**Literal globals**: created by `buildArcGlobal` in `src/codegen.cpp`; `strong_count = ARC_IMMORTAL` so retain/release are no-ops. GEP index is `(0, 3, 0)` into the struct.

**NUL safety**: All string operations are now fully NUL-safe. Complete list:
- `byte_len`, `length`, `==`/`!=`/`<`/`>`, `+`, `*`, Map/Set key hash+compare (#1022)
- `contains`, `starts_with`, `ends_with`, `find` (#1047)
- `replace` (#1048)
- `substring`, `char_at`, `reverse`, `split("", _)`, `for c in str:`, `enumerate(str)` (#1049)
- `is_empty` (#1069)
- `to_upper`, `to_lower`, `trim`, `trim_start`, `trim_end` (#1050)
- `split(str, delim)` with non-empty delimiter (runtime `__ry_str_split` via `memmem`), `join`, `repeat`, `*` operator (#1051)
- `regex_match`, `regex_search`, `regex_replace`, `regex_split`, `regex_find_all` and all UFCS variants (`is_match`, `search`, `replace`, `split`, `find_all`) — subject + pattern + replacement all length-driven, no `strlen` (#1052). Regex *literal* syntax also supports `\0` to encode a NUL byte in the pattern (`/a\0b/` matches `"a\0b"`) since #1076.
- `json.parse`, `json.stringify`, `json.to_str`, `json.get`, `json.keys` (#1053) — `JsonValue::string_val` and `JsonObjectVal::keys[i]` are now StringHeader handles allocated via `makeString`; `stringByteLen` is the single length source; `escape_string` iterates by index and emits `\u0000` for NUL bytes; `Parser` uses explicit `src_len` from `stringByteLen` instead of `strlen`/`'\0'` sentinel. C++ test helpers updated to wrap literals in `makeString` since `__ry_json_parse` / `__ry_json_get` expect StringHeader handles.
- `io.write_text`, `io.append_text` (#1133) — replaced `fputs(content, f)` (stops at first `\0`) with `fwrite(content, 1, stringByteLen(content), f)`. Binary-transparent round-trip for content now matches `io.write_bytes`. `fclose` return code is still checked so buffered-write errors surface as `Err`.

### `arc_str_managed_vars_` side-table for str ARC dispatch (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, side-table, offset, emitStrGetHeaderFromData

**Rule**: Whenever an `AllocaInst` holds a `str` value that participates in ARC (i.e., was retained on assignment), insert it into `arc_str_managed_vars_`. This side-table is the canonical way to select `emitStrGetHeaderFromData` vs `emitArcGetHeaderFromData` at release time in `emitArcReleaseVar`.

**Why**: LLVM IR has no type-level distinction between a `str` handle (`ptr` at offset −24) and a collection handle (`ptr` at offset −16). Without the side-table, every release would use the wrong offset for one of the types, silently corrupting the ARC header.

**How to apply**: After inserting into `arc_managed_vars_` (via `markArcManaged`) for a str-typed alloca, also insert into `arc_str_managed_vars_`. Intermediary tmp allocas created in RecordPattern / EnumConstructorPattern must be added too, so that downstream VariablePattern bindings loaded from them inherit the correct offset. Do NOT add allocas to `arc_backed_vars_` (the CoW set) for str — CoW uses `emitArcGetHeaderFromData` offset and would corrupt the str header.

### `@parallel for` str captures must use `emitStrGetHeaderFromData` (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, parallel_for, concurrency, offset, segfault

**Rule**: In `emitParallelForRange` (`src/codegen_stmt_loop.cpp`), when a captured variable is a `str` (i.e., it is in `arc_str_managed_vars_`), the thunk entry retain and the `arc_str_managed_vars_` side-table insertion for the thunk-local dst alloca must both use `emitStrGetHeaderFromData` (offset −24), not `emitArcGetHeaderFromData` (offset −16).

**Why**: Before #1046, str was never ARC-managed, so no str captures reached the retain path. After #1046, a `str` variable captured by `@parallel for` entered the retain path with the wrong offset: `emitArcGetHeaderFromData` pointed to `weak_count` (not `strong_count`). Each worker retained/released `weak_count` instead. When the last worker's release decremented `weak_count` to 0, the destructor called `free()` on a literal global's static allocation → segfault.

**How to apply**: Snapshot `capIsArcStr[i] = arc_str_managed_vars_.count(src) > 0` alongside `capIsArcManaged` and `capIsArcBacked` in the parent context (before `FnScope` clears the side-tables). In the thunk: after `markArcManaged(dst)`, if `capIsArcStr[i]` insert `dst` into `arc_str_managed_vars_`; at the retain site use `capIsArcStr[i] ? emitStrGetHeaderFromData(dataPtr) : emitArcGetHeaderFromData(dataPtr)`. The release via `popScope()→emitArcReleaseVar()` will then automatically select the correct offset via `arc_str_managed_vars_`.

### `emitPatternBindingArc` path 2b must propagate `arc_str_managed_vars_` from source (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, pattern-binding, emitPatternBindingArc, arc_str_managed_vars_, segfault

**Rule**: In `emitPatternBindingArc` path 2b ("no type signature — probe heuristically"), when `tryRetainArcSource(val)` returns true and `val` is a `LoadInst` from a source alloca in `arc_str_managed_vars_`, the destination `bindAlloca` must also be inserted into `arc_str_managed_vars_` — **not** into `arc_backed_vars_`. The check must mirror the full set hierarchy: closure → str → arc_backed.

**Why**: `emitArcReleaseVar` dispatches on the alloca's set membership to pick the header offset. `arc_backed_vars_` members use `emitArcGetHeaderFromData` (offset −16). For a `str` value, offset −16 is the `weak_count` field, not `strong_count`. When the `strong_count` check reads `weak_count = 0` instead of `ARC_IMMORTAL`, the immortal guard is bypassed and the code tries to atomically decrement `*(handle − 16)`. Since Ry string literal globals are compiled as `isConstant=true` LLVM globals (mapped read-only by the JIT), any write to them causes a SIGSEGV.

**How to apply**:
```cpp
// Correct propagation in emitPatternBindingArc path 2b:
if (auto *ld = llvm::dyn_cast<llvm::LoadInst>(val)) {
    auto *src = llvm::dyn_cast<llvm::AllocaInst>(ld->getPointerOperand());
    if (src && closure_managed_vars_.count(src))
        closure_managed_vars_.insert(bindAlloca);
    else if (src && arc_str_managed_vars_.count(src))   // ← must check str before arc_backed
        arc_str_managed_vars_.insert(bindAlloca);
    else
        arc_backed_vars_.insert(bindAlloca);
} else if (arc_str_owned_values_.count(val)) {          // non-LoadInst str source
    arc_str_managed_vars_.insert(bindAlloca);
} else {
    arc_backed_vars_.insert(bindAlloca);
}
```

This matters any time a `str` is extracted from an enum field via `EnumConstructorPattern` with a `VariablePattern` binding: the tmp alloca for the field is in `arc_str_managed_vars_`, and when `emitPatternBindings` recurses into `VariablePattern`, the resulting binding alloca inherits the str classification.

### Str literal globals are `isConstant=true` — wrong offset → SIGSEGV, not SIGABRT

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, StringHeader, isConstant, LLVM, offset, segfault

**Rule**: In `cachedGlobalString` (`src/codegen.cpp`), the StringHeader global for string literals is created with `isConstant=true`. The JIT maps such globals as read-only pages. Any code that uses `emitArcGetHeaderFromData` (offset −16) instead of `emitStrGetHeaderFromData` (offset −24) on a literal `str` will bypass the `ARC_IMMORTAL` guard (because `weak_count=0 ≠ INT64_MAX`), then attempt an atomic write to the read-only page → SIGSEGV (exit 139), not SIGABRT.

This makes wrong-offset bugs on str literals almost always fatal immediately, which is useful for diagnosis but means the stack trace will point into JIT code at an unusual address. Cross-reference with `arc_str_managed_vars_` membership at the crashing alloca's declaration site.

### `List<str>` / `Map<K,str>` / `Set<str>` destructor specialization (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, collection, destructor, List, Map, Set, memory-leak

**Rule**: `getOrCreateCollectionDestructor` uses a `(CollectionKind, elemSig, valSig)` tuple cache key, not `CollectionKind` alone. Caching by kind only would produce a single generic destructor that does not release str elements, causing leaks for any `List<str>`, `Map<K,str>`, or `Set<str>`.

**Why**: When `elemSig == "str"` (or `valSig == "str"` for Map values), the destructor must iterate the dense element array and call `emitArcRelease(emitStrGetHeaderFromData(elem))` for each element before freeing the buffer. The generic destructor (cache key with empty sigs) only frees the data buffer.

**How to apply**: `resolveCollectionDestructor(alloca)` reads `list_elem_type_name` / `map_key_type_name` / `map_value_type_name` from the alloca's `ValueMeta` and calls `resolveTypeAlias` before passing to `getOrCreateCollectionDestructor`. Always ensure the alloca carries the correct type name metadata before this call.

**Extension (#1108)**: The same requirement applies to `emitArcReleaseLoadedElement`. When an ARC-managed element (e.g. `List<str>`) is stored in a slot (`List<List<str>>` element, `Map<K, List<str>>` value, record field), overwriting the slot must call `getOrCreateCollectionDestructor` with the **inner** elem/val signatures derived from the element's type name, not the generic (empty-sig) version. Fix: `emitArcReleaseLoadedElement` now accepts `elemTypeName` and calls `resolveTypeAlias` + `splitGenericTypeName` internally to derive `innerElemSig`/`innerValSig`. Callers must pass the declared element type name; passing `""` falls back to the generic destructor (leaks inner str). Rule: any new ARC release helper that calls `getOrCreateCollectionDestructor` must pass the full element type name, not just the `CollectionKind`.

### CoW copy of `List<str>` must retain str elements (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, CoW, copy-on-write, double-free

**Rule**: When CoW-copying a `List<str>` (or `Map<K,str>`, `Set<str>`), str elements **must** be retained immediately after the copy, regardless of whether the `retainElements` parameter is `true`. In `emitCowCheckSlot`, `doElemRetain` is forced `true` when `elemArcKind == CollectionKind::Str`.

**Why**: Before #1046 the `List<str>` destructor did not release str elements (only freed the buffer), so no retention was needed during CoW. After #1046 the destructor releases elements — without retention the two lists (old + CoW copy) both try to release the same str elements, causing double-free.

**How to apply**: In `emitCowRetainArcElements`, pass `elemArcKind` and use `emitStrGetHeaderFromData(elem)` (offset −24) when `elemArcKind == CollectionKind::Str`, otherwise `emitArcGetHeaderFromData(elem)` (offset −16). The wrong offset silently corrupts `weak_count` or `byte_len` instead of `strong_count`.

### `memcpy` of an element buffer must be paired with ARC retain (#1204)

**Source**: #1204 (2026-04-19)
**Tags**: codegen, arc, collections, slice, memcpy, retain, memory-safety

**Rule**: Any codegen path that duplicates a container's element buffer with `memcpy` — `emitListSlice`, `emitCollOp_take_impl`, `emitListConcat`, future helpers — must follow the `memcpy` with a retain loop when the source element type is ARC-managed (`List<str>`, `List<List<T>>`, `List<Map<K,V>>`, closures, …). The canonical pattern is:

```cpp
CollectionKind elemArcKind = CollectionKind::List;
if (elementTypeIsArcManaged(srcListPtr, CollectionKind::List, &elemArcKind)) {
    emitCowRetainArcElements(newData, count, "<tag>", elemArcKind);
}
```

**Why**: `memcpy` copies raw pointers — it does not bump the embedded ARC strong_count. When the source list is later released (scope exit, reassignment, destructor), its destructor walks the element buffer and decrements each element's refcount. Without retention the new buffer's elements are freed out from under it, producing a dangling pointer the holder will eventually dereference (heap-use-after-free on subsequent read, double-free on subsequent release).

**How to apply**: `elementTypeIsArcManaged(containerPtr, kind, &outElemKind)` reads `list_elem_type_name` / `map_*_type_name` from the *source* container's metadata — make sure the source pointer (not the newly-allocated header) is passed. Scalar element types (`List<int>`, `List<f64>`) naturally take the false branch, so the scalar path is unchanged. `emitCowRetainArcElements` handles str offset (−24) vs collection offset (−16) internally based on `elemArcKind`, so the caller does not need to distinguish. `emitCowClone` in `src/codegen_arc_cow.cpp` is the reference implementation — it performs the same memcpy+retain sequence for the CoW path.

**Related**: #1046 (CoW str retain), #855 (slot overwrite release), #1184 (slice helper extracted from `emitCollOp_slice`), #1235 (fixed for `emitCollOp_take_impl`), #1236 (same defect in `emitListConcat`).

### `markArcManaged(tmp)` pre-mark must be guarded by `fieldTypeIsArcManaged`, and `str` fields must also be inserted into `arc_str_managed_vars_`

**Source**: #1016 (updated #1046)
**Tags**: ARC, str, pattern-binding, arc_str_managed_vars_, CollectionKind

TuplePattern / RecordPattern / EnumConstructorPattern pre-mark a temporary alloca
(`tmp`) as ARC-managed so the recursive leaf `VariablePattern` binding can emit a
single retain via `tryRetainArcSource` Case 1. Without the guard, _any_ `ptrTy_`
field (including bare fn-ptr and resource ptrs) is incorrectly marked.
Fix (post-#1046): use `CollectionKind fk; if (fieldTypeIsArcManaged(elemSig, &fk)) { markArcManaged(tmp); if (fk == CollectionKind::Str) arc_str_managed_vars_.insert(tmp); }` at all three sites (`src/codegen_match.cpp`). `fieldTypeIsArcManaged` returns true for List/Map/Set/str and false for fn-ptr/resource/etc. The `arc_str_managed_vars_` insertion is required so `tryRetainArcSource` Case 1 dispatches to `emitStrGetHeaderFromData` (offset −24) instead of `emitArcGetHeaderFromData` (offset −16) for str fields. Capturing closure in tuple/record/enum fields is intentionally excluded: `fn_type_info` metadata is not propagated by `propagateTypeMeta` onto `ExtractValue` intermediates, so closure detection is impossible here; this was also the pre-#1008 behaviour.

### `emitStrGetDataPtr` must insert into `arc_str_owned_values_`, not `arc_owned_values_`

**Source**: PR #1148 review (2026-04-18)
**Tags**: codegen, str, ARC, arc_str_owned_values_, emitStrGetDataPtr, StringHeader

**Rule**: `emitStrGetDataPtr` recovers the str data handle from a StringHeader pointer (GEP by `+STRING_HEADER_SIZE`). The result must go into `arc_str_owned_values_` (not `arc_owned_values_`), because downstream retain/release for str uses `emitStrGetHeaderFromData` (offset −24) while collection ARC uses `emitArcGetHeaderFromData` (offset −16). Putting a str handle in `arc_owned_values_` causes every subsequent retain/release to corrupt memory by reading the wrong 8 bytes.

**How to apply**: Search for `arc_owned_values_.insert` at call sites that received their value from `emitStrGetDataPtr` or from a `GEP(i8*, strHeaderPtr, STRING_HEADER_SIZE)` — these should be `arc_str_owned_values_.insert` instead.

### Map CoW must retain str keys independently of value retention

**Source**: PR #1148 review (2026-04-18)
**Tags**: codegen, CoW, Map, str, ARC, emitCowClone, elementTypeIsArcManaged

**Rule**: `elementTypeIsArcManaged(containerPtr, CollectionKind::Map, &kind)` only checks `map_value_type_name`, not `map_key_type_name`. Therefore the CoW clone loop (`emitCowClone`) retains values but never keys. For `Map<str, V>`, str keys are released by the destructor but never retained after the clone — use-after-free. After the value-retention block, emit a separate key-retention loop if `meta->map_key_type_name` resolves to an ARC-managed type (check via `fieldTypeIsArcManaged`). The Map destructor already handles str key release (L964 in `codegen_arc.cpp`: `if (elemSig == "str") emitStrElemLoop(keys, len, "mkey")`), so the missing piece is only on the retain side.

### Map literal `{k: v}` with str keys/values needs side-table retain + explicit str stamp

**Source**: #1347 (2026-04-24)
**Tags**: codegen, Map, literal, str, ARC, inferCollectionTypeName, retainArcValue, map_key_type_name

**Rule**: In `emitExprVariant(MapExpr)` (`src/codegen_expr_literal.cpp`), the existing retain gate `keyTypeName = inferCollectionTypeName(keyVals[0])` returns `""` for plain str values, so the gate short-circuits at `!keyTypeName.empty()` = `false`. A parallel `isStrHandle(v)` side-table check (`arc_str_owned_values_.count(v) > 0 || LoadInst from arc_str_managed_vars_ alloca`) is required; then call `retainArcValue(keyVals[i])` unconditionally on str handles. `retainArcValue → tryRetainArcSource` Case 1 emits retain for bound `LoadInst`; Case 2b is a **transfer-semantic no-op** for fresh `+1` `makeString` values (returns `true` without emitting retain), so the unconditional call doesn't leak on `{"foo": "bar"}`.

The retain side alone is insufficient for Map literals: the non-empty-literal VarDecl path in `codegen_stmt.cpp` only propagates `map_value_type_name` from `val`'s meta, not `map_key_type_name`. The str annotation at `L269-271` only fires for the empty-literal branch. Add symmetric `map_key_type_name` meta propagation from `valMeta` / `loadMeta` / annotation in the non-empty branch (mirror of the existing `mvtn` block), AND stamp `map_key_type_name = "str"` / `map_value_type_name = "str"` on the literal's returned `headerPtr` when `isStrHandle` detects str keys/values. Without those stamps, `resolveCollectionDestructor` dispatches to `__ry_arc_dtor_map` with empty sigs, so `emitInnerReleaseLoop` returns false for the keys array and the str keys never get released — swapping a UAF for a leak.

Why this differs from #1346 SetItem: `m: Map<str, str> = {}` (empty literal, then `m[k] = v`) hits the annotation-driven L269-282 path that stamps both `map_key_type_name` and `map_value_type_name`, so the #1346 fix only had to lift the `!= CollectionKind::Str` gate. `m: Map<str, str> = {k: v}` (non-empty literal) never hits L269-282 and has a fundamentally different root cause (`inferCollectionTypeName` returns empty for str) even though the user-visible symptom ("map key not found") is identical.

**Scope note**: List<str> / Set<str> literal variants were deferred when #1347 landed and are resolved in #1354 by the same side-table retain + explicit stamp pattern. The #1266 carve-out's "no stamp without retain" rule was narrowed (see the #1266 entry): stamp-with-retain is safe because the per-element retain cancels the `makeString`/`__ry_arc_alloc_counted` counter asymmetry at insert time, so the str-aware destructor can run without the large-negative `arc_live_count` delta that originally motivated the carve-out.

### List/Set literal `[s]` / `{s}` with str elements needs side-table retain + explicit str stamp (#1354)

**Source**: #1354 (2026-04-24)
**Tags**: codegen, List, Set, literal, str, ARC, inferCollectionTypeName, retainArcValue, list_elem_type_name, set_elem_type_name

**Rule**: In `emitExprVariant(ListExpr)` and `emitExprVariant(SetExpr)` (`src/codegen_expr_literal.cpp`), mirror the PR #1353 Map fix exactly: use the `CodeGen::isStrHandle` member helper (declared in `include/ry/codegen.hpp`, implemented in `src/codegen_arc.cpp`) to consult both `arc_str_owned_values_` (fresh `+1` `makeString` values) and `arc_str_managed_vars_` (borrowed `LoadInst` sources), apply `retainArcValue` per-entry when `elemTy == ptrTy_ && isStrHandle(v)`, track `anyElemIsStr` for a post-loop stamp of `list_elem_type_name = "str"` / `set_elem_type_name = "str"` on the `headerPtr`. For Set the retain must fire inside the `insertBB` branch (not the main loop body) so that duplicate insertions skipped by dedup do not double-retain. Then in `codegen_stmt.cpp` List tracking, propagate the literal's `list_elem_type_name = "str"` stamp into the indexer-facing side-channel `list_elem_is_str = true` via a single `if (letn == "str") inner_is_str = true;`.

**Why**: `inferCollectionTypeName` returns `""` for plain str values (same root cause as #1347 Map case), so the existing retain gate `element_needs_retain(elemTy)` / `setElemNeedsRetain` / `elemNeedsRetain` short-circuits. Without the side-table retain, `List<str> = [s]` / `Set<str> = {s}` with locally-constructed str elements produces a dangling pointer once the source var goes out of scope. Without the explicit `"str"` stamp on the literal header, `resolveCollectionDestructor` picks the non-str variant and the str elements silently leak. The per-entry retain + post-loop stamp pattern (not "first-entry-only" heuristic) is required because mixed-origin literals like `[bound_s, "literal_s"]` alternate between `LoadInst` and `ExtractValue` origins that escape first-entry detection.

**Chosen resolution framing — "third option"**: The issue body listed two options: (1) reconcile the counter conventions by making `makeString` increment by +1 to match `__ry_arc_alloc_counted`, or (2) provide a parallel dispatch path for str elements. This PR adopts neither, and instead mirrors PR #1353: side-table retain + explicit stamp is the pragmatic path that (a) fits a v0.0.13 hotfix scope, (b) matches the empirical delta = +1 per function call baseline already observed for `Map<int,int>`, and (c) leaves Option 1 (global counter reconciliation) for v0.0.14+ where `include/ry/ry_layout.hpp:29-32` already flags str as "not fully ARC-managed by codegen yet".

**How to apply**: When you see `element_needs_retain(elemTy)` (or its Map/Set analogues) gating a retain on an `inferCollectionTypeName`-returned name, and the element type is `ptrTy_`, audit whether str values can flow through that site with locally-constructed origins. If yes, add the `isStrHandle` side-table check as an OR condition and stamp the corresponding `*_type_name = "str"` on the literal header so the str-aware destructor matches the insert-side retain.

### Record ARC reassignment: use `!CallInst && !InvokeInst` not `LoadInst || ExtractValueInst`

**Source**: PR #1148 review (2026-04-18)
**Tags**: codegen, ARC, record, reassignment, InsertValueInst

**Rule**: The guard in `codegen_stmt.cpp` (`AssignStmt` record-with-ARC-fields path) that determines whether to call `emitRecordArcFieldsRetain` must be `!isa<CallInst>(val) && !isa<InvokeInst>(val)` — **not** `isa<LoadInst>(val) || isa<ExtractValueInst>(val)`. Fresh constructions (CallInst = direct call, InvokeInst = call with unwind) are sole owners and must not be retained. Every other value — including `InsertValueInst` chains like `r2 = { r.field, new_val }` — is a view of existing state and must be retained. The original allowlist of two cases was too narrow and caused use-after-free for `InsertValueInst` records.

### `str` does not support `[]` index syntax — guard with `isStringValue` before list/map dispatch

**Source**: #1026 (2026-04-16, diagnostic improvement)
**Tags**: codegen, diagnostics, str, index-access, emitExprVariant, IndexExpr, IndexAssignStmt

**Rule**: In `emitExprVariant(IndexExpr)` (`src/codegen_expr_literal.cpp`) and the `IndexAssignStmt` emitter (`src/codegen_stmt_misc.cpp`), `str` values are `ptrTy_` pointers and pass the "must be ptr" gate. Without an explicit guard they fall through the Map check (no map metadata) and then either (a) hit the List fallthrough and emit the misleading `"cannot determine list element type for index access"` error or (b) reach `emitCowCheck(objPtr, ..., CollectionKind::List)` which may corrupt state.

**Fix**: After the `"index operator requires list or map"` gate and before the Map dispatch, check `isStringValue(objPtr)` and emit a targeted diagnostic:
- Read path: `"str does not support index access; use char_at(s, i) instead"`
- Write path: `"str does not support index assignment; strings are immutable"` — and critically, this guard must come **before** the `emitCowCheck(objPtr, receiverAlloca, CollectionKind::List)` call; passing a `str` pointer with `CollectionKind::List` to `emitCowCheck` is unsafe.

**Why `isStringValue` is the right predicate**: `isStringValue(val)` (`src/codegen_any.cpp:28`) returns true when `val->getType() == ptrTy_` and `isNonStrPointer(val)` is false (i.e., no collection/resource metadata). Lists, Maps, Sets, and resource handles all have metadata (`hasAnyMeta() == true`), so only `str` triggers the new guard. This is the canonical predicate used throughout codegen to distinguish `str` from other `ptrTy_` values.

- **#1184 related**: The same emitter also rejects index values that are `RangeExpr` nodes and routes them to the slice path — see "IndexExpr first index is scalar only — RangeExpr routes to emitListSlice" below.

### `emitExprVariant` (CaseExpr): capture `armEndBB` AFTER `popScope()`

**Source**: #997 (2026-04-16, fix)
**Tags**: codegen, arc, match, phi, case-expr, ir-verification

**Rule**: In `emitExprVariant` for CaseExpr (`src/codegen_match.cpp`), always
record `armEndBB = builder_.GetInsertBlock()` **after** `popScope()`, never
before.

**Why it matters**: `emitPatternBindingArc` may insert ARC retain calls
(`arc.retain` / `arc.retain.done` BBs), and `popScope()` → `emitArcReleaseVar`
may insert `arc.var_release` / `arc.var_skip` BBs. Both operations advance the
insert block. Recording `armEndBB` *before* `popScope()` captures a stale BB
as the PHI predecessor; the actual branch to `mergeBB` comes from the final
`arc.var_skip` BB, causing:

```
IR verify error: PHI node entries do not match predecessors!
  %match.expr = phi i64 [ 0, %match.expr.then ], [ %list_len, %arc.retain.done ]
label %arc.retain.done
label %arc.var_skip
```

**Correct pattern**:
```cpp
llvm::Value *armVal = emitExpr(*arm.value);
popScope();                                          // changes insert block
llvm::BasicBlock *armEndBB = builder_.GetInsertBlock(); // correct post-cleanup BB
builder_.CreateBr(mergeBB);
incoming.push_back({armVal, armEndBB});
```

The same rule applies to any code that records an insert-block BB for later
PHI use while a scope with ARC-managed variables is still open.

### `isStringValue` must not be used to dispatch `weak` header offset

**Source**: #1022 (2026-04-16, implementation)
**Tags**: codegen, weak, str, StringHeader, isStringValue, emitWeakExpr

**Rule**: Do not use `isStringValue(val)` to choose between `STRING_HEADER_SIZE` (24) and `ARC_HEADER_SIZE` (16) offsets when computing the ARC header pointer for a `weak` variable. `isStringValue` returns `true` for **any** `ptrTy_` value that has no `hasAnyMeta()` flag — captured `List<int>` / `Map` / `Set` values inside closure bodies lack collection metadata after the capture injection in `codegen_fn.cpp` / `codegen_lambda.cpp`, so they are misclassified as `str` and get the wrong `-24` offset.

**How**: The inner type name is available from the annotation at the `LetStmt` site (`weakInnerTypeName(*annot)`) and from `weak_inner_type_names_[ptr]` at reassignment. Use `innerName == "str"` there:

```cpp
// codegen_stmt.cpp — initial let
llvm::Value *headerPtr = (innerName == "str")
    ? emitStrGetHeaderFromData(val)
    : emitArcGetHeaderFromData(val);

// codegen_stmt.cpp — reassignment (ptr is the existing weak alloca)
auto it = weak_inner_type_names_.find(ptr);
const std::string &weakInner = (it != weak_inner_type_names_.end())
    ? it->second : std::string{};
llvm::Value *headerPtr = (weakInner == "str")
    ? emitStrGetHeaderFromData(val)
    : emitArcGetHeaderFromData(val);
```

`emitExprVariant(WeakExpr)` (codegen_expr.cpp) must return the raw data pointer; the conversion happens at the call sites above, not inside the emitter.

### Ry records are SSA struct values, not pointers — nested field writes unroll up to the root alloca

**Source**: #812 (2026-04-11, implementation)
**Tags**: codegen, records, lvalue, insertvalue, chained-lhs

**Context**: Records in Ry are *value types*: a `record Point: x: int; y: int`
is emitted as `{i64, i64}` stored in an alloca. There is no stable address
for an individual field — `CreateLoad(struct) → ExtractValue(x) → ...` gives
you an SSA value, not a pointer you can `CreateStore` into. The #812 fix
for `rec.a.b = v` therefore has to be: load the outer struct, `InsertValue`
at the right field index, and if the target lives deeper in a chain
(`rec.a.b.c = v`) walk back up inserting each updated inner struct into its
parent until we reach the root alloca and do a single `CreateStore`.

**Rule**: When implementing any write through a record field chain, treat
every intermediate record as an SSA value. Never try to take a pointer into
a struct field — use `ExtractValue` / `InsertValue` instead, and end the
chain with one store at the root alloca. See `writeBackFieldChain` in
`src/codegen_stmt_misc.cpp` for the reference pattern.

### Compound assignment must be expanded in codegen, not desugared in parser

**Source**: #812 (2026-04-11, implementation)
**Tags**: codegen, compound-assign, side-effects, chained-lhs

**Context**: Naively desugaring `a[f()] += v` as `a[f()] = a[f()] + v` in
the parser would evaluate `f()` twice — once for the load and once for the
store. That violates the "each subexpression evaluates once" rule and
multiplies CoW work. The fix for #812 introduced `applyCompoundOp` in
`src/codegen_stmt_misc.cpp` and expanded `IndexAssignStmt` / `FieldAssignStmt`
with an optional `compound_op` field that codegen interprets as
"load at the resolved slot → apply op → store at the same slot" — a single
index evaluation per statement.

**Rule**: When adding a new compound-assignment-capable statement form, do
NOT desugar in the parser. Reuse `applyCompoundOp` and evaluate the LHS
address exactly once in codegen. For maps, a compound op on a missing key
is a runtime error (`"compound assignment to missing map key"`) — users
must insert the key explicitly before accumulating.

### Path copy-on-write isolates chained writes through nested collections

**Source**: #812 (shallow baseline) + #854 (path CoW) (2026-04-11)
**Tags**: codegen, cow, arc, nested-collections, path-cow, semantics

**Context**: `emitCowCheck` only copies the top-level header and data
buffer; inner element pointers are bit-copied. The top-level 1-hop CoW
path used by `var[i] = v` / `var.append(...)` keeps this "shallow"
behavior — cheap, documented in `docs/reference/collections.md`, tested
by `tests/spec/cow.test.ry`. Top-level mutations on simple collections
do not need element retain because the leaf slot-overwrite protocol
(#855) owns ownership transfer.

Chained writes (`b[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`) cannot
use the top-level path: the LHS root would resolve to nullptr CoW anchor
and the mutation would leak through aliases. The #854 fix introduces
**path CoW** — a second code path rooted in `emitPathCowForChain` that
walks the LHS inside-out, privatizes every container level whose
`strong_count > 1`, and retains each ARC element of the cloned buffer
(via the previously-unused `emitCowRetainArcElements`). The new
`emitCowCheckSlot` generalizes `emitCowCheck` to accept any writable
slot (alloca, module-global storage, nested record field GEP, heap
container data GEP) rather than just an alloca.

The collection destructors (`__ry_arc_dtor_list/map/set` in
`src/codegen_arc.cpp:633-665`) still only free internal buffers and do
not release ARC-managed elements. This is the pre-existing "element leak
on destructor" bug, accepted by the `detect_leaks=0` budget. Path CoW's
retain-on-clone amplifies this leak by a small constant (one extra leak
per mutation chain on shared inner collections) — same order of
magnitude, tracked as a separate follow-up along with insertion retain
protocol changes.

**Rule**: When implementing a new mutation path that can reach a
container through a chained LHS, route through `emitPathCowForChain`
(the caller passes `s.object` — everything except the innermost leaf
index). Do NOT evaluate `s.object` with a plain `emitExpr` first —
that re-loads the chain against stale parents after privatization.
The top-level 1-hop CoW in `emitCowCheck` still uses
`retainElements=false`; the path CoW path uses `retainElements=true`.
The asymmetry is deliberate: top-level CoW relies on #855's
retain-then-release-old slot protocol at the leaf store to transfer
inner ownership, whereas path CoW needs each intermediate container's
siblings to survive (inner lists that are not on the mutation path).

**Unsupported shapes** (compile-time error from path CoW):
- Method-call roots: `f().items[i] = v`
- Chains that interleave an index hop inside a record field walk
  (e.g. `rec.arr[0].items[i] = v`). Assign the intermediate to a local.
These match the narrow #854 scope; record-of-records chains without
interleaved index hops (`d.out.items[i] = v`) ARE supported via a
multi-level struct GEP through the root alloca.

### Compound-op loaded slot values must propagate container metadata

**Source**: #858, #862 (2026-04-11)
**Tags**: codegen, compound-assign, metadata, arc, collection, index-assign, field-assign

**Context**: `emitStmt(IndexAssignStmt &)` and `emitStmt(FieldAssignStmt &)`
in `src/codegen_stmt_misc.cpp` load the compound LHS from the slot via a
bare `CreateLoad` (for collection slots, #858) or `CreateExtractValue`
(for record fields, #862) before passing it into `applyCompoundOp`. When
the element / field type is an ARC-managed collection (`List<T>`,
`Map<K, V>`, `List<List<T>>`, …) the loaded value is an opaque pointer
with no metadata attached. The value then flows through
`applyCompoundOp → emitBinaryOp → emitArithmeticOp`, whose list-concat
dispatch at `src/codegen_expr.cpp:1015-1023` reads `TypeMeta::ListElem`
from the SSA value (via `getListElementType(lhs)`), **not** the `lhsHint`
string parameter. Without metadata on the load, dispatch falls through
to the str-vs-non-str rejection path and emits a completely misleading
error for a legal `List<int> + List<int>` operation.

The fix is to call `propagateTypeMeta(name, loadedVal)` immediately
after the load, matching the canonical pattern already documented for
`valueToString` element loads. `propagateTypeMeta` internally rebuilds
every `TypeMeta::*` slot from the name string, so one call restores the
full dispatch context. The name source depends on the code path:

- `IndexAssignStmt` (#858): pull from the container's metadata
  (`container_meta->list_elem_type_name` or `map_value_type_name`).
  **Snapshot the name into a local `std::string` *before* calling
  `propagateTypeMeta`** — the helper inserts into `value_metadata_` and
  may rehash, invalidating the `ValueMetadata *` returned by `getMeta`.
- `FieldAssignStmt` (#862): use `info.fields[fieldIdx].type->toString()`
  directly. `.toString()` returns a new `std::string` by value, so the
  rehash aliasing concern from #858 does not apply here. This matches
  the canonical pattern at `src/codegen_expr_literal.cpp:120-122`.

Also note that `src/codegen_stmt.cpp` (empty-list-declaration path) must
record `list_elem_type_name` for `List<List<T>>` exactly the way it
already does for `List<Map>` and `List<Set>` — the asymmetry in the
pre-#858 code caused the `xs: List<List<int>> = []; xs.append(...); xs[0]
+= ...` case to fail even after the `codegen_stmt_misc.cpp` fix.

**Rule**: Any codegen site that loads an element from a container slot
and then feeds it into a **type-dispatched operation** (formatter, binary
op, builtin call) MUST propagate the container's element type name onto
the loaded value. The bare `LoadInst` / `CreateExtractValue` result is
metadata-less. Hint parameters on helper signatures (`lhsHint` on
`emitBinaryOp`) are not the dispatch key — `TypeMeta::*` slots on the SSA
value are.

**How to verify** (grep before opening a PR):

```bash
grep -nE 'CreateLoad.*elem|CreateExtractValue.*field|applyCompoundOp' \
    src/codegen_stmt_misc.cpp src/codegen_tostring.cpp
```

Every such load that flows into `applyCompoundOp`, `emitBinaryOp`, or
`valueToString` should be followed by a metadata-propagation block (or
be an i64/bool/float path where metadata is unnecessary). The remaining
fixed-length array compound path at `src/codegen_stmt_misc.cpp:597-600`
is **not** a missing-metadata site: fixed-length array element types
are restricted to low-level primitive types by a hard compile-time
check at `src/codegen_type.cpp:125` (`array element type must be a
low-level type`), so ARC-managed collection element types are rejected
at type resolution and never reach the compound path.

### `thread_spawn` captures do NOT emit ARC retain/release in the thunk

**Source**: #872
**Tags**: thread_spawn, arc, concurrency, codegen, gotcha

**Rule**: Unlike `@parallel for` — which explicitly emits atomic
`emitArcRetain(hdr, true)` at thunk entry and a matching `emitArcRelease` at
scope exit for each ARC-managed capture — `thread_spawn` thunks do NOT
retain/release their captures.  The capture is stored as a raw pointer copy
into the env struct; the thunk loads the pointer and calls the lambda, and
`FnScope` is destroyed at CODEGEN time without ever calling `popScope()`, so
no runtime release instruction is emitted.

Consequence: the main thread's original reference keeps the ARC object alive
for the worker's lifetime.  As long as `thread_join` is called before the
outer scope's variable goes out of scope, this is correct.  TSan does NOT
report a race for concurrent str captures via `thread_spawn` because no
concurrent ARC ops are emitted.

Correctness contract: the caller MUST ensure the captured ARC-managed value
outlives all spawned workers (i.e., call `thread_join` before the capture's
owning scope exits).  No retain/release means no race protection beyond that
lifetime ordering.  A future follow-up could add optional retain-at-capture /
release-at-thunk-exit for `thread_spawn` (distinct from #877, which tracks
ARC-managed **return types**, not capture semantics).

### `parallel_for_depth_` design decision — keep scope-counter for now

**Source**: #872
**Tags**: arc, parallel_for, design-decision, concurrency

**Rule**: The current mechanism (`parallel_for_depth_` counter in `CodeGen`
→ `isArcAtomic()` returns true → `atomicrmw seqcst` for all ARC ops inside
the `@parallel for` thunk body) is correct for values emitted **directly**
inside the thunk.  It does **not** propagate to helper functions the worker
calls; those emit non-atomic ARC ops.

Options considered for #872:
1. **Keep `parallel_for_depth_`** (current) — simple, no perf cost outside
   `@parallel for`.  Limitation: helper-function races are not covered.
2. **Whole-program "may cross threads" analysis** — correct but expensive;
   requires call-graph walk and is a significant compiler feature.
3. **User-visible `Send`-like marking** — expressive but a language design
   change with breaking implications.
4. **Unconditional atomic ARC everywhere** — correct and simple but adds
   measurable latency to every single-threaded ARC operation.

**Decision for #872**: Keep option 1.  The stress tests added in #872
(`ConcurrencySpecSuite`, `concurrency_stress.test.ry`,
`test_runtime_arc_contention_stress.cpp`) did NOT expose a helper-function
race.  Revisit only if future stress tests expose such a race.

### Destructor recursion into inner ARC elements forces retain-on-store at every write site

**Source**: #1242 (2026-04-21). **Tags**: ARC, destructor, retain, list-literal, map-literal, set-literal, appended, insert, merge, uaf, regression

**Rule**: `getOrCreateCollectionDestructor` for List/Map/Set now walks the element buffer and calls `emitArcRelease` on each ARC-managed inner element (List/Map/Set), not just `free(dataBuf)`. Every codegen site that stores an ARC pointer into a collection slot — literal construction, `appended`, `insert`, set `add`, `append(!)`, map `merge`, IndexAssignStmt Map insert — must emit a matching retain, or the destructor will free shared elements out from under other holders.

The retain discipline is **symmetric** with the destructor:
- Destructor releases ⇒ source must retain on store (literal + container op).
- Destructor does not release (str element slot because `list_elem_type_name` is never stamped "str" per #1266) ⇒ retain is skipped.

**Why**: Pre-#1242 the destructor only freed the buffer, so the "raw-pointer aliasing" bug between a collection and its inner elements was invisible — inner elements leaked forever and stayed dereferenceable. The destructor fix flips that: the freed memory is now reachable from any dropped alias, producing UAF on the first rebind. `xs: List<List<int>> = [[0]]; while i < N: xs = [[1,2],[3,4]]` showed delta = 3*N+2 pre-fix (pure leak). Once the destructor recurses, the same rebind frees the previous inner lists — safe if and only if every site that duplicated those pointers retained.

**How to apply**: When adding a new collection write site, ask "does the destructor now release this slot?" — if yes (List/Map/Set element type resolves via `fieldTypeIsArcManaged` with `CollectionKind != Str`), emit `retainArcValue(val)` on freshly-loaded values and `emitCowRetainArcElements(newBuf, len, tag, elemArcKind)` on memcpy'd ranges before the store. For update-in-place patterns (Map insert-or-update, IndexAssignStmt update branch), release the overwritten element first. Callers must read `list_elem_type_name` / `map_*_type_name` / `set_elem_type_name` from the *source* container's metadata, not the newly-allocated header, since the new one is still being populated. The canonical sites fixed in #1242: `codegen_expr_literal.cpp` (List/Map/Set literal store loops), `codegen_call_collection.cpp::emitCollOp_add` / `_append` / `_appended` / `_insert` / `emitMapMergeCore`, `codegen_stmt_misc.cpp::IndexAssignStmt` Map insert path.

**Str carve-out preserved**: The destructor extension covers only List/Map/Set inner elements. The str element destructor path (`if (elemSig == "str") emitStrElemLoop(...)` in `getOrCreateCollectionDestructor`) is unchanged per #1266 — `AssignStmt` never stamps `list_elem_type_name = "str"` because flipping the destructor exposes a counter asymmetry between `__ry_arc_alloc_counted` (+1) and `makeString` (no-op), producing a large negative `arc_live_count()` delta in `arc_split_chars.test.ry`. The side-channel `ValueMetadata::list_elem_is_str` scoped to the indexer (#1266) remains the correct way to propagate str-elementness for read paths. A full str-destructor-switch belongs to a future issue.

**Related**: #1204 (slice retain), #1235 (take retain), #1046 (CoW str retain + original destructor extension for str), #1108 (emitArcReleaseLoadedElement inner-sig propagation), #855 (slot overwrite release discipline), #1266 (str destructor carve-out + `list_elem_is_str` side-channel).

---

### `appended` / `insert` / `merge` duplicate pointers without retain — symmetric to `slice`/`take`

**Source**: #1242 (2026-04-21, discovered during destructor extension). **Tags**: ARC, appended, insert, merge, memcpy, retain-on-store, uaf

**Rule**: Functions that construct a *new* collection whose element buffer is produced by memcpy from a source (`emitListSlice`, `emitCollOp_take_impl`, `emitCollOp_appended`, `emitMapMergeCore`'s m1 copy) must retain each ARC-managed element after the memcpy. Functions that store an individual ARC value into an existing or new collection (`emitCollOp_add`, `emitCollOp_append`, `emitCollOp_insert`, map merge's m2 insert/update) must retain the value before the store. Update-in-place also needs to release the overwritten element.

**Why**: Same defect class as #1204 (slice) / #1235 (take). Pre-#1242 the defect was latent: the source's destructor did not release inner elements, so the memcpy'd pointers stayed valid even after source release. Post-#1242 the destructor does release, so the second holder of the memcpy'd pointer is left with a dangling pointer. Rebind of either side triggers UAF.

**How to apply**: A repro is `m1 = {"a": a}; merged = merge(m1, m2); m1 = {"x": [99]}; print(merged["a"][0])` — pre-fix on a post-destructor-extension build this reads freed memory. Tests that rebind the source AND run an allocation-churning loop before reading the new collection reliably surface the UAF (without the churn, the freed memory still holds the old value and the read looks correct). See `tests/spec/arc_release_on_rebind.test.ry` "merge(Map<str,List<int>>, ...) survives source rebind" and "appended(List<List<int>>, ...) survives source rebind" for the canonical pattern.

**Related**: #1204, #1235, #1046 (original CoW retain), #1242.

---

