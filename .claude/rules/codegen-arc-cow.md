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

**Rule**: A list/map/set slot whose element type is itself an ARC-managed collection holds the sole owning ref to the inner allocation. Overwriting via `list[i] = v`, `m[k] = v`, or compound forms MUST load the previously-stored pointer and `emitArcRelease` it before storing the new one. Order: **retain new before releasing old** so self-assignment `xs[i] = xs[i]` cannot transiently drop the refcount to zero.

**How to apply**:
1. Check `elementTypeIsArcManaged(containerPtr, kind, &elemKind)` (type-name based; strings, weak refs, closures, records intentionally excluded).
2. Plain form: retain new value first via `tryRetainArcSource`, falling back to manual `emitArcRetain` when rhs is a Load from a GEP (`tryRetainArcSource` does NOT cover GEP loads — see next entry).
3. Load old slot value with null check, release via `emitArcReleaseLoadedElement(...)` (handles null guard + CFG branching).
4. Store new value.
5. Compound form (`xs[i] += v`): reuse `oldVal` already loaded for `applyCompoundOp` rather than re-loading — preserves "evaluate index/slot exactly once" (#812). Compound op produces fresh allocation that never aliases the slot, so no retain of new value needed.

**Sibling write paths**: `FieldAssignStmt` (#857) mirrors this protocol via `fieldTypeIsArcManaged` (consults AST `FieldDef.type->toString()`). Deep-chain middle hops use path CoW (`emitPathCowForChain` in `src/codegen_arc_cow.cpp`, #854) — see "Path copy-on-write isolates chained writes" below.

### `tryRetainArcSource` LoadInst cases must be metadata-gated

**Source**: #1266 (2026-04-21)
**Tags**: codegen, arc, retain, tryRetainArcSource, container-element, GEP-load, memory-safety

**Rule**: Any new LoadInst case in `tryRetainArcSource` must gate on container-element metadata, not on `ptrTy_` alone:

```cpp
if (llvm::isa<llvm::LoadInst>(val) && val->getType() == ptrTy_) {
    auto *meta = getMeta(val);
    if (meta && (meta->list_elem || meta->map_key ||
                 meta->map_value || meta->set_elem)) { /* retain */ }
}
```

A bare `ptrTy_` check is unsafe: weak refs, capturing closures, bare function pointers, and resource handles are all `ptrTy_` and would receive spurious retains that crash on `−16` GEPs into non-ArcHeader memory. Same protection #999 added to Case 3 for ExtractValueInst.

**Don't switch all callers to `retainArcValue`**: its fallback (`emitArcGetHeaderFromData` + `emitArcRetain`) is unconditional. Safe for IndexAssignStmt / FieldAssignStmt where the caller has already proven the slot is ARC-managed; unsafe for `tryRetainArcSource` callers (`return 42`, `f(closureArg)`, `let x = weakRef`) that lack upstream type guarantees.

**`map_value` belongs in the gate too**: For `Map<K,V>`, `propagateTypeMeta` sets both `MapKey` and `MapValue` flags. Verified for Case 4 in #1266; not audited for Case 3.

**str elements need a separate flag and offset**: A str container element uses `StringHeader` at offset −24, not `ArcHeader` at −16. Case 4 dispatches on dedicated `ValueMetadata::str_elem` bool to `emitStrGetHeaderFromData`.

**`str_elem` must ONLY be stamped in List<str> / Map<K,str> / Set<str> indexer paths** — `codegen_expr_literal.cpp` (List indexer reads `list_elem_is_str`) and wherever a map/set str value is loaded. Do NOT stamp from a shared helper like `propagateTypeMeta("str", ...)`: ~50 callsites (pattern bindings, enum variant fields, Result/Option payload, return meta, tuple destructure) handle str values that are NOT container-element borrows. Misstamping makes Case 4 dispatch through `emitStrGetHeaderFromData(val)` on a pointer that does not own a `StringHeader`, corrupting adjacent heap state. Often invisible under macOS libSystem malloc but crashes under glibc (CI Linux, SIGSEGV / exit 139). Don't reuse `low_level_type_name == "str"` either — `isLowLevelTypeName` matches numeric types only.

**Stamping `list_elem_type_name = "str"` is now safe (post-#1576)**: `makeString` / `makeStringUninit` / `freeStringSlot` touch `g_arc_live_count` symmetrically with `__ry_arc_alloc_counted`, so str-aware destructors no longer underflow the counter. The `split()` emitter and `xs: List<str>` annotation branch in `codegen_stmt.cpp` stamp directly. The `list_elem_is_str` side-channel set by AssignStmt annotation parser is still consulted by the indexer (`codegen_expr_literal.cpp` IndexExpr list branch) to stamp `str_elem` on the loaded element.

**Function parameters use a different path**: `applyParamTypeMeta` in `codegen_fn.cpp` calls `propagateTypeMeta(ptype, alloca)` directly — `xs: List<str>` parameter alloca picks up `list_elem_type_name = "str"` via the existing path. Safe because parameter alloca is a borrowed view; destructor selection happens at the caller's allocation site.

**Narrowing (#1353, #1354)**: "Never stamp `list_elem_type_name = "str"` on the allocation path" was narrowed to "never stamp **without** a matching insert-side retain". Map/List/Set literals stamp the type-name on `headerPtr` AFTER emitting per-element `retainArcValue` (via `isStrHandle` side-table check). Pre-increment compensates the `makeString`/`__ry_arc_alloc_counted` counter asymmetry at insert time. Pure-annotation AssignStmt path still avoids stamping for non-literal sources — use `inner_is_str` side-channel.

**Verification**: Behavioural ARC tests via `runtime_internal.arcLiveCount()` cannot detect missed retains directly (counter tracks live allocations, not refcount). Use FileCheck IR goldens (`tests/filecheck/arc_retain_*`) asserting `arc.retain:` / `arc.retain.done:` blocks appear after container-element load with `getelementptr i8, ptr %elem, i64 -16` (ArcHeader) or `i64 -24` (StringHeader).

### Pattern binding ARC and `emitPatternBindingArc`

**Source**: #997 (2026-04-16)
**Tags**: codegen, arc, pattern, match, retain, memory-safety

**Rule**: Every pattern binding arm that extracts a sub-value (via `ExtractValue` or `Load`) must call `emitPatternBindingArc(val, bindAlloca, typeSig)` after storing. Without this, scoped ARC release calls `free(ptr - 16)` on a never-retained pointer → crash or refcount underflow.

**typeSig per pattern**:
- `SomePattern` / `OkPattern`: `extractGenericTypeArg(subjectEnumType, "Option<"/"Result<", 0)`
- `ErrPattern`: `extractGenericTypeArg(subjectEnumType, "Result<", 1)`
- `EnumConstructorPattern`: `fit->second.fieldTypeNames[bi]` per field
- `VariablePattern` / `TuplePattern` / `RecordPattern`: `""` (fall through to source-alloca heuristic)

**What `emitPatternBindingArc` does**:
1. If `val` is a record struct with ARC fields: retain ARC fields for `LoadInst`/`ExtractValueInst` sources via `emitRecordArcFieldsRetain`, insert `bindAlloca` into `arc_field_record_vars_`. Return.
2. If `val->getType() != ptrTy_` (scalar) → return.
3. If `typeSig` is collection (`isCollectionTypeName`): retain + mark ARC-managed + insert into `arc_backed_vars_`. Return.
4. If `typeSig` is function type: retain + mark ARC-managed only for capturing/uniform closures (`fn_type_info` metadata). Bare function pointers skipped. Return.
5. If `typeSig == "str"`: retain via `emitStrGetHeaderFromData` (offset −24), mark ARC-managed, insert into `arc_str_managed_vars_`. Return.
6. Otherwise (no `typeSig`): fall through to `tryRetainArcSource(val)` → `arc_owned_values_` → collection-type metadata.

**`str` is ARC-managed (#1046, updated #1105)**: Use `emitStrGetHeaderFromData(handle)` (offset −24, `STRING_HEADER_SIZE`), **not** `emitArcGetHeaderFromData` (offset −16, `ARC_HEADER_SIZE`). Dispatch via `resolveTypeAlias(typeName) == "str"` or `arc_str_managed_vars_.count(alloca) > 0`. `fieldTypeIsArcManaged("str")` returns true with `outKind = CollectionKind::Str`. Collection destructors for `List<str>` / `Map<K,str>` / `Set<str>` iterate elements and call `emitArcRelease(emitStrGetHeaderFromData(elem))`.

**ARC header offset dispatch — use helpers, not inline `emitArcGetHeaderFromData` (#1105)**: Never call `emitArcGetHeaderFromData` inline at retain/release sites that may handle `str`. Helpers:
- **`emitArcHeaderForAlloca(handle, alloca)`** — checks `arc_str_managed_vars_.count(alloca)`, dispatches correctly. Use at all alloca-origin sites.
- **`CapturedArcKind::Str`** — `detectCapturedArcKind` returns this when captured alloca is in `arc_str_managed_vars_`. Closure construction/destructor switch on it.
- **`arc_str_owned_values_` guard in ExprStmt** — bare `str` temporaries are in `arc_str_owned_values_`, not `arc_owned_values_`. `emitStmt(ExprStmt)` checks both.

Inline calls miss future header-offset additions silently — `−16` into `StringHeader` lands on `weak_count`, atomic-add increments the wrong field. The `@parallel for` capture site in `codegen_stmt_loop.cpp` uses local `capIsArcStr[i]` instead of `emitArcHeaderForAlloca` because the thunk's `FnScope` clears `arc_str_managed_vars_` before thunk body executes.

### StringHeader layout — `str` memory representation (#1022)

**Source**: #1022 (2026-04-16)
**Tags**: str, StringHeader, ARC, memory-layout, NUL, byte_len, makeString

**Rule**: Every Ry `str` is a pointer to the *data* region of a `StringHeader` block. Always use `makeString`/`makeStringUninit` to create, `freeStringSlot` to free. Never treat as raw `char*`.

**Layout** (`include/ry/ry_layout.hpp`, `include/ry/runtime_string.hpp`):

```text
handle - 24       strong_count : i64  (ARC_IMMORTAL for literals; 1 for dynamic)
handle - 16       weak_count   : i64  (0 always in PR #1022)
handle - 8        byte_len     : i64  (STRING_BYTELEN_OFFSET = 8)
handle + 0 ..     data bytes          ← the char* passed to Ry / codegen
handle + byte_len '\0'                (null terminator for libc compat)
```

Constants: `ARC_HEADER_SIZE = 16`, `STRING_HEADER_SIZE = 24`, `STRING_BYTELEN_OFFSET = 8`.

**APIs**: `stringByteLen(handle)` (C++) / `emitStringByteLen(handle)` (IR) for length; `freeStringSlot(handle)` calls `free(handle - 24)`. Literal globals built by `buildArcGlobal` in `src/codegen.cpp` use `strong_count = ARC_IMMORTAL`; GEP index `(0, 3, 0)`.

**NUL safety**: All string operations are NUL-safe — `byteLen`, `len`, comparison/concat, hashing (#1022); `contains`/`startsWith`/`endsWith`/`find` (#1047); `replace` (#1048); `substr`/`charAt`/`reverse`/`split("",_)`/iteration (#1049); `isEmpty` (#1069); case/trim ops (#1050); `split(s,delim)`/`join`/`repeat`/`*` (#1051); regex APIs incl. `\0` literal in pattern (#1052, #1076); JSON APIs (#1053); `io.writeText`/`appendText` use `fwrite` not `fputs` (#1133).

### `arc_str_managed_vars_` side-table for str ARC dispatch (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, side-table, offset, emitStrGetHeaderFromData

**Rule**: Any `AllocaInst` holding a `str` that participates in ARC must be inserted into `arc_str_managed_vars_`. This is the canonical dispatch source for `emitStrGetHeaderFromData` vs `emitArcGetHeaderFromData` in `emitArcReleaseVar`.

**Why**: LLVM IR has no type-level distinction between str (`ptr` at −24) and collection (`ptr` at −16). Without the side-table, every release uses the wrong offset.

**How to apply**: After `markArcManaged` for a str-typed alloca, also insert into `arc_str_managed_vars_`. Intermediary tmp allocas in RecordPattern / EnumConstructorPattern need it too (downstream VariablePattern bindings inherit). Do NOT add str allocas to `arc_backed_vars_` (CoW set) — CoW uses −16 and corrupts.

### `@parallel for` str captures must use `emitStrGetHeaderFromData` (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, parallel_for, concurrency, offset, segfault

**Rule**: In `emitParallelForRange` (`src/codegen_stmt_loop.cpp`), captured `str` variables (in `arc_str_managed_vars_`) require `emitStrGetHeaderFromData` (−24) at thunk entry retain AND for the thunk-local dst alloca's `arc_str_managed_vars_` insertion. Wrong offset → `weak_count` decrement → `free()` on literal global's read-only static allocation → segfault.

**How to apply**: Snapshot `capIsArcStr[i] = arc_str_managed_vars_.count(src) > 0` in parent context BEFORE `FnScope` clears the side-tables. In thunk: after `markArcManaged(dst)`, if `capIsArcStr[i]` insert `dst` into `arc_str_managed_vars_`; retain via `capIsArcStr[i] ? emitStrGetHeaderFromData : emitArcGetHeaderFromData`. Release via `popScope→emitArcReleaseVar` auto-selects.

### `emitPatternBindingArc` path 2b must propagate `arc_str_managed_vars_` from source (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, pattern-binding, arc_str_managed_vars_, segfault

**Rule**: In path 2b (no type signature, probe heuristically), when `tryRetainArcSource(val)` returns true and `val` is a `LoadInst` from a source alloca in `arc_str_managed_vars_`, the destination `bindAlloca` must be inserted into `arc_str_managed_vars_`, NOT `arc_backed_vars_`. Set hierarchy: closure → str → arc_backed.

**Why**: `arc_backed_vars_` members use offset −16. For str, that's `weak_count` (0, not `ARC_IMMORTAL`), bypassing the immortal guard, then atomically writing to a read-only literal global → SIGSEGV.

```cpp
if (auto *ld = llvm::dyn_cast<llvm::LoadInst>(val)) {
    auto *src = llvm::dyn_cast<llvm::AllocaInst>(ld->getPointerOperand());
    if (src && closure_managed_vars_.count(src)) closure_managed_vars_.insert(bindAlloca);
    else if (src && arc_str_managed_vars_.count(src)) arc_str_managed_vars_.insert(bindAlloca);
    else arc_backed_vars_.insert(bindAlloca);
} else if (arc_str_owned_values_.count(val)) {
    arc_str_managed_vars_.insert(bindAlloca);
} else {
    arc_backed_vars_.insert(bindAlloca);
}
```

Triggers when a `str` is extracted from an enum field via `EnumConstructorPattern` with a `VariablePattern` binding — the field's tmp alloca is in `arc_str_managed_vars_`, and `emitPatternBindings` recursing into `VariablePattern` must inherit the str classification.

### Str literal globals are `isConstant=true` — wrong offset → SIGSEGV not SIGABRT

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, StringHeader, isConstant, offset, segfault

**Rule**: `cachedGlobalString` creates StringHeader globals with `isConstant=true`; JIT maps them as read-only. Using `emitArcGetHeaderFromData` (−16) on a literal `str` reads `weak_count = 0 ≠ INT64_MAX`, bypasses `ARC_IMMORTAL` guard, then atomically writes to read-only page → SIGSEGV (exit 139), not SIGABRT. Stack trace points into JIT code at unusual address. Cross-reference `arc_str_managed_vars_` membership at the crashing alloca's declaration site.

### `List<str>` / `Map<K,str>` / `Set<str>` destructor specialization (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, collection, destructor, leak

**Rule**: `getOrCreateCollectionDestructor` cache key is `(CollectionKind, elemSig, valSig)`, not kind alone. When `elemSig == "str"` (or `valSig == "str"` for Map values), the destructor iterates the dense element array and calls `emitArcRelease(emitStrGetHeaderFromData(elem))` before freeing the buffer. Caching by kind only would produce a single generic destructor that leaks str elements.

**How to apply**: `resolveCollectionDestructor(alloca)` reads `list_elem_type_name` / `map_key_type_name` / `map_value_type_name` from `ValueMeta`, calls `resolveTypeAlias` before passing to `getOrCreateCollectionDestructor`. Ensure the alloca carries correct type-name metadata.

**Extension (#1108)**: Same applies to `emitArcReleaseLoadedElement`. Overwriting an ARC-managed element slot (`List<List<str>>`, `Map<K, List<str>>`, record field) must call `getOrCreateCollectionDestructor` with the **inner** elem/val sigs derived from the element's type name. `emitArcReleaseLoadedElement` accepts `elemTypeName` and resolves internally; passing `""` falls back to the generic destructor (leaks inner str).

### CoW copy of `List<str>` must retain str elements (#1046)

**Source**: #1046 (2026-04-17)
**Tags**: ARC, str, CoW, double-free

**Rule**: When CoW-copying a `List<str>` / `Map<K,str>` / `Set<str>`, str elements MUST be retained immediately after the copy regardless of `retainElements`. In `emitCowCheckSlot`, `doElemRetain` is forced `true` when `elemArcKind == CollectionKind::Str`. Post-#1046 the destructor releases str elements — without retention both old and CoW copy would double-free.

**How to apply**: In `emitCowRetainArcElements`, pass `elemArcKind`; use `emitStrGetHeaderFromData(elem)` (−24) when `Str`, otherwise `emitArcGetHeaderFromData(elem)` (−16). Wrong offset silently corrupts `weak_count` or `byte_len`.

### `memcpy` of an element buffer must be paired with ARC retain (#1204)

**Source**: #1204 (2026-04-19)
**Tags**: codegen, arc, slice, memcpy, retain, memory-safety

**Rule**: Any path that duplicates a container's element buffer with `memcpy` (`emitListSlice`, `emitCollOp_take_impl`, `emitListConcat`, future helpers) must follow with a retain loop when source elements are ARC-managed:

```cpp
CollectionKind elemArcKind = CollectionKind::List;
if (elementTypeIsArcManaged(srcListPtr, CollectionKind::List, &elemArcKind)) {
    emitCowRetainArcElements(newData, count, "<tag>", elemArcKind);
}
```

**Why**: `memcpy` copies raw pointers without bumping refcount. When source is later released, its destructor walks elements and decrements each — without retention the new buffer's elements are freed underneath.

**How to apply**: `elementTypeIsArcManaged` reads from the *source* container's metadata (pass source pointer, not new header). Scalar types take false branch. `emitCowRetainArcElements` handles str (−24) vs collection (−16) offset internally based on `elemArcKind`. Reference: `emitCowClone` in `src/codegen_arc_cow.cpp`.

### `markArcManaged(tmp)` pre-mark must be guarded by `fieldTypeIsArcManaged`, str fields also into `arc_str_managed_vars_`

**Source**: #1016 (updated #1046)
**Tags**: ARC, str, pattern-binding, arc_str_managed_vars_, CollectionKind

**Rule**: TuplePattern / RecordPattern / EnumConstructorPattern pre-mark a tmp alloca so the leaf `VariablePattern` binding can retain via `tryRetainArcSource` Case 1. Without `fieldTypeIsArcManaged` guard, ANY `ptrTy_` field (incl. fn-ptr, resource ptrs) gets marked. Fix:

```cpp
CollectionKind fk;
if (fieldTypeIsArcManaged(elemSig, &fk)) {
    markArcManaged(tmp);
    if (fk == CollectionKind::Str) arc_str_managed_vars_.insert(tmp);
}
```

at all three sites (`src/codegen_match.cpp`). Capturing closure in tuple/record/enum fields is intentionally excluded: `fn_type_info` metadata is not propagated by `propagateTypeMeta` onto ExtractValue intermediates (pre-#1008 behaviour).

### `emitStrGetDataPtr` must insert into `arc_str_owned_values_`, not `arc_owned_values_`

**Source**: PR #1148 review (2026-04-18)
**Tags**: codegen, str, ARC, arc_str_owned_values_, emitStrGetDataPtr

**Rule**: `emitStrGetDataPtr` recovers str data via `GEP(+STRING_HEADER_SIZE)`. Result must go into `arc_str_owned_values_` — downstream retain/release uses `emitStrGetHeaderFromData` (−24). Putting in `arc_owned_values_` causes every retain/release to corrupt 8 bytes via wrong offset.

**How to apply**: Search for `arc_owned_values_.insert` at sites receiving values from `emitStrGetDataPtr` or `GEP(i8*, strHeaderPtr, STRING_HEADER_SIZE)` — should be `arc_str_owned_values_.insert`.

### `emitArithmeticOp` str+str: input owned temps must be released at concat site

**Source**: #1583 (2026-05-05)
**Tags**: codegen, arc, str, concat, leak, chained-binary

**Rule**: After the second `memcpy` in str+str branch of `emitArithmeticOp` and before `arc_str_owned_values_.insert(buf)`, emit `emitArcRelease(emitStrGetHeaderFromData(operand), atomic=false)` + `arc_str_owned_values_.erase(operand)` for every input (lhs, rhs) currently in `arc_str_owned_values_`. The freshly allocated `__ry_string_make_uninit` buffer is the only owned-temp that survives the call.

**Why**: `BinaryExpr("+", BinaryExpr("+", "x", "y"), "z")` — inner concat produces inner buf in `arc_str_owned_values_`; outer concat copies it but leaves it. `emitStmt(ExprStmt)` only releases outermost SSA; inner buf leaks per chained inner concat. Releasing at concat site is enclosing-context-independent (works for ExprStmt, let-binding, return, function arg, nested binary).

**Edge cases handled by `count(operand) > 0` guard**: literal globals (never inserted, skipped); borrowed VariableExpr loads (skipped); `lhs == rhs` (`s + s`) — first iteration releases-and-erases, second sees count == 0; immortal handles (`emitArcRelease` short-circuits on `ARC_IMMORTAL`). Release after `memcpy` so source data is still valid when copied.

**How to apply**: Any future path producing a fresh str into `arc_str_owned_values_` and consuming inputs from the same set must mirror this pattern. Verified via `tests/spec/str_arc.test.ry` (chained 3/4-arg, let-binding `arcLiveCount` deltas) and IR golden `tests/filecheck/str_concat_chain_release.ry`.

### Map CoW must retain str keys independently of value retention

**Source**: PR #1148 review (2026-04-18)
**Tags**: codegen, CoW, Map, str, ARC, emitCowClone

**Rule**: `elementTypeIsArcManaged(containerPtr, CollectionKind::Map, &kind)` checks `map_value_type_name` only, NOT `map_key_type_name`. The CoW clone loop (`emitCowClone`) thus retains values but never keys → `Map<str, V>` str keys are released by destructor but never retained after clone → use-after-free. Emit a separate key-retention loop after value-retention if `meta->map_key_type_name` resolves to ARC-managed (check via `fieldTypeIsArcManaged`). Map destructor already handles str key release (`codegen_arc.cpp:964` `emitStrElemLoop(keys, len, "mkey")`).

### Map literal `{k: v}` with str keys/values needs side-table retain + explicit str stamp

**Source**: #1347 (2026-04-24)
**Tags**: codegen, Map, literal, str, ARC, isStrHandle, retainArcValue

**Rule**: In `emitExprVariant(MapExpr)` (`src/codegen_expr_literal.cpp`), the existing retain gate `keyTypeName = inferCollectionTypeName(keyVals[0])` returns `""` for plain str values. Use parallel `isStrHandle(v)` side-table check (`arc_str_owned_values_.count(v) > 0 || LoadInst from arc_str_managed_vars_ alloca`); call `retainArcValue(keyVals[i])` unconditionally on str handles. `retainArcValue → tryRetainArcSource` Case 1 emits retain for bound LoadInst; Case 2b is transfer-semantic no-op for fresh `+1` `makeString` (no leak on `{"foo": "bar"}`).

Retain alone is insufficient: the non-empty-literal VarDecl path in `codegen_stmt.cpp` only propagates `map_value_type_name`, not `map_key_type_name` (str annotation at L269-271 fires only for empty-literal). Add symmetric `map_key_type_name` propagation from `valMeta` / `loadMeta` / annotation in non-empty branch, AND stamp `map_key_type_name = "str"` / `map_value_type_name = "str"` on the literal's `headerPtr` when `isStrHandle` detects str. Without stamps, `resolveCollectionDestructor` dispatches to `__ry_arc_dtor_map` with empty sigs — str keys leak (UAF traded for leak).

**Why this differs from #1346 SetItem**: `m: Map<str, str> = {}` (empty + `m[k] = v`) hits annotation-driven L269-282 path stamping both names; #1346 only had to lift `!= CollectionKind::Str` gate. Non-empty literal never hits L269-282 — different root cause despite identical user-visible symptom.

### List/Set literal `[s]` / `{s}` with str elements needs side-table retain + explicit str stamp (#1354)

**Source**: #1354 (2026-04-24)
**Tags**: codegen, List, Set, literal, str, ARC, isStrHandle

**Rule**: In `emitExprVariant(ListExpr)` and `emitExprVariant(SetExpr)`, mirror #1353 Map fix: use `CodeGen::isStrHandle` (decl `include/ry/codegen.hpp`, impl `src/codegen_arc.cpp`) consulting `arc_str_owned_values_` (fresh +1) and `arc_str_managed_vars_` (borrowed LoadInst). Apply `retainArcValue` per-entry when `elemTy == ptrTy_ && isStrHandle(v)`; track `anyElemIsStr` for post-loop stamp of `list_elem_type_name = "str"` / `set_elem_type_name = "str"` on `headerPtr`. For Set, retain fires inside `insertBB` branch (not main loop body) so dedup-skipped duplicates don't double-retain. In `codegen_stmt.cpp` List tracking, propagate `list_elem_type_name = "str"` into indexer-facing `list_elem_is_str = true`.

**Why**: `inferCollectionTypeName` returns `""` for plain str (same as #1347 Map). Without side-table retain, `[s]` / `{s}` produces dangling pointer once source goes out of scope. Without explicit `"str"` stamp, `resolveCollectionDestructor` picks non-str variant → leak. Per-entry retain + post-loop stamp is required for mixed-origin literals like `[bound_s, "literal_s"]` that alternate between LoadInst and ExtractValue origins.

**How to apply**: When `element_needs_retain(elemTy)` (or Map/Set analogue) gates retain on `inferCollectionTypeName`-returned name and element type is `ptrTy_`, audit whether str values can flow through with locally-constructed origins. If yes, add `isStrHandle` as OR condition and stamp `*_type_name = "str"` on the literal header.

### Record ARC reassignment: use `!CallInst && !InvokeInst` not `LoadInst || ExtractValueInst`

**Source**: PR #1148 review (2026-04-18)
**Tags**: codegen, ARC, record, reassignment, InsertValueInst

**Rule**: The guard in `codegen_stmt.cpp` AssignStmt record-with-ARC-fields path that decides whether to call `emitRecordArcFieldsRetain` must be `!isa<CallInst>(val) && !isa<InvokeInst>(val)` — NOT `isa<LoadInst>(val) || isa<ExtractValueInst>(val)`. Fresh constructions (CallInst/InvokeInst) are sole owners and must not be retained. Every other value — including `InsertValueInst` chains like `r2 = { r.field, new_val }` — is a view of existing state and must be retained. The original allowlist of two cases caused use-after-free for InsertValueInst records.

### `str` does not support `[]` index syntax — guard with `isStringValue` before list/map dispatch

**Source**: #1026 (2026-04-16)
**Tags**: codegen, diagnostics, str, IndexExpr, IndexAssignStmt

**Rule**: In `emitExprVariant(IndexExpr)` (`src/codegen_expr_literal.cpp`) and IndexAssignStmt emitter (`src/codegen_stmt_misc.cpp`), `str` values are `ptrTy_` and pass the "must be ptr" gate. After "index operator requires list or map" gate and before Map dispatch, check `isStringValue(objPtr)`:
- Read: `"str does not support index access; use charAt(s, i) instead"`
- Write: `"str does not support index assignment; strings are immutable"` — must come BEFORE `emitCowCheck(objPtr, ..., CollectionKind::List)` (passing str ptr to `emitCowCheck` is unsafe).

**Why `isStringValue` is the right predicate**: returns true when `val->getType() == ptrTy_` and `isNonStrPointer(val)` is false (no collection/resource metadata). Lists/Maps/Sets/resource handles all have `hasAnyMeta() == true`, so only `str` triggers. Canonical predicate at `src/codegen_any.cpp:28`.

### `emitExprVariant(CaseExpr)`: capture `armEndBB` AFTER `popScope()`

**Source**: #997 (2026-04-16)
**Tags**: codegen, arc, match, phi, case-expr, ir-verification

**Rule**: In `emitExprVariant` for CaseExpr (`src/codegen_match.cpp`), record `armEndBB = builder_.GetInsertBlock()` AFTER `popScope()`, never before. `emitPatternBindingArc` may insert `arc.retain` / `arc.retain.done` BBs; `popScope() → emitArcReleaseVar` may insert `arc.var_release` / `arc.var_skip` BBs. Both advance the insert block. Recording before `popScope()` captures stale BB → IR verify error "PHI node entries do not match predecessors".

```cpp
llvm::Value *armVal = emitExpr(*arm.value);
popScope();                                          // changes insert block
llvm::BasicBlock *armEndBB = builder_.GetInsertBlock();
builder_.CreateBr(mergeBB);
incoming.push_back({armVal, armEndBB});
```

Same rule applies to any code recording an insert-block BB for later PHI use while a scope with ARC-managed variables is still open.

### `isStringValue` must NOT be used to dispatch `weak` header offset

**Source**: #1022 (2026-04-16)
**Tags**: codegen, weak, str, StringHeader, emitWeakExpr

**Rule**: Don't use `isStringValue(val)` to choose `STRING_HEADER_SIZE` (24) vs `ARC_HEADER_SIZE` (16) for weak ARC header. `isStringValue` returns true for ANY `ptrTy_` without `hasAnyMeta()` — captured `List<int>` / `Map` / `Set` inside closure bodies lack collection metadata after capture injection in `codegen_fn.cpp` / `codegen_lambda.cpp`, getting misclassified as str with wrong −24 offset.

Use the inner type name from the LetStmt annotation (`weakInnerTypeName(*annot)`) and `weak_inner_type_names_[ptr]` at reassignment:

```cpp
// codegen_stmt.cpp — initial let
llvm::Value *headerPtr = (innerName == "str")
    ? emitStrGetHeaderFromData(val) : emitArcGetHeaderFromData(val);

// codegen_stmt.cpp — reassignment
auto it = weak_inner_type_names_.find(ptr);
const std::string &weakInner = (it != weak_inner_type_names_.end()) ? it->second : std::string{};
llvm::Value *headerPtr = (weakInner == "str")
    ? emitStrGetHeaderFromData(val) : emitArcGetHeaderFromData(val);
```

`emitExprVariant(WeakExpr)` returns the raw data pointer; conversion happens at the call sites above, not inside the emitter.

### Ry records are SSA struct values, not pointers — nested field writes unroll up to root alloca

**Source**: #812 (2026-04-11)
**Tags**: codegen, records, lvalue, insertvalue, chained-lhs

**Rule**: Records are *value types*: `record Point: x: int; y: int` is `{i64, i64}` in an alloca. `CreateLoad(struct) → ExtractValue(x)` gives an SSA value, not a writable pointer. For `rec.a.b.c = v`, walk back inserting each updated inner struct into its parent until reaching the root alloca, then single `CreateStore`. Never take a pointer into a struct field — use `ExtractValue` / `InsertValue`. Reference: `writeBackFieldChain` in `src/codegen_stmt_misc.cpp`.

### Compound assignment must be expanded in codegen, not desugared in parser

**Source**: #812 (2026-04-11)
**Tags**: codegen, compound-assign, side-effects, chained-lhs

**Rule**: Naively desugaring `a[f()] += v` as `a[f()] = a[f()] + v` in parser evaluates `f()` twice — violates "each subexpression evaluates once" and multiplies CoW work. Use `applyCompoundOp` in `src/codegen_stmt_misc.cpp` with optional `compound_op` field on `IndexAssignStmt` / `FieldAssignStmt` — codegen interprets as "load resolved slot → apply op → store same slot" (single index evaluation per statement). For maps, compound op on missing key is runtime error (`"compound assignment to missing map key"`); users must insert explicitly before accumulating.

### Path copy-on-write isolates chained writes through nested collections

**Source**: #812 (shallow baseline) + #854 (path CoW) (2026-04-11)
**Tags**: codegen, cow, arc, nested-collections, path-cow, semantics

**Rule**: `emitCowCheck` only copies top-level header + data buffer; inner element pointers are bit-copied. Top-level 1-hop CoW (`var[i] = v` / `var.append(...)`) keeps this shallow behavior — cheap, documented in `docs/reference/collections.md`. Top-level mutations don't need element retain because leaf slot-overwrite (#855) owns ownership transfer.

Chained writes (`b[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`) cannot use top-level path: LHS root resolves to nullptr CoW anchor and mutation leaks through aliases. **Path CoW** (`emitPathCowForChain` in `src/codegen_arc_cow.cpp`) walks LHS inside-out, privatizes every container level whose `strong_count > 1`, and retains each ARC element of the cloned buffer (`emitCowRetainArcElements`). `emitCowCheckSlot` generalizes `emitCowCheck` to accept any writable slot (alloca, module-global, nested record field GEP, heap container data GEP).

**How to apply**: When implementing a mutation path that reaches a container through a chained LHS, route through `emitPathCowForChain` (caller passes `s.object`). Do NOT pre-evaluate `s.object` with plain `emitExpr` — that re-loads against stale parents after privatization. Top-level 1-hop uses `retainElements=false`; path CoW uses `retainElements=true` (deliberate asymmetry: top-level relies on #855's retain-then-release-old leaf protocol; path CoW needs intermediate siblings to survive).

**Unsupported shapes** (compile-time error from path CoW):
- Method-call roots: `f().items[i] = v`
- Index-hop interleaved inside record-field walk: `rec.arr[0].items[i] = v` (assign intermediate to local). Record-of-records WITHOUT interleaved index hops (`d.out.items[i] = v`) IS supported via multi-level struct GEP through root alloca.

### Compound-op loaded slot values must propagate container metadata

**Source**: #858, #862 (2026-04-11)
**Tags**: codegen, compound-assign, metadata, arc, collection

**Rule**: `IndexAssignStmt` / `FieldAssignStmt` in `src/codegen_stmt_misc.cpp` load compound LHS via bare `CreateLoad` (collection slots, #858) or `CreateExtractValue` (record fields, #862) before `applyCompoundOp`. The loaded value is metadata-less. `applyCompoundOp → emitBinaryOp → emitArithmeticOp` reads `TypeMeta::ListElem` from the SSA value (via `getListElementType(lhs)`), NOT the `lhsHint` string — without metadata, list-concat dispatch falls through to str-vs-non-str rejection and emits misleading error for legal `List<int> + List<int>`.

Call `propagateTypeMeta(name, loadedVal)` immediately after the load. Name source:
- **IndexAssignStmt** (#858): pull from container's metadata (`container_meta->list_elem_type_name` or `map_value_type_name`). **Snapshot the name into a local `std::string` BEFORE calling `propagateTypeMeta`** — the helper inserts into `value_metadata_` and may rehash, invalidating the `ValueMetadata *` from `getMeta`.
- **FieldAssignStmt** (#862): use `info.fields[fieldIdx].type->toString()` directly — returns `std::string` by value, no rehash aliasing concern.

`src/codegen_stmt.cpp` (empty-list-declaration path) must record `list_elem_type_name` for `List<List<T>>` as it does for `List<Map>` / `List<Set>` — pre-#858 asymmetry caused `xs: List<List<int>> = []; xs.append(...); xs[0] += ...` to fail.

**Rule (general)**: Any codegen site that loads from a container slot and feeds into a type-dispatched op (formatter, binary op, builtin) MUST propagate the element type name onto the load. Bare LoadInst / CreateExtractValue is metadata-less. Hint parameters (`lhsHint`) are not the dispatch key — `TypeMeta::*` slots on the SSA value are.

**Verify**: `grep -nE 'CreateLoad.*elem|CreateExtractValue.*field|applyCompoundOp' src/codegen_stmt_misc.cpp src/codegen_tostring.cpp` — every load flowing into `applyCompoundOp` / `emitBinaryOp` / `valueToString` should have a metadata-propagation block (or be i64/bool/float). Fixed-length array compound path at `src/codegen_stmt_misc.cpp:597-600` is NOT a missing site — array element types are restricted to low-level primitives by hard check at `src/codegen_type.cpp:125`.

### `threadSpawn` captures do NOT emit ARC retain/release in the thunk

**Source**: #872
**Tags**: threadSpawn, arc, concurrency, codegen, gotcha

**Rule**: Unlike `@parallel for` (which emits atomic `emitArcRetain(hdr, true)` at thunk entry + matching release at scope exit), `threadSpawn` thunks do NOT retain/release captures. The capture is stored as raw pointer copy into the env struct; the thunk loads and calls the lambda, and `FnScope` is destroyed at CODEGEN time without `popScope()` — no runtime release emitted.

Consequence: main thread's original reference keeps the ARC object alive for the worker's lifetime. Caller MUST ensure the captured ARC value outlives all spawned workers (call `threadJoin` before the capture's owning scope exits). No retain/release means no race protection beyond that lifetime ordering. TSan does NOT report a race for concurrent str captures via `threadSpawn` because no concurrent ARC ops are emitted. Future follow-up may add optional retain-at-capture / release-at-thunk-exit (distinct from #877 which tracks ARC return types).

### `parallel_for_depth_` design decision — keep scope-counter for now

**Source**: #872
**Tags**: arc, parallel_for, design-decision, concurrency

**Rule**: Current mechanism (`parallel_for_depth_` counter → `isArcAtomic()` returns true → `atomicrmw seqcst` for all ARC ops inside `@parallel for` thunk body) is correct for values emitted **directly** in the thunk. Does NOT propagate to helper functions the worker calls; those emit non-atomic ARC ops.

Alternatives considered: (1) keep current — simple, no perf cost outside `@parallel for`, but helper-function races uncovered; (2) whole-program "may cross threads" analysis — correct but expensive; (3) user-visible `Send`-like marking — language design change; (4) unconditional atomic ARC everywhere — adds latency to every single-threaded ARC op.

**Decision**: Keep option 1. #872 stress tests (`ConcurrencySpecSuite`, `concurrency_stress.test.ry`, `test_runtime_arc_contention_stress.cpp`) did NOT expose helper-function races. Revisit only if future stress exposes one.

### Destructor recursion into inner ARC elements forces retain-on-store at every write site

**Source**: #1242 (2026-04-21)
**Tags**: ARC, destructor, retain, list-literal, map-literal, set-literal, appended, insert, merge, uaf

**Rule**: `getOrCreateCollectionDestructor` for List/Map/Set walks the element buffer and calls `emitArcRelease` on each ARC-managed inner element (List/Map/Set), not just `free(dataBuf)`. Every codegen site that stores an ARC pointer into a collection slot — literal construction, `appended`, `insert`, set `add`, `append(!)`, map `merge`, IndexAssignStmt Map insert — must emit a matching retain, or destructor frees shared elements.

Symmetric discipline: destructor releases ⇒ source must retain on store; destructor doesn't release (str element slot, since `list_elem_type_name` is never stamped "str" per #1266) ⇒ retain skipped.

**Why**: Pre-#1242 the destructor only freed the buffer — raw-pointer aliasing between collection and inner elements was invisible (inner elements leaked but stayed dereferenceable). Post-#1242, freed memory is reachable from any dropped alias → UAF on first rebind. `xs: List<List<int>> = [[0]]; while i < N: xs = [[1,2],[3,4]]` showed delta = 3*N+2 pre-fix.

**How to apply**: New collection write site → "does the destructor release this slot?" If yes (List/Map/Set element type resolves via `fieldTypeIsArcManaged` with `CollectionKind != Str`), emit `retainArcValue(val)` on freshly-loaded values and `emitCowRetainArcElements(newBuf, len, tag, elemArcKind)` on memcpy'd ranges before store. For update-in-place (Map insert-or-update, IndexAssignStmt update), release overwritten element first. Read source container's metadata, not the new header (still being populated). Canonical sites fixed in #1242: `codegen_expr_literal.cpp` literal stores, `codegen_call_collection.cpp::emitCollOp_add`/`_append`/`_appended`/`_insert`/`emitMapMergeCore`, `codegen_stmt_misc.cpp::IndexAssignStmt` Map insert.

**Str carve-out preserved**: Destructor extension covers List/Map/Set inner elements only. Str element destructor path (`if (elemSig == "str") emitStrElemLoop(...)`) unchanged per #1266 — `AssignStmt` never stamps `list_elem_type_name = "str"` because flipping the destructor exposes counter asymmetry between `__ry_arc_alloc_counted` (+1) and `makeString` (no-op). Side-channel `list_elem_is_str` scoped to indexer remains correct for read paths. Full str-destructor-switch is future work.

### `appended` / `insert` / `merge` duplicate pointers without retain — symmetric to `slice`/`take`

**Source**: #1242 (2026-04-21)
**Tags**: ARC, appended, insert, merge, memcpy, retain-on-store, uaf

**Rule**: Functions constructing a *new* collection whose buffer is `memcpy`'d from a source (`emitListSlice`, `emitCollOp_take_impl`, `emitCollOp_appended`, `emitMapMergeCore`'s m1 copy) must retain each ARC-managed element after memcpy. Functions storing an individual ARC value into an existing/new collection (`emitCollOp_add`, `emitCollOp_append`, `emitCollOp_insert`, map merge's m2 insert/update) must retain the value before store. Update-in-place also releases the overwritten element.

**Why**: Same defect class as #1204 (slice) / #1235 (take). Pre-#1242 latent — source destructor didn't release inner elements, so memcpy'd pointers stayed valid even after release. Post-#1242 destructor releases, leaving second holder of memcpy'd pointer dangling.

**Repro**: `m1 = {"a": a}; merged = merge(m1, m2); m1 = {"x": [99]}; print(merged["a"][0])` — pre-fix on post-destructor-extension build reads freed memory. Tests rebinding source AND running allocation-churning loop before reading reliably surface UAF (without churn, freed memory holds old value). Canonical: `tests/spec/arc_release_on_rebind.test.ry`.

### Tuple-element collections need symmetric retain/release on tuple components, dispatched locally

**Source**: #1667 (2026-05-09)
**Tags**: ARC, tuple, destructor, retain, items, enumerate, zip

**Rule**: For `List<(K, V)>` from `items` / `enumerate` / `zip` (and derivations `slice` / `take` / `appended` / `concat` propagating `list_elem_type_name = "(K, V)"` via `propagateMeta`), the destructor must walk the dense tuple-struct buffer and release each ARC-managed component (`str` / `List` / `Map` / `Set`), and every codegen site producing such results must retain components symmetrically.

| Stage | Site | Helper |
|---|---|---|
| Per-component retain | `emitCollOp_items`, `enumerate`, `zip` (InsertValue setup) | `emitTupleComponentRetain(val, fSig)` |
| Buffer-wide retain | `emitListSlice`, `emitCollOp_take_impl`, `emitCollOp_appended` (memcpy half), `emitListConcat` | `emitTupleElemRetainLoop(arrayPtr, len, tag, tupleSig, tupleTy)` |
| Buffer-wide release | `getOrCreateCollectionDestructor` → `emitInnerReleaseLoop` | `emitTupleElemReleaseLoop(arrayPtr, len, tag, tupleSig, tupleTy)` |

**Why not extend `fieldTypeIsArcManaged`**: making it return true for `"(K, V)"` would short-circuit ~30 callers (`elementTypeIsArcManaged`, `emitCowRetainArcElements`, `emitListSlice`, etc.) into pointer-array retain/release loops. But `List<(K, V)>` data buffer holds **inline tuple struct values** (`sizeof(StructType)` bytes per slot), not pointers — those callers would read struct as pointer and apply `−16` GEP → memory corruption. Tuple isn't a `Kind`-level entity (no `CollectionKind::Tuple`); each field is independently ARC-managed. Dispatch added locally at each tuple-buffer site, ahead of `elementTypeIsArcManaged`:

```cpp
const std::string elemSig = srcMeta ? srcMeta->list_elem_type_name : std::string{};
const bool elemIsTuple = elemSig.size() >= 2 && elemSig.front() == '(' && elemSig.back() == ')';
if (elemIsTuple) {
    auto *tupleTy = llvm::cast<llvm::StructType>(elemTy);
    emitTupleElemRetainLoop(newData, count, "<tag>_telem", elemSig, tupleTy);
} else if (elementTypeIsArcManaged(srcListPtr, CollectionKind::List, &kind)) {
    emitCowRetainArcElements(newData, count, "<tag>_elem", kind);
}
```

`emitTupleComponentRetain` dispatches by source-level type name (`"str"` → −24, `List<...>` / `Map<...>` / `Set<...>` → −16) — authoritative metadata, not value-reading heuristic.

**Caching invariant**: Destructor cache key `(CollectionKind, elemSig, valSig)` — tuple sigs live in `elemSig`, distinct shapes cached as distinct functions automatically.

**Caveat — literal-built `List<(K, V)>`**: `inferCollectionTypeName` returns empty for tuple values; `xs: List<(int, str)> = [(1, "a")]` has `list_elem_type_name = ""`. New destructor branch is gated on non-empty tuple sig — NOT dispatched for literal-built lists (pre-fix behavior preserved). Issue tracking literal-path metadata gap is out of scope for #1667.

**Slot overwrite (`xs[i] = (a, b)`)**: resolved by #1670 — see next entry.

### IndexAssignStmt slot overwrite on `List<(K, V)>` must retain new tuple components and release old

**Source**: #1670 (2026-05-09)
**Tags**: ARC, tuple, IndexAssignStmt, slot-overwrite, retain-before-release, traceInsertValueField

**Rule**: When `IndexAssignStmt` writes a tuple value into a slot of `List<(K, V)>`, codegen must (i) retain each ARC-managed component of the *new* tuple via `emitTupleComponentRetain`, then (ii) release each ARC-managed component of the *old* slot via `emitTupleElemReleaseSlot`, then store. Retain-before-release is required for self-assignment safety (`e[i] = e[i]`).

Block added **alongside** (not `else if` of) the existing `if (elemIsArc)` branch — `elementTypeIsArcManaged` returns false for tuples by design (inline structs, not ARC-managed pointer types). Gate on `resolveTypeAlias(elemTypeName)`:

```cpp
const std::string resolvedElemTypeName = resolveTypeAlias(elemTypeName);
const bool elemIsTuple = resolvedElemTypeName.size() >= 2 &&
                          resolvedElemTypeName.front() == '(' &&
                          resolvedElemTypeName.back() == ')';
```

**Why resolve aliases first**: `type Pair = (int, str)` propagates the alias name into `list_elem_type_name`, not the canonical tuple shape. Naive `elemTypeName.front() == '('` skips alias-typed lists, leaking on every overwrite. `resolveTypeAlias` walks the chain to canonical form. Resolved name must also feed `splitTupleSig` and `emitTupleElemReleaseSlot`.

**Why non-empty gate matters**: `inferCollectionTypeName` returns `""` for tuple values. Literal-built `xs: List<(int, str)> = [(1, "a")]` has empty name. Destructor branch added in #1667 is also gated on non-empty — keeps symmetry (same blind spot as `List<str>` literals). Lists from `enumerate` / `items` / `zip` / `slice` / `take` / `appended` / `concat` all stamp concrete tuple sig.

**Trace InsertValue chains recursively for nested tuples**: When rhs is `InsertValue(InsertValue(undef, k, {0}), v, {1})`, `CreateExtractValue(finalVal, {i})` does NOT fold back to the original SSA value — `IRBuilder<>` uses `ConstantFolder` which only folds constants. Single-step `traceInsertValueField` is **insufficient for nested shapes** like `xs[i] = (0, ([1], 2))`: trace recovers the inner InsertValue (the nested tuple), not the leaf `[1]`. Leaf `arc_owned_values_.count(...)` misses, and redundant retain leaks the inner allocation by +N per overwrite (post-#1667 destructor releases each leaf once on overwrite — asymmetry shows as leak proportional to iteration; `delta = +2` per iteration in `nestedTupleSlotOverwriteNoLeak` test).

Fix: `emitTupleComponentRetainTraced(fieldVal, sourceAgg, topIdx, fSig)` — (a) trace `sourceAgg` at `topIdx` for original operand, (b) if `fSig` resolves to tuple shape, recurse over each sub-component using the **traced sub-aggregate** as new source, (c) at leaves check `arc_owned_values_` / `arc_str_owned_values_` on traced value before falling through to `emitTupleComponentRetain`. Callsite:

```cpp
for (unsigned i = 0; i < n; ++i) {
    llvm::Value *fieldVal = builder_.CreateExtractValue(finalVal, {i}, "tup_new_f" + std::to_string(i));
    emitTupleComponentRetainTraced(fieldVal, finalVal, i, components[i]);
}
```

**`emitTupleElemReleaseSlot` helper**: Factored from per-slot body of `emitTupleElemReleaseLoop`. Avoids 4 throwaway BBs (header/body/latch/post) for a single slot; reused by the loop body. Loads slot, extracts components, releases ARC components by source-level type name (str −24, List/Map/Set −16, nested tuples recurse).

**Compound op N/A**: tuples have no arithmetic; parser/typechecker rejects compound `IndexAssignStmt` on `List<(K, V)>` upstream of codegen.

**Symmetry principle**: When destructor recursively releases ARC components of a typed slot (not just whole-pointer ARC), every write site must (i) retain new components and (ii) release evicted components retain-before-release. Future tuple mutators (`xs.append((a, b))`, `xs.insert(i, (a, b))`) need same per-component retain — verified by delta-based regression pattern.
