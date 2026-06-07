---
paths:
  - "src/codegen_arc*.cpp"
  - "src/codegen_any.cpp"
  - "src/codegen_stmt_misc.cpp"
  - "src/codegen_stmt_loop.cpp"
  - "src/codegen_match.cpp"
  - "src/codegen_expr_literal.cpp"
---

# Codegen ARC and Copy-on-Write

### Container slot writes and memcpy must retain new + release old, symmetric with destructor

Every write into an ARC-managed slot or buffer (`list[i] = v`, `m[k] = v`, `append`/`insert`/`appended`, `slice`/`take`/`concat`/`merge`, IndexAssignStmt, FieldAssignStmt, CoW clone, path CoW) must retain new BEFORE releasing old, so `xs[i] = xs[i]` cannot transiently drop refcount to zero. Pair every `memcpy` of an ARC element buffer with `emitCowRetainArcElements(buf, count, tag, kind)`. `tryRetainArcSource` does NOT cover `LoadInst` from a GEP — manually retain via `emitArcGetHeaderFromData` + `emitArcRetain` when it returns false. Destructors (`getOrCreateCollectionDestructor`, cache key `(CollectionKind, elemSig, valSig)`) walk the element buffer and release each ARC-managed element; every write site must mirror retain-on-store. `emitArcReleaseLoadedElement` requires the inner element type name (not just `CollectionKind`) so `List<List<T>>` etc. dispatch correctly.

### Tuple-element collections dispatch retain/release locally, not through `fieldTypeIsArcManaged`

`List<(K, V)>` data buffers hold inline tuple struct values, not pointers. Tuple is not a `CollectionKind`; making `fieldTypeIsArcManaged("(K,V)")` return true would corrupt callers reading the struct as a pointer. Per-site dispatch (after `propagateMeta` stamps `list_elem_type_name = "(K, V)"` on results of `items` / `enumerate` / `zip` / `slice` / `take` / `appended` / `concat`): buffer sites call `emitTupleElemRetainLoop` / `emitTupleElemReleaseLoop`; per-component sites call `emitTupleComponentRetain` keyed by source-level component type name (str at −24, List/Map/Set at −16). IndexAssignStmt slot-overwrite gates on `resolveTypeAlias(elemTypeName)` + tuple shape and uses `emitTupleComponentRetainTraced` to recurse through nested InsertValue chains; otherwise `arc_owned_values_` leaves get double-retained.

### `tryRetainArcSource` LoadInst cases must be metadata-gated

Each new LoadInst case must gate on container-element metadata (`list_elem || map_key || map_value || set_elem`). A bare `ptrTy_` check spuriously retains weak refs, capturing closures, bare fn-ptrs, and resource handles, then crashes on the `−16` GEP. `str` elements need a dedicated `str_elem` flag and dispatch to `emitStrGetHeaderFromData` (offset −24); stamp `str_elem` ONLY in List<str> / Map<K,str> / Set<str> indexer paths — never from `propagateTypeMeta("str", ...)`, which is called from many non-container-element sites and would corrupt heap state. Do NOT switch callers to `retainArcValue`: its fallback is unconditional and unsafe for non-ARC `ptrTy_` callers like `return 42` or `let x = weakRef`.

### Fn-typed param values loaded for `return` need a dedicated retain helper, not a `tryRetainArcSource` case

**Source**: #1770 (2026-05-17, fix)
**Tags**: codegen, arc, fn-typed-param, uniform-closure, return-stmt, uaf

**Context**: A higher-order callee like `fn pickLambda(f: fn() -> Unit) -> fn() -> Unit: return f` crashed at JIT time when the caller invoked the returned value via a bound local (`g = pickLambda((): return); g()`). Fn-typed parameter allocas are *not* registered in `arc_managed_vars_` — `applyParamTypeMeta` (`codegen_fn.cpp`) only marks `isCollectionTypeName(ptype)`. Callers instead own the uniform-closure wrap temp via `releaseUniformClosureTemps` after the call. Returning the value out of the callee therefore leaves no retain on the way out; the caller's post-call temp release then frees storage that the caller still holds in the bound local. The fix is **not** to add a new LoadInst case to `tryRetainArcSource`: that helper fires at every load site (compound op LHS reloads, pass-through fn args, etc.), and at non-return sites the caller has already arranged ownership correctly — `wrapFnTypedArgs` skips the wrap when the source is already a uniform closure, so the temp set is empty and a load-site retain would leak.

**Rule**: When a value class needs a retain *only* on the return path (because the caller's post-call ownership protocol — like `releaseUniformClosureTemps` — would otherwise drop the value mid-flight), add a dedicated helper called from `emitStmt(ReturnStmt)` alongside `tryRetainArcSource`, gated on the metadata that uniquely identifies that class. For fn-typed params this is `meta->fn_type_info && meta->fn_type_info->isUniformClosure`. Do **not** widen `tryRetainArcSource`'s LoadInst cases to cover it — every non-return LoadInst site would then over-retain. The complementary entry "`tryRetainArcSource` LoadInst cases must be metadata-gated" applies; this is its return-only counterpart.

**How to apply**: `CodeGen::retainFnTypedParamForReturn(val)` in `src/codegen_arc.cpp` is the canonical example. Call it from `emitStmt(ReturnStmt)` after `tryRetainArcSource(val)` and before `emitScopeCleanupToDepth(0)`. When introducing a similar helper for another value class, verify the three pass-through paths still balance: (1) direct `return f` from a 1-level callee — retain balances caller's release; (2) pass-through `inner(f)` from a wrapper — `wrapFnTypedArgs` returns empty temps, so no caller release fires, and `return inner(f)` is a `CallInst` (not `LoadInst`), so the helper does not double-retain; (3) two-level chain — only the innermost retain fires.

### `str` is fully ARC-managed via `StringHeader`; offset by side-table membership, not by `isStringValue`

`str` is a pointer to the `data` region of a `StringHeader`: offsets `−24 strong_count`, `−16 weak_count`, `−8 byte_len`, `+0 data`. `STRING_HEADER_SIZE=24` vs `ARC_HEADER_SIZE=16`. Use `makeString` / `makeStringUninit` / `freeStringSlot`; never `malloc` / `strdup`. Literal globals are `isConstant=true` — wrong offset writes the read-only page → SIGSEGV (not SIGABRT), pointing into JIT addresses. Side-tables select offsets: `arc_str_managed_vars_` (str allocas), `arc_str_owned_values_` (fresh str temps from `emitStrGetDataPtr`, `__ry_string_make_uninit`, …). Use the helpers `emitArcHeaderForAlloca(handle, alloca)` and `CapturedArcKind::Str` at sites that may handle str. Never use `isStringValue` to dispatch the offset (captured `List<int>` / `Map` / `Set` inside closure bodies lose collection metadata after capture and get misclassified) — use the declaration-time inner-type name. `@parallel for` snapshots `capIsArcStr[i]` before `FnScope` clears side-tables. `markArcManaged(tmp)` pre-mark for pattern fields must be guarded by `fieldTypeIsArcManaged` (see the dedicated entry below). `emitPatternBindingArc` path 2b must check the source set hierarchy `closure → str → arc_backed` in order. CoW copy of `List<str>` forces `doElemRetain=true` (else post-destructor double-free). str+str concat in `emitArithmeticOp` releases input owned temps from `arc_str_owned_values_` at the concat site (not deferred to ExprStmt) so chained `(a + b) + c` doesn't leak inner bufs.

### Map/List/Set literal `{k:v}` / `[s]` / `{s}` with str: side-table retain + `*_type_name = "str"` stamp

`inferCollectionTypeName` returns `""` for plain str, so the default retain gate short-circuits. Use `isStrHandle(v)` (consults `arc_str_owned_values_` + `arc_str_managed_vars_`) to apply `retainArcValue` per-entry. **The post-loop stamp must use a wider signal**: `isStrHandle` excludes immortal literal globals from `cachedGlobalString` (e.g. plain `{"a": 1}` keys), so a separate `anyXxxIsStrLike` flag based on `isStringValue` must drive the metadata stamp of `list_elem_type_name` / `set_elem_type_name` / `map_key_type_name` / `map_value_type_name = "str"` on the literal `headerPtr`. Use `(anyXxxIsStr || anyXxxIsStrLike)` as the stamp condition. For Set, retain inside `insertBB` so dedup-skipped duplicates don't double-retain. Without the stamp, `resolveCollectionDestructor` picks the non-str variant and str elements leak; downstream consumers like `verifyCalledWith`'s metadata-gated record path silently fail to recognize `Set<str>` / `Map<str, …>` literals (cf. 2def9dc7 for Set, #1705 for Map). Map CoW must retain str **keys** independently — `elementTypeIsArcManaged` only checks `map_value_type_name`. Propagate `list_elem_is_str = true` from the stamp into the indexer-facing side-channel via `inner_is_str` in `codegen_stmt.cpp`.

### Pattern binding ARC: every extracted sub-value goes through `emitPatternBindingArc`

Every arm that extracts a sub-value via `ExtractValue` or `Load` must call `emitPatternBindingArc(val, bindAlloca, typeSig)` after the store; without it, scoped release calls `free(ptr - 16)` on a non-retained pointer. `typeSig` per pattern: SomePattern / OkPattern = `extractGenericTypeArg(subjectEnumType, "Option<" / "Result<", 0)`; ErrPattern = `extractGenericTypeArg("Result<", 1)`; EnumConstructorPattern = `fieldTypeNames[bi]`; VariablePattern / TuplePattern / RecordPattern = `""` (heuristic). In `emitExprVariant(CaseExpr)`, capture `armEndBB = builder_.GetInsertBlock()` AFTER `popScope()` — `emitPatternBindingArc` + `popScope()` advance the insert block via inserted ARC BBs; capturing before breaks PHI predecessor matching.

### `markArcManaged(tmp)` pre-mark must be guarded by `fieldTypeIsArcManaged`; str fields also into `arc_str_managed_vars_`

**Source**: #1016, updated #1046
**Tags**: codegen, arc, pattern-binding, markArcManaged, str, fieldTypeIsArcManaged

TuplePattern / RecordPattern / EnumConstructorPattern pre-mark a temporary alloca
(`tmp`) as ARC-managed so the recursive leaf `VariablePattern` binding can emit a
single retain via `tryRetainArcSource` Case 1. Without the guard, _any_ `ptrTy_`
field (including bare fn-ptr and resource ptrs) is incorrectly marked.
Fix (post-#1046): use `CollectionKind fk; if (fieldTypeIsArcManaged(elemSig, &fk)) { markArcManaged(tmp); if (fk == CollectionKind::Str) arc_str_managed_vars_.insert(tmp); }` at all three sites (`src/codegen_match.cpp`). `fieldTypeIsArcManaged` returns true for List/Map/Set/str and false for fn-ptr/resource/etc. The `arc_str_managed_vars_` insertion is required so `tryRetainArcSource` Case 1 dispatches to `emitStrGetHeaderFromData` (offset −24) instead of `emitArcGetHeaderFromData` (offset −16) for str fields. Capturing closure in tuple/record/enum fields is intentionally excluded: `fn_type_info` metadata is not propagated by `propagateTypeMeta` onto `ExtractValue` intermediates, so closure detection is impossible here; this was also the pre-#1008 behaviour.

### Record ARC reassignment guard: `!isa<CallInst> && !isa<InvokeInst>`

The AssignStmt record-with-ARC-fields guard for `emitRecordArcFieldsRetain` must be `!isa<CallInst>(val) && !isa<InvokeInst>(val)`. Fresh constructions (Call/Invoke) are sole owners; every other value (including `InsertValueInst` chains like `r2 = { r.field, new_val }`) is a view and must be retained. The original `LoadInst || ExtractValueInst` allowlist was too narrow and caused UAF for `InsertValueInst` records.

### Records are SSA struct values; nested field writes unroll to the root; compound assignment is codegen-side

Records emit as `{T1, T2, …}` value types in an alloca — there is no stable address for an individual field. `rec.a.b = v` walks via `InsertValue` at each hop until reaching the root alloca, ending with one `CreateStore` (`writeBackFieldChain`). Never desugar compound assignment in the parser (`a[f()] += v` would evaluate `f()` twice); `applyCompoundOp` evaluates the LHS address exactly once. Compound-op loaded slot values must call `propagateTypeMeta(name, loadedVal)` immediately after the load — snapshot the name to a local `std::string` before the call (the helper may rehash and invalidate `getMeta` results). Path CoW (`emitPathCowForChain` + `emitCowCheckSlot`) handles chained writes through nested collections (`b[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`); top-level 1-hop CoW (`emitCowCheck`) uses `retainElements=false`, path CoW uses `retainElements=true`. Do not evaluate `s.object` with `emitExpr` first — that re-loads against stale parents. Unsupported chained shapes (compile error): method-call roots, index hops interleaved in record-field walks. Map compound op on a missing key is a runtime error.

### `str` rejects `[]` syntax — guard with `isStringValue` before list/map dispatch

`str` is `ptrTy_` and passes the "must be ptr" gate. After the "index operator requires list or map" gate and BEFORE Map dispatch, check `isStringValue(objPtr)` and emit targeted diagnostics: read path → `"str does not support index access; use charAt(s, i) instead"`, write path → `"str does not support index assignment; strings are immutable"`. The write-path guard MUST come before `emitCowCheck` — passing a `str` pointer with `CollectionKind::List` is unsafe.

### `wrapInAny` / `unwrapFromAny` retain on collection AND str wrap; `emitAnyReleaseVar` tag-dispatched scope-end release

**Source**: #1697 (2026-05-18, feat — collections); #1799 (2026-05-18, fix — str integration); #1797 (2026-05-18, feat — records via heap box); #1798 (2026-05-19, feat — enums via heap box)
**Tags**: codegen, arc, any, collection, str, record, enum, retain-release, scope-cleanup, descriptor

**Context**: Issue #1697 extended `any` to hold `List` / `Map` / `Set` headers in `data[8]` (tags 5/6/7) with retain/release wiring for collections only. #1799 closed the str gap. #1797 added record support (tag 8) by **heap-boxing** the record so a record-typed SSA struct value can be referenced from `any.data[8]` like the existing pointer-shaped payloads. #1798 extends the same box pattern to enum values (tag 9) — organic `enum` declarations plus the built-in `Option<T>` / `Result<T, E>`. Even simple enums (no payload, native `i64` discriminant) flow through the Enum tag (not the Int tag) so the source-level enum identity survives the `any` round-trip. The 16-byte `RyAny` struct layout is preserved.

**Rule**: Every site that wraps an ARC-managed payload in `any` must emit a retain on the appropriate header, and every `any`-typed alloca must be registered in `arc_any_managed_vars_` so scope cleanup emits a tag-dispatched release. Element-type metadata is erased on wrap — register the declared source type at decl time so the release dispatcher can pick the right destructor for collections. **str uses `emitStrGetHeaderFromData` (offset −24); collections AND record / enum boxes use `emitArcGetHeaderFromData` (offset −16) — confusing the two corrupts the header words.** Literal globals are ARC_IMMORTAL-marked, so the retain/release become no-ops via the immortal guard inside `emitArcRetain` / `emitArcRelease`.

**Record box layout** (per-record-type, descriptor-driven):

```
+---------------------------------+   <-- ArcHeader (16B, with strong/weak counts)
| ArcHeader                       |
+---------------------------------+   <-- data ptr stored in any.data[8]
| descriptor ptr (8B)             |       (so emitArcGetHeaderFromData(-16) recovers ArcHeader)
+---------------------------------+
| record struct (record_size B)   |
+---------------------------------+
```

Each record type has a singleton `__ry_record_desc_<typename>` LLVM global carrying `{ dtor_fn_ptr, eq_fn_ptr, type_name_cstr, parent_desc }` (32B = 4 pointers, post-#1802). The descriptor is what makes cross-function-boundary safe: even after the static type erases to `any` and `arc_any_managed_vars_::sourceTypeName` becomes stale (e.g. `"any"` or empty for a value returned out of a fn), release / equality / `str` / **subtype unwrap** all dispatch through the descriptor word stored inside the box. Trampoline `__ry_arc_dtor_record_dispatch` reads the descriptor's `dtor_fn_ptr` and tail-calls it; `__ry_record_is_subtype_desc` reads `parent_desc` to walk the inheritance chain at unwrap time.

**Enum box layout** (per-enum-type, descriptor-driven; structurally identical to record box):

```
+---------------------------------+   <-- ArcHeader (16B, with strong/weak counts)
| ArcHeader                       |
+---------------------------------+   <-- data ptr stored in any.data[8]
| descriptor ptr (8B)             |       (so emitArcGetHeaderFromData(-16) recovers ArcHeader)
+---------------------------------+
| enum payload (i64 / ADT struct) |
+---------------------------------+
```

Each enum type has a singleton `__ry_enum_desc_<typename>` LLVM global carrying `{ dtor_fn_ptr, eq_fn_ptr, type_name_cstr }` (24B = 3 pointers — **no `parent_desc`**, because Ry enums have no inheritance relationship). The descriptor key includes the full generic parameterization for `Option<T>` / `Result<T, E>` (e.g. `"Option<int>"` is distinct from `"Option<str>"`, `"Result<List<int>, str>"` is distinct from `"Result<int, str>"`). Trampoline `__ry_arc_dtor_enum_dispatch` reads the descriptor's `dtor_fn_ptr` and tail-calls it; the dtor body switches on the discriminant and releases the active variant's ARC fields only.

**How to apply**:
- `wrapInAny` (`src/codegen_any.cpp`) detects collection inputs via `getMeta(val)->{list_elem,map_key,map_value,set_elem}` and emits `emitArcGetHeaderFromData` + `emitArcRetain` BEFORE storing the pointer into `any.data`. **str inputs (detected via `isStringValue(val)`) are retained via `emitStrGetHeaderFromData` + `emitArcRetain` in the symmetric `else if` branch.** **Record inputs (detected via `llvm::isa<StructType>(val->getType()) && findRecordInfoForType(...)`) heap-box the struct via `emitArcAlloc(8 + record_size)`, store the per-type descriptor at offset +0, store the record struct at offset +8, and write the box data ptr into `any.data[8]`.** **Enum inputs (detected via `findEnumLikeTypeNameForBoxing` returning a non-empty name — order: organic ADT via `findAdtEnumName`, then `Option<T>` / `Result<T, E>` via `reverse_option_types_` / `reverse_result_types_`, then simple enum via `meta->enum_value_type` for i64 inputs) heap-box the payload via `emitArcAlloc(8 + payload_size)`, store the per-enum descriptor at offset +0, store the enum payload at offset +8, and write the box data ptr into `any.data[8]`.** The enum-arm reuses the record reassignment guard pattern (`!isa<CallInst> && !isa<InvokeInst>` to decide field-wise retain for ARC fields in the active variant). The record-arm follows the existing record ARC reassignment guard to decide between move (fresh construction) and field-wise retain (existing record alias); the field-wise retain reuses `emitRecordArcFieldsRetain`. Primitive (int/float/bool) inputs are not ARC-managed and need no retain. Function pointers and resource types (`TcpListener`, `TcpStream`, …) remain rejected.
- `unwrapFromAny(anyVal, targetTy, targetTypeName)` emits the symmetric retain when the unwrapped pointer becomes a new alias: collection unwrap (driven by `targetTypeName` matching `isListTypeName` / `isMapTypeName` / `isSetTypeName`) calls `emitArcGetHeaderFromData` + `emitArcRetain`; **str unwrap (`isStrUnwrap` = `targetTy == ptrTy_ && !isCollectionUnwrap && expectedTag == Str`) calls `emitStrGetHeaderFromData` + `emitArcRetain`** after the tag-match branch. **Record unwrap** (driven by `llvm::isa<StructType>(targetTy) && findRecordInfoForType(...)`) is gated by a **runtime descriptor chain walk** via `__ry_record_is_subtype_desc(actualDesc, expectedDesc)` — the helper walks the `RyRecordDescriptor::parent_desc` chain so `let a: Parent = anyHoldingChild` succeeds when `expectedDesc` is reachable from `actualDesc` (exact-type match returns 1 on the first iteration; unrelated types walk to null and return 0 → trap via `emitRuntimeError`) (#1802). On match: GEP the record struct at box+8 using the **target type's** struct layout (Parent struct is a prefix of Child by Ry's inheritance layout — all Ry fields are `alignof ≤ 8` so `roundUp(8, alignof(ParentStructTy)) == 8` is invariant), field-wise retain via `emitRecordArcFieldsRetain(recordVal, ParentStructTy)` (touches only Parent fields; Child-only fields stay owned by the box dtor), then load and return the struct value. **Enum unwrap** (driven by `unwrapEnumFromAny` — reached for `llvm::isa<StructType>(targetTy)` with `findAdtEnumName` / `isOptionType` / `isResultType` non-empty, or for `targetTy == i64Ty_` with `isSimpleEnumTypeName(targetTypeName)`) is gated by **descriptor-pointer equality only** (no chain walk — enums have no parent). Mismatch → `emitRuntimeError("any enum type mismatch (expected <Expected>, got a different enum type)")`. On match: GEP the enum payload at box+8 using the **target type's** native layout, field-wise retain ARC payload fields in the active variant (switch on discriminant) if any, then load and return the payload value. **Crucially, `canAnyHoldType` is not widened** for record or enum — both are reached through dedicated branches parallel to the pointer-tag dispatch. See [[codegen-type-and-metadata]] for the metadata-side rule (including the `source_type_name` propagation that lets `wrapInAny` recover the full `Option<int>` / `Result<List<int>, str>` name through alloca → load boundaries). Without the per-target field-wise retain, two `any` values pointing at the same str / collection / record / enum box would each release once at scope exit and double-free.
- `any`-typed allocas must call `registerAnyManagedVar(alloca, sourceTypeName)` at decl time and `emitAnyReleaseVar(name, alloca, sourceTypeName)` at scope exit. `emitAnyReleaseVar` emits `switch (tag)` over `Str=3 / List=5 / Map=6 / Set=7 / Record=8 / Enum=9` with `doneBB` default (Int/Float/Bool/Unit are no-op); the Str arm uses `emitStrGetHeaderFromData` + `emitArcRelease(hdr, isArcAtomic(ptr), FunctionCallee{}, nullptr)`; collection arms dispatch `emitArcReleaseLoadedElement(ptr, CollectionKind::{List,Map,Set}, sourceTypeName, ...)`; the **Record arm** dispatches through the descriptor trampoline `__ry_arc_dtor_record_dispatch` and the **Enum arm** dispatches through `__ry_arc_dtor_enum_dispatch` so a stale `sourceTypeName` does not misroute the dtor — the descriptor pointer inside the box is the source of truth. `emitArcReleaseLoadedElement` leaves the builder on its own continuation block — each switch arm must explicitly `CreateBr(doneBB)` after the helper call (else codegen emits a basic block without a terminator and LLVM verify fails). The same pattern applies to `emitAnyRetainPayload` and `emitAnyReleasePayload` (any-to-any copy and reassignment paths) — both must include Str, Record, and Enum arms symmetric to the collection arms.
- When `sourceTypeName` is empty at registration time, `emitAnyReleaseVar` falls back to the generic destructor for the tag's kind (e.g. `"List"`). This is sufficient for collections of primitives but is approximate for nested ARC element types — that limitation is acknowledged in `docs/reference/types.md` and tracked as element-type metadata erasure (see [[codegen-type-and-metadata]] for the metadata-side rule). str, record, and enum need no per-type fallback because the StringHeader / descriptor word carries enough information to release correctly without consulting `sourceTypeName`.

### `using` statement: evaluate init **before** `pushScope` so `?`-Err in the initializer skips auto-close

**Source**: #1817 (2026-05-21, feat)
**Tags**: codegen, using, scope-cleanup, resource, question-mark, ordering, arc

**Rule**: In `emitStmt(UsingStmt)` (`src/codegen_stmt_loop.cpp`), call
`emitExpr(*s->value)` **before** `pushScope()` and **before** writing
to `resource_managed_vars_`. The expression may contain `?`
propagation that emits an early `return` via `emitScopeCleanupToDepth(0)`;
that early-return must observe a scope stack that does **not** yet
contain the `using` binding, otherwise it would emit
`__ry_io_file_cleanup` on a value that was never successfully bound.

Once `detectResourceKind(val)` confirms `rk_file`, push the new scope,
create the alloca, store the value, mark it ARC-managed, and add it to
`resource_managed_vars_[ptr] = rk_file`. From that point on,
`popScope()` / early `return` / `?` / `break` / `continue` all reach
`emitScopeCleanupToDepth` and the existing ARC release path drops the
File's strong count to zero, invoking `__ry_io_file_cleanup` →
`fclose`. No explicit close emission is needed in the `using` codegen
itself.

**How to apply**: When introducing a new scope-binding construct that
guards a resource (TCP stream, lock handle, etc.), follow the same
ordering: evaluate the init expression, validate the kind, **then**
push the scope and register the variable. Adding `pushScope()` before
the init evaluation — even briefly — creates a window where `?` in the
init runs the destructor on a partially-initialized binding. The
load-bearing nature of this ordering is documented at
`src/codegen_stmt_loop.cpp:834-837`.

### Resource-handle `close()` is decoupled from ARC release so live iterators survive

**Source**: #1818 (2026-05-21, feat — `lines()` iterator)
**Tags**: codegen, resource, close, arc, iterator, lifetime, file

**Rule**: For `File` and any future resource that exposes a user-facing
`close(handle)` together with a long-lived iterator over the same
handle (`lines(f) -> Iterator<str>`), do **not** route `close()` through
`emitResourceFree`. `emitResourceFree` combines the runtime
cleanup call (`__ry_io_file_cleanup`) with an immediate
`__ry_arc_release` on the ARC header — that releases the box even
though a live `Iterator<File>` is still pointing at it via its `state`
field. The next iteration loads `fp` from freed memory → heap-UAF.

Instead, `close(File)` must call only the user-facing
`__ry_io_file_close` runtime helper, which sets
`IoFileHandle::fp = nullptr` inside the still-alive ARC box. The
File's ARC header is released exactly once at scope exit (or when
the last alias drops). The `lines()` iterator's `next_fn` checks
`fp == nullptr` at the top of every step and terminates the
iteration cleanly (no error raised — Python-compatible).

**Why**: Tcp resources (`TcpListener` / `TcpStream` / `TlsStream`)
historically used `emitResourceFree` because they have no
long-lived iterator companion — close = the end. File is the first
resource where close and iterate must coexist, so the contract
changes shape: close = invalidate the handle inside the box; ARC
release = drop the box. Bundling the two breaks iterator safety.

**How to apply**:
- `src/codegen_call.cpp` — `close(File)` dispatch emits a direct
  call to `__ry_io_file_close` (returns Unit), not
  `emitResourceFree`.
- `src/runtime/native/io.cpp` — keep `__ry_io_file_close` (user-facing,
  `fp = nullptr`) and `__ry_io_file_cleanup` (ARC destructor,
  `fclose` + `arc_free`) as separate functions. Never collapse
  them. The destructor calls `fclose` only if `fp != nullptr`, so
  close-then-destructor is correct and idempotent.
- New resources following this pattern must adopt the same
  split as soon as a streaming iterator API lands. Until then,
  `emitResourceFree` is acceptable for fire-and-forget close
  (no iterator companion).

### Resource handles bound through pattern matching need explicit `resource_managed_vars_` registration

**Source**: #1818 (2026-05-21, feat — `lines()` iterator UAF debug)
**Tags**: codegen, resource, pattern, arc, file, ok-bind, using-alias

**Rule**: When a resource handle (`File`, `TcpListener`, etc.) is
extracted from a `Result<Resource, Error>` via `Ok(f):` (or similar
pattern), `emitPatternBindingArc` must (a) call `markArcManaged`
on the bind alloca, (b) insert into `resource_managed_vars_`
keyed by the alloca with the kind looked up via
`ResourceKindRegistry::lookupByTypeName(resolved)`, and (c) call
`addResourceKind(bindAlloca, rk)`. Without all three,
`emitScopeCleanupToDepth` will not invoke the resource cleanup
function on scope exit, and `nullifyResourceVar` (called from
`close(f)`) will be a no-op (only zeros allocas in
`resource_managed_vars_`).

A subtle complication: `Result<File, Error>` is **not** in
`arc_tagged_union_vars_` because `fieldTypeIsArcManaged("File")`
returns false (File is a resource, not an ARC-payload struct).
That means the subject of the `case` does NOT emit an ARC release
at scope exit. Consequently, `emitPatternBindingArc` for File must
NOT emit a balancing `emitArcRetain` — the strong=1 from
`__ry_io_file_open` transfers directly to the bind through the
ExtractValue without a refcount adjustment. Adding a retain in
that path leaks.

**Why**: The `arc_tagged_union_vars_` registration filter and the
`emitPatternBindingArc` retain logic must agree on whether the
subject will release at scope end. Mismatches cause leak (retain
without release) or double-free (no retain but the subject did
release).

**How to apply**:
- In `emitPatternBindingArc` (path 2a — record-typed bind), when
  `resolved` is a known resource type name (`File`, `TcpListener`,
  `TcpStream`, `TlsStream`), look up the kind via
  `ResourceKindRegistry`, register all three side-tables, and
  return WITHOUT emitting retain.
- Aliasing the bind via `using fh = f` then requires a one-time
  retain at the `using` site (next entry), because `popScope`
  will release the alias and would underflow without the retain.

### `using <alias> = <variable>` retains when the source is a tracked binding

**Source**: #1818 (2026-05-21, feat — `lines()` iterator UAF debug)
**Tags**: codegen, using, alias, arc, resource, retain

**Rule**: `using fh = f` where `f` is already in
`resource_managed_vars_` (e.g. `f` came from `Ok(f):` and was
registered per the previous entry) MUST emit a one-time
`emitArcRetain` on the source's ARC header before storing it into
the alias alloca. Without this retain, the alias's scope-end
release runs `__ry_io_file_cleanup` on a handle that the source
still holds; when the source then scope-exits, it releases a
zero-refcount box → double-free.

The retain is conditional: `using f = open(...)` (the fresh-temp
case) does NOT need a retain because the runtime allocator already
returns strong=1 and the alias is the sole owner. Only the
variable-source case adds an aliasing reference.

**Why**: The `using` statement's contract is "scope-bound release".
For fresh temps the source is anonymous, so the alias inherits the
sole ownership and one release is correct. For aliased
variable-source bindings, both the source and the alias must
release independently, requiring one retain.

**How to apply**: In `emitStmt(UsingStmt)`
(`src/codegen_stmt_loop.cpp`), after evaluating the init expression
but before any pointer store, check whether the init was a
`VariableExpr` whose alloca is in `resource_managed_vars_`. If yes,
call `emitArcGetHeaderFromData(val)` + `emitArcRetain(hdr, /*atomic=*/false)`.
Skip the retain for any other init shape (call expression, field
access, etc.). When generalising to other resource kinds, keep this
predicate purely structural — do not rely on the resource kind
itself; only on whether the source is a tracked variable.

### `threadSpawn` thunks emit no ARC ops; `parallel_for_depth_` is the atomic-ARC scope marker

`threadSpawn` thunks do NOT retain/release captures — env stores raw pointer copies and the thunk's `FnScope` is destroyed at codegen without `popScope()`. Caller MUST call `threadJoin` before the captured value's owning scope exits. `parallel_for_depth_` counter (`isArcAtomic() → atomicrmw seqcst`) covers ARC ops emitted directly inside `@parallel for` thunk bodies; it does NOT propagate to helper-function calls. Decision: keep the scope-counter design; whole-program "may cross threads" analysis was rejected as too expensive.

### `for` loop over a collection: snapshot via `emitArcRetain` to prevent buffer-pointer UAF

**Source**: #1021 (2026-04-16), #1041 (2026-04-17), #1091 (2026-04-17)
**Tags**: codegen, for-loop, list, set, map, arc, cow, use-after-free, iteration

**Rule**: Never cache a collection's internal buffer pointer (`data`, `keys`,
`vals`) in an SSA value that is read inside the loop body **when the iterable
carries an external alias** (`VariableExpr`, `FieldAccessExpr`, or `IndexExpr`). `append!`/`add`/map-insert grow the
collection; when the buffer is full, they `arc_alloc` a new buffer, copy, and
**`free` the old buffer** — leaving any pre-loop SSA pointer dangling (UAF).

**External-alias gate**: Apply retain+snapshot when
`iterableHasExternalAlias(*s->iterable)` returns true — i.e., the iterable is
`VariableExpr`, `FieldAccessExpr` (e.g. `obj.items`, `self.xs`), **or** `IndexExpr`
(e.g. `xs[i]`, `m["key"]`). For true temporaries (`range(100)`, result of a call
expression, `emitStringToCharList` output, list/set/map literals, etc.) there is no
external alias that can mutate the collection through CoW — pre-loop SSA values are safe.

**FieldAccessExpr / IndexExpr semantic difference**: `tryGetReceiverAlloca` returns `nullptr`
for `FieldAccessExpr` (only handles `VariableExpr`), so no CoW fork occurs on
mutation — `append!` mutates in-place. The retain still protects the snapshot:
the header's `strong_count` is bumped to ≥ 2, so the buffer is not freed. The
loop iterates the original length read from the snapshot before mutation.


**Fix**:

1. Before emitting the loop header, call `emitArcGetHeaderFromData(iterable)` →
   `emitArcRetain(arcHdr)` to bump `strong_count`.
2. Store the collection data pointer in a new scope-owned alloca (e.g.,
   `__for_iter_snap_N`). Register it with `setTypeMeta` (correct
   `ListElem`/`SetElem`/`MapKey`) and `markArcManaged` so `popScope()` releases
   it on normal exit, `return`, and `break`.
3. Load `len` once from the snapshot alloca right before `emitIndexedForLoop`.
   Inside the body lambda, load `data`/`keys`/`vals` from the snapshot alloca per
   iteration (the retain freezes the header; CoW on the source alias forks a new
   header without touching ours).
4. **Do NOT emit an explicit release.** `popScope()` / `emitScopeCleanupToDepth`
   walk `arc_managed_vars_` and call `emitArcReleaseVar` on every exit path.

**For non-alias iterables**: load `data`/`keys`/`vals`/`len` once before
`emitIndexedForLoop` as SSA values, capture them in the body lambda.

**Why retain works**: any mutation through the source alias inside the loop body
triggers `emitCowCheckSlot`, which sees `strong_count ≥ 2` and forks a new
header into the source alloca. Our snapshot alloca still points to the original
frozen header; the loop iterates the original content.

**Sequential-loop safety**: use a per-ForStmt counter for unique snapshot names
(`__for_iter_snap_0`, `__for_iter_snap_1`, …). `getOrCreateVar` looks up only
the **current scope**, so sequential loops in the same function scope cannot
accidentally share a snapshot alloca if the names differ.

**How to verify**: in `codegen_stmt_loop.cpp`, per-iteration buffer loads for
`iterableHasExternalAlias` paths should read from a `for_snap` alloca; for
non-alias paths, the captured SSA `dataPtr`/`keysPtr`/`valsPtr` values are used
directly.

### Thread / parallel-for thunk epilogue: `popScope()` before `CreateRetVoid()`

**Source**: #1090 (2026-04-17)
**Tags**: codegen, thread, parallel-for, arc, thunk, ir-verification

**Rule**: In any internally-generated thunk function (`__ry_thread.N`,
`__ry_parallel_for.N`), call `popScope()` **before** `builder_.CreateRetVoid()`.
`popScope()` → `emitScopeCleanupToDepth()` → `emitArcReleaseVar()` emits
`CreateLoad` + `CreateCondBr` diamond blocks. If `CreateRetVoid()` has already
terminated the current BB, those instructions land after the terminator, producing
malformed IR (multiple terminators / post-terminator instructions). The JIT
optimizer (`LowerExpectIntrinsicPass` for O2) crashes on such IR.

**Correct epilogue pattern** (from `emitParallelForRange`, `codegen_stmt_loop.cpp:765-766`):

```cpp
// body terminated by explicit return?  ReturnStmt already drained
// arc_managed_vars_; iterator_malloc_stack_ items also emitted; skip.
if (!builder_.GetInsertBlock()->getTerminator()) {
    popScope();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateRetVoid();
}
```

Note: if `iterator_malloc_stack_` at the current scope has items,
`emitScopeCleanupToDepth` emits `free` calls but does **not** clear the vector
(`codegen.cpp:384-390`). If `ReturnStmt` already fired `emitScopeCleanupToDepth(0)`,
those free calls were already emitted; an unconditional subsequent `popScope()` would
re-emit them into the (terminated) block. The `if (!terminated)` outer guard prevents
this double-free / post-terminator insertion.

**IR verification gap**: `threadSpawn` thunks now have `llvm::verifyFunction`
(`codegen_call_thread.cpp:284-288`). `emitParallelForRange` does not yet have it
(follow-up opportunity). Root cause of #1090 was the missing verify — the malformed
IR survived codegen and crashed only during JIT optimization.

### Block-valued expression tails must retain a scope-owned ARC value before `popScope` (current-frame-gated)

**Source**: #1891 (2026-06-07, implementation — advisor-caught)
**Tags**: codegen, arc, case-expr, if-block-expr, block-tail, scope-cleanup, use-after-free, escape-retain

**Context**: A block-valued expression — a `case`-expression block arm (#1891) or
an `if`-expression block branch (`IfBlockExpr`, #798) — evaluates intermediate
statements then a **tail expression** whose value escapes the arm/branch scope
through the merge PHI. When that tail is a bare load of an ARC-managed variable
bound *inside* the scope (a block-local `items = [..]` then tail `items`, or a
pattern binding `Some(v):` … `v`), the imminent `popScope()` releases the binding
and drops the value's `strong_count` to zero — the PHI input then dangles
(use-after-free; the result comes back freed/empty, e.g. `[]`). `IfBlockExpr`
shipped with this latent UAF; #1891 fixed both it and the new case-expr arms.

**Rule**: Before the arm/branch `popScope()`, call
`retainBlockTailIfScopeOwned(tailVal)` (`src/codegen_arc.cpp`). It retains (via
`tryRetainArcSource`) **iff** `tailVal` is a `LoadInst` whose source alloca is in
the innermost frame (`scope_stack_.back()`). That single gate is exactly right:

- block-local / pattern-bound var (in frame) → retain +1, balanced by the frame's
  own release — the `ReturnStmt` escape-retain, scoped to one frame instead of all;
- outer-scope var (NOT in frame) → skipped, because this `popScope()` does not
  release it, so retaining would **leak** (this is the case to get right — the
  current-frame check excludes it for free);
- fresh temp (`arc_owned`, not a `LoadInst`) → skipped; it already owns its +1 and
  transfers through the PHI;
- non-ARC frame var → `tryRetainArcSource` self-gates on `isArcManaged`, no-op.

**How to apply**: any new block-valued expression form that emits `stmts* tail`
then `popScope()` must call `retainBlockTailIfScopeOwned(tailVal)` between the
tail emit and `popScope()`. Verify with a value-asserting ASan matrix
(block-local / pattern-bound / fresh-temp / outer-var / returned-from-fn / nested
tails): under-retain corrupts the value (an `expect` assertion catches it),
over-retain leaks (only ASan catches it). Canonical tests: the "case expression
block-arm ARC ownership" describe block in `tests/spec/case_unification.test.ry`.
