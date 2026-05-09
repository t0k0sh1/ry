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

### Container slot writes and memcpy must retain new + release old, symmetric with destructor

Every write into an ARC-managed slot or buffer (`list[i] = v`, `m[k] = v`, `append`/`insert`/`appended`, `slice`/`take`/`concat`/`merge`, IndexAssignStmt, FieldAssignStmt, CoW clone, path CoW) must retain new BEFORE releasing old, so `xs[i] = xs[i]` cannot transiently drop refcount to zero. Pair every `memcpy` of an ARC element buffer with `emitCowRetainArcElements(buf, count, tag, kind)`. `tryRetainArcSource` does NOT cover `LoadInst` from a GEP — manually retain via `emitArcGetHeaderFromData` + `emitArcRetain` when it returns false. Destructors (`getOrCreateCollectionDestructor`, cache key `(CollectionKind, elemSig, valSig)`) walk the element buffer and release each ARC-managed element; every write site must mirror retain-on-store. `emitArcReleaseLoadedElement` requires the inner element type name (not just `CollectionKind`) so `List<List<T>>` etc. dispatch correctly.

### Tuple-element collections dispatch retain/release locally, not through `fieldTypeIsArcManaged`

`List<(K, V)>` data buffers hold inline tuple struct values, not pointers. Tuple is not a `CollectionKind`; making `fieldTypeIsArcManaged("(K,V)")` return true would corrupt callers reading the struct as a pointer. Per-site dispatch (after `propagateMeta` stamps `list_elem_type_name = "(K, V)"` on results of `items` / `enumerate` / `zip` / `slice` / `take` / `appended` / `concat`): buffer sites call `emitTupleElemRetainLoop` / `emitTupleElemReleaseLoop`; per-component sites call `emitTupleComponentRetain` keyed by source-level component type name (str at −24, List/Map/Set at −16). IndexAssignStmt slot-overwrite gates on `resolveTypeAlias(elemTypeName)` + tuple shape and uses `emitTupleComponentRetainTraced` to recurse through nested InsertValue chains; otherwise `arc_owned_values_` leaves get double-retained.

### `tryRetainArcSource` LoadInst cases must be metadata-gated

Each new LoadInst case must gate on container-element metadata (`list_elem || map_key || map_value || set_elem`). A bare `ptrTy_` check spuriously retains weak refs, capturing closures, bare fn-ptrs, and resource handles, then crashes on the `−16` GEP. `str` elements need a dedicated `str_elem` flag and dispatch to `emitStrGetHeaderFromData` (offset −24); stamp `str_elem` ONLY in List<str> / Map<K,str> / Set<str> indexer paths — never from `propagateTypeMeta("str", ...)`, which is called from many non-container-element sites and would corrupt heap state. Do NOT switch callers to `retainArcValue`: its fallback is unconditional and unsafe for non-ARC `ptrTy_` callers like `return 42` or `let x = weakRef`.

### `str` is fully ARC-managed via `StringHeader`; offset by side-table membership, not by `isStringValue`

`str` is a pointer to the `data` region of a `StringHeader`: offsets `−24 strong_count`, `−16 weak_count`, `−8 byte_len`, `+0 data`. `STRING_HEADER_SIZE=24` vs `ARC_HEADER_SIZE=16`. Use `makeString` / `makeStringUninit` / `freeStringSlot`; never `malloc` / `strdup`. Literal globals are `isConstant=true` — wrong offset writes the read-only page → SIGSEGV (not SIGABRT), pointing into JIT addresses. Side-tables select offsets: `arc_str_managed_vars_` (str allocas), `arc_str_owned_values_` (fresh str temps from `emitStrGetDataPtr`, `__ry_string_make_uninit`, …). Use the helpers `emitArcHeaderForAlloca(handle, alloca)` and `CapturedArcKind::Str` at sites that may handle str. Never use `isStringValue` to dispatch the offset (captured `List<int>` / `Map` / `Set` inside closure bodies lose collection metadata after capture and get misclassified) — use the declaration-time inner-type name. `@parallel for` snapshots `capIsArcStr[i]` before `FnScope` clears side-tables. `markArcManaged(tmp)` pre-mark in TuplePattern / RecordPattern / EnumConstructorPattern must be guarded by `fieldTypeIsArcManaged`; for `Str`-kind fields also insert into `arc_str_managed_vars_`. `emitPatternBindingArc` path 2b must check the source set hierarchy `closure → str → arc_backed` in order. CoW copy of `List<str>` forces `doElemRetain=true` (else post-destructor double-free). str+str concat in `emitArithmeticOp` releases input owned temps from `arc_str_owned_values_` at the concat site (not deferred to ExprStmt) so chained `(a + b) + c` doesn't leak inner bufs.

### Map/List/Set literal `{k:v}` / `[s]` / `{s}` with str: side-table retain + `*_type_name = "str"` stamp

`inferCollectionTypeName` returns `""` for plain str, so the default retain gate short-circuits. Use `isStrHandle(v)` (consults `arc_str_owned_values_` + `arc_str_managed_vars_`) to apply `retainArcValue` per-entry, then post-loop stamp `list_elem_type_name` / `set_elem_type_name` / `map_key_type_name` / `map_value_type_name = "str"` on the literal `headerPtr`. For Set, retain inside `insertBB` so dedup-skipped duplicates don't double-retain. Without the stamp, `resolveCollectionDestructor` picks the non-str variant and str elements leak. Map CoW must retain str **keys** independently — `elementTypeIsArcManaged` only checks `map_value_type_name`. Propagate `list_elem_is_str = true` from the stamp into the indexer-facing side-channel via `inner_is_str` in `codegen_stmt.cpp`.

### Pattern binding ARC: every extracted sub-value goes through `emitPatternBindingArc`

Every arm that extracts a sub-value via `ExtractValue` or `Load` must call `emitPatternBindingArc(val, bindAlloca, typeSig)` after the store; without it, scoped release calls `free(ptr - 16)` on a non-retained pointer. `typeSig` per pattern: SomePattern / OkPattern = `extractGenericTypeArg(subjectEnumType, "Option<" / "Result<", 0)`; ErrPattern = `extractGenericTypeArg("Result<", 1)`; EnumConstructorPattern = `fieldTypeNames[bi]`; VariablePattern / TuplePattern / RecordPattern = `""` (heuristic). In `emitExprVariant(CaseExpr)`, capture `armEndBB = builder_.GetInsertBlock()` AFTER `popScope()` — `emitPatternBindingArc` + `popScope()` advance the insert block via inserted ARC BBs; capturing before breaks PHI predecessor matching.

### Record ARC reassignment guard: `!isa<CallInst> && !isa<InvokeInst>`

The AssignStmt record-with-ARC-fields guard for `emitRecordArcFieldsRetain` must be `!isa<CallInst>(val) && !isa<InvokeInst>(val)`. Fresh constructions (Call/Invoke) are sole owners; every other value (including `InsertValueInst` chains like `r2 = { r.field, new_val }`) is a view and must be retained. The original `LoadInst || ExtractValueInst` allowlist was too narrow and caused UAF for `InsertValueInst` records.

### Records are SSA struct values; nested field writes unroll to the root; compound assignment is codegen-side

Records emit as `{T1, T2, …}` value types in an alloca — there is no stable address for an individual field. `rec.a.b = v` walks via `InsertValue` at each hop until reaching the root alloca, ending with one `CreateStore` (`writeBackFieldChain`). Never desugar compound assignment in the parser (`a[f()] += v` would evaluate `f()` twice); `applyCompoundOp` evaluates the LHS address exactly once. Compound-op loaded slot values must call `propagateTypeMeta(name, loadedVal)` immediately after the load — snapshot the name to a local `std::string` before the call (the helper may rehash and invalidate `getMeta` results). Path CoW (`emitPathCowForChain` + `emitCowCheckSlot`) handles chained writes through nested collections (`b[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`); top-level 1-hop CoW (`emitCowCheck`) uses `retainElements=false`, path CoW uses `retainElements=true`. Do not evaluate `s.object` with `emitExpr` first — that re-loads against stale parents. Unsupported chained shapes (compile error): method-call roots, index hops interleaved in record-field walks. Map compound op on a missing key is a runtime error.

### `str` rejects `[]` syntax — guard with `isStringValue` before list/map dispatch

`str` is `ptrTy_` and passes the "must be ptr" gate. After the "index operator requires list or map" gate and BEFORE Map dispatch, check `isStringValue(objPtr)` and emit targeted diagnostics: read path → `"str does not support index access; use charAt(s, i) instead"`, write path → `"str does not support index assignment; strings are immutable"`. The write-path guard MUST come before `emitCowCheck` — passing a `str` pointer with `CollectionKind::List` is unsafe.

### `threadSpawn` thunks emit no ARC ops; `parallel_for_depth_` is the atomic-ARC scope marker

`threadSpawn` thunks do NOT retain/release captures — env stores raw pointer copies and the thunk's `FnScope` is destroyed at codegen without `popScope()`. Caller MUST call `threadJoin` before the captured value's owning scope exits. `parallel_for_depth_` counter (`isArcAtomic() → atomicrmw seqcst`) covers ARC ops emitted directly inside `@parallel for` thunk bodies; it does NOT propagate to helper-function calls. Decision: keep the scope-counter design; whole-program "may cross threads" analysis was rejected as too expensive.
