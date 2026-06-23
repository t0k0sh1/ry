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

This file covers only hazards that are not visible from reading the code.

### Container slot writes and memcpy must retain new + release old, symmetric with destructor

Every write into an ARC-managed slot or buffer must retain new BEFORE releasing old, so `xs[i] = xs[i]` cannot transiently drop the refcount to zero. Every `memcpy` of an ARC element buffer must be paired with the element-retain helper — destructors mirror this by walking and releasing the buffer.

### Tuple-element collections dispatch retain/release locally, not through `fieldTypeIsArcManaged`

`List<(K, V)>` buffers hold inline tuple struct values, not pointers. Making `fieldTypeIsArcManaged("(K,V)")` return true would corrupt callers that read the struct as a pointer — the asymmetry is intentional.

### `tryRetainArcSource` LoadInst cases must be metadata-gated; `isStringValue` is the canonical positive-evidence gate

**Tags**: codegen, arc, str, metadata-gate, heap-corruption, gotcha

Never pass an unguarded container-element load to `−24` retain. A bare `ptrTy_` check spuriously retains weak refs, fn-ptrs, and resource handles, then crashes on the `−16` GEP. The errant `+1` on a Map/List/Set header writes into the adjacent allocation's tail 8 bytes — visible to glibc as `invalid next->prev_inuse` (Linux SIGSEGV) and to macOS as a silent heap corruption surfacing downstream ("map key not found"). `isStringValue` is positive-evidence (true only when one of four channels confirms str); `isStrLike` is negative-evidence legacy (true for every metadata-less `ptrTy_`) and must never drive a `−24` dispatch. The `@it`-block scaffolding stabilises the heap and masks this failure in unit tests — the only reliable detector is a subprocess 100-iter regression loop.

### Fn-typed param values loaded for `return` need a dedicated retain helper, not a `tryRetainArcSource` case

**Tags**: codegen, arc, fn-typed-param, uniform-closure, return-stmt, uaf

Do NOT widen `tryRetainArcSource`'s LoadInst cases to cover fn-typed-param UAF — the helper fires at every load site and non-return sites already have caller-side ownership via `releaseUniformClosureTemps`; widening would over-retain every compound-op reload. The fix is a dedicated helper called only from `emitStmt(ReturnStmt)`, gated on `meta->fn_type_info && meta->fn_type_info->isUniformClosure`.

### `str` is fully ARC-managed via `StringHeader`; offset by side-table membership, not by `isStringValue`

`str` offsets: `−24 strong_count`, `−16 weak_count`, `−8 byte_len`, `+0 data` (`STRING_HEADER_SIZE=24` vs `ARC_HEADER_SIZE=16`). Literal globals are `isConstant=true` — writing the wrong offset hits the read-only page → SIGSEGV into JIT addresses (not SIGABRT, counter-intuitive). Never dispatch the offset by `isStringValue` for captured closures — List/Map/Set inside closure bodies lose collection metadata after capture and get misclassified; use the declaration-time inner-type name.

### Map/List/Set literal with str: side-table retain + `*_type_name = "str"` stamp

The post-loop stamp must use `isStrLike`, not `isStrHandle`. `isStrHandle` excludes immortal literal globals from `cachedGlobalString` (plain `{"a": 1}` keys), so the stamp would silently drop and `resolveCollectionDestructor` would pick the non-str destructor — leaking str elements.

### Pattern binding ARC: every extracted sub-value goes through `emitPatternBindingArc`

Without the call, scoped release calls `free(ptr - 16)` on a non-retained pointer. In `emitExprVariant(CaseExpr)`, capture `armEndBB` AFTER `popScope()` — the ARC BBs inserted by `emitPatternBindingArc` + `popScope()` advance the insert block, so capturing before breaks PHI predecessor matching.

### `markArcManaged(tmp)` pre-mark must be guarded by `fieldTypeIsArcManaged`; str fields also into `arc_str_managed_vars_`

**Tags**: codegen, arc, pattern-binding, markArcManaged, str, fieldTypeIsArcManaged

Without the guard, any `ptrTy_` field (including fn-ptr and resource ptrs) is incorrectly marked ARC. The `arc_str_managed_vars_` insertion is also required so `tryRetainArcSource` dispatches to offset `−24` instead of `−16` for str fields. Capturing closures in tuple/record/enum fields are intentionally excluded — `fn_type_info` metadata is not propagated onto `ExtractValue` intermediates.

### Record ARC reassignment guard: `!isa<CallInst> && !isa<InvokeInst>`

The guard must be `!isa<CallInst>(val) && !isa<InvokeInst>(val)`. The original `LoadInst || ExtractValueInst` allowlist was too narrow — `InsertValueInst` records (`r2 = { r.field, new_val }`) are aliased views and must be retained, but the allowlist let them through without a retain, causing UAF.

### Compound assignment is codegen-side; snapshot type name before `propagateTypeMeta`

Never desugar compound assignment in the parser (`a[f()] += v` would evaluate `f()` twice). Snapshot the element type name to a local `std::string` before calling `propagateTypeMeta` — the helper may rehash and invalidate the `getMeta` pointer.

### `str` rejects `[]` syntax — guard with `isStrLike` before list/map dispatch

The write-path `str` guard must come before `emitCowCheck` — passing a `str` pointer with `CollectionKind::List` is unsafe. Use `isStrLike` (not `isStringValue`): a user-fn-returned str is a `CallInst` outside every positive-evidence channel and would slip past `isStringValue` into the CoW dispatch.

### `wrapInAny` / `unwrapFromAny` retain on collection AND str wrap; `emitAnyReleaseVar` tag-dispatched scope-end release

**Tags**: codegen, arc, any, collection, str, record, enum, retain-release, scope-cleanup, descriptor

`str` uses offset `−24`; collections and record/enum boxes use offset `−16` — confusing the two corrupts the header words. Without per-target field-wise retain at `unwrapFromAny`, two `any` values pointing at the same box each release once at scope exit and double-free. The descriptor word inside record/enum boxes is the source of truth for release/equality/subtype dispatch after the static type erases to `any` — `sourceTypeName` may be stale. `canAnyHoldType` is deliberately NOT widened for record or enum.

### `using` statement: evaluate init **before** `pushScope` so `?`-Err in the initializer skips auto-close

**Tags**: codegen, using, scope-cleanup, resource, question-mark, ordering, arc

`emitExpr` must run before `pushScope()` and before writing to `resource_managed_vars_`. A `?` in the initializer emits an early return via `emitScopeCleanupToDepth(0)` — if the scope already contains the `using` binding, it emits `__ry_io_file_cleanup` on a value that was never successfully bound.

### Resource-handle `close()` is decoupled from ARC release so live iterators survive

**Tags**: codegen, resource, close, arc, iterator, lifetime, file

Do NOT route `close(File)` through `emitResourceFree`. That helper combines the runtime cleanup with an immediate `__ry_arc_release` — releasing the box while a live `Iterator<File>` still holds a `state` pointer into it. The next iteration loads `fp` from freed memory → heap-UAF. `close(File)` calls only `__ry_io_file_close` (sets `fp = nullptr` inside the still-alive box); the ARC header releases at scope exit.

### Resource handles bound through pattern matching must NOT emit a balancing retain

**Tags**: codegen, resource, pattern, arc, file, ok-bind, using-alias

`Result<File, Error>` is NOT in `arc_tagged_union_vars_` because `fieldTypeIsArcManaged("File")` returns false, so the `case` subject does NOT emit an ARC release at scope exit. `emitPatternBindingArc` for File must therefore NOT emit a balancing `emitArcRetain` — strong=1 transfers directly through the ExtractValue. Adding a retain leaks.

### `using <alias> = <variable>` retains when the source is a tracked binding

**Tags**: codegen, using, alias, arc, resource, retain

`using fh = f` (variable source, `f` in `resource_managed_vars_`) must emit a one-time retain. `using f = open(...)` (fresh temp, strong=1) must NOT retain. Without the retain on the variable-source case, the alias's scope-end release runs on a box the source still holds → double-free.

### `threadSpawn` thunks emit no ARC ops; `parallel_for_depth_` is the atomic-ARC scope marker

`threadSpawn` thunks do NOT retain/release captures — env stores raw pointer copies; the thunk's `FnScope` is destroyed without `popScope()`. Caller MUST call `threadJoin` before the captured value's owning scope exits. `parallel_for_depth_` does NOT propagate to helper-function calls emitted inside the thunk.

### `for` loop over a collection: snapshot via `emitArcRetain` to prevent buffer-pointer UAF

**Tags**: codegen, for-loop, list, set, map, arc, cow, use-after-free, iteration

Never cache a collection's buffer pointer in an SSA value read inside the loop body when the iterable has an external alias. `append!`/`add`/map-insert may `arc_alloc` a new buffer and `free` the old — leaving the pre-loop SSA pointer dangling (UAF). Gate retain+snapshot on `iterableHasExternalAlias`; true temporaries have no external alias and are safe.

### Thread / parallel-for thunk epilogue: `popScope()` before `CreateRetVoid()`

**Tags**: codegen, thread, parallel-for, arc, thunk, ir-verification

`popScope()` emits `CreateLoad` + `CreateCondBr` diamond blocks. If `CreateRetVoid()` has already terminated the current BB, those land after the terminator — malformed IR that crashes the JIT optimizer (`LowerExpectIntrinsicPass` at O2). Guard `popScope()` with `if (!builder_.GetInsertBlock()->getTerminator())` — if `ReturnStmt` already fired `emitScopeCleanupToDepth(0)`, an unconditional `popScope()` would re-emit free calls into a terminated block.

### Block-valued expression tails must retain a scope-owned ARC value before `popScope` (current-frame-gated)

**Tags**: codegen, arc, case-expr, if-block-expr, block-tail, scope-cleanup, use-after-free, escape-retain

When a block-arm tail is an ARC-managed value bound inside the scope, `popScope()` drops the `strong_count` to zero before the PHI consumer reads it (use-after-free; result comes back freed/empty, e.g. `[]`). Retain only when the owning alloca is in `scope_stack_.back()`. Gating on the tail being a *direct* alloca load is the trap: borrowed sub-values (`box.items` ExtractValue, `xss[i]` GEP element load) trace back to the same frame owner and must also be retained. Outer-scope owners must NOT be retained — this `popScope()` does not release them, so retaining would leak.

### Pre-scan AST walkers must traverse every `ExprPtr` slot

**Tags**: codegen, pre-scan, ast-walker, slot-coverage, blind-spot

A missed slot is silent at compile time; it only surfaces as a runtime correctness bug at a specific emission order — and may be masked if `emitMockCall` happens to emit before the callsite. Document intentionally excluded slots with an inline comment (silent omission is the entry point for bugs). Update `collectTestTargetsFromStmt` / `collectTestTargetsFromExpr` (`src/codegen.cpp`, #1765) whenever a new AST variant is added.

### Hot-path helpers that gate on a rarely-populated map should be lookup-first, not normalize-first

**Tags**: codegen, allocation, heap-perturbation, alias-resolution, lookup-pattern, latent-bug

A normalize-first helper causes allocation churn on the common case (empty map), perturbing heap layout and increasing the reproduction rate of downstream latent bugs (#1888: `collection_meta_propagation.test.ry` went from ~3% to ~63%; root cause is #2246). Try a direct lookup first; call normalization only on a miss when the map is non-empty. Detection recipe: run the failing downstream test 30 times, then revert to `origin/main` — if the failure disappears, allocation churn is the trigger. This is symptom mitigation, not a root-cause fix — surface the exposed latent bug via `/triage-side-finding` Q4(b) as a separate issue.
