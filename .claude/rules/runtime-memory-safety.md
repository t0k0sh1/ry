---
paths:
  - "src/runtime_*.cpp"
  - "include/ry/runtime_*.hpp"
  - "include/ry/runtime_alloc.hpp"
  - "src/codegen_stmt_loop.cpp"
---

# Runtime / Memory

### Runtime functions returning ARC-managed structs must use `arc_alloc`, not `checked_malloc`

**Source**: #997, #1007, #1011
**Tags**: arc, runtime, memory-safety, iolistheader, listheader, mapheader, gotcha

**Rule**: Any runtime function returning a heap-allocated struct (`IOListHeader`, `ListHeader`, `MapHeader`, etc.) **to Ry code** must allocate with `arc_alloc`, not `checked_malloc`. `emitVarDecl` emits a retain that reads `header_ptr - ARC_HEADER_SIZE` (16 bytes) to bump strong-count; with `checked_malloc` that region is malloc metadata and `arc_release` corrupts it (`malloc: *** error for object 0x...: pointer being freed was not allocated`). ASan layout differs and may not reproduce the crash.

**Error-path pairing**: when `arc_alloc` succeeds but the function bails before returning, free the header with `arc_free`, not `free`. Inner `data` buffers allocated with `checked_malloc` still use plain `free` (e.g. `__ry_tcp_receive`: `free(header->data)` then `arc_free(header)`).

**C++ tests** that wrap these headers must use `arc_free` (from `include/ry/runtime_arc.hpp`), not `free`. Individual string elements created by `dupString` are plain `checked_malloc` and freed with plain `free`.

Helpers that already use `arc_alloc`: `makeStringList` / `makeMatchList` (`runtime_list.hpp`), `makeByteList` (`runtime_io.hpp`), `makeEmptyIOList` (`runtime_net.cpp`), `build_str_map` (`runtime_http.cpp`).

### JIT-visible atomic counters must not expose `std::atomic<T>` storage via raw `T*`

**Source**: #1158
**Tags**: runtime, atomic, jit, abi, layout, arc

**Rule**: If JIT IR needs the address of a counter as a raw `int64_t*`, back the storage with `alignas(T) T` and access via `__atomic_*` builtins. Do **not** store as `std::atomic<T>` and return `reinterpret_cast<T*>(&atomic_obj)` to the JIT — C++ does not guarantee `std::atomic<int64_t>` has the same layout as plain `int64_t`. Ry's JIT emits `atomicrmw` directly against `__ry_arc_counter_address()`. `__atomic_fetch_add` / `__atomic_fetch_sub` / `__atomic_load_n` preserve relaxed-atomic semantics without depending on `std::atomic` object layout.

### RWLock dispatch state must be thread-local, not guarded by a shared mutex

**Source**: #871 (follow-up to #630)
**Tags**: rwlock, runtime-thread, concurrency, deadlock, thread-local, gotcha

**Rule**: In `src/runtime_thread.cpp`, the per-thread counter that tells `__ry_rwlock_unlock` whether to call `unlock_shared()` vs `unlock()` lives in a `thread_local std::unordered_map<RWLockHandle*, int>` at namespace scope. Do NOT reintroduce a shared `unordered_map<thread::id, int>` guarded by a separate `mode_mu`.

**Why**: holding `mode_mu` across `lock_shared()` produces a three-way deadlock under writer contention — writer waits for existing reader, existing reader waits for `mode_mu`, `mode_mu` owner waits for writer. Thread-local tracking sidesteps it because no shared data structure exists for the dispatch.

**Caveat**: unbalanced `rwlockReadLock`/`rwlockUnlock` leaves a stale TLS entry until thread exit; if the handle is freed and reallocated at the same address, the stale count could be misinterpreted. The previous map-based tracker had the same property — unbalanced lock usage is a program bug.

### Recursive `lock_shared()` on `std::shared_mutex` is undefined behavior in C++17

**Source**: #871 (CodeRabbit on PR #885)
**Tags**: rwlock, runtime-thread, std-shared-mutex, ub, gotcha

**Rule**: `__ry_rwlock_read_lock` must call `mu.lock_shared()` **only on the outermost read acquire**, gated by `tls_rwlock_read_counts` being empty for that rwlock. Nested reads only bump the TLS counter. `__ry_rwlock_unlock` mirrors this: `unlock_shared()` runs only when the counter drops 1 → 0. Per [thread.sharedmutex.requirements], calling `lock_shared` while the thread already owns the mutex (any mode) is UB. Earlier draft called `lock_shared()` unconditionally — passed on libc++ (benign UB) but a time-bomb under stricter stdlibs / instrumented builds. Regression covered by `tests/test_runtime_rwlock_stress.cpp::NestedReadLockPerThread`.

### `for` loop over a collection: snapshot via `emitArcRetain` to prevent buffer-pointer UAF

**Source**: #1021, #1041, #1091
**Tags**: codegen, for-loop, list, set, map, arc, cow, use-after-free, iteration

**Rule**: Never cache a collection's internal buffer pointer (`data`/`keys`/`vals`) in an SSA value read inside the loop body **when the iterable carries an external alias**. `append!` / `add` / map-insert grow the collection: a full buffer triggers `arc_alloc` of a new buffer, copy, and `free` of the old — leaving any pre-loop SSA pointer dangling.

**External-alias gate** (`iterableHasExternalAlias(*s->iterable)`): true for `VariableExpr`, `FieldAccessExpr` (e.g. `obj.items`), `IndexExpr` (e.g. `xs[i]`, `m["key"]`). True temporaries (`range(100)`, call results, list/set/map literals) have no external alias — pre-loop SSA values are safe.

**FieldAccessExpr / IndexExpr nuance**: `tryGetReceiverAlloca` returns `nullptr` for `FieldAccessExpr` (only handles `VariableExpr`), so no CoW fork on mutation — `append!` mutates in-place. The retain still protects the snapshot: `strong_count ≥ 2`, buffer not freed, loop iterates the original length.

**Fix**:
1. Before the loop header, call `emitArcGetHeaderFromData(iterable)` → `emitArcRetain(arcHdr)` to bump `strong_count`.
2. Store the data pointer in a scope-owned alloca (`__for_iter_snap_N`). Register with `setTypeMeta` (correct `ListElem`/`SetElem`/`MapKey`) and `markArcManaged` so `popScope()` releases on every exit path.
3. Load `len` once from the snapshot before `emitIndexedForLoop`. In the body lambda, load `data`/`keys`/`vals` from the snapshot per iteration.
4. **Do NOT emit explicit release** — `popScope()` / `emitScopeCleanupToDepth` walk `arc_managed_vars_` and emit `emitArcReleaseVar` on every exit.

**Sequential-loop safety**: per-ForStmt counter for unique snapshot names (`__for_iter_snap_0`, `__for_iter_snap_1`, …). `getOrCreateVar` looks up only the current scope, so different names prevent accidental sharing.

**Non-alias iterables**: load `data`/`keys`/`vals`/`len` once before the loop, capture as SSA in the body lambda.

### Thread / parallel-for thunk epilogue: `popScope()` before `CreateRetVoid()`

**Source**: #1090
**Tags**: codegen, thread, parallel-for, arc, thunk, ir-verification

**Rule**: In any internally-generated thunk (`__ry_thread.N`, `__ry_parallel_for.N`), call `popScope()` **before** `builder_.CreateRetVoid()`. `popScope()` → `emitScopeCleanupToDepth()` → `emitArcReleaseVar()` emits `CreateLoad` + `CreateCondBr` diamonds; if `CreateRetVoid()` already terminated the BB, those land after the terminator → malformed IR → `LowerExpectIntrinsicPass` (O2) crashes.

```cpp
// from emitParallelForRange, codegen_stmt_loop.cpp:765-766
if (!builder_.GetInsertBlock()->getTerminator()) {
    popScope();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateRetVoid();
}
```

If `iterator_malloc_stack_` at the current scope has items, `emitScopeCleanupToDepth` emits `free` calls but does NOT clear the vector (`codegen.cpp:384-390`). If `ReturnStmt` already fired `emitScopeCleanupToDepth(0)`, an unconditional `popScope()` would re-emit them into the terminated block. The `if (!terminated)` outer guard prevents this.

**Verification gap**: `threadSpawn` thunks now have `llvm::verifyFunction` (`codegen_call_thread.cpp:284-288`); `emitParallelForRange` does not yet — root cause of #1090 was the missing verify.

### `~CodeGen()` glibc heap-check crashes are not destructor bugs

**Source**: #1161 / #1167
**Tags**: codegen, arc, glibc, debugging, heap-corruption, gotcha

**Rule**: When `CodeGen::~CodeGen()` aborts on Linux glibc with `corrupted size vs. prev_size while consolidating` and reproduces persistently (especially under ASan), treat it as an **earlier** OOB write or UAF in ARC-managed runtime buffers (Map/List/Set CoW copies, ARC headers) detected only when a neighbouring chunk is freed during `ThreadSafeModule` teardown. NOT a destructor-ordering or member-lifetime bug.

`~CodeGen` is compiler-generated; `mod_`/`ctx_` are moved into `ThreadSafeModule` at `src/codegen.cpp:575` (destructor never touches IR). ARC tracking sets store bare `llvm::Value*` and only walk hash buckets — no dereference.

**Diagnostic checklist**:
1. Reproduce on Linux with ASan (ARM64 Docker via `docker/run.sh asan`; x86_64 CI asan on `v*` push). macOS libSystem malloc doesn't expose glibc chunk-metadata checks — Linux-only even with the same bug.
2. ASan pinpoints the actual OOB/UAF site instead of the late `free()`.
3. Look first at recent changes in `emitCowCheckSlot` / `emitCowDeepCopy*` / `emitMapKeyLookup` / `emitSetElementLookup`: ARC retain gating, `keyName`/`elemName` propagation, hash-vs-linear-scan branch selection.

**Scope**: this entry is for crashes ASan reports as heap-buffer-overflow / UAF. Crashes where all tests pass and only the teardown crashes (disappears on rerun) are pre-existing LLVM ORC flakiness — see the `LLVM ORC JIT intermittent crash` entry instead.

### Ry runtime panics bypass C++ exceptions — they `fprintf + exit(1)`

**Source**: #828
**Tags**: panic, runtime, exception, thread, gotcha

**Rule**: `CodeGen::emitRuntimeError` emits IR that runs `fprintf(stderr, ...)` + `exit(1)` + `CreateUnreachable` (`src/codegen_call_user.cpp:600-618`). Ry's user-level panics (divide-by-zero, OOB, range/contract violations, integer overflow) **terminate the entire process** — they do NOT propagate as C++ exceptions.

Features planning to "catch a worker panic and report as a value" (`threadJoin → Err`, `blockOn → Err`) cannot rely on existing defensive `try/catch (std::exception &)` blocks — those only fire for C++ exceptions escaping LLVM-generated code, which never happens for user panics. The catch in `__ry_thread_spawn` is dormant. Fixing this requires refactoring `emitRuntimeError` to `throw std::runtime_error` + a top-level catch in `ry run`/`ry test` (tracked in #880).

**Before writing tests** that trigger a panic and expect `Err(error_msg)`, verify the panic actually throws a C++ exception. Current throw paths from runtime: `__ry_task_join` double-join (`src/runtime_parallel.cpp:292`) and a few compile-time lexer/loader errors.

### NUL-safety at C API boundaries: `stringByteLen`, `hasEmbeddedNul`, propagating nullptr

**Source**: PR #1054
**Tags**: nul-safety, c-api-boundary, stringByteLen, hasEmbeddedNul, Result

**Rule**: PR #1022 introduced `StringHeader.byte_len`, allowing Ry `str` to contain embedded NUL. C APIs (`realpath`, `stat`, `mkdir`, `getaddrinfo`) silently truncate at `\0`. Apply three patterns:

**1. Use `stringByteLen(handle)` instead of `strlen(handle)`** for any function receiving a Ry `str` handle.

**2. Validate with `hasEmbeddedNul` before NUL-sensitive C APIs**:
```cpp
if (hasEmbeddedNul(p)) {
    setLastError("path.basename: argument contains an embedded NUL byte");
    return nullptr;  // wraps as Result<str, Error> via ResultPtr
}
```

**3. Propagate `nullptr` through chained join helpers**:
```cpp
char *ab = join2_impl(a, b);
if (!ab) return nullptr;  // without this, join2_impl(nullptr, c) silently returns c
char *result = join2_impl(ab, c);
```

**How to apply**:
- Binary-safe consumers (HTTP body, multipart values): use `stringByteLen` for length; do NOT reject NUL.
- Text/path consumers (URLs, host names, header names, file paths): reject NUL with `hasEmbeddedNul`, return `Err`/nullptr.
- Upgrade bare `-> str` runtime fns passing user strings to NUL-sensitive C APIs to `-> Result<str, Error>` (return nullptr + `setLastError`; set `ReturnWrapping::ResultPtr`).
- **Audit scope**: PR #1054 covered libc-sensitive modules (path, net, http, http_client, filesystem) but not binary-safe stdlib (e.g. base64). When adding a new module, grep its runtime for `strlen(input)`. `base64` was a post-#1054 follow-up in #1129.

### `hasEmbeddedNul` / `stringByteLen` are only safe on Ry string handles — never on raw C strings

**Source**: PR #1054
**Tags**: nul-safety, stringheader, c-api-boundary, runtime

**Rule**: Both read a `StringHeader` struct **before** the pointer to obtain `byte_len`. Valid only when `handle` originated from a Ry string slot (`makeString` or read from a Ry `str`).

**UB** when applied to:
- C string literals: `hasEmbeddedNul("POST")` — no `StringHeader` prefix.
- `std::string::c_str()` results — STL has no `StringHeader`.
- `strdup`/`malloc`-allocated strings (e.g. map keys built by C++ tests).
- Any other raw `const char *` not from a Ry slot.

**Symptoms** are silent: `hasEmbeddedNul` false positive returns `nullptr` from runtime fn → HTTP mock server `::accept` waits forever (test hangs). `stringByteLen` garbage value → `Content-Length` wildly wrong → server reads wrong byte count → hang.

**How to apply**: Only call from codegen emitters (where args are known Ry handles), or from runtime fns whose callers pass exclusively Ry handles. If the runtime fn is also called from C++ tests with `.c_str()` or C literals, move the NUL check to the codegen emitter.

### Do not call `setLastError` in bool-return runtime functions — thread-local pollution

**Source**: #1128
**Tags**: nul-safety, setLastError, thread-local, runtime, bool-return

**Rule**: `setLastError` writes a thread-local buffer (`last_error_buf` in `runtime_error.cpp`). In runtime fns whose Ry return type has **no error channel** (e.g. bool-returning `__ry_file_exists`), calling `setLastError` on NUL rejection leaves stale text in the buffer. The next Result-returning call on the same thread that fails an unrelated check reads the buffer for `e.message` and silently reports the stale NUL message instead of its own.

**How to apply**:
- Bool-return-only fns detecting an invalid argument: return the safe conservative value (`false`/`0`) silently — do NOT call `setLastError`.
- Result-returning fns: `setLastError("fn: argument contains an embedded NUL byte")` before returning `nullptr`/`1`.
- Reference split in `src/runtime_io.cpp` after #1128: `__ry_file_exists` has `if (hasEmbeddedNul(path)) return 0;` with no `setLastError`; all other path-taking fns call it.

### Forbidden heap allocation functions in new C++ code

**Source**: #1498
**Tags**: runtime, memory-safety, malloc, oom, checked_malloc, forbidden-functions, lint

**Rule**: Use the safe wrappers from `include/ry/runtime_alloc.hpp`. The following must NOT be called directly:

| Forbidden | Replacement | Reason |
|-----------|-------------|--------|
| `malloc` | `checked_malloc` | OOM → null → segfault |
| `realloc` | `checked_realloc` | OOM → null |
| `calloc` | `checked_malloc` + `memset` | OOM → null |
| `strdup` | `checked_strdup` | OOM → null |
| `strndup` | `checked_strndup` | OOM → null |
| `malloc(count * sizeof(T))` | `checked_array_malloc(count, sizeof(T))` | int overflow → heap buffer overflow |

Additional rules:
- On OOM, call `oom_abort(n)` with the requested size and abort — do not return `nullptr`.
- Before passing external input (HTTP body, JSON parse result, etc.) to `strcmp` / `strlen`, check for NULL.
- The CI `lint` job auto-blocks new direct calls to forbidden functions.
