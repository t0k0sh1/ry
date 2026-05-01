---
paths:
  - "src/runtime_*.cpp"
  - "include/ry/runtime_*.hpp"
  - "include/ry/runtime_alloc.hpp"
  - "src/codegen_stmt_loop.cpp"
  - "src/codegen_arc*.cpp"
---

# Runtime / Memory

### Runtime functions returning ARC-managed structs must use `arc_alloc`, not `checked_malloc`

**Source**: #1007, #1011 (2026-04-16, bug fixes), PR #997 (pattern established for ListHeader)
**Tags**: arc, runtime, memory-safety, iolistheader, listheader, mapheader, http, gotcha

**Rule**: Any runtime function that returns a heap-allocated struct (e.g.
`IOListHeader`, `ListHeader`, `MapHeader`) **to Ry code** must allocate the
struct with `arc_alloc`, not `checked_malloc`.

**Why**: `emitVarDecl` in the codegen emits a retain that reads
`header_ptr - ARC_HEADER_SIZE` (16 bytes) to increment the strong-count.
When the header was allocated with `checked_malloc` that region is malloc
metadata, not an ARC counter. Corrupting it and then calling
`arc_release`/`__ry_arc_free_counted` on scope exit causes a crash:

```text
malloc: *** error for object 0x...: pointer being freed was not allocated
```

ASan builds do not reproduce the crash because the allocator's internal
layout differs and the metadata region happens not to be misinterpreted as
zero.

**Affected sites in this codebase** (checked 2026-04-16):

| Function | File | Fixed by |
|---|---|---|
| `makeStringList`, `makeMatchList` | `include/ry/runtime_list.hpp` | PR #997 |
| `makeByteList` | `include/ry/runtime_io.hpp` | PR #1007 |
| `__ry_read_bytes` | `src/runtime_io.cpp` | PR #1007 |
| `makeEmptyIOList`, `__ry_tcp_receive` | `src/runtime_net.cpp` | PR #1007 |
| `__ry_tls_receive` | `src/runtime_tls.cpp` | PR #1007 |
| `build_str_map` | `src/runtime_http.cpp` | PR #1011 |

**Error-path pairing**: when an allocation succeeds with `arc_alloc` but
the function later bails out before returning to Ry, free with `arc_free`,
not `free`. Example: `__ry_tcp_receive` allocates the header with
`arc_alloc`, then on `recv()` error calls `free(header->data)` (plain `free`
because the data buffer uses `checked_malloc`) and `arc_free(header)`.

**Also**: C++ tests that call these runtime functions directly must also use
`arc_free` (not `free`) to release the returned struct.

### JIT-visible atomic counters must not expose `std::atomic<T>` storage via raw `T*`

**Source**: #1158 (2026-04-23, implementation)
**Tags**: runtime, atomic, jit, abi, layout, arc

**Rule**: If JIT-generated IR needs the address of a counter as a raw
`int64_t*` (or other plain-data pointer), back the storage with an
`alignas(T) T` object and access it through `__atomic_*` builtins. Do
not store the counter as `std::atomic<T>` and then return
`reinterpret_cast<T*>(&atomic_obj)` to the JIT.

**Why**: Ry's JIT emits `atomicrmw` directly against the address returned
by `__ry_arc_counter_address()`. C++ does not guarantee that
`std::atomic<int64_t>` has the same layout or address semantics as a
plain `int64_t`, so exposing its storage as `int64_t*` is a portability
bug even if it happens to work on one toolchain. `__atomic_fetch_add`,
`__atomic_fetch_sub`, and `__atomic_load_n` preserve the required
relaxed-atomic semantics without relying on `std::atomic` object layout.

### RWLock dispatch state must be thread-local, not guarded by a shared mutex

**Source**: #871 (2026-04-11, implementation; follow-up to #630 P1 audit)
**Tags**: rwlock, runtime-thread, concurrency, deadlock, thread-local, gotcha

**Rule**: In `src/runtime_thread.cpp`, the per-thread counter that tells
`__ry_rwlock_unlock` whether to call `unlock_shared()` vs `unlock()` on
the underlying `std::shared_mutex` lives in a
`thread_local std::unordered_map<RWLockHandle*, int>` at namespace
scope. Do NOT reintroduce a shared `std::unordered_map<std::thread::id, int>`
guarded by a separate `mode_mu`.

**Why**: The #630 audit suggested two fixes for the original two-step
race (where `mu.lock_shared()` was held before the tracking map was
updated). Option A — "hold mode_mu across lock_shared()" — **deadlocks**
under writer contention: a thread holding `mode_mu` while blocked
inside `lock_shared()` prevents any existing reader from taking
`mode_mu` to dispatch its own unlock, so the writer it is waiting on
can never make progress. Three-way deadlock: writer waits for existing
reader, existing reader waits for `mode_mu`, `mode_mu` owner waits
for writer. Thread-local tracking (Option B) sidesteps this entirely
because no shared data structure exists for the dispatch.

**Caveat**: unbalanced `rwlockReadLock` without a matching
`rwlockUnlock` leaves a stale TLS entry that persists until thread
exit. If an RWLockHandle is then freed and a new one is allocated at
the same address, the stale count could be misinterpreted. The
previous map-based tracker had the same property — unbalanced lock
usage is a program bug, not something the runtime should engineer
around.

### Recursive `lock_shared()` on `std::shared_mutex` is undefined behavior in C++17

**Source**: #871 (2026-04-11, CodeRabbit review on PR #885)
**Tags**: rwlock, runtime-thread, std-shared-mutex, ub, gotcha

**Rule**: `__ry_rwlock_read_lock` must call `rwlock->mu.lock_shared()`
**only on the outermost read acquire**, gated by `tls_rwlock_read_counts`
being empty for that rwlock. Nested reads just bump the TLS counter.
`__ry_rwlock_unlock` mirrors this: the underlying `unlock_shared()`
only runs when the TLS counter drops from 1 to 0.

**Why**: Per [thread.sharedmutex.requirements] / cppreference
"If lock_shared is called by a thread that already owns the mutex in
any mode (exclusive or shared), the behavior is undefined." The TLS
counter exists solely to dispatch unlock correctly; it does not make
the underlying mutex reentrant. An earlier draft of the #871 fix
called `lock_shared()` unconditionally and still passed tests on
libc++ because the UB happened to be benign, but it would be a
time-bomb under a stricter standard library or instrumented build.
The regression is covered by
`tests/test_runtime_rwlock_stress.cpp::NestedReadLockPerThread`.

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

---

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

---

### `~CodeGen()` glibc heap-check crashes are not destructor bugs

**Source**: #1161 / #1167 (2026-04-18)
**Tags**: codegen, arc, glibc, debugging, heap-corruption, gotcha

**Scope**: This entry covers crashes that **persist under ASan** (i.e. ASan itself reports
a heap-buffer-overflow or use-after-free). For crashes where all test cases pass and only
the teardown crashes — and the crash disappears on re-run — see the ORC JIT flake entry
([`LLVM ORC JIT intermittent crash`](#llvm-orc-jit-intermittent-crash-in-lljit--removeresourcetracker--codegen-linux--macos))
instead; those are pre-existing LLVM ORC flakiness, not user-code bugs.

**Rule**: When `ry::CodeGen::~CodeGen()` aborts on Linux glibc with
`corrupted size vs. prev_size while consolidating` **and the crash reproduces persistently**
(especially under ASan), treat it as an **earlier** OOB write or UAF in ARC-managed
runtime buffers (Map/List/Set CoW copies, ARC headers) that glibc only detects when a
neighbouring chunk is freed during `ThreadSafeModule` teardown — not as a destructor-ordering
or member-lifetime bug in `CodeGen` itself.

**Why `~CodeGen()` is not the culprit**:
- `~CodeGen` is compiler-generated. `mod_` / `ctx_` are moved into
  `ThreadSafeModule` at `src/codegen.cpp:575`; the destructor never touches IR objects.
- ARC tracking sets (`include/ry/codegen.hpp:184-193`) store bare `llvm::Value*`
  and only walk hash buckets at destruction — no dereference.

**Diagnostic checklist**:
1. Reproduce on Linux with ASan (ARM64 Docker via `docker/run.sh asan`; x86_64 CI
   asan job on `v*` push). macOS libSystem malloc does not expose glibc's chunk
   metadata checks, so the crash is Linux-only even with the same bug.
2. ASan will pinpoint the actual OOB/UAF site instead of just the late `free()`.
3. Look first at recent changes to `emitCowCheckSlot` / `emitCowDeepCopy*` /
   `emitMapKeyLookup` / `emitSetElementLookup`: ARC retain gating, `keyName` /
   `elemName` propagation, and hash-vs-linear-scan branch selection are the
   class of change that introduced `~CodeGen()` crash #1161.

**History**: #1161 crash (`corrupted size vs. prev_size`) was observed after PR #1148
landed. The bug was most likely a heap-layout-dependent OOB that became non-reproducible
after PR #1160 (`emitCowCheckSlot` `doKeyRetain` unconditional + `emitMapKeyLookup`
`keyName` fix). CI ASan clean on x86_64 Linux was confirmed at commit `998edc42`
(CI run #24601715375, 2026-04-18). The exact commit that fixed it was not bisected;
if the crash re-appears, revert to the diagnostic checklist above.

---

### Ry runtime panics bypass C++ exceptions — they `fprintf + exit(1)`

**Source**: #828
**Tags**: panic, runtime, exception, thread, gotcha

**Rule**: Every `CodeGen::emitRuntimeError` call site emits IR that
runs `fprintf(stderr, ...)` followed by `exit(1)` +
`CreateUnreachable` (`src/codegen_call_user.cpp:600-618`). Ry's user-level
panics (divide-by-zero, array OOB, range/contract violations,
integer overflow, etc.) therefore **terminate the entire process
immediately** — they do not propagate as C++ exceptions. Any
feature that plans to "catch a worker panic and report it as a
value" (e.g. `threadJoin(t) -> Err` for a panicking worker,
`blockOn(task) -> Err` for a panicking async body) cannot rely on
existing defensive `try { ... } catch (std::exception &)` blocks
inside runtime functions — those only fire for C++ exceptions
that escape from LLVM-generated code, which never happens for
user panics. The existing catch in `__ry_thread_spawn`
(`src/runtime_thread.cpp`) is dormant defensive code, not an
active error-propagation path. Fixing this requires refactoring
`emitRuntimeError` to `throw std::runtime_error(...)` + installing
a top-level catch in `ry run` / `ry test` — tracked in #880.
Before writing tests that trigger a panic and expect
`Err(error_msg)`, verify that the panic *actually* throws a C++
exception; the only current throw path from runtime code is
`__ry_task_join` double-join (`src/runtime_parallel.cpp:292`) and a
handful of compile-time lexer/loader errors.

### `ListHeader` / `IOListHeader` returned to Ry code must be `arc_alloc`'d, not `checked_malloc`'d

**Source**: #997 (2026-04-16, fix)
**Tags**: runtime, arc, list, memory-safety, allocation

**Rule**: Every list header (`ListHeader*` or `IOListHeader*`) returned from a
C++ runtime function to Ry code must be allocated with `arc_alloc(sizeof(…))`,
not `checked_malloc(sizeof(…))`. The Ry ARC machinery reads/writes
`header_ptr - 16` (the ARC prefix) when retaining or releasing the list.
If the header is plain `checked_malloc`, this access lands in malloc metadata
→ use-after-free / bad-free at scope exit.

Applies to:
- `ListHeader` (string/match lists): use `makeStringList` / `makeMatchList` in
  `include/ry/runtime_list.hpp` — these already use `arc_alloc`.
- `IOListHeader` (byte lists): use `makeByteList` in `include/ry/runtime_io.hpp`;
  also `makeEmptyIOList` in `runtime_net.cpp` and inline allocations in
  `runtime_net.cpp` / `runtime_tls.cpp` / `runtime_io.cpp` — all converted in #997.

**C++ tests that wrap these headers**: Use `arc_free(header)` (from
`include/ry/runtime_arc.hpp`) instead of `free(header)`. The `data` array
inside the header is separately `checked_malloc`'d and must be freed with plain
`free`. Individual string elements created by `dupString` are also plain
`checked_malloc` and freed with plain `free`.

### NUL-safety at C API boundaries: `stringByteLen`, `hasEmbeddedNul`, and propagating nullptr

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: nul-safety, c-api-boundary, stringByteLen, hasEmbeddedNul, Result

**Rule**: PR #1022 introduced `StringHeader` with an explicit `byte_len` field, allowing Ry `str`
to contain embedded NUL bytes. C library APIs (`realpath`, `stat`, `mkdir`, `getaddrinfo`, etc.)
assume NUL-terminated strings and silently truncate at the first `\0`. Apply the following three
patterns consistently when implementing or auditing runtime functions that pass Ry string handles
to POSIX/libc:

**1. Use `stringByteLen(handle)` instead of `strlen(handle)`**:
```cpp
// Wrong (silently truncates at \0):
size_t n = strlen(handle);
// Right:
size_t n = static_cast<size_t>(stringByteLen(handle));
```

**2. Validate with `hasEmbeddedNul` before passing to NUL-sensitive C APIs**:
```cpp
// Defined in include/ry/runtime_string.hpp:
inline bool hasEmbeddedNul(const char *handle) {
    if (!handle) return false;
    size_t n = static_cast<size_t>(stringByteLen(handle));
    return n > 0 && memchr(handle, '\0', n) != nullptr;
}

// Usage in runtime functions:
if (hasEmbeddedNul(p)) {
    setLastError("path.basename: argument contains an embedded NUL byte");
    return nullptr;  // caller wraps as Result<str, Error> via ResultPtr
}
```

**3. Propagate `nullptr` through chained join-style helpers**:
When a helper like `join2_impl(a, b)` returns nullptr on NUL, callers must guard and propagate:
```cpp
extern "C" const char *__ry_path_join3(const char *a, const char *b, const char *c) {
    char *ab = join2_impl(a, b);
    if (!ab) return nullptr;  // without this, join2_impl(nullptr, c) silently returns c
    char *result = join2_impl(ab, c);
    freeStringSlot(ab);
    return result;
}
```
Without the `if (!ab)` guard, `join2_impl(nullptr, c)` treats nullptr as empty string and returns
a copy of `c` — silently succeeding when it should propagate the error.

**How to apply**:
- Binary-safe consumers (HTTP body, multipart file values): use `stringByteLen` for length; do NOT
  reject NUL — these legitimately carry binary payloads.
- Text/path consumers (URLs, host names, header names, file paths): reject NUL with `hasEmbeddedNul`
  and return `Err` / nullptr.
- Upgrade bare `-> str` runtime functions that pass user strings to NUL-sensitive C APIs to
  `-> Result<str, Error>` (return nullptr + `setLastError`; set `ReturnWrapping::ResultPtr` in
  the dispatch table).
- **Audit scope**: PR #1054 covered libc-sensitive modules (path, net, http, http_client,
  filesystem) but not binary-safe stdlib (e.g. base64). When adding a new stdlib module, grep
  its runtime for `strlen(input)` and replace with `stringByteLen(input)` for any function that
  receives a Ry `str` handle. `base64` was fixed as a post-#1054 follow-up in #1129.

### `hasEmbeddedNul` / `stringByteLen` are only safe on Ry string handles — never on raw C strings

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: nul-safety, stringheader, c-api-boundary, runtime

**Rule**: Both `hasEmbeddedNul(handle)` and `stringByteLen(handle)` read a `StringHeader` struct
immediately *before* the pointer to obtain `byte_len`. This is only valid when `handle` points
into a Ry string slot (i.e., the pointer came from `makeString` or was read from a Ry `str` handle).

**It is undefined behaviour** (reads garbage memory) when applied to:
- C string literals: `hasEmbeddedNul("POST")` — no `StringHeader` prefix
- `std::string::c_str()` results — allocated by the STL without a `StringHeader`
- `strdup`/`malloc`-allocated strings (e.g. map keys built by C++ tests) — no `StringHeader` prefix
- Any other raw `const char *` that did not originate from a Ry string slot

**Symptoms**:
- `hasEmbeddedNul` false positive: reads random bytes as `byte_len`, `memchr` finds NUL → returns
  `true` unexpectedly, causing the runtime function to return nullptr and HTTP mock server tests
  to hang (server `::accept` waits for a client that never connects).
- `stringByteLen` garbage value: random bytes interpreted as body/data length → `Content-Length`
  header wildly wrong → HTTP server reads wrong number of bytes → test hangs.

**How to apply**: Only call `hasEmbeddedNul` inside codegen emitters (where arguments are known
Ry handles) or inside runtime functions whose callers pass exclusively Ry handles. If a runtime
fn is also called from C++ unit tests with `.c_str()` or with C literals, move the NUL
check to the codegen emitter.

### Do not call `setLastError` in bool-return runtime functions — thread-local `last_error_buf` pollution

**Source**: #1128 (2026-04-18). **Tags**: nul-safety, setLastError, thread-local, runtime, bool-return

**Rule**: `setLastError` writes to a thread-local buffer (`last_error_buf` in `runtime_error.cpp`). In runtime functions whose Ry return type has **no error channel** (e.g. `bool`-returning `__ry_file_exists`), calling `setLastError` when rejecting NUL would leave stale error text in the buffer. The next Result-returning call on the same thread that fails an unrelated check reads the buffer for `e.message`, and would silently report the stale NUL rejection message instead of its own.

**How to apply**:
- Bool-return-only functions (e.g. `exists`) that detect an invalid argument: return the safe conservative value (`false` / `0`) silently — do **not** call `setLastError`.
- Result-returning functions: call `setLastError("fn: argument contains an embedded NUL byte")` before returning `nullptr` / `1`.
- This matches the split used in `src/runtime_io.cpp` after #1128: `__ry_file_exists` has `if (hasEmbeddedNul(path)) return 0;` with no `setLastError`, while all other path-taking functions call it.

