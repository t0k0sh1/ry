---
paths:
  - "src/runtime/**/*.cpp"
  - "include/ry/runtime/**/*.hpp"
---

# Runtime / Memory

This file covers only hazards that are not visible from reading the code.
Coding conventions (forbidden functions, naming, allocator selection, etc.) are enforced by the `lint` job in `.github/workflows/ci.yml` ("Check for banned unsafe allocation functions" / "Check for banned direct exit() calls in runtime").

### Local types in `namespace ry` must not shadow public class names in `include/ry/*.hpp`

**Tags**: odr, comdat, gotcha

Declaring `namespace ry { struct Parser { ... } }` in `src/runtime/native/*.cpp` causes the destructor mangling (`_ZN2ry6ParserD1Ev`) to collide with `ry::Parser` from `include/ry/parser/parser.hpp`. The linker resolves the COMDAT collision by picking one definition and applying it to all TUs. While both destructors are trivial the problem goes undetected; the moment one side gains a non-POD member (#1723) the wrong destructor runs on the other TU's heap memory, causing heap corruption — a latent class that can hide for years while both sides remain trivial. An anonymous namespace hides external linkage but does not prevent the COMDAT collision when both definitions are under `namespace ry`. Rename TU-local types with a module qualifier, such as `JsonParser` or `HttpParser`.

### `arc_alloc` / `checked_malloc` mixing is masked by ASan

**Tags**: arc, asan, gotcha

Allocating a header struct returned to Ry with `checked_malloc` corrupts malloc metadata when the `header_ptr - 16` retain fires. ASan happens to stay silent due to different allocator layout; `pointer being freed was not allocated` fires only on the **default Linux build**. When suspecting an allocator mismatch, always reproduce on the default build — "ASan green" is not proof of absence. The forbidden-function table (`malloc` / `realloc` / `calloc` / `strdup` / `strndup` / `exit` in `src/runtime/`) is enforced by CI lint.

### JIT-visible atomic counters

**Tags**: atomic, abi

C++ does not guarantee layout identity between `std::atomic<int64_t>` and plain `int64_t`. Counters exposed to the JIT as raw `int64_t*` via `atomicrmw` (e.g., `__ry_arc_counter_address()`) must be implemented with `alignas(T) T` and `__atomic_*` builtins — `reinterpret_cast`ing `std::atomic` storage for JIT use is a toolchain-dependent time bomb.

### RWLock dispatch requires thread-local storage — shared map + `mode_mu` creates a three-way deadlock

**Tags**: rwlock, deadlock, gotcha

`__ry_rwlock_unlock`'s `unlock_shared()` vs `unlock()` decision must use only `thread_local std::unordered_map<RWLockHandle*, int>`. The `std::unordered_map<std::thread::id, int>` + separate `mode_mu` approach (audit Option A in #630) creates a three-way deadlock: the `mode_mu` holder blocks inside `lock_shared` → existing reader cannot acquire `mode_mu` to unlock → writer waits forever.

### Nested `std::shared_mutex::lock_shared()` calls are UB

**Tags**: rwlock, ub, gotcha

Under [thread.sharedmutex.requirements], calling `lock_shared()` on a `shared_mutex` already held by the same thread is UB. libc++ passes silently, but strict or instrumented builds will detonate. Nested reads must only increment a TLS counter; call `unlock_shared()` on the underlying mutex only at the 1→0 transition. Regression coverage: `tests/test_runtime_rwlock_stress.cpp::NestedReadLockPerThread`.

### Ry panic terminates immediately via `_Exit(1)` — it is not thrown as a C++ exception

**Tags**: panic, trap-path, _Exit, gotcha

Both `emitRuntimeError` (codegen) and `ry_runtime_trap_exit` (shared helper in `include/ry/runtime/core/alloc.hpp`) emit `fflush(stdout)+fflush(stderr)+_Exit(1)`. The purpose is to bypass the `atexit` chain (LLVM `ManagedStatic` destructors, etc.): using `exit()` causes glibc's `atexit` to run `PassRegistry::~PassRegistry` on JIT-live heap and fire `free(): invalid pointer` SIGABRT (#1838).

As a consequence, a `try { ... } catch (std::exception &)` cannot intercept a worker panic. The catch in `__ry_thread_spawn` (`src/runtime/native/thread.cpp`) is dormant defensive code, not an active error propagation path. To implement `threadJoin(t) -> Err` / `blockOn(task) -> Err`, `emitRuntimeError` / `ry_runtime_trap_exit` would need to be refactored to throw-based, with a top-level catch in `ry run` / `ry test` (#880 closed not-planned). Before writing a test that expects a panic to return `Err(msg)`, verify in code that the panic path actually throws a C++ exception.

### `hasEmbeddedNul` / `stringByteLen` are Ry-handle-only — calling them on raw C strings is UB

**Tags**: nul-safety, stringheader, gotcha

Both functions read a `StringHeader` immediately before the pointer. Passing a non-`makeString` pointer (C string literals like `"POST"`, `std::string::c_str()`, `strdup`/`malloc`-derived keys in C++ tests) interprets random memory as `byte_len`:

- `hasEmbeddedNul` false positive → function returns `nullptr` → HTTP mock-server tests hang forever on `accept`
- `stringByteLen` garbage → `Content-Length` corrupted → server reads wrong byte count and hangs

Call these functions only inside codegen emitters (where arguments are guaranteed to be Ry handles) or runtime functions that exclusively receive Ry handles. For runtime functions that may be called from C++ tests via `.c_str()` / literals, move the NUL check to the codegen emitter side.

Side note: since #1022, Ry `str` can contain embedded NULs (`StringHeader.byte_len`). Reject them with `hasEmbeddedNul` before passing to NUL-sensitive C APIs (`realpath` / `stat` / `getaddrinfo`, etc.); binary-safe consumers (HTTP body, etc.) must not reject them.

### Calling `setLastError` from a bool-returning function pollutes the TLS buffer

**Tags**: setLastError, thread-local, gotcha

`setLastError` writes to a thread-local buffer (`last_error_buf` in `runtime_error.cpp`). Calling it from a function with no error channel (e.g., `bool`-returning `__ry_file_exists`) causes a subsequent Result-returning function to read the buffer and **report a stale, unrelated message as `e.message`**. When a bool-returning function detects an invalid argument, return only `false` / `0` without calling `setLastError`.
