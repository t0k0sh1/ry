---
name: stdlib-module-add
description: Procedure for adding a new stdlib module (`@native`) — 5 steps plus constant-addition and extending existing modules. Use when adding a stdlib module, declaring `@native`, editing `src/runtime/native/<mod>.cpp`, calling `add_ry_native_lib`, adding constants, or touching `share/std/<mod>/<mod>.ry`. Also fires on Japanese triggers stdlib モジュール追加, 新しい標準ライブラリ, @native 宣言, 定数の追加.
allowed-tools: Read, Grep, Glob, Bash
---

# Stdlib Module Add

Procedure for adding a new standard library module, constants, or functions. Terminology is defined in `docs/reference/glossary.md`.

## Steps

### 1. Ry declaration file

Declare `@native("mod")` in `share/std/<mod>/<mod>.ry`. Use bare `@native` only when all functions are custom-emitted and no `libry_<mod>.*` exists; the argument otherwise drives runtime library loading. `manifest.json` needs no update.

```ry
@native("crypto")
fn sha256(data: str) -> str
```

### 2. C++ runtime implementation

Implement `extern "C"` functions in `src/runtime/native/<mod>.cpp`, named per the `__ry_<mod>_<name>` convention.

```cpp
extern "C" const char *__ry_crypto_sha256(const char *data) { ... }
```

### 3. Build setup

Add `add_ry_native_lib(<mod> src/runtime/native/<mod>.cpp)` to `CMakeLists.txt` and append the library to `RY_NATIVE_LIBS` (linked by `ry` and `ry_tests`).

### 4. Codegen dispatcher (custom logic only)

Simple functions (forward args to the runtime) are handled by `emitGenericNativeCall`; no codegen file needed.

For resource tracking, receiver-type dispatch, `Option` wrapping, etc.:
1. Create `src/codegen_call_<mod>.cpp` with `RY_REGISTER_STDLIB_PACKAGE` (self-registration), a `NativeDispatchEntry` table, and a free-function `custom_emitter`.
2. For opaque resource types, register the kind in static init via `ResourceKindRegistry::instance().registerKind(...)`.
3. Add the source to `ry_lib` in `CMakeLists.txt`.

Shared helpers (in `codegen_call_dispatch.cpp`):

| Helper | Purpose |
|--------|---------|
| `wrapPtrAsResult(ptr, errFn)` | nullable ptr → `Result<T, Error>` |
| `wrapStatusAsResult(status, errFn)` | int status → `Result<Unit, Error>` |
| `emitResultBranch(isErr, resTy, buildOk, buildErr)` | custom Result construction |
| `buildErrorFromRuntime(errFn)` | build an `Error` struct from runtime |

### 5. Tests

- Module import.
- Execution of a representative native function.
- Registry consistency of declaration file / native constants (if applicable).

## Adding Constants

Add an `@const` declaration in `share/std/<mod>/<mod>.ry`. From the dispatch file, call `StdlibRegistry::instance().registerConstant(...)` in static init (registry: `include/ry/stdlib_registry.hpp`; example: `MathConstReg` in `src/codegen_call.cpp`). `codegen_stmt.cpp` needs no changes.

## Adding Functions to Existing Modules

Touch:

1. `share/std/<mod>/<mod>.ry` — `@native("mod") fn` declaration.
2. `src/runtime/native/<mod>.cpp` — C++ implementation.
3. `src/codegen_call_<mod>.cpp` — `custom_emitter` if custom dispatch is required (skip for simple functions).
4. Tests — selective-import and execution cases.

## Declaration Invariants

- A stdlib record type needed even when users do not import its name must be registered programmatically in `CodeGen`; `Match` follows this pattern.
- Name-keyed dispatchers may declare default arguments directly on one `@native fn`.
- Table-driven custom emitters use strict arity matching; declare one signature per supported arity and test each arity.
