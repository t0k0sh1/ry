---
paths:
  - "src/codegen_call*.cpp"
  - "src/codegen_call_dispatch.cpp"
  - "src/codegen_call_native.cpp"
  - "src/codegen_call_string.cpp"
  - "src/codegen_call_collection.cpp"
  - "include/ry/codegen.hpp"
---

# Codegen Stdlib Dispatcher

### One dispatch-table entry per fn name handles multiple overloaded arities

**Source**: #842
**Tags**: codegen, stdlib, dispatch-table, custom-emitter, overload

**Rule**: Stdlib dispatch tables (`math_table`, `string_table`, etc.) are scanned first-match-wins by `emitTableDrivenNativeCall` (`src/codegen_call_native.cpp:21-28`); a second entry for the same name is dead. For overloaded fns (`math.round` 1-arg→int / 2-arg→float; `math.pow` (float,float) vs (int,int)) use a **single entry whose `customEmitter` handles every arity / type combination internally**.

**Why**: The custom-emitter gate (`codegen_call_native.cpp:135-149`) dispatches on the call's actual `e.args.size()` against all registered `@native` sigs, not the table entry's `arity` field. Upstream type matching (L172-189) runs only on the **non**-custom-emitter path, so custom emitters must perform their own argument-type checks and emit `codegenError` for unsupported combinations — otherwise `pow(1, 2.0)` silently reaches the wrong branch.

**How to apply**: When adding an overload, (1) add the `@native fn` declaration in `share/std/<mod>/<mod>.ry`, (2) extend the existing custom emitter, (3) add explicit type checks. See `emitMathFloorCeilRound` / `emitMathLog` / `emitMathPow` in `src/codegen_call.cpp`.

### `contains()` on Map must be intercepted in `emitStrOp_contains`, not via `@native`

**Source**: #1185
**Tags**: codegen, collections, map, dispatch-order, contains

**Rule**: `emitBuiltinString` runs before `emitBuiltinCollection` in `src/codegen_call_dispatch.cpp`, so every `contains(x, y)` reaches `emitStrOp_contains` first. Under opaque pointers all collection pointers share `ptrTy_` with strings, so Set/Map intercept blocks must live inside `emitStrOp_contains` (`src/codegen_call_string.cpp`) before the str fall-through. Do NOT declare `@native contains` in the stdlib module file — that routes through the Pattern-A table and interacts unpredictably with the string handler.

The Map intercept mirrors `has_key` (`src/codegen_call.cpp`):
```cpp
if (llvm::Type *mapKeyTy = getMapKeyType(s)) {
    if (e.args.size() != 2) codegenError("Map.contains() takes exactly 1 argument");
    llvm::Value *key = emitExpr(*e.args[1]);
    if (key->getType() != mapKeyTy) codegenError("contains() key type mismatch");
    llvm::Value *idx = emitMapKeyLookup(s, key, mapKeyTy);
    return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "map_contains");
}
```

`emitMapKeyLookup` is called without threading `keyName` here, matching `has_key`. Structural-key correctness for pointer-typed map keys is a separate concern (see "Every caller of emitSetElementLookup..." entry below).

### Mutating collection operations belong in `codegen_call_higher_order.cpp`, not `codegen_call_string.cpp`

**Source**: #1124
**Tags**: codegen, dispatch, placement, list, string

**Rule**: `sort!`, `reverse!`, and any future in-place list mutation must be registered in `emitBuiltinHigherOrder` (`src/codegen_call_higher_order.cpp`), not the String dispatcher. String runs first in the dispatch chain (`codegen_call_dispatch.cpp`), so a stray entry there unconditionally claims the call for every type — including lists — before HigherOrder gets a chance.

**How to apply**: Mirror `sort!`'s placement in HigherOrder. To error on strings, add `getListElementType() == nullptr` guard in the HigherOrder handler with a clear message (e.g. "requires a list; strings are immutable, use reverse() instead").

### Every caller of `emitSetElementLookup` / `emitMapKeyLookup` with a pointer-typed element must thread the type name

**Source**: #963
**Tags**: codegen, collections, set, map, equality, hash-table

**Rule**: `emitSetElementLookup` and `emitMapKeyLookup` use an empty name as the signal to skip structural equality and fall back to hash-by-ptr-as-C-string (via `__ry_ht_find_str` → `strcmp`). For nested collections (`List<int>` keys, etc.) this collapses distinct values: every length-2 list starts with `\x02\0` so they all hash equal and silently dedupe during literal construction, `add`, `remove`, `in`, `contains`.

Pattern for sets:
```cpp
std::string elemName = getSetElemName(setPtr);  // reads set_elem_type_name
if (!elemName.empty()) propagateTypeMeta(elemName, elem);
emitSetElementLookup(setPtr, elem, elemTy, elemName);
```

Pattern for maps (no dedicated accessor — use `getMeta()`):
```cpp
const ValueMetadata *meta = getMeta(mapPtr);
std::string keyName = meta ? meta->map_key_type_name : std::string{};
if (!keyName.empty()) propagateTypeMeta(keyName, key);
emitMapKeyLookup(mapPtr, key, keyTy, keyName);
```

Also stamp the type-name field at container build time (set/map literal emitter) so downstream callers can retrieve it via `getMeta`. `emitSubsetCheck` (`src/codegen_call_set_ops.cpp`) is the canonical reference. Affected sites historically: `emitSetLiteral` / `emitCollOp_add` / `emitSetRemove` (`codegen_call_collection.cpp`), `in`/`not in` (`codegen_expr.cpp`), `contains()` on Set (`codegen_call_string.cpp`).

### Bare builtins calling native-lib runtime symbols must register the library via `used_native_libraries_`

**Source**: #1261
**Tags**: stdlib, codegen, native-library, builtin, library-loading, @native

**Rule**: When adding a bare builtin (via `builtins_` map or inline branch in `emitBuiltinCore`) whose runtime lives in a separate `libry_<mod>.dylib`, the codegen emitter MUST call `used_native_libraries_.insert("<mod>")` before emitting `CreateCall`. Otherwise the JIT fails at runtime with `Symbols not found: [ ___ry_<name> ]` because bare `@native` declarations leave `sig.library` empty (no automatic `native_lib_index_` population).

**How to apply**:
- Runtime in `src/runtime_<mod>.cpp` → linked into `libry_<mod>.dylib` → **insert `<mod>`**.
- Runtime in `runtime_print.cpp` / `runtime_arc.cpp` → linked into `ry_lib` (resolved by `DynamicLibrarySearchGenerator::GetForCurrentProcess`) → no registration needed.

Regression-test by exercising the builtin from a program that does NOT `import` the target module — that's the user-facing usage shape that exposes missing registration. Compile-only tests pass at IR emission and the failure defers to JIT symbol lookup.

### Builtin native-underscore wrappers must update the builtin dispatcher too

**Source**: #1277
**Tags**: stdlib, builtin, wrapper, default-args, ufcs, codegen

**Rule**: When converting a bare builtin from `@native fn name(...)` to the wrapper pattern (`@native fn _name(...)` + `fn name(... = default)`), update the C++ dispatcher: (1) register the underscored alias (e.g. `"_split"`) in the relevant table (`emitBuiltinString`, `emitBuiltinCore`) — otherwise the wrapper body fails with `undefined function: _name`; (2) mirror new optional/default-arg behavior in the builtin emitter, because UFCS / direct builtin calls may still route through C++ instead of the Ry wrapper.

**How to apply**: Test both call shapes — `name(...)` and UFCS `value.name(...)`. A passing spec for only one path is not enough.

### Byte-list APIs require `TypeMeta::ListElem == i8Ty_`; plain int list literals are rejected at compile time

**Source**: PR #1055
**Tags**: codegen, runtime, IOListHeader, ListHeader, bytesToStr, writeBytes, send

**Rule**: Any native function that casts a `List` argument to `IOListHeader *` must set `NativeDispatchEntry::requireListU8Arg` to the 0-based argument index (or stamp `ListElemMeta::I8` for producers). Codegen enforces the gate in `CodeGen::emitTableDrivenNativeCall`. `IOListHeader` reads `data` with 1-byte stride, but plain `[97, 0, 98]` defaults to `int` (8-byte stride) — passing such a list silently reads 8× too much.

### `emitTableDrivenNativeCall` variadic path must use raw `ptr` type for `ResultPtr` entries

**Source**: PR #1054
**Tags**: codegen, ResultPtr, variadic, emitTableDrivenNativeCall, nul-safety

**Rule**: The fixed-arity path in `emitTableDrivenNativeCall` already handles `ReturnWrapping::ResultPtr` (uses `ptrTy_` for the C ABI return, then `wrapPtrAsResult`). The variadic path (e.g. `path.join` 2/3/4 overloads) calls `resolveType(matchedSig->returnTypeName)` instead — once `.ry` signatures are upgraded to `Result<str, Error>`, this maps to a struct type (not `ptr`) and crashes with ABI mismatch.

```cpp
llvm::Type *cRetTyVariadic = (entry->wrapping == ReturnWrapping::ResultPtr)
    ? ptrTy_
    : resolveType(matchedSig->returnTypeName);
auto *fnTy = llvm::FunctionType::get(cRetTyVariadic, argTypes, false);
// CreateCall, then:
if (entry->wrapping == ReturnWrapping::ResultPtr) {
    return wrapPtrAsResult(callResult, getErrFnName().c_str());
}
return callResult;
```

**How to apply**: When upgrading a multi-arity variadic fn from bare `-> str` to `Result<str, Error>`, update both `NativeDispatchEntry::wrapping` and the variadic code path. Fixed-arity is automatic once `wrapping = ResultPtr` is set.

### Dual-path builtins must share terminator / cleanup handling

**Source**: #821
**Tags**: codegen, builtins, exit, diverging-call

**Rule**: When a builtin or helper has post-emit fixup — dead-block creation, ARC cleanup, metadata propagation, trace exit — put the fixup inside the core helper (`emitExit`, `emitReturn`, …) so every dispatch path inherits it. If a fixup lives in only one caller, the other caller is a latent bug. (Origin: `exit()` had separate expression-path and statement-path dispatchers; only the expression path created the `exit.dead` block, breaking BB invariants for trailing code on the statement path.)

**How to verify**: grep all callers of the core helper; each must either expect the post-state or explicitly re-establish its own.

### Custom-emitter native functions bypass argument type-checking

**Source**: #828
**Tags**: codegen, native-dispatch, type-checking, gotcha

**Rule**: Functions with a `customEmitter` in `NativeDispatchEntry` early-return at `src/codegen_call_native.cpp:135-150` and **skip** regular argument type validation (L165-203). The Ry declaration in `share/std/<mod>/<mod>.ry` constrains only IDE/docs consumers — the custom emitter dictates what is actually accepted. When extending (e.g. broadening `threadSpawn` to non-Unit lambdas), you can relax effective inputs without touching the Ry declaration. Update docs/declaration for hygiene, but don't assume the parser is gating you.

### NUL checks belong in the codegen emitter, not the runtime, for multi-context functions

**Source**: PR #1054
**Tags**: nul-safety, codegen, runtime, c-api-boundary

**Rule**: A runtime function called from Ry codegen *and* from C++/internal C code (e.g. `__ry_http_client_request` invoked from user Ry, C++ unit tests with `.c_str()`, and `__ry_http_post` with literal `"POST"`) must put NUL checks in the codegen emitter only. `hasEmbeddedNul` reads a `StringHeader` before the pointer, which is garbage memory for non-Ry strings (false positives → tests hang). Treat the runtime fn as an internal C API. Add a comment in the runtime explaining the split:

```cpp
// NUL checks for method and url are done in codegen (emitHttpClientCall) for
// the user-facing Ry paths. This function is also called directly from C++
// tests and internally with plain C strings that carry no Ry StringHeader.
```

### String negative-index wrap uses UTF-8 char count, not byte length

**Source**: PR #1199
**Tags**: codegen, substring, emitNegativeIndexWrap, UTF-8, __ry_utf8_len_n

**Rule**: For `emitNegativeIndexWrap(idx, wrapBase, prefix)` on string (char) indices, `wrapBase` MUST be the UTF-8 codepoint count from `__ry_utf8_len_n(s, byteLen)`, **not** `emitStringByteLen(s)`. For `"あいうえお"` (5 chars, 15 bytes), byte-length wrap gives `-1 + 15 = 14` (garbage); char-count gives `-1 + 5 = 4` (correct). The `_n` variant is NUL-safe.

```cpp
llvm::Value *byteLen = emitStringByteLen(s);  // hoist: reused downstream
auto utf8LenFn = getRuntimeFn("__ry_utf8_len_n", i64Ty_, {ptrTy_, i64Ty_});
llvm::Value *charLen = builder_.CreateCall(utf8LenFn, {s, byteLen}, "charlen");
llvm::Value *wrapped = emitNegativeIndexWrap(idx, charLen, "prefix");
// then zero-clamp; very-negative idx still negative after wrap (-100+5=-95)
```

Hoist `emitStringByteLen(s)` so the same byte length feeds `__ry_utf8_len_n` and the downstream call (e.g. `__ry_utf8_substring`). Pattern source: `src/codegen_call_string.cpp:218-245`. Differs from `emitCollOp_slice` because list length already equals storage length — no byte/codepoint distinction.
