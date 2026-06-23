---
paths:
  - "src/codegen_call*.cpp"
---

# Codegen Stdlib Dispatcher

> **Future direction (recorded by #2231).** The rules in this file describe the current `NativeDispatchEntry` + `customEmitter` + hand-written-dispatcher contract. They will be retired as the descriptor-driven dispatch model described in [Native Call Boundary](../../docs/architecture/native-call-boundary.md) lands. Retirement happens in the follow-on implementation issues — until then, follow these rules verbatim.

### One dispatch-table entry per fn name; custom emitters bypass type-checking

Dispatch tables are first-match-wins by name — a second entry for the same name is dead code. Custom emitters skip the upstream argument-type validation, so they must do their own type checks; otherwise `pow(1, 2.0)` silently routes to the wrong branch.

### contains() on Map / Set must be intercepted in emitStrOp_contains, not via @native

`emitBuiltinString` runs before `emitBuiltinCollection` — every `contains(x, y)` reaches `emitStrOp_contains` first. Set/Map intercept blocks must live there before the str fall-through. A `@native contains` overload races unpredictably with the string handler.

### Mutating collection ops belong in higher_order dispatcher, not string

`sort!` / `reverse!` register in `emitBuiltinHigherOrder`, NOT in the String dispatcher. The String dispatcher runs first — a stray entry claims the call for every type (`reverse!` on a list silently routed through `emitStrOp_reverse_mut` and produced "requires a list" when called on a string).

### Pointer-element set / map lookups must thread set_elem_type_name / map_key_type_name

Empty `elemName` + `elemTy == ptrTy_` falls through to hash-by-ptr-as-C-string, using `strcmp` over the header bytes. For any two `List<int>` of length 2 the headers start with `\x02\0` and compare equal — silently deduping distinct elements during set-literal construction, `add`, `remove`, `in`, `contains`. Every caller must thread `set_elem_type_name`/`map_key_type_name`.

### Byte-list APIs require TypeMeta::ListElem == i8Ty_

Any native fn that casts a `List` argument to `IOListHeader *` must set `NativeDispatchEntry::requireListU8Arg`. `IOListHeader` reads `data` with 1-byte stride; plain Ry int-list literals default to 8-byte stride — passing them reads 8× too much data.

### Bare builtins calling native-lib symbols must register via used_native_libraries_

When a bare builtin calls a runtime symbol from a separate `libry_<mod>.dylib`, call `used_native_libraries_.insert("<mod>")` before `CreateCall`. Bare `@native` (no library tag) does not auto-populate `native_lib_index_`, so omitting the insert produces `Symbols not found` at JIT runtime. Regression tests must exercise the builtin from a program that does NOT `import` the target module — compile-only tests miss this because the failure is deferred to JIT symbol lookup.

### Stdlib dispatchers must insert their module library at the top, not rely on emitTableDrivenNativeCall alone

**Tags**: stdlib, dispatch, used_native_libraries, custom-emitter, inline-codegen, jit-symbol-resolution, regression

`emitTableDrivenNativeCall` registers the library only for calls that fall through to the table. Any dispatcher that branches to a `customEmitter` or emits LLVM IR inline skips the table-driven helper entirely — `libry_<mod>.dylib` is never `dlopen`ed and JIT lookup fails. A single `used_native_libraries_.insert` at dispatcher entry covers every branch and is idempotent; per-emit insert is preferred when the dispatcher has a broad sig-presence gate (any io import, including programs that never call an io fn).

### Dispatchers that intercept generic callees must gate on per-package sig registration to avoid cross-package races

**Tags**: stdlib, dispatch, generic-callee, sig-registration, race, json, json5, isXxxImported

`dispatchJson` and `dispatchJson5` both intercepted `load<T>(...)` at the top; since `StdlibRegistry` sort is priority-then-insertion, `dispatchJson` typically wins and routes `from json5 import load; load[Map<...>]("{a:1}")` through the JSON parser, which rejects JSON5 syntax. Any dispatcher intercepting a generic callee must gate on `cg.getNativeFnSigs()` presence before `used_native_libraries_.insert`. Gate on sigs, not `qualified_module` — unqualified calls after `from <mod> import fn` carry no per-call package info, but `native_fn_sigs_` is populated during `@native` declaration processing.

### Descriptor-migrated stdlib dispatcher stubs must `return nullptr` — routing via emitGenericNativeCall causes 3× argument re-emission

**Tags**: stdlib, dispatch, descriptor-migration, emitGenericNativeCall, double-emission, regression, dispatch-loop, fall-through

A stub that routes to `emitGenericNativeCall` when arg types don't match any registered overload causes 3× arg re-emission: `emitGenericNativeCall` emits args, fails resolution, returns nullptr; the StdlibRegistry loop calls it again; `emitUserFnCall` runs a third time. For a side-effecting arg, the user sees it printed three times. Migrated stubs must be a bare `return nullptr;`.

### Migrated `@native` declarations become the source of truth for argument types

**Tags**: stdlib, dispatch, descriptor-migration, requireListU8Arg, list-u8, declaration-drift, @native, behavior-change

Pre-#2285, `base64_table`'s hardcoded `requireListU8Arg` silently rejected mistyped params regardless of the declaration. Post-#2285, descriptor-driven dispatch reads the `@native` declaration as the source of truth — the parameter must actually be declared `List<u8>` for the requireListU8Arg check to fire. For non-migrated modules, the table-driven check fires regardless of declared type.

### `emitPtrToResult` discards runtime `setLastError` messages — prefer `wrapPtrAsResult` + `addResourceKind`

**Tags**: stdlib, dispatch, error-message, wrapPtrAsResult, emitPtrToResult, setLastError, runtime-error-channel, regression

`emitPtrToResult` embeds a compile-time string literal and discards the runtime's detailed error message (`"open: cannot open '<path>' in mode '<mode>'"` becomes `"open failed"`). Default to `wrapPtrAsResult(ptr)` + `addResourceKind(res, rk_X)`. Use `emitPtrToResult` only when verified that the runtime fn does not call `setLastError` on the failure path.

### Dual-path builtins must share post-emit fixup in the core helper

`exit()` had two dispatch paths; only the expression path created the `exit.dead` block after `unreachable`. Trailing code on the statement path attached to a terminated block, breaking LLVM BB invariants. Post-emit fixup belongs inside the core helper so every dispatch path inherits it.

### Unimported stdlib module diagnostic lives at codegen dispatch top, not parser

**Tags**: stdlib, dispatch, parser-vs-codegen, unimported-module, ufcs, scope-info, blind-spot, alias-suggestion

Rejecting `<mod>.fn(...)` without `import <mod>` in the parser breaks the local-shadow case: `path: str = "/tmp/foo.txt"; path.basename()` is legitimate UFCS over a local sharing a stdlib package name. The parser has no scope info to discriminate — the diagnostic must live at the top of `emitExprVariant<CallExpr>`, gated on `!findVar(receiver->name)`.

### `kReservedBuiltinFunctionNames` is empirically maintained — exclude `@native` generic stubs and type-aware-dispatched names

**Tags**: stdlib, dispatch, reserved-names, builtin-shadow, @native, type-aware-dispatch, generic-fn, maintenance

`@native` generic fn stubs reach `registerGenericFnTemplate` before the `@native` branch in `emitStmt(FnStmt)` — the reserved-name check there must skip when `hasDirective` is true, or the stdlib itself fails to load. `str` is a user override extension point for record stringification and must NOT be in the reserved set. Whether any name wins against `findFunction` depends on per-name fall-through behavior that only an empirical check reveals — do not derive the set from the dispatch chain alone.

### corrosion v0.5.0: IMPORTED CMake target name follows `[lib].name`, not `[package].name`

**Tags**: cmake, corrosion, rust, build, ffi, naming, gotcha

`corrosion_import_crate` creates one IMPORTED target per cargo artifact, named from the cargo artifact's base name (for cdylib: `[lib].name` if set, else `[package].name` with `-` → `_`). For a workspace member with `[package].name = "X"` and `[lib].name = "Y"`, the IMPORTED target is `Y` and the cdylib output is `libY.{so,dylib}`. All CMake references (`set_target_properties` / `add_dependencies` / `target_link_libraries` / `RY_NATIVE_LIBS`) must use `Y` — using `X` causes a silent configure-time failure: `set_target_properties Can not find target to add properties to: X`.

**Linux gotcha**: `-l<name>` expansion does not pick up `UTILITY`/`INTERFACE`-flavored IMPORTED targets; `$<TARGET_FILE:Y>` returns "Target is not an executable or library", so `DT_NEEDED libY.so` is not emitted into the binary. Symptom: `JIT session error: Symbols not found: [ __ry_<X>_<fn> ]` at JIT runtime during `runSource`-based codegen tests. On macOS the `dlopen`-driven path works without `DT_NEEDED` — Linux-only non-symmetric failure. Workaround: pass the artifact's absolute path directly to `target_link_libraries`:

```cmake
set(RY_NATIVE_LIBS_LINK
    "${CMAKE_BINARY_DIR}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}Y${CMAKE_SHARED_LIBRARY_SUFFIX}"
    ry_path ry_convert …)
```

Keep `${RY_NATIVE_LIBS}` (CMake target name list) on the `add_dependencies(...)` side for build-order edges. The `emit` crate (`[package].name = "emit"` only, no `[lib]` section) is not a precedent — package == lib == target == output, so the distinction does not surface. Canonical reference: `crates/native_base64/Cargo.toml` + `CMakeLists.txt` (#2282).
