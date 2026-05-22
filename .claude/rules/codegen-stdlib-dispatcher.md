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

### One dispatch-table entry per fn name handles all overloaded arities; custom emitters bypass type-checking

`math_table` / `string_table` and friends are keyed by name with first-match-wins linear scan, so a second entry for the same name is dead code. For overloaded fns (`math.round`, `math.pow`), use a single entry whose `customEmitter` handles every arity / type combination. Custom emitters skip the upstream argument-type validation in `emitTableDrivenNativeCall` (the early return at the customEmitter check), so they MUST do their own type checks and emit clear `codegenError` messages — otherwise `pow(1, 2.0)` silently routes to the wrong branch. This also means the `@native` declaration in `share/std/<mod>/<mod>.ry` documents inputs but does NOT gate codegen — relaxing the effective input without touching the declaration is allowed (update docs for hygiene).

### contains() on Map / Set must be intercepted in emitStrOp_contains, not via @native

`emitBuiltinString` runs before `emitBuiltinCollection`, so every `contains(x, y)` reaches `emitStrOp_contains` first; Set/Map intercept blocks must live there (mirroring `has_key`) before the str fall-through. Do NOT declare `@native contains` overloads in stdlib — they would route through Pattern-A dispatch and clash unpredictably with the string handler. Add intercepts via `getMapKeyType(s)` / `getSetElementType(s)`.

### Mutating collection ops belong in higher_order dispatcher, not string

`sort!` / `reverse!` / future in-place list mutations register in `emitBuiltinHigherOrder` (`codegen_call_higher_order.cpp`), NOT in the String dispatcher. The String dispatcher runs first in the dispatch chain, so a stray entry there unconditionally claims the call for every type — including lists — before HigherOrder is consulted (`reverse!` on a list silently routed through `emitStrOp_reverse_mut` and produced "requires a list" when called on a string). When emitting an error for "wrong type", use the HigherOrder handler with a `getListElementType() == nullptr` guard.

### Pointer-element set / map lookups must thread set_elem_type_name / map_key_type_name

`emitSetElementLookup` decides between linear-scan and `__ry_ht_find_str` based on `elemName`. Empty `elemName` + `elemTy == ptrTy_` falls through to hash-by-ptr-as-C-string, which uses `strcmp` over the list/map/set header bytes — for any two `List<int>` of length 2 the headers start with `\x02\0` and compare equal, silently dedupe-ing distinct elements during set-literal construction, `add`, `remove`, `in`, `contains`. Every caller (`emitSetLiteral`, `emitCollOp_add`, `emitSetRemove`, `in`/`not in`, contains-on-Set) MUST thread `set_elem_type_name`; map analog must thread `map_key_type_name`. Set the type-name field at container construction so callers can retrieve via `getMeta`. `emitSubsetCheck` is the canonical reference.

### Byte-list APIs require TypeMeta::ListElem == i8Ty_; plain int list literals are rejected at compile time

Any native fn that casts a `List` argument to `IOListHeader *` must set `NativeDispatchEntry::requireListU8Arg` (0-based index of that argument) — `IOListHeader` reads `data` with 1-byte stride, but plain Ry list literals like `[97, 0, 98]` default to int with 8-byte stride, so passing them to a byte-list consumer reads 8× too much data. Codegen enforces the gate via `emitTableDrivenNativeCall`. Do NOT add a new byte-list consumer without updating the audit table and adding this field.

### Bare builtins calling native-lib symbols must register via used_native_libraries_; native-underscore wrappers must update the builtin dispatcher

When a bare builtin (in `builtins_` map or `emitBuiltinCore` inline branch) calls a runtime symbol from a separate `libry_<mod>.dylib` (not `ry_lib`), the codegen emitter MUST call `used_native_libraries_.insert("<mod>")` before `CreateCall`. Otherwise the JIT fails at runtime with `Symbols not found: [ ___ry_<name> ]` because bare `@native` (no library tag) does not auto-populate `native_lib_index_`. Symbols in `ry_lib` (the static lib linked into the main binary) resolve through `DynamicLibrarySearchGenerator::GetForCurrentProcess` without registration. Regression test: exercise the builtin from a program that does NOT `import` the target module — compile-only tests miss this because the failure is deferred to JIT symbol lookup. When converting a bare builtin from `@native fn name(...)` to the wrapper pattern (`@native fn _name(...)` + `fn name(... = default)`), also register the underscored alias (e.g. `"_split"`) in the relevant dispatch table — otherwise the wrapper body fails with `undefined function: _name`. Test both call-syntax and UFCS forms.

### Stdlib dispatchers must insert their module library at the top, not rely on emitTableDrivenNativeCall alone

**Source**: #1856 (2026-05-22, fix)
**Tags**: stdlib, dispatch, used_native_libraries, custom-emitter, inline-codegen, jit-symbol-resolution, regression

**Rule**: A stdlib dispatcher (`dispatchIO`, `dispatchHttp`, future module dispatchers wired via `RY_REGISTER_STDLIB_PACKAGE`) MUST call `cg.used_native_libraries_.insert("<mod>")` at the **top of the dispatcher function**, before any custom-emitter or inline-codegen branch that might early-return. Do not rely on the final `emitTableDrivenNativeCall(... "<mod>", <mod>_table, ...)` fallthrough to register the library, because every early-return path bypasses it.

**Why**: `emitTableDrivenNativeCall` reads `sig.library` from the matched table entry and registers it, so functions that go through the table are covered. But any dispatcher that branches to a `customEmitter` or emits LLVM IR inline (e.g. `dispatchIO`'s `open` → `emitFileOpen`, `readAll(File)` → `emitFileReadAll`, `lines(File)` → `emitFileLines`, `writeText(File,_)` → `emitFileWriteText`, `writeText(str,str)` inline branch directly calling `__ry_write_text`) skips the table-driven helper entirely. With nothing registering the library, `libry_<mod>.dylib` is never `dlopen`ed and JIT lookup fails with `Symbols not found: [ ___ry_<name> ]`. The failure only surfaces when the user imports a non-table function in isolation; if any other import in the program reaches the table-driven path or another emitter that explicitly inserts the library, the bug is masked (e.g. `close(f)` in `codegen_call.cpp` explicitly inserted `"io"`, so any program importing `close` accidentally worked). Per-emitter inserts are fragile — each new custom emitter must remember to add the line. A single insert at the top of the dispatcher covers every branch and is idempotent (`std::set::insert`), so the `emitTableDrivenNativeCall` fallthrough's own insert is a harmless no-op.

**How to apply**:
- When writing a new dispatcher, the first statement after parameter unpacking should be `cg.used_native_libraries_.insert("<mod>")` where `<mod>` matches the `RY_REGISTER_STDLIB_PACKAGE(<mod>, ...)` declaration.
- Skip this rule only for modules that genuinely have no `libry_<mod>.dylib` (purely inline codegen, like `math`). For those modules, the bare-`@native` form is correct per `stdlib-module-additions.md` and inserting the library would cause a "native library not found" runtime error.
- Add at least one regression test per inline / custom-emitter entry point using **partial single-symbol imports**: `from <mod> import <one-fn>` with no other imports from the same module. One file per partial import (combining them in one file lets the first dispatch register the library and masks the bug for the rest). Mirror `tests/spec/io_partial_import_open.test.ry` and `tests/spec/io_partial_import_writetext.test.ry`.

### Dual-path builtins must share post-emit fixup in the core helper

`exit()` had two dispatch paths (expression via `emitBuiltinCore` and statement via `builtins_["exit"]`); only the expression path created the `exit.dead` block after `unreachable`. Trailing code on the statement path attached to a terminated block and broke LLVM BB invariants. Put post-emit fixup (dead-block creation, ARC cleanup, metadata propagation, trace exit) inside the core helper (`emitExit`, `emitReturn`, ...) so every dispatch path inherits it. If a fixup lives in only one caller, the other caller is a latent bug.

### Named arguments: parser-generic, codegen-only dispatch restriction

`print(end="", sep=", ")` style support added `std::vector<NamedArg>` to `CallExpr`/`CallStmt`, taught `parseArgList()` to detect `ident = expr` (lookahead unambiguous because `=` is never inside an expression — equality is `==`), and restricted usage to builtins at codegen time (non-builtin calls with named args emit a codegen error). To add the next named-arg builtin, only the emitter (e.g. `emitPrint`) and the `builtins_` map need updating. Lifting the builtin-only restriction later means dropping the codegen check and adding type-checker support.

### NUL checks belong in the codegen emitter, not the runtime, for multi-context functions

Runtime fns like `__ry_http_client_request` are called from three contexts: user Ry code via codegen (Ry handles with StringHeader), C++ tests with `c_str()`, internal runtime with C string literals. Applying `hasEmbeddedNul` inside the runtime would read garbage memory before the pointer for the latter two cases (false positives). Put NUL checks in the codegen emitter only; treat the runtime function as an internal C API and document the split with a comment.

### String negative-index wrap uses UTF-8 char count, not byte length

When applying `emitNegativeIndexWrap(idx, wrapBase, prefix)` to string indices, `wrapBase` MUST be the UTF-8 codepoint count via `__ry_utf8_len_n(s, byteLen)`, not `emitStringByteLen(s)`. For 5-char / 15-byte `"あいうえお"`, byte length gives `-1 + 15 = 14` (out-of-range); char count gives `-1 + 5 = 4` (correct last-char index). The `_n` variant is NUL-safe. Always hoist `emitStringByteLen` into a local so it feeds both `__ry_utf8_len_n` and the downstream string-indexed runtime call (`__ry_utf8_substring` etc.). Zero-clamp after wrap is still required (`-100 + 5 = -95`). `emitCollOp_slice` does NOT need this — list length is element count, no byte/codepoint distinction.

### Unimported stdlib module diagnostic lives at codegen dispatch top, not parser

**Source**: #1746 (2026-05-15, implementation); #1747 (2026-05-15, alias-aware variant)
**Tags**: stdlib, dispatch, parser-vs-codegen, unimported-module, ufcs, scope-info, blind-spot, alias-suggestion

**Context**: The naïve fix for `<mod>.fn(...)` without `import <mod>` (e.g. `math.sqrt(4.0)` without `import math`) is to reject it in the parser at the UFCS lowering site (`parseQualifiedCall` / `parsePostfixContinuation`): the parser already maintains `imported_modules_` and `StdlibRegistry::instance().packages()` is reachable from the parser TU. This was the original Plan approach for #1746, and it works for the trivial case.

It silently breaks for the **local-shadow case**: `path: str = "/tmp/foo.txt"; path.basename()` is legitimate UFCS over a local that happens to share a stdlib package name. The parser has no scope info to discriminate the local from a real unimported package use — its earliest scope-aware pass is type checking inside codegen (`findVar`). A parser-side check therefore false-positives every locally-named-after-stdlib variable.

**Rule**: The "module not imported" diagnostic for `<mod>.fn(...)` and `<mod>.field` lives at the **top of `emitExprVariant<CallExpr>` and `emitExprVariant<FieldAccessExpr>`** (`src/codegen_call_dispatch.cpp`, `src/codegen_expr_literal.cpp`), gated on:
- `!e->qualified_module.has_value()` (skip when the user wrote a properly imported `<mod>.xxx` — that path has its own qualified-module diagnostic),
- the receiver is a bare `VariableExpr`,
- `isStdlibPackageName(receiver->name)` (matches against `StdlibRegistry::instance().packages()`),
- `!findVar(receiver->name)` (skip when the name is a local — local shadowing is intentional and the existing "undefined function: <method>" path is correct for it).

The check fires before any dispatch (table-driven stdlib, generic native, user-fn fallthrough), so it overrides every misleading diagnostic those paths would otherwise emit (`"sqrt() takes exactly 1 argument"` from `requireArgs` in the math table-driven path; `"undefined function: sqrt (hint: forward references...)"` from `resolveOverload`'s user-fn fallthrough).

**How to apply**:
- Do not move this check to the parser. The local-shadow case requires `findVar` and the parser cannot discriminate.
- Use `StdlibRegistry::instance().packages()` (statically populated via `RY_REGISTER_STDLIB_PACKAGE` initializers), not `native_fn_sigs_` (lazily populated by `import` statements — empty for the very case we want to diagnose).
- Cache the package-name set in a function-local `static std::unordered_set` inside `isStdlibPackageName` (registry is immutable after startup).
- The bare unqualified-call case (`sqrt(4.0)` without `from math import sqrt`) is intentionally out of scope: distinguishing "user forgot the import" from "user defined `fn sqrt` later in the file" requires context the codegen dispatcher does not have at the call site, and the existing forward-reference hint is genuinely useful for the second case.
- **#1747 alias-aware variant**: before constructing the "module not imported" message, call `findAliasForCanonical(ve->name)` (linear scan over `effective_to_canonical_`). If an alias exists, emit `"'<canonical>' is not defined. Did you mean '<alias>' (aliased from '<canonical>')?"` instead. The alias check is required because `import math as m` hides the canonical name per the Python-style contract documented in `docs/reference/modules.md`, so the generic `add 'import math'` hint is misleading — the user has already imported the module under a different name. The reverse-lookup helper lives next to `findModuleNamespace` in `include/ry/codegen.hpp` and `src/codegen.cpp`; both guard sites (`codegen_call_dispatch.cpp` CallExpr and `codegen_expr_literal.cpp` FieldAccessExpr) must apply the alias check uniformly so suggestion behavior matches whether the user wrote `math.fn(...)` or `math.field`.
