---
paths:
  - "src/codegen_call*.cpp"
---

# Codegen Stdlib Dispatcher

> **Future direction (recorded by #2231).** The rules in this file describe the current `NativeDispatchEntry` + `customEmitter` + hand-written-dispatcher contract. They will be retired as the descriptor-driven dispatch model described in [Native Call Boundary](../../docs/architecture/native-call-boundary.md) lands. Retirement happens in the follow-on implementation issues — until then, follow these rules verbatim.

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

**Rule**: A stdlib dispatcher (`dispatchIO`, `dispatchHttp`, future module dispatchers wired via `RY_REGISTER_STDLIB_PACKAGE`) MUST register `cg.used_native_libraries_.insert("<mod>")` before any custom-emitter or inline-codegen branch returns a non-null value. Do not rely on the final `emitTableDrivenNativeCall(... "<mod>", <mod>_table, ...)` fallthrough to register the library, because every early-return path bypasses it. Two equivalent patterns:

1. **Top-insert (simpler)**: place `cg.used_native_libraries_.insert("<mod>")` at the dispatcher entry (after the sig-presence gate, if any). Inserts on every dispatcher entry, including non-matching callees that ultimately return nullptr — wastes a dlopen on `from <mod> import X; <only-non-mod-calls>` programs.
2. **Per-emit insert (more precise)**: wrap each return point with a helper that inserts only on non-null returns. Insert fires only on actual dispatch success, so non-matching callees don't pollute `required_libraries`. See `dispatchIO`'s `markIo` lambda (PR #2332, addressing CodeRabbit review on #2299):

   ```cpp
   auto markIo = [&](llvm::Value *v) -> llvm::Value * {
       if (v) cg.used_native_libraries_.insert("io");
       return v;
   };
   // ... return markIo(emitFileOpen(cg, e));
   ```

   Per-emit is preferred when the dispatcher has a broad sig-presence gate (e.g. `isIoImported` passes for any io import, including programs that never call an io fn).

**Why**: `emitTableDrivenNativeCall` reads `sig.library` from the matched table entry and registers it, so functions that go through the table are covered. But any dispatcher that branches to a `customEmitter` or emits LLVM IR inline (e.g. `dispatchIO`'s `open` → `emitFileOpen`, `readAll(File)` → `emitFileReadAll`, `lines(File)` → `emitFileLines`, `writeText(File,_)` → `emitFileWriteText`, `writeText(str,str)` inline branch directly calling `__ry_write_text`) skips the table-driven helper entirely. With nothing registering the library, `libry_<mod>.dylib` is never `dlopen`ed and JIT lookup fails with `Symbols not found: [ ___ry_<name> ]`. The failure only surfaces when the user imports a non-table function in isolation; if any other import in the program reaches the table-driven path or another emitter that explicitly inserts the library, the bug is masked (e.g. `close(f)` in `codegen_call.cpp` explicitly inserted `"io"`, so any program importing `close` accidentally worked). Per-emitter inserts are fragile — each new custom emitter must remember to add the line. A single insert at the top of the dispatcher covers every branch and is idempotent (`std::set::insert`), so the `emitTableDrivenNativeCall` fallthrough's own insert is a harmless no-op.

**How to apply**:
- When writing a new dispatcher, the first statement after parameter unpacking should be `cg.used_native_libraries_.insert("<mod>")` where `<mod>` matches the `RY_REGISTER_STDLIB_PACKAGE(<mod>, ...)` declaration.
- Skip this rule only for modules that genuinely have no `libry_<mod>.dylib` (purely inline codegen, like `math`). For those modules, the bare-`@native` form is correct per `/stdlib-module-add` and inserting the library would cause a "native library not found" runtime error.
- Add at least one regression test per inline / custom-emitter entry point using **partial single-symbol imports**: `from <mod> import <one-fn>` with no other imports from the same module. One file per partial import (combining them in one file lets the first dispatch register the library and masks the bug for the rest). Mirror `tests/spec/io_partial_import_open.test.ry` and `tests/spec/io_partial_import_writetext.test.ry`.

### Dispatchers that intercept generic callees must gate on per-package sig registration to avoid cross-package races

**Source**: #1855 (2026-06-21, fix — json5 addition surfaced latent json race)
**Tags**: stdlib, dispatch, generic-callee, sig-registration, race, json, json5, isXxxImported

**Context**: `dispatchJson` historically intercepted every `load<T>(...)` call at the top by inspecting `e.callee` (`load<...>` prefix match) and routing to `emitJsonLoad`. With a single dispatcher this worked — `json::load` is the only candidate. After #1855 added `dispatchJson5` with the same `load<T>` interceptor (both declare `@native fn load<T>(...)` in their `.ry` files), both dispatchers raced for every `load<T>` call. The `StdlibRegistry` lazy sort (`src/stdlib_registry.cpp:30`) is stable, priority-then-insertion, so equal-priority dispatchers iterate in registration order — `dispatchJson` typically wins and routes `from json5 import load; load[Map<...>]("{a:1}")` through `__ry_json_parse_to_any`, which rejects JSON5 syntax (`expected string key at line 1, column 2`).

**Rule**: Any dispatcher whose top-level intercept matches a **generic callee** (e.g. `load<T>`, `cast<T>`, `into<T>`) — i.e. any name that another package can plausibly export under the same identifier — MUST gate the intercept on whether the package's sig actually registered. Mirror `emitTableDrivenNativeCall:17`'s check pattern: read `cg.getNativeFnSigs()` and return `nullptr` early when none of the package's `"<pkg>::<name>"` keys are present. Apply the gate BEFORE `cg.used_native_libraries_.insert("<mod>")` so unused-package dispatchers don't pollute `used_native_libraries_` either.

```cpp
static bool isJson5Imported(CodeGen &cg) {
    const auto &sigs = cg.getNativeFnSigs();
    return sigs.count("json5::load") || sigs.count("json5::stringify")
        || sigs.count("json5::stringifySafe") || sigs.count("json5::dump");
}

static llvm::Value *dispatchJson5(CodeGen &cg, const CallExpr &e) {
    if (!isJson5Imported(cg))
        return nullptr;
    cg.used_native_libraries_.insert("json5");
    // ... load<T> intercept, then table dispatch ...
}
```

**Why gate on sigs vs `qualified_module`**: `qualified_module` is only populated for `<mod>.fn(...)` calls. Unqualified calls after `from <mod> import fn` carry no per-call package info, so the dispatcher cannot tell which `<mod>` to claim from the call alone. `native_fn_sigs_` is populated as a side effect of `@native` declaration processing (`src/codegen_fn.cpp:676`), so a module imported via `from <mod> import fn` reliably populates `"<mod>::fn"` keys before any call site is codegen'd. Only the dispatchers whose package was actually imported claim the symbol.

**How to apply**:
- For any dispatcher intercepting a name that could plausibly belong to another package (`load<T>`, `parse<T>`, `from<T>`, `into<T>`, etc.), add the per-package sig-presence gate at the top.
- The gate's helper should check every public function name your dispatcher could possibly claim — listing all of them is fine because they share the same "is this package imported?" semantics. Concretely: list the bare names from `share/std/<mod>/<mod>.ry`'s `@public @native` declarations.
  - For dispatchers whose `share/std/<mod>/<mod>.ry` surface is large (e.g. `dispatchIO` covers 14 unique names with overloads), use a hybrid form: prefix scan over `cg.getNativeFnSigs()` for `"<mod>::"` keys (covers canonical `from <mod> import` / `@native("<mod>")` user code), PLUS a bare-name presence check for the same names (covers the C++ test-harness pattern — `runSource()` skips ModuleLoader, so test sources declare `@native fn writeText(...)` without a library tag and register under bare keys like `"writeText"`). See `isIoImported` (#2299) for the canonical hybrid.
  - **Drift behavior is loud, not silent**: real user code always exercises the prefix-scan branch, so a missing bare name in the helper can only fail a C++ test loudly (`no matching overload for @native fn '<name>'`), never silently break a runtime path. This makes the bare-name list safer to maintain than the json/json5 enumeration where every miss is silent — but still maintain it alongside `share/std/<mod>/<mod>.ry`.
- For dispatchers that only emit type-keyed exact-match logic via `emitTableDrivenNativeCall` (no top-level generic intercept), the gate is optional for correctness — `emitTableDrivenNativeCall:17` already does the sig check before claiming. However, for dispatchers with unconditional top-inserts (i.e. those obliged to insert by rule #1856 to cover custom-emitter early-returns), a sig-presence gate is also recommended for `required_libraries_` hygiene — without it, every base64-/json-/etc.-only program pulls the dispatcher's library into the JIT load set even though no symbol from it is called. See `dispatchIO` (#2299) for the canonical refactor.

### Descriptor-migrated stdlib dispatcher stubs must `return nullptr` without invoking emitGenericNativeCall — routing causes 3× argument re-emission

**Source**: #2285 (2026-06-21, refactor — base64 descriptor migration; advisor catch)
**Tags**: stdlib, dispatch, descriptor-migration, emitGenericNativeCall, double-emission, regression, dispatch-loop, fall-through

**Rule**: When a stdlib dispatcher is reduced to a stub (because the module's table has been removed in favor of descriptor-driven dispatch — the post-#2231 migration), the stub MUST be a bare `return nullptr;`. Do NOT route to `emitGenericNativeCall` from inside the stub:

```cpp
static llvm::Value *dispatchBase64(CodeGen &, const CallExpr &) {
    return nullptr;  // ← correct: bare stub
}
```

The tempting "ownership" pattern is wrong:

```cpp
// ← WRONG: causes 3× side-effect evaluation on fall-through
static llvm::Value *dispatchBase64(CodeGen &cg, const CallExpr &e) {
    if (cg.getNativeFnSigs().count("base64::" + std::string(e.callee)))
        return cg.emitGenericNativeCall(e);
    return nullptr;
}
```

**Why**: `emitGenericNativeCall` emits args (`emitExpr` into a `args` vector) BEFORE running overload resolution. When the call's actual arg types do not match any registered `@native("<pkg>")` overload, it returns nullptr — but the args have already been emitted into the builder's current block. The StdlibRegistry dispatch loop in `codegen_call_dispatch.cpp:197` then continues; line 294 calls `emitGenericNativeCall` AGAIN (re-emitting the same args), and finally `emitUserFnCall` runs (emitting the args a third time before invoking the user overload). For an arg with side effects (e.g. `print(process(mk()))` where `mk()` prints), the user observes `mk` printed three times.

The cost of bare `return nullptr;` is that sibling dispatchers that follow the "insert at the top" rule pollute `used_native_libraries_` with libraries that will be dlopen'd at JIT time but never called. Pre-#2299, a base64-only program loaded `{base64, io}` — `dispatchIO` was the only dispatcher with an unconditional top-insert; `dispatchJson` / `dispatchJson5` already had `isJson*Imported` sig-presence gates that suppress the insert when the package is not imported; `dispatchNet` / `dispatchHttp` / `dispatchPath` / `dispatchThread` / `dispatchRuntimeInternal` insert only via `emitTableDrivenNativeCall` and so do not fire on a non-matching call. #2299 added `isIoImported` (hybrid sig-presence gate: prefix scan over `cg.getNativeFnSigs()` for `"io::"` keys, PLUS bare-name presence check for io's 14 fn names to cover the C++ test-harness `@native fn <name>` pattern — see #1855 rule above) and moved `dispatchIO`'s insert behind it, so the empirically measured set is now `{base64}` — matching the original `libs.size() == 1` invariant.

`dispatchIO` is still **not** descriptor-driven — it retains 27 custom emitters bypassing `emitTableDrivenNativeCall`, so the #1856 "insert before custom-emitter early-return" requirement still holds (placed after the gate now). Full close of #2299 requires descriptor migration of `dispatchIO` (and any future unconditional-top-insert dispatcher) per `docs/architecture/native-call-boundary.md`; until then, #2299 stays open even though criterion 2 is met.

**How to apply**:
- For every module migrated to descriptor-driven dispatch (base64 + path today; io / json / net / http / thread in subsequent PRs per `docs/architecture/native-call-boundary.md` follow-up issues), the stub dispatcher must be a bare `return nullptr;` with a comment explaining the trade-off. #2337 added path to the migrated set and also lifted `return_wrapping` / `error_channel` / `require_list_u8_arg` from dispatch-time inference into descriptor population at `@native` declaration time, with `out_param_type_name` carried alongside `return_wrapping` for `ResultOutParam`-shaped Pattern C consumers (filesystem `Result<int>` / `Result<bool>`).
- The `GetRequiredLibrariesOnlyIncludesCalledFunctions` regression test (`tests/test_codegen_directive.cpp`) asserts the exact expected set via `EXPECT_EQ(libs, expected_libs)` where `expected_libs = {"base64"}` (#2299 close criterion 2, met by the `isIoImported` gate). A new dispatcher silently joining the set, or an existing dispatcher dropping its sig-presence gate, will fail the assertion and require an explicit `expected_libs` update.
- Any future "fix" that re-introduces routing via `emitGenericNativeCall` from inside a stub MUST be paired with a test using a side-effecting arg (e.g. `fn mk() -> int: print("mk"); 42` then `print(process(mk()))` with a base64-declared `process(str)` and user overload `process(int)`) asserting `mk` printed once, not three times.

### Migrated `@native` declarations become the source of truth for argument types — declaration drift no longer auto-corrected by the table

**Source**: #2285 (2026-06-21, refactor — base64 descriptor migration)
**Tags**: stdlib, dispatch, descriptor-migration, requireListU8Arg, list-u8, declaration-drift, @native, behavior-change

**Rule**: Pre-#2285, `@native("base64") fn encodeBytes(input: List<int>) -> str` was silently rejected at the call site by `base64_table`'s hardcoded `requireListU8Arg = 0`, regardless of the declared parameter type. Post-#2285, descriptor-driven dispatch reads the `@native` declaration as the source of truth: the parameter type must be `List<u8>` for the generic path's requireListU8Arg check to fire. A user-declared `List<int>` param now means "I want int-list dispatch" — codegen will obediently mis-call the runtime symbol with int-stride data.

**Why**: The migration goal (`docs/architecture/native-call-boundary.md`) is to elevate the `.ry` declaration to the canonical descriptor. Hand-written table overrides that contradict the declaration (the `.claude/rules/docs-reference-conventions.md` "@native declarations can silently drift from their dispatcher implementation" pattern) cannot survive the migration — the override only ever lived in `base64_table`'s C++ constants and there is no longer a table to consult.

**How to apply**:
- For migrated modules (currently base64), `@native` declarations in user code or stdlib `.ry` files MUST declare the actual runtime ABI shape. The generic path's `requireListU8Arg` check (`src/codegen_call_native.cpp:601`) fires only when `matchedSig->params[i].typeName == "List<u8>"`.
- When writing rejection tests for misdeclared natives, set up the declaration with the *correct* shape (`List<u8>`) and pass a *wrong* argument (`[1, 2, 3]` int literal). Pre-migration tests that declared the wrong shape (`List<int>`) and relied on the table to catch it are no longer valid — update them per `tests/test_codegen_fail.cpp::Base64EncodeBytesRejectsNonU8List` (#2285).
- For modules NOT yet migrated, the table-driven `requireListU8Arg` continues to fire regardless of declared param type. Document this asymmetry in any tests or migration audits.

### `emitPtrToResult(ptr, name, "static msg", rk_X)` discards runtime `setLastError` messages — prefer `wrapPtrAsResult(ptr) + addResourceKind`

**Source**: #1847 (2026-05-22, fix)
**Tags**: stdlib, dispatch, error-message, wrapPtrAsResult, emitPtrToResult, setLastError, runtime-error-channel, regression

**Rule**: When a `customEmitter` constructs a `Result<T, Error>` from a runtime fn that calls `setLastError` on failure (the canonical channel: every io / fs / net runtime symbol does this), use `wrapPtrAsResult(ptr)` (default `errFnName = "__ry_get_last_error"`) + an explicit `addResourceKind(res, rk_X)` call — NOT `emitPtrToResult(ptr, "<name>", "<static msg>", rk_X)`. The latter embeds a compile-time string literal (`buildStaticError`) and silently throws away every detailed message the runtime worked to construct (`"open: cannot open '<path>' in mode '<mode>'"` becomes `"open failed"`, etc.).

**Why**: Both helpers live in `src/codegen_call_dispatch.cpp`. `wrapPtrAsResult` (L437-454) emits a NULL check and calls the runtime error-getter (default `__ry_get_last_error`, overridable for net which uses `__ry_net_get_last_error`); `emitPtrToResult` is a convenience wrapper that gives up the runtime channel and substitutes a literal — useful only when the runtime fn does NOT call `setLastError` on the failure path (rare; mostly legacy). The two functions look similar in call shape, so emitter authors who copy from sibling emitters can accidentally pick the lossy variant. `emitFileOpen` (#1847) used `emitPtrToResult(... "open failed" ...)` while every other `io` emitter (`emitFileReadAll`, `emitFileReadLine`, `emitFileLines`, `emitFileWriteText`) used `wrapPtrAsResult`, so users saw a generic "open failed" instead of `"open: invalid mode 'z'"` / `"open: path contains an embedded NUL byte"` etc.

**How to apply**:
- New custom emitters: default to `wrapPtrAsResult(ptr)` (or `wrapPtrAsResult(ptr, "__ry_<mod>_get_last_error")` for modules with a private error channel like net) + `addResourceKind(res, rk_<kind>)`. Only use `emitPtrToResult` when you have verified that the runtime fn does not call `setLastError` on the failure path being wrapped, AND the static string genuinely captures all the diagnostic value.
- When auditing existing emitters, grep for the lossy pattern: `grep -nE 'emitPtrToResult\([^,]+, *"[^"]+", *"[^"]+"' src/codegen_call_*.cpp`. Each hit is a candidate for replacement. Cross-check against the corresponding runtime fn — if it calls `setLastError`, the codegen side is throwing away the message.
- Test coverage: assert on the message substring via `.toContain("...")` for each rejection branch, not just `.toBeErr()`. A `toBeErr` test passes equally for `"open failed"` and `"open: cannot open ..."`, so it does not lock in the detailed-message contract. Mirror the assertions in `tests/spec/io_file_handle.test.ry` (L23-58).

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

### `kReservedBuiltinFunctionNames` is empirically maintained — exclude `@native` generic stubs and type-aware-dispatched names

**Source**: #1889 (2026-05-27, fix)
**Tags**: stdlib, dispatch, reserved-names, builtin-shadow, @native, type-aware-dispatch, generic-fn, maintenance

**Context**: #1889 added defense-in-depth guards at codegen entry that reject top-level user `fn` declarations whose name collides with a stdlib built-in (`sum`, `min`, `max`, `len`, `range`, `print`, …). Without the guards the user body was silently ignored — the hardcoded `e.callee == "..."` dispatch chain and `@native` stdlib entries always win over `findFunction`. The reserved list lives in `include/ry/builtin_names.hpp` as `kReservedBuiltinFunctionNames` and is consulted by `rejectIfReservedBuiltin(name)` at three sites: `emitStmt(FnStmt)` (non-generic top-level user fns), `registerGenericFnTemplate` (top-level generic fn templates), and `emitStmt(ImportAliasStmt)` `Kind::Fn` (import alias rename).

**Rule**: The reserved list is determined **empirically**, not by enumerating every `emitBuiltin*` hardcoded `e.callee == "..."` checker or every `@native fn` declaration in `share/std/*.ry`. Most candidates from those two sources are *fall-through* — when the user defines `fn add(x: int, y: int)` and calls `add(1, 2)`, dispatch falls through past the stdlib `add` to the user fn because `add` is type-overloaded by metadata. Including such names in the reserved set produces over-rejection of legitimate user code.

**Exclusions** (entries that look like candidates but must NOT go in `kReservedBuiltinFunctionNames`):
1. **`@native` generic fn stubs** — declarations like `share/std/json/json.ry`'s `@native("json") fn load<T>(text: str) -> Result<T, Error>` reach `registerGenericFnTemplate` BEFORE the `@native` branch in `emitStmt(FnStmt)` (the generic branch fires first at `src/codegen_fn.cpp:612`). Guard 2 (`registerGenericFnTemplate`) must skip reserved-name rejection when `hasDirective(tmpl.fnStmt->directives, "native")` is true. Without the skip, the stdlib itself fails to load. The complementary check is unnecessary for Guard 1 (non-generic `@native`) because the `@native` branch at `src/codegen_fn.cpp:624+` returns before reaching `declareFunction`.
2. **Type-aware-dispatched names** — `str` for records is a documented user override extension point (`docs/reference/builtins-string.md` `## str` section; `tests/spec/records.test.ry` "should override auto-generated str with user-defined"). For record types, the codegen dispatcher consults user `fn str(p: TheRecordType)` FIRST before falling back to the auto-generated builtin. Including `str` in the reserved set breaks every legitimate record stringification override. By contrast `int` / `float` are name-keyed (`emitBuiltinConversion` rejects a non-str argument outright, so a user `fn int` never wins) and therefore ARE reserved — the conversion trio is deliberately split, not uniform. Spot-check similar extension points (`unwrap`, `iter`) whose dispatch is metadata-driven rather than name-keyed before adding them to the reserved list.

**How to verify a candidate name belongs in the reserved set**:

```bash
# Build a single-int-arg user fn with the candidate name and check whether
# the user body or the stdlib wins:
n=<candidate>
printf 'fn %s(x:int)->int:\n    return 999\n\nprint(%s(1))' "$n" "$n" | ./build/ry -c
```

If the output is `999`, the user fn wins (fall-through) — the name does NOT belong in the reserved set. If the output is anything else (or an error), the stdlib silently overrides — the name belongs in the reserved set, subject to the two exclusion rules above. Repeat for `List<int>` and `str` signatures to catch type-dispatched names whose single-int test falls through but list/str test doesn't (`iter`, `pop`).

**When to update `kReservedBuiltinFunctionNames`**:
- Any new `emitBuiltin*` sub-handler added to the hardcoded `e.callee == "..."` dispatch chain.
- Any new `@native fn` declaration in `share/std/*.ry` (excluding internal `_xxx` aliases per `/stdlib-module-add`).
- After the change, run the empirical check above for every newly-touched name and add the ones where the stdlib wins. Cross-check the existing list with `grep -rEn "^fn <name>\b" tests/spec/ examples/` to confirm no legitimate top-level user fn would be over-rejected.

**Why this rule cannot be derived from the dispatch chain alone**: the chain's order (qualified_module → ADT → verifyCalledWith → `emitBuiltin*` chain → StdlibRegistry → struct constructor → `findFunction`) is a static property, but whether any specific name "wins" against `findFunction` depends on whether each `emitBuiltin*` checker rejects unknown signatures and falls through, or unconditionally claims the call. That predicate is per-name and only the empirical check reveals it.
