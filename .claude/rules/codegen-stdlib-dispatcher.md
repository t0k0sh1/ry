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

**Source**: #842 (2026-04-11, math overloads implementation)
**Tags**: codegen, stdlib, dispatch-table, custom-emitter, overload

**Context**: The stdlib dispatch tables (`math_table`, `string_table`,
etc.) in `src/codegen_call*.cpp` are keyed by function name, and
`emitTableDrivenNativeCall` (`src/codegen_call_native.cpp:21-28`) does a
first-match-wins linear scan — so adding a second entry for the same
name is a dead entry. For overloaded functions like `math.round` (1-arg
→ int, 2-arg → float) or `math.pow` (`(float, float)` vs `(int, int)`),
you must use a **single entry whose `customEmitter` handles every
arity / type combination internally**.

The custom-emitter gate at `codegen_call_native.cpp:135-149` dispatches
based on the CALL's actual argument count against all registered
`@native` sigs — not against the table entry's `arity` field. That
means: once the new overload is declared as `@native` in
`share/std/<pkg>/<pkg>.ry`, the custom emitter just checks
`e.args.size()` (and argument LLVM types) and routes accordingly. The
table-entry `arity` is effectively metadata / legacy hint for custom
emitters; only the pure-table path (nullptr `customEmitter`) reads it.

Upstream type matching (the type-match loop at
`codegen_call_native.cpp:172-189`) runs only on the non-custom-emitter
path, so **custom emitters must perform their own argument-type checks
and emit clear `codegenError` messages** when types don't match any
supported overload. Otherwise a mixed-type call like
`pow(1, 2.0)` would silently reach the wrong branch.

**Rule**: When adding an overload for an existing stdlib function with
a custom emitter, (1) add the `@native fn` declaration, (2)
extend the existing custom emitter to handle the new arity / type
combination, (3) add explicit type checks with clear errors for
unsupported combinations — do NOT add a second table entry, it will
never fire. See `emitMathFloorCeilRound` / `emitMathLog` /
`emitMathPow` in `src/codegen_call.cpp` for the canonical pattern.

### `contains()` on Map must be intercepted in `emitStrOp_contains`, not via `@native`

**Source**: #1185 (2026-04-19, fix)
**Tags**: codegen, collections, map, dispatch-order, contains

`emitBuiltinString` is invoked before `emitBuiltinCollection` in
`src/codegen_call_dispatch.cpp`. Therefore every `contains(x, y)` call reaches
`emitStrOp_contains` first. Since all collection pointers share `ptrTy_` with
strings under the opaque pointer model, the Set and Map intercept blocks must
live inside `emitStrOp_contains` — before the str fall-through logic — rather
than in `emitBuiltinCollection`.

**Rule**: If a new collection type needs `contains()` semantics, add a
`getXxxType(s)` intercept block immediately after the Set block in
`emitStrOp_contains` (`src/codegen_call_string.cpp`). Do NOT declare a
`@native` `contains` overload in the stdlib package file; that would route
through the Pattern-A dispatch table and interact unpredictably with the
string handler.

The Map intercept mirrors `has_key` (`src/codegen_call.cpp`) exactly:
```cpp
if (llvm::Type *mapKeyTy = getMapKeyType(s)) {
    if (e.args.size() != 2)
        codegenError("Map.contains() takes exactly 1 argument");
    llvm::Value *key = emitExpr(*e.args[1]);
    if (key->getType() != mapKeyTy)
        codegenError("contains() key type mismatch");
    llvm::Value *idx = emitMapKeyLookup(s, key, mapKeyTy);
    return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "map_contains");
}
```

Note: `emitMapKeyLookup` is called without threading `keyName` here, matching
the existing `has_key` implementation. Structural-key correctness (pointer-typed
map keys) is a separate issue — see the L1106 entry.

### Mutating collection operations belong in `codegen_call_higher_order.cpp`, not `codegen_call_string.cpp`

**Source**: #1124 (2026-04-18)
**Tags**: codegen, dispatch, placement, list, string

**Rule**: `sort!` and `reverse!` (and any future in-place list mutation) must be registered in `emitBuiltinHigherOrder` (`src/codegen_call_higher_order.cpp`), not in the String dispatcher table in `emitBuiltinString` (`src/codegen_call_string.cpp`). The String dispatcher is tried before HigherOrder in the dispatch chain (`codegen_call_dispatch.cpp`), so a stray entry in the String dispatcher will unconditionally claim the call for every type — including lists — before HigherOrder gets a chance. This caused `reverse!` to silently route list calls through a misleadingly-named `emitStrOp_reverse_mut` function, and to produce the confusing error "requires a list" when called on a string.

**How to apply**: When adding a new mutating built-in that applies to lists, check whether `sort!` already lives in HigherOrder and mirror that placement. If a built-in should error on strings with a user-facing message, add the `getListElementType() == nullptr` guard in the HigherOrder handler and emit a clear diagnostic (e.g. "requires a list; strings are immutable, use reverse() instead").

### Every caller of emitSetElementLookup with a pointer-typed element must thread set_elem_type_name

**Source**: #963 (2026-04-15, bugfix)
**Tags**: codegen, collections, set, linear-scan, metadata, equality, hash-table

**Context**: `emitSetElementLookup` decides whether to use structural
equality (linear scan) or hash-by-pointer-as-C-string (hash-table path,
via `__ry_ht_find_str`) based on the `elemName` argument:

```cpp
const bool needsLinearScan =
    llvm::isa<llvm::StructType>(elemTy) ||
    (!elemName.empty() && elemName != "str" && ...);
```

When `elemName` is empty and `elemTy == ptrTy_`, it falls through to
`emitHashTableLookup`, which calls `__ry_ht_find_str(set_ptr, elem_ptr)`.
`__ry_ht_find_str` uses `strcmp` + string hash, so it reads the list/map/set
header bytes as a null-terminated C string. For any two `List<int>` values
of length 2, the ARC data starts with `\x02` (length=2 in varint encoding)
followed by a zero byte — every length-2 list has the same "string" `"\x02"`,
causing all of them to hash and compare equal. This silently deduplicates
distinct elements during set-literal construction, `add`, `remove`, `in`,
and `contains`.

**Root cause**: Several callers of `emitSetElementLookup` did not pass
`elemName` (set it to the empty string default), triggering the wrong path
for nested collections:

- `emitSetLiteral` (set-literal dedup): `src/codegen_expr_literal.cpp`
- `emitCollOp_add`: `src/codegen_call_collection.cpp`
- `emitSetRemove`: `src/codegen_call_collection.cpp`
- `in`/`not in` operator: `src/codegen_expr.cpp`
- `contains()` on a Set (routed via `emitStrOp_contains`): `src/codegen_call_string.cpp`

**Rule**: Every call site that passes a pointer-typed element to
`emitSetElementLookup` must thread `set_elem_type_name`; every call site
that passes a pointer-typed key to `emitMapKeyLookup` must thread
`map_key_type_name`. Both functions use an empty name as the signal to
skip structural equality and fall back to hash-by-ptr-as-C-string, so
omitting the name silently breaks correctness.

Pattern for sets:
```cpp
std::string elemName = getSetElemName(setPtr);  // reads set_elem_type_name
if (!elemName.empty())
    propagateTypeMeta(elemName, elem);
emitSetElementLookup(setPtr, elem, elemTy, elemName);
```

Pattern for maps (use `getMeta()->map_key_type_name`; there is no dedicated
accessor yet):
```cpp
const ValueMetadata *meta = getMeta(mapPtr);
std::string keyName = meta ? meta->map_key_type_name : std::string{};
if (!keyName.empty())
    propagateTypeMeta(keyName, key);
emitMapKeyLookup(mapPtr, key, keyTy, keyName);
```

Also set the type-name field when building the container (e.g. in the
set/map literal emitter), so downstream callers can retrieve it via `getMeta`.
`emitSubsetCheck` (`src/codegen_call_set_ops.cpp`) already follows this
pattern correctly for sets and is the canonical reference.

### Bare builtins that call native-lib runtime symbols must register the library via `used_native_libraries_`

**Source**: #1261 (implementation of `input()` builtin)
**Tags**: stdlib, codegen, native-library, builtin, library-loading, @native

**Rule**: When adding a new bare builtin (dispatched via the `builtins_` map or
an inline branch in `emitBuiltinCore`) whose runtime implementation lives in a
native shared library (`libry_<pkg>.dylib`), the codegen emitter MUST call
`used_native_libraries_.insert("<pkg>")` before emitting the `CreateCall`.
Otherwise the JIT fails at runtime with
`JIT session error: Symbols not found: [ ___ry_<name> ]`, because no
`@native("pkg")` directive populated `native_lib_index_` automatically.

**Why**: Bare `@native` declarations in `share/std/builtins.ry` leave
`sig.library` empty (see "Bare `@native` vs `@native("pkg")` — NOT
cosmetically equivalent"), so library-load registration does NOT happen
through the declaration path. The burden shifts entirely to the codegen
dispatcher.

Most bare builtins (`print`, `len`, `range`, `arguments`, etc.) happen to
call symbols in `ry_lib` — the static library linked directly into the main
binary — so they resolve through `DynamicLibrarySearchGenerator::GetForCurrentProcess`
without any explicit registration. The trap surfaces only when the runtime
symbol lives in a **separate** `.dylib`. For `input()`, `__ry_read_line` and
`__ry_input_prompt` live in `libry_io.dylib`; without
`used_native_libraries_.insert("io")`, a user who writes `print(input())`
(with no `import` from `io`) hits the JIT error.

**How to apply**: When adding a bare builtin branch in `emitBuiltinCore`,
check which source file defines its runtime symbols:

- If in `src/runtime_<pkg>.cpp` (linked into `libry_<pkg>.dylib` via
  `add_ry_native_lib(<pkg> ...)` in `CMakeLists.txt`), add
  `used_native_libraries_.insert("<pkg>")`.
- If in `src/runtime_*.cpp` linked directly into `ry_lib` (e.g. `runtime_print.cpp`,
  `runtime_arc.cpp`), no registration is needed.

Regression test: exercise the builtin from a program that does NOT
`import` anything from the target package — this is exactly the usage
pattern users expect for a bare builtin, and it's the shape that
surfaces missing registration. Compile-only tests do not catch this
because the dispatcher succeeds at IR emission; the failure is deferred
to JIT symbol lookup.

### Builtin native-underscore wrappers must update the builtin dispatcher too

**Source**: #1277 (2026-04-23, implementation)
**Tags**: stdlib, builtin, wrapper, default-args, ufcs, codegen

**Rule**: When converting a bare builtin in `share/std/*.ry` from a direct
`@native fn name(...)` declaration to the native-underscore wrapper
pattern (`@native fn _name(...)` + `fn name(... = default)`),
update the C++ builtin dispatcher as well.

**Why**: Bare builtins like `split` are intercepted before ordinary Ry
fn resolution. Two separate changes may be required:

- Register the underscored alias (for example `"_split"`) in the relevant
  dispatch table (`emitBuiltinString`, `emitBuiltinCore`, etc.), otherwise the
  wrapper body itself fails with `undefined function: _name`.
- Mirror any new optional arity/default-argument behavior in the builtin
  emitter, because UFCS/direct builtin calls may still route through the C++
  handler instead of the Ry wrapper body.

**How to apply**: After introducing a builtin wrapper, test both forms:
ordinary call syntax (`name(...)`) and UFCS (`value.name(...)` when
supported). A passing spec for only one path is not enough.

### Byte-list APIs require `TypeMeta::ListElem == i8Ty_`; plain int list literals are rejected at compile time

**Source**: PR #1055. **Tags**: codegen, runtime, IOListHeader, ListHeader, bytesToStr, writeBytes, send

**Rule**: Any native function that casts a `List` argument to `IOListHeader *` must set `NativeDispatchEntry::requireListU8Arg` to the 0-based index of that argument (or stamp `ListElemMeta::I8` for producers). Codegen enforces the gate via `CodeGen::emitTableDrivenNativeCall`. Do not add a new byte-list consumer without updating the audit table above and adding this field.

`IOListHeader` (`include/ry/runtime_io.hpp`) reads `data` with 1-byte stride (`int8_t *`).
Plain Ry list literals like `[97, 0, 98]` default element type to `int`, which codegen stores
with 8-byte (`i64`) stride. Passing such a list to a byte-list consumer produces silent memory
corruption (reads 8× too much data, interprets padding as bytes).

### `emitTableDrivenNativeCall` variadic path must use raw `ptr` type for `ResultPtr` entries

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: codegen, ResultPtr, variadic, emitTableDrivenNativeCall, nul-safety

**Rule**: In `src/codegen_call_native.cpp`, the fixed-arity path of `emitTableDrivenNativeCall`
already handled `ReturnWrapping::ResultPtr` (uses `ptrTy_` for the C ABI return, then wraps via
`wrapPtrAsResult`). The variadic-arity path (for functions like `path.join` with 2/3/4 overloads)
did **not** — it called `resolveType(matchedSig->returnTypeName)` to determine the C return type.
After upgrading `.ry` signatures to `Result<str, Error>`, `matchedSig->returnTypeName` becomes
`"Result<str, Error>"`, which `resolveType` maps to an LLVM struct type instead of `ptr` — causing
an ABI mismatch crash (the runtime still returns `char *`).

**Fix pattern**:
```cpp
// In the variadic path, before CreateCall:
llvm::Type *cRetTyVariadic = (entry->wrapping == ReturnWrapping::ResultPtr)
    ? ptrTy_
    : resolveType(matchedSig->returnTypeName);
auto *fnTy = llvm::FunctionType::get(cRetTyVariadic, argTypes, false);
// ... CreateCall, then:
if (entry->wrapping == ReturnWrapping::ResultPtr) {
    std::string errFn = getErrFnName();
    return wrapPtrAsResult(callResult, errFn.c_str());
}
return callResult;
```

**How to apply**: Any time a function with multiple overloads at different arities (variadic
dispatch path) is upgraded from bare `-> str` to `-> Result<str, Error>`, both the
`NativeDispatchEntry::wrapping` field AND the variadic code path in `emitTableDrivenNativeCall`
need updating. The fixed-arity path handles it automatically once `wrapping = ResultPtr` is set;
the variadic path requires explicit `ResultPtr` detection before `resolveType`.

### Dual-path builtins must share terminator / cleanup handling

**Source**: #821 sweep (2026-04-10)
**Tags**: codegen, builtins, exit, diverging-call

**Context**: `exit()` had two dispatch paths: (1) as an expression via
`emitBuiltinCore` in `src/codegen_call.cpp`, which explicitly created a
fresh `exit.dead` block after emitting `unreachable`, and (2) as a
statement via `builtins_["exit"]` lambda in `src/codegen.cpp`, which
just called `emitExit()` and left the insertion point on the terminated
block. The statement path (the most common usage) broke LLVM basic
block invariants for any trailing code. Putting the dead-block switch
inside `emitExit()` itself made both paths behave consistently.

**Rule**: When a builtin or helper has post-emit fixup — dead block
creation, ARC cleanup, metadata propagation, trace exit — put the
fixup inside the core helper (`emitExit`, `emitReturn`, ...) so every
dispatch path inherits it. If a fixup lives only in one caller, the
other caller is a latent bug waiting to be triggered.

**How to verify**: grep for all callers of the core helper and confirm
each one either expects the helper's post-state or explicitly
re-establishes its own. If any caller is silent about the post-state,
the fixup belongs in the helper.

### Named arguments for builtins: generic parse, builtin-only dispatch

**Source**: #747 (2026-04-14)
**Tags**: codegen, builtins, named-args, parser, print

**Context**: `print(end="", sep=", ")` required named/keyword argument
support, which did not exist in Ry's function call syntax.

Three options were considered:
1. Full language-wide named args (large scope: AST, type checker,
   overload resolution)
2. Special-case `print` in the parser (parser knows builtin names —
   violates separation of concerns)
3. Generic named-arg parse + builtin-only codegen restriction ← **chosen**

Option 3 adds `std::vector<NamedArg> named_args` to `CallExpr` /
`CallStmt`, teaches `parseArgList()` to detect `ident = expr` (same
lookahead as directive parsing), and restricts usage to builtins at
codegen time. Non-builtin calls with named args emit a codegen error.

The `=` token is never valid inside an expression (equality uses `==`),
so the lookahead is unambiguous.

**Rule**: When adding the next named-arg builtin, only `emitPrint()` in
`src/codegen_call_io.cpp` and the `builtins_` map in `src/codegen.cpp`
need updating. To lift the builtin-only restriction later, remove the
check in `codegen_call_user.cpp` and add type-checker support.

### Custom-emitter native functions bypass argument type-checking

**Source**: #828
**Tags**: codegen, native-dispatch, type-checking, gotcha

**Rule**: Native functions registered with a `customEmitter` in
their `NativeDispatchEntry` go through the early-return at
`src/codegen_call_native.cpp:135-150` and **skip** the regular
argument type validation path (L165-203). That means the Ry
declaration in `share/std/<pkg>/<pkg>.ry` (e.g.
`fn threadSpawn(body: fn() -> Unit) -> Thread`)
constrains only what IDE/docs consumers see, not what the custom
emitter actually accepts. When extending a custom-emitter native
fn (e.g. broadening `threadSpawn` to accept non-Unit
lambdas, or `assert` to accept new argument shapes), you can
relax the effective input without touching the Ry declaration.
Do update the docs and declaration for hygiene, but do not
assume the parser/type-checker is gating you — your codegen is.

### NUL checks belong in the codegen emitter, not the runtime, for multi-context functions

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: nul-safety, codegen, runtime, c-api-boundary

**Context**: `__ry_http_client_request(method, url, headers, body)` is invoked from three contexts:
1. User Ry code → codegen emitter → all arguments are Ry handles (StringHeader present)
2. C++ unit tests calling the function directly with `url.c_str()` or C string literals
3. Internal runtime functions (`__ry_http_post` calls it with the literal `"POST"`)

Applying `hasEmbeddedNul` inside the runtime covers case 1 but produces false positives for
cases 2 and 3 (reads garbage memory before the pointer).

**Rule**: If a runtime function is called from Ry codegen *and* from C++ or internal C code,
put NUL checks in the codegen emitter only. Leave the runtime function unchecked (treat it as
an internal C API). Add a comment in the runtime explaining the split:

```cpp
// NUL checks for method and url are done in codegen (emitHttpClientCall) for
// the user-facing Ry paths. This function is also called directly from C++
// tests and internally with plain C strings that carry no Ry StringHeader —
// applying hasEmbeddedNul here would read garbage before those pointers.
```

### String negative-index wrap uses UTF-8 char count, not byte length

**Source**: PR #1199 fix for `emitStrOp_substring` (2026-04-20)
**Tags**: codegen, substring, emitNegativeIndexWrap, UTF-8, char-count, __ry_utf8_len_n

**Rule**: When applying `emitNegativeIndexWrap(idx, wrapBase, prefix)` to string (char) indices, `wrapBase` MUST be the UTF-8 codepoint count obtained via `__ry_utf8_len_n(s, byteLen)`, not `emitStringByteLen(s)`. For a 5-char, 15-byte string like `"あいうえお"`, using byte length as wrap base gives `-1 + 15 = 14` (a garbage index into a 5-char string); using char count gives `-1 + 5 = 4` (correct last-char index). The `_n` variant is NUL-safe (counts embedded `\0` as one codepoint).

**Pattern** (copy from `src/codegen_call_string.cpp:218-245`):
```cpp
llvm::Value *byteLen = emitStringByteLen(s);    // hoisted: reused for runtime call
auto utf8LenFn = getRuntimeFn("__ry_utf8_len_n", i64Ty_, {ptrTy_, i64Ty_});
llvm::Value *charLen = builder_.CreateCall(utf8LenFn, {s, byteLen}, "charlen");
llvm::Value *wrapped = emitNegativeIndexWrap(idx, charLen, "prefix");
// ... then zero-clamp (wrap of very negative idx can still be negative), then runtime call
```

Always hoist `emitStringByteLen(s)` into a local so the same byte length feeds both `__ry_utf8_len_n` and the downstream string-indexed runtime call (e.g. `__ry_utf8_substring`). The zero-clamp after wrap is still required: `-100 + 5 = -95` needs to be clamped to `0`.

**Why this differs from `emitCollOp_slice`**: `emitCollOp_slice` uses list length (element count = storage length), so there is no "byte vs codepoint" distinction. String call sites must remember this extra indirection.

---

## #1156: split match subject type into enum-only vs broad source name

**Source**: PR #1156 fix for `codegen_match.cpp` subjectEnumType wrong channel
**Tags**: pattern, match, ARC, codegen, Option, Result, tuple, subjectEnumType

**Rule**: `emitPatternTest`, `emitPatternBindings`, and `checkMatchExhaustiveness`
take **two** source-name parameters:

- `subjectEnumName` — narrow channel (`ValueMetadata::enum_value_type`).
  Only set for enum/ADT subjects (from `resolveEnumType()`). Used by
  `EnumPattern`/`EnumConstructorPattern` generic-instantiated lookup, by
  `Some`/`Ok`/`Err` binding `extractGenericTypeArg`, and by `VariablePattern`
  binding's `enum_value_type` write (still guarded by
  `enum_types_.count(resolveTypeAlias(name))` per the KNOWLEDGE L2946 rule).
- `subjectSourceTypeName` — broad channel (`resolveSubjectSourceTypeName()`).
  Reconstructs `Option<T>` / `Result<T, E>` / `(T1, T2, ...)` from the LLVM
  subject type when no enum annotation exists. Used by `TuplePattern` /
  `RecordPattern` structural verification and by `emitPatternBindingArc`
  Path 2a from `VariablePattern` only.

**Reconstruction lossiness**: `reverseResolveTypeName(ptrTy_)` returns `"str"`;
unknown structs return `"any"`. So `Option<List<int>>` reconstructs as
`"Option<str>"`. The reconstructed string is therefore **only** safe for
structural checks ("is this a tuple struct?" / "does this match record name X?").
Do NOT re-feed it into `extractGenericTypeArg` for ARC payload classification —
`Option<List<int>>` would be misclassified as `Option<str>` (wrong header
offset: str uses -24, List uses -16), causing heap corruption under ASan.

**Defense-in-depth on TuplePattern**: the test arm rejects via
`!sTy || !isTupleStructType(sTy)` regardless of any source name. This fires
even when both names are empty — closing the path where Option's `{i1, T}`
2-element struct silently passed the 2-arity check and crashed with ICmp
type mismatch (original #1156 crash vector).

**Why split rather than unify**: the previous single `subjectEnumType`
parameter was overloaded with non-enum names via the new broad helper
would force every consumer to add an `enum_types_.count(name)` guard.
The split signature makes "enum-only" vs "broader subject type" a
compile-time-enforced distinction. The guard at VariablePattern binding
(`KNOWLEDGE L2946`) remains necessary but is now purely defensive
(subjectEnumName is already enum-only).

**Follow-up**: Add a lossless `source_type_name` field to `ValueMetadata` so
`Option<List<int>>` reconstruction is accurate, enabling ARC Path 2a for nested
generics (currently handled by Path 2b heuristic via `propagateMeta`).
- Pre-existing defect in `emitListSlice`: ARC retain omitted for reference-typed elements (#1204) — tracked separately.

