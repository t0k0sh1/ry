---
paths:
  - "src/codegen_expr.cpp"
  - "src/codegen_expr_cast.cpp"
  - "src/codegen_builtin.cpp"
  - "src/codegen.cpp"
  - "include/ry/codegen.hpp"
  - "tests/filecheck/**/*.ry"
---

# Codegen LLVM IR Conventions

### New dispatch branches in `emitArithmeticOp` must precede the str-vs-non-str reject

**Source**: #863 (2026-04-11)
**Tags**: codegen, arithmetic, type-error, collection, dispatch-order

**Context**: `src/codegen_expr.cpp::emitArithmeticOp` uses a
str-vs-non-str reject (`if (lhsIsStr || rhsIsStr) codegenError(...)`) as
the last line of defense for ill-typed `+` operands. The catch is that
`isStringValue()` (`src/codegen_any.cpp:28`) returns `true` for **any**
`ptrTy_` value whose metadata side table has no `hasAnyMeta()` flag —
it is effectively "unknown pointer type", not "string". Consequently
any dispatch branch added AFTER this reject is unreachable for
metadata-less pointer operands (they are already classified as str and
rejected with a misleading message), and adding a branch BEFORE it must
be careful not to steal legitimate str mismatches like `"x" + myMap`.

**Rule**: When adding a new type-specific branch for `+` in
`emitArithmeticOp`, insert it **after** the list-concat success path
but **before** the str-vs-non-str reject. Use the typed metadata
accessors (`getMapKeyType` / `getMapValueType` / `getSetElementType` /
`getListElementType`) to recognize the operand kind. The current order
in `codegen_expr.cpp::emitArithmeticOp` is:
1. String concat / repeat (early return)
2. List + List success → `emitListConcat`
3. Map + Map success → `emitMapMergeCore` (#866)
4. Set + Set success → `emitSetUnionCore` (#866)
5. Mixed/one-sided Map-or-Set reject (named diagnostic, #863)
6. str-vs-non-str reject

**Related**: The companion #858/#862 entry covers the upstream side of
the same trap — metadata propagation on compound-assign loaded slots —
which must also be correct for the dispatch branch to see accurate
kinds on LHS values produced by `m[k] += ...` and friends.
`emitMapMergeCore` and `emitSetUnionCore` are extracted from
`emitCollOp_merge` / `emitSetOp_union` and shared between `CallExpr`
and `BinaryExpr` sites (#866).

### `promoteToInt` / `promoteToFloat` silently widen `i1` (bool) — callers must gate with `rejectBoolInOperator`

**Source**: PR #1030 (2026-04-17)
**Tags**: codegen, arithmetic, bitwise, bool, type-safety, promoteToInt

`CodeGen::promoteToInt` (`src/codegen.cpp`) `ZExt`s `i1` → `i64` without
any type check — it only rejects struct/pointer types via `ensureNumericType`.
Similarly `promoteToFloat` uses `SIToFP` on `i1`. The `**` branch in
`emitArithmeticOp` uses direct `SIToFP` on the raw operand.

**Rule**: Any arithmetic or bitwise branch that calls `promoteToInt`,
`promoteToFloat`, or a direct `CreateSIToFP` on a Ry operand must call
`rejectBoolInOperator(v, op, "arithmetic"|"bitwise")` FIRST if the
expression must reject bool operands. The helper is declared in
`include/ry/codegen.hpp` and implemented in `src/codegen.cpp`.

Do NOT gate the call inside `promoteToInt` / `promoteToFloat` themselves —
those helpers are also used by ARC / string-repeat paths where i1→i64
widening is intentional or at least harmless. The reject belongs at each
arithmetic/bitwise call site.

### Arithmetic bool-reject must be inside each operator's branch, not at function entry

**Source**: PR #1030 (2026-04-17)
**Tags**: codegen, arithmetic, bitwise, bool, type-safety, dispatch-order

`emitArithmeticOp` processes string concat / repeat and collection-concat
BEFORE the numeric branches. `"x" + true` and `"x" + false` auto-stringify
the bool operand and return a `str` — this is intentional and must remain
working. If `rejectBoolInOperator` were called at function entry, those legal
expressions would break.

**Rule**: Place `rejectBoolInOperator` at the entry of each individual
operator branch in `emitArithmeticOp`, not as a top-of-function guard.
This way each branch independently rejects bool without interfering with
string/collection dispatch. The current order in `emitArithmeticOp`:

1. `checkLowLevelTypeMix` — low-level type validation (no bool issue here)
2. `arithLowLevel` branch — exits early for low-level operands (bool is never low-level)
3. `**` branch — dedicated early-exit branch; **insert reject inside this branch** (before `CreateSIToFP`). This branch runs before string dispatch but that's fine: there is no string `**` operation.
4. String concat/repeat auto-stringify (early return) — must NOT be preceded by any bool reject at function level
5. List/Map/Set concat (early return)
6. str-vs-non-str reject (catch-all)
7. `//`, `/`, `%` branches — **insert reject at each branch entry** (before `promoteToInt`/`promoteToFloat`)
8. `+`, `-`, `*` default — **insert reject before `promoteToInt`**

For `emitBitwiseOp`, there is no string/collection dispatch, so
`rejectBoolInOperator` can be placed before `promoteToInt` after the
`bwLowLevel` block exits.

### `in`/`not in` dispatch order in `emitExprVariant`

**Source**: #1032 (2026-04-17, feat)
**Tags**: codegen, in-operator, str, dispatch-order, isStringValue

**Rule**: The dispatch order in `emitExprVariant` for `in` / `not in` is:
user overload (`tryOperatorCall`) → Set → Map → List → **str** → error.
The str branch must come *after* Set/Map/List because `isStringValue(container)`
returns `true` for any `ptrTy_` value without collection/resource metadata —
and since Set, Map, and List headers are all tagged with metadata
(`hasAnyMeta() == true`), `isStringValue` correctly excludes them.
Adding str before Set/Map/List would fire on plain `str` RHS before the
collection branches even run.

**LHS widening**: when the RHS is `str` but the LHS has type `any`, unwrap with
`unwrapFromAny(elem, ptrTy_)`. The "wrapInAny" direction is not needed — str has
no element type to promote into. Any other LHS type (int, float, bool, collection)
emits a compile error: `"'in'/'not in' operator: left side must be str when right side is str"`.

**Empty needle**: `"" in s` is `true` for any `s` — the runtime (`__ry_str_find_byte`)
returns `0` when `nl == 0`, matching Python and the existing `contains` semantics.

### FileCheck golden authoring conventions

**Source**: #897 (2026-04-18)
**Tags**: filecheck, codegen, ir, testing, ci

**Rule**: FileCheck goldens live in `tests/filecheck/*.ry`. Each file is both valid Ry source and a FileCheck script — Ry uses `#` line comments, and `# CHECK:` lines work because FileCheck searches for the `CHECK:` substring regardless of prefix. **Do not use `//`** — that is not a Ry comment and causes a parse error.

Key constraints:

1. **Ry comment syntax is `#`**: Write `# CHECK:`, `# CHECK-NEXT:`, `# CHECK-NOT:`, `# CHECK-DAG:`, `# CHECK-LABEL:`. Never `// CHECK:`.
2. **Unoptimized IR only**: `ry --emit-llvm-ir` emits codegen output before any LLVM optimization passes. `alloca`/`store`/`load` patterns for every function argument are always present. `mem2reg` has not run.
3. **Opaque pointers (LLVM 17+)**: All pointer types are `ptr`; never write `i64*`, `i8*`, etc. in CHECK patterns.
4. **ARC retain/release visibility**: `@ry_retain` / `@ry_release` BasicBlocks only appear in CoW clone paths, lambda captures, and `@parallel for` patterns. They do not appear in simple scalar or string identity functions — choose goldens accordingly.
5. **Result type layout**: `%Result = type { i1, i64, ptr }` — `i1` is the `is_ok` flag; `Err` uses constant aggregate `{ i1 false, ... }`, `Ok` uses `insertvalue %Result { i1 true, ... }`.
6. **LLVM version bumps**: Goldens are LLVM-version-sensitive. After any LLVM version bump, re-run `ctest -L filecheck` and update patterns if IR structure changed.
7. **FileCheck installation**: Source-built LLVM 21 inside the `ghcr.io/<owner>/ry-ci:llvm-21` container includes `FileCheck` at `/usr/local/llvm/bin/FileCheck`, so CI does not need a separate install step (#1505 replaced the older `apt-get install llvm-{MAJOR}-tools` flow). On a Linux host outside the container: `sudo apt-get install llvm-{MAJOR}-tools` → `/usr/lib/llvm-{MAJOR}/bin/FileCheck`. macOS: `brew install llvm@{MAJOR}` → `/opt/homebrew/opt/llvm@{MAJOR}/bin/FileCheck`.

### All str handles passed to `emitStringByteLen` must be StringHeader-backed (#1159)

**Source**: Issue #1159 / PR fix/1159-fstring-enum-unknown-stringheader. **Tags**: codegen, strings, stringheader, fstring, arc, emitStringByteLen, cachedGlobalString

**Rule**: `emitStringByteLen(handle)` reads `handle - 8` (STRING_BYTELEN_OFFSET) to obtain `byte_len`. This is only safe when `handle` points into a `StringHeader`-prefixed allocation — i.e., the pointer was produced by `cachedGlobalString`, `buildArcGlobal`, or a Ry ARC runtime that calls `makeString`/`makeStringUninit`. `IRBuilder::CreateGlobalString` produces a plain `[N x i8]*` global **without** a StringHeader prefix; passing such a pointer to `emitStringByteLen` reads the 8 bytes immediately before the global (typically unrelated global data or relocation metadata) and interprets them as `byte_len`, causing truncation, garbage output, or UB.

**Root cause**: `src/codegen_tostring.cpp` — `hasExplicitValues` enum branch — had a default fallback BB that used `builder_.CreateGlobalString("?", ".enum_unknown")`. The PHI on that branch collected the raw pointer, which `emitStringByteLen` later misread. The ADT/union default on the same file already used `cachedGlobalString` correctly; the explicit-value enum default was the outlier.

**How to apply**: When adding a new `str` constant in codegen, always use `cachedGlobalString`. When reviewing existing codegen, grep for `CreateGlobalString(` and verify that the result never flows into `emitStringByteLen`, `emitStringLen`, or f-string concat helpers. The only safe uses of raw `CreateGlobalString` are LLVM format strings for `printf`-style calls that are never read back as Ry `str` handles.

### Collection ops on pointer elements: guard on `list_elem_type_name`, not on `NestedListElem` alone

**Source**: #1262, #1268, #1269 (2026-04-21, bugfix)
**Tags**: codegen, collections, distinct, remove, in, guard, UB, strcmp, list_elem_type_name, positive-whitelist

**Rule**: When a collection helper's pointer-element branch uses `strcmp` (or any other C-string operation), the guard **must** use a positive allowlist based on `list_elem_type_name`, not a single-field blacklist via `getNestedListElementType`:

```cpp
if (elemTy == ptrTy_) {
    const ValueMetadata *meta = getMeta(listVal);
    const std::string &elemName = meta ? meta->list_elem_type_name : std::string{};
    const bool isNonStrName = !elemName.empty() && elemName != "str";
    const bool hasNestedList = meta && meta->nested_list_elem != nullptr;
    const bool hasFnInfo = meta && meta->list_elem_fn_type_info.has_value();
    if (isNonStrName || hasNestedList || hasFnInfo)
        codegenError("<op>() is only supported for lists of primitive values or strings");
}
```

**Why**: `getNestedListElementType` only checks `TypeMeta::NestedListElem`, which `propagateTypeMeta` sets only in the `isListTypeName` branch. Map/Set/function element kinds live in different fields (`map_value_type_name`, `list_elem_fn_type_info`). A blacklist on a single field is structurally incomplete — `List<Map<K,V>>` / `List<fn(...)>` / `List<Set<T>>` all slip through and `strcmp` runs on Map/Set/closure headers (undefined behaviour). `inferCollectionTypeName` (`src/codegen_builtin.cpp:242-274`) returns non-empty `"Map<...>"` / `"List<...>"` / `"Set<...>"` for non-str pointer types, so treating `""` as str is safe (see sibling entry "List<str> literals can have empty list_elem_type_name"). Resource element lists (`List<TcpStream>`) get a non-empty `list_elem_type_name` and are caught by `isNonStrName`; do not check the list value's own `resource_kinds` — lists are not resources themselves, so that field is always empty for list containers. Access `nested_list_elem` directly on the cached `meta` pointer instead of calling `getNestedListElementType` to avoid a second `value_metadata_` lookup.

**How to apply**: Fixed in `emitCollOp_distinct` by #1262, `emitListRemove` by #1268, and the `in` / `not in` list branch (`src/codegen_expr.cpp` around line 1555) by #1269. Any new collection op that takes a `List<T>` and compares pointer elements must apply the same positive-allowlist guard before any `strcmp` call. Always pair with direct regression tests: add a `List<Map<str,int>>` / `List<fn(...)>` / `List<Set<T>>` case that expects `expectCompileError`; a single-element list must not be the sole reproduction because `curOutLen == 0` masks the symptom on the first iteration of the dedup loop.

**Related**: #1241 (`propagateMeta` added to `distinct` — did not address the guard), "List<str> literals can have empty list_elem_type_name" (#1235), "Same-element-type collection helpers must pair `setTypeMeta` with `propagateMeta`".

### emitRuntimeError terminates its block; callers must pre-split to continue

**Source**: #745/#795/#796 implementation (2026-04-11)
**Tags**: codegen, runtime-error, unreachable, branch, control-flow

**Context**: `CodeGen::emitRuntimeError` (`src/codegen_call_user.cpp:600-618`)
emits `fprintf(stderr, ...)` → `call exit(1)` → `CreateUnreachable`. Unlike
`emitExit`, it does **not** switch to a fresh dead block afterwards. If the
caller still wants to continue emitting IR along a happy path, the split
must be done **before** the call:

```cpp
llvm::BasicBlock *okBB  = llvm::BasicBlock::Create(*ctx_, "foo.ok",  fn_);
llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "foo.err", fn_);
builder_.CreateCondBr(cond, okBB, errBB);

builder_.SetInsertPoint(errBB);
emitRuntimeError("error: %s\n", ".foo_err", {msgPtr});
// errBB is now terminated by `unreachable` — do NOT keep emitting here.

builder_.SetInsertPoint(okBB);  // continue happy path here
```

Callers that just dump `emitRuntimeError` into the current block without
creating a separate happy-path block will lose any IR they emit afterward
(it gets attached to a block that already ends in `unreachable`, which LLVM
verify rejects).

**Rule**: Treat `emitRuntimeError` like `CreateRet` / `CreateBr` — the call
site owns the basic-block split. See `emitIntZeroDivGuard`
(`src/codegen_call_user.cpp:625-636`) and the top-level `?` path in
`emitExprVariant(ErrorPropagateExpr)` for canonical examples.

### LLVM ORC JIT intermittent crash in `~LLJIT()` / `removeResourceTracker` / `~CodeGen()` (Linux + macOS)

**Source**: #1022 (2026-04-16) CI run 24507853564; #1088 (2026-04-17) macOS Darwin 25.3.0; #1043 (2026-04-17) CI runs 24547110395 + 24547575356; #1187 (2026-04-19) macOS Darwin 25.3.0 residual ~16 % rate
**Tags**: llvm, orc, jit, ci, flaky, linux, macos, cleanup, parallel-test

**Symptom**: The Ry self-test (`ry test -p`) completes all `it` blocks
successfully, then crashes during JIT teardown.

- **Linux CI**: glibc heap consolidation crash (`cfree`) during LLVM teardown. The crash frame
  varies — most commonly `removeResourceTracker`, but `CodeGen::~CodeGen()` has also been observed
  ("corrupted size vs. prev_size while consolidating"). Same root cause; different destruction order.
- **macOS (non-ASan)**: intermittent `~40%` failure rate in parallel mode — worker subprocess
  exits with signal (`128+N`) silently; the parent counts `+1 total failures` with no red line.

On Linux (`removeResourceTracker` variant):
```text
135 passed, 0 failed
PLEASE submit a bug report to https://github.com/llvm/llvm-project/issues/...
#4  cfree (/lib/x86_64-linux-gnu/libc.so.6)
#5  llvm::orc::ExecutionSession::removeResourceTracker(...)
#7  scope_exit destructor in jit_runner.cpp
#8  runRySource(...)
```

On Linux (`~CodeGen()` variant):
```text
135 passed, 0 failed
corrupted size vs. prev_size while consolidating
#4  cfree (/lib/x86_64-linux-gnu/libc.so.6)
#5  CodeGen::~CodeGen()
```

**Discriminating evidence for flake vs. regression**: If all test cases in the failing file report
success (N passed, 0 failed) and only the teardown crashes, and a re-run on the same commit passes,
classify as flake. A genuine heap corruption from user code would fail a specific test case or fail
deterministically. If the crash **persists under ASan** (ASan reports a heap error on its own, not
just glibc's `cfree` check), it is user-code OOB/UAF — not an ORC flake. In that case, see the
[`~CodeGen() glibc heap-check crashes`](#codegen-glibc-heap-check-crashes-are-not-destructor-bugs)
entry in the Runtime / Memory section for the diagnostic checklist.

**Fix applied** (two-step suppression — both steps are required):

1. `(void)jit.release()` — guarded by `#if defined(__linux__) || defined(__APPLE__)`. Leaks the
   LLJIT so `~LLJIT()` never runs. Extended from Linux-only to macOS in #1088 (suppressed the
   `~LLJIT()` crash frame).
2. `rtCleanup.release()` — added immediately before `jit.release()` in the same `#if` block (#1187).
   Cancels the `scope_exit` destructor so `RT->remove()` never fires during stack unwind. This is
   necessary because `jit.release()` leaks but does NOT destroy the LLJIT; the leaked
   `ExecutionSession` remains alive, so `RT->remove()` → `handleRemoveResources` →
   `InProcessMemoryManager::deallocate` → `WrapperFunctionCall::runWithSPSRet` hits the same
   JITLink deallocation crash. Suppressing only the `~LLJIT()` frame left the
   `removeResourceTracker` frame, causing a residual ~16 % failure rate in `ry test -p`.

**Rule**: On Linux CI, trigger a re-run if this crash appears — it is pre-existing LLVM ORC
flakiness, not a regression. The `~CodeGen()` frame variant is the same flake family as
`removeResourceTracker`. On macOS, both workarounds together suppress the crash; if any failure
rate reappears after the fix, the root cause is broader than these two frames and needs fresh
investigation. Do not suppress the LLVM crash reporter or add `|| true` — a genuine double-free in
user code would produce the same frame.

