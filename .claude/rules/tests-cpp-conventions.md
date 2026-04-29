---
paths:
  - "tests/test_*.cpp"
  - "tests/test_*.hpp"
---

# Tests — C++ Conventions

### Renaming stdlib `@native` functions: also sweep embedded Ry source in C++ tests

**Source**: #1414 (2026-04-29, implementation — advisor call-out)
**Tags**: testing, codegen-test, stdlib, rename, refactor, blind-spot

**Context**: When renaming stdlib `@native` functions (e.g. snake_case → camelCase in v0.0.16's `#1437` / `#1438` / `#1414`), the obvious places to update are the `share/std/<pkg>/<pkg>.ry` declarations, the C++ runtime symbol exports (`__ry_<pkg>_<oldName>` → `__ry_<pkg>_<newName>`), the dispatcher tables in `src/codegen_call_*.cpp`, and `tests/spec/*.test.ry`. A grep across `share/`, `tests/spec/`, `examples/`, and `docs/` will find all of these.

It will **not** find Ry source strings embedded inside C++ tests. Files like `tests/test_codegen_directive.cpp` contain `runSource("@native(\"path\")\nfn is_absolute(p: str) -> bool\n...")` literals — these reference the old name from inside a `.cpp` file, so a `.ry`-only sweep misses them entirely. The build links cleanly because the exported symbol matches whatever the embedded source declares (the C++ test re-declares `@native fn is_absolute`, which dispatches through `__ry_path_isAbsolute` only if the linker sees that symbol — and it doesn't, since we renamed it).

**Rule**: When renaming any `@native` function, also grep across `tests/` (including `.cpp` files) and `src/` for the old name as a bare word, filtering out C++ STL false positives:

```bash
grep -rnE '\b(<old1>|<old2>|...)\b' src/ include/ tests/ share/ examples/ \
  | grep -v 'std::filesystem::' \
  | grep -v 'fs::'
```

Don't rely on `.ry`-only sweeps. The build is the authoritative check — if you miss an embedded reference, `cmake --build build` succeeds (the C++ test compiles fine) but `./build/ry_tests` fails when the test runs the embedded source through codegen and finds no matching symbol. Catch it at grep time, not at test time.

**How to apply**:
- Always run the cross-tree grep before declaring the rename complete, even if `.ry` files all look clean.
- For PRs in the snake_case→camelCase series, the embedded-source pattern is `runSource("...@native(\\"<pkg>\\")...fn <oldName>...")` — focus the grep on the unique old function name token.

### Stdlib-declared directives need `withStdlibDirectiveDecls()` in C++ tests

**Source**: #1390 (2026-04-27, implementation)
**Tags**: testing, codegen-test, directives, stdlib, module-loader, harness

**Context**: After #1390, the non-bootstrap built-in directives (`@inline`, `@parallel`, `@const`, `@deprecated`, `@each`, `@property`) and `@it` / `@describe` are declared in stdlib `.ry` files (`share/std/core/directive.ry`, `share/std/testing/testing.ry`). Their declarations reach a normal `./build/ry` invocation only via `ModuleLoader` → `from std import` (wildcard) → `builtins.ry` re-export → `core/directive.ry`, plus the explicit `from testing import ...` for the testing pair.

The codegen test harness (`runSource`, `runSourceWithWarnings`, `runTestSource`, `compileSource` in `tests/test_codegen_common.hpp`) goes `Parser → CodeGen` directly and **skips `ModuleLoader` entirely**. Source that uses any of these directives without inline declarations therefore fails at codegen with `unknown directive '@inline'` (or `@parallel`, etc.) — even though the same source runs fine through the real CLI.

**Rule**: Any C++ test that exercises a directive declared in stdlib `.ry` (the 8 listed above) must wrap its source with `withStdlibDirectiveDecls()` from `tests/test_codegen_common.hpp`. The helper prepends inline `@directive(target=...)` declarations equivalent to what the loader would inject.

**How to apply**:
- Adding a new test that uses any of `@inline / @parallel / @const / @deprecated / @each / @property / @it / @describe`: wrap the source string — `runSource(withStdlibDirectiveDecls("..."))`. Don't try to express the directive declaration inline ad-hoc.
- Adding a new directive to the stdlib `.ry` declarations: extend `withStdlibDirectiveDecls()` in `tests/test_codegen_common.hpp` to include the new declaration so existing tests that exercise it continue to work.
- Removing a directive from the C++ registry: registry deletion + stdlib `.ry` declaration + helper update + applying the helper to affected tests must land in **one commit** because `emitStmt(DirectiveDefStmt)`'s collision check rejects builds where a registry entry and a `.ry` declaration coexist for the same name.

**Why a smoke test is still needed**: The helper bypasses the actual `ModuleLoader` → `builtins.ry` re-export chain, so all-green C++ tests prove the codegen logic but not the loader path. Always also run a smoke test through the real CLI (`./build/ry /tmp/<file>.ry`) when changing the directive declaration locations or the re-export wiring.

### CodeGenTest::runSource cannot compile code that imports stdlib packages

**Source**: #842 (2026-04-11, implementation)
**Tags**: testing, codegen-test, stdlib, module-loader, harness

**Context**: `CodeGenTest::runSource` / `expectCompileError` in
`tests/test_codegen_common.hpp` goes directly from `Parser` to
`CodeGen::compile` without invoking `ModuleLoader`. Source that contains
`from math import ...` (or any stdlib import) therefore fails with
`error: unresolved import: math (ModuleLoader should have resolved this)`
because codegen expects the import node to have been pre-resolved.

The only test harness that currently runs `ModuleLoader` is
`ImportTest` (`tests/test_codegen_stmt.cpp:617`), which uses a tempdir
+ `writeFile()` — it is designed for user-level imports of files you
write yourself, NOT for pulling in the real `share/std/*` packages.

**Consequences for custom-emitter compile-error tests**: Rejection
branches inside stdlib-package custom emitters
(e.g. `emitMathPow`'s "requires (float, float) or (int, int)" error)
cannot be covered via `expectCompileError` today. Workarounds:

- Smoke-verify the error via `printf '...\n' | ./build/ry -c` during development
  and document the expected error text in the PR description.
- Add a happy-path test in `tests/spec/<pkg>.test.ry` that exercises the
  *successful* branches of the custom emitter, so any refactor that
  breaks dispatch is still caught by the Ry self-test suite.
- A proper fix is to extend the C++ test harness with a helper that
  sets up a `ModuleLoader` pointing at the repo's `share/` directory.
  Tracked as a future enhancement; not blocking feature work.

**Rule**: If you need a C++ unit test for a rejection branch inside a
stdlib custom emitter, the harness limitation applies — fall back to
smoke tests + document the gap in the PR. Don't add failing
`expectCompileError` tests for `from math import` / `from json import`
/ etc.

### Test loader-pipeline changes for AST variants with codegen no-op via `resolveImportsOnly` introspection

**Source**: #709 (2026-04-27, implementation — advisor call-out)
**Tags**: testing, module-loader, codegen-no-op, multi-pr-chain, blind-spot

**Context**: When a new AST variant is introduced across a multi-PR chain (parser lands first, loader/export updates next, codegen registration last), the middle PR adds the variant to `isExportable()` / `getExportName()` in `src/module_loader.cpp` while codegen for the variant is still a deliberate no-op. Execution-based tests (`runWithImports` returning a printed value) cannot directly verify the variant flows through the loader, because nothing in the running program observes its presence. In #709, AC #4 ("private `_`-prefixed directive defs are excluded by wildcard import") had no execution-based test that could distinguish "directive def is in program but does nothing" from "directive def was filtered out". Arguing "`isPrivateName` is a string check, so it must work uniformly for all exportable variants" is logically sound but only indirect evidence — a future refactor of the wildcard path could introduce per-variant filtering and the indirect-coverage tests would not catch it.

**Rule**: When adding a new AST variant to `module_loader.cpp`'s exportable list, and that variant has a codegen no-op until a later PR, add a `resolveImportsOnly()` helper to the `ImportTest` fixture (mirror `runWithImports`, drop `CodeGen::compile` + `runModule`, return the `Program`). Write a Program-introspection test that walks top-level statements with `std::holds_alternative<TheVariant>` and asserts the expected names are present (public) and absent (private).

**How to apply**:
- Don't rely solely on `EXPECT_THROW` for `from pkg import _name` for the variant — that named-private throw fires at `module_loader.cpp:73-82` before AST traversal in `extractDefinitions`, so it doesn't exercise the variant-specific path.
- Reference: `ImportTest.DirectiveDefWildcardExcludesPrivate` and the `resolveImportsOnly` helper in `tests/test_codegen_stmt.cpp` (#709).

### Use `-> Unit` in @it/@describe rejection tests to isolate the directive check

**Source**: #1122 (2026-04-18, implementation)
**Tags**: testing, directives, codegen-test

**Rule**: When writing a C++ rejection test for `@it` or `@describe` return-type enforcement, use `-> Unit` as the return type annotation — not `-> int`, `-> bool`, `-> str`, etc.

**Why**: If the test function body (e.g. `expect(1).toEq(1)`) doesn't return a value of the declared type, codegen fires a secondary error — "function does not return a value on all code paths" — **before** the directive check. The test then passes even if the directive enforcement is removed, silently breaking the regression guard.

`-> Unit` is safe because `expect(...)` naturally returns `Unit`, so the body satisfies the return type. The only path that throws is the directive enforcement itself.

### RWLock stress tests for #871 must be C++ GoogleTests, not Ry spec files

**Source**: #871 (2026-04-11, implementation)
**Tags**: rwlock, testing, tsan, spec-loader, gotcha

**Rule**: When adding a TSan-gated stress test for
`src/runtime_thread.cpp` primitives, put it in a pure C++
GoogleTest under `tests/` (e.g.
`tests/test_runtime_rwlock_stress.cpp`), NOT in
`tests/spec/concurrency.test.ry`. Wire it into `ry_tests` via
`CMakeLists.txt` so it runs under the **required** `build-tsan/ry_tests`
step.

**Why**: The C++ test harness used by `CodeGenTest.ConcurrencySpecSuite`
is `runTestSource` in `tests/test_codegen_common.hpp`, which goes
`Lexer → Parser → CodeGen` directly and never invokes `ModuleLoader`.
Any `from thread import ...` statement in a spec run via that harness
fails with `unresolved import: thread (ModuleLoader should have
resolved this)`. Adding a stress test to `concurrency.test.ry`
therefore silently breaks the TSan-required gate. A pure C++ test
that calls `__ry_rwlock_*` directly via `include/ry/runtime_thread.hpp`
works under all sanitizers, runs in the required step, and is more
direct anyway — we are testing a runtime invariant, not a language
feature. See also the entry at the top of this "Testing" section for
the `runSource` / `runTestSource` limitation.

### C++ `\xNN` hex escape consumes ALL following hex digits — never use `\xNNX` when X is a hex char

**Source**: PR #1053 (test authoring, 2026-04-17). **Tags**: c++, test, nul-safe, string-literal

**Rule**: In a C++ string literal, `\x` consumes every subsequent hex character (0–9, a–f, A–F) as part of a single escape sequence. So `"\x00a"` is NOT `NUL + 'a'`; it is the single byte `0x00a = 10 = '\n'`. This silently produces the wrong byte value instead of a compile error.

**Examples of the trap**:
```cpp
// WRONG: "\x00a" = '\n' (0x0a), "\x00b" = '\x0b', "\x00A" = '\n', "\x00F" = '\x0f'
makeString("k\x00a", 3);  // produces k + 0x0a, NOT k + NUL + 'a'

// CORRECT options:
const char raw[] = {'k', '\0', 'a'};        // char array initializer — unambiguous
makeString(raw, 3);

// OR adjacent string literals (concatenated by the compiler):
makeString("k\x00" "a", 3);                 // "k\x00" ends the hex sequence; "a" is next literal
```

**Why it matters here**: NUL-key disambiguation tests build keys like `k\0a` vs `k\0b`. Using `"k\x00a"` / `"k\x00b"` produces identical keys (`k\n`) and the test silently passes for the wrong reason. Always use the char-array or adjacent-literal form when the byte after `\xNN` is a hex character.

The non-empty-delim `split` now uses `__ry_str_split` in `src/runtime_string.cpp` (replaces inline `strstr`/`strlen`/`malloc` IR). The regex ABI was extended to `(pattern, patternLen, text, textLen[, replacement, replacementLen])` across `include/ry/runtime_regex.hpp`, `src/runtime_regex.cpp`, `src/codegen_call_io.cpp`, and `src/codegen_call_string.cpp`.

**`markArcManaged(tmp)` pre-mark must be guarded by `fieldTypeIsArcManaged`, and `str` fields must also be inserted into `arc_str_managed_vars_`** (Source: #1016, updated #1046):
TuplePattern / RecordPattern / EnumConstructorPattern pre-mark a temporary alloca
(`tmp`) as ARC-managed so the recursive leaf `VariablePattern` binding can emit a
single retain via `tryRetainArcSource` Case 1. Without the guard, _any_ `ptrTy_`
field (including bare fn-ptr and resource ptrs) is incorrectly marked.
Fix (post-#1046): use `CollectionKind fk; if (fieldTypeIsArcManaged(elemSig, &fk)) { markArcManaged(tmp); if (fk == CollectionKind::Str) arc_str_managed_vars_.insert(tmp); }` at all three sites (`src/codegen_match.cpp`). `fieldTypeIsArcManaged` returns true for List/Map/Set/str and false for fn-ptr/resource/etc. The `arc_str_managed_vars_` insertion is required so `tryRetainArcSource` Case 1 dispatches to `emitStrGetHeaderFromData` (offset −24) instead of `emitArcGetHeaderFromData` (offset −16) for str fields. Capturing closure in tuple/record/enum fields is intentionally excluded: `fn_type_info` metadata is not propagated by `propagateTypeMeta` onto `ExtractValue` intermediates, so closure detection is impossible here; this was also the pre-#1008 behaviour.

### Thread-local HTTP error buffer is shared across tests in the same process

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: nul-safety, testing, http, thread-local

**Rule**: `http_last_error_buf` in `runtime_http_error.cpp` is `thread_local` and persists for
the lifetime of the thread. If test A sets an error message (e.g. "url contains embedded NUL")
and test B then performs an operation that fails but does *not* write to `http_last_error_buf`
(e.g. a connection refused), test B's `e.message` will still contain test A's stale message.

**How to apply**: Do not write spec tests that assert error message contents after a network
failure that may or may not set the buffer. For NUL-safety tests, assert only that the result
is `Err` and that `e.message` contains the expected string. Never assert the message is
*absent* across test boundaries.

