---
paths:
  - "share/std/**/*.ry"
  - "src/runtime_*.cpp"
  - "include/ry/runtime_*.hpp"
  - "src/codegen_call_*.cpp"
  - "CMakeLists.txt"
---

# Stdlib Module Additions

> **Terminology**: per `docs/reference/glossary.md` (introduced in v0.0.17 by #1480), the unit imported via `from xxx import ...` is called a **module**. The word "package" is reserved for the future `ry add` external-library feature. Older internal C++ identifiers (`effectivePackage`, `deriveNativePackage`, `RY_REGISTER_STDLIB_PACKAGE`, etc.) keep the legacy "package" naming for ABI/source-stability reasons; refer to those identifiers verbatim, but use "module" in prose.

### Bare `@native` vs `@native("mod")` — NOT cosmetically equivalent

**Source**: #907 / PR #901 review (+ self-test regression during fix)
**Tags**: stdlib, @native, codegen, native-library, review-feedback-patterns

The two forms differ in a critical way beyond codegen dispatch:

- `@native("mod")` sets `sig.library = "mod"`, which populates
  `native_lib_index_`. At runtime the JIT attempts to **dlopen
  `libry_<mod>.*`** based on this index.
- Bare `@native` leaves `sig.library` empty, so no library loading is
  triggered.

For dispatch key calculation (`effectivePackage`), both forms produce the
same key because `deriveNativePackage()` extracts the module name from
the file path regardless of the directive argument. So codegen dispatch
is identical.

**But**: if the module is **entirely** handled by a custom codegen
emitter (like `math`, whose functions are emitted as LLVM IR inline) and
has **no separate `libry_<mod>.dylib`**, using `@native("mod")` will
cause a runtime "native library not found" error.

**Rule**: Use bare `@native` (no argument) for stdlib modules whose
functions are all handled by custom codegen emitters and have no
separate shared library. Use `@native("mod")` only when the module
has a corresponding `libry_<mod>.*` shared library built via
`add_ry_native_lib()`.

### `Match` type is registered programmatically in codegen, not via a `record` declaration in regex.ry

**Source**: #830 (2026-04-14, implementation)
**Tags**: regex, record-type, codegen, stdlib-design

**Rule**: The `Match` type (`{full: str, groups: List<str>}`) used as the element type of
`findAll`'s return value is registered in `record_types_` inside the `CodeGen` constructor
(same pattern as `Error`), not declared as a `record` in `share/std/regex.ry`.

**Why**: If `Match` were declared in `regex.ry`, it would only be registered when a user
explicitly imports it (e.g., `from regex import Match, findAll`). Omitting `Match` from the
import would cause codegen to silently fail to find the type in `record_types_` when it tries
to tag the `findAll` result. By registering it unconditionally at construction time (same as
`Error`), users can write `from regex import findAll` without importing `Match` and still get
the fully-typed `List<Match>` result, including field access via `.full` and `.groups`.

**How to apply**: When a stdlib function returns a record type and that type must be available
regardless of explicit import, register it programmatically in the `CodeGen` constructor
alongside `Error`. If instead the type should only be available on explicit import (e.g., a
user-facing constructor), define it in the `.ry` file as a normal `record`.

### `@native` declarations support default-argument syntax directly — prefer over wrapper pattern

**Source**: #1412 (2026-04-29, implementation)
**Tags**: stdlib, @native, default-args, wrapper-pattern, codegen-call

**Rule**: When a stdlib function needs default arguments, declare them
directly on the `@native` line:

```ry
@native
fn startsWith(string: str, prefix: str, ignoreCase: bool = false) -> bool
```

The parser accepts default-value expressions on `@native fn` declarations
exactly as it does for ordinary `fn` declarations, and the codegen
dispatcher resolves both `f(a, b)` and `f(a, b, true)` to the same handler
based on `e.args.size()`.

**Why**: Earlier `share/std/str.ry` versions used a wrapper pattern —
declare an underscored `@native fn _starts_with(...)` that took every
argument explicitly, then a non-`@native` Ry wrapper
`fn startsWith(... = false): return _starts_with(...)`. This was redundant:
- The wrapper allocates a stack frame and forwards arguments verbatim, so
  it costs an extra non-inlinable Ry call vs. a direct C runtime call.
- It requires the C++ dispatcher to register both names (the public one
  AND the underscored alias).
- Under v0.0.16's stricter naming rules (PR #1411), the wrapper itself
  becomes a non-`@native` camelCase fn — currently rejected by the parser
  for non-`@native` declarations (#1417 will lift that). The wrapper
  pattern thus became outright unbuildable for camelCase exports.

**How to apply**:

- For new stdlib functions with default args, write a single `@native fn`
  declaration with the defaults inline.
- When porting existing wrapper-style functions, drop the underscored
  `_xxx` declaration and the public wrapper, and remove the matching
  `_xxx` alias from the C++ dispatch table (e.g., `dispatch` in
  `src/codegen_call_string.cpp`).
- The dispatcher handler must still validate
  `e.args.size() < min || e.args.size() > max` and supply the default
  value when the arg is omitted (existing handlers already do this; see
  `emitStrOp_starts_with` for the canonical pattern).

The earlier "Builtin native-underscore wrappers must update the builtin
dispatcher too" entry in `.claude/rules/codegen-stdlib-dispatcher.md`
(#1277) documents the wrapper pattern; that entry stays relevant for
**bare builtins** (declared in `share/std/builtins.ry`, intercepted in
`emitBuiltinCore`), where the underscored alias is sometimes still
required because of how the builtin dispatcher resolves names. For
ordinary stdlib modules (str / convert / json / regex / math / …),
prefer the direct-default form documented here.

**Carve-out (#1578): default-arg form only works for name-keyed
dispatchers, NOT for table-driven custom-emitter paths.**

The "single declaration with inline default" pattern above
(`emitStrOp_starts_with`-style) works because str's dispatcher
(`src/codegen_call_string.cpp`) keys on the **function name only** and
re-derives arity inside the handler from `e.args.size()`. The same is
true for any handler reached through name-keyed dispatch.

The math module uses a different path:
`emitTableDrivenNativeCall` (`src/codegen_call_native.cpp:191`) gates
the dispatch with a strict arity check —
`if (sig.params.size() == e.args.size())`. With a single
`@native fn digits(n: int, base: int = 10)` declaration, only the
2-param signature is registered in `native_fn_sigs_`, and a 1-arg call
site (`digits(0)`) silently fails the arity gate, falling through to
"undefined function: digits".

**Rule for table-driven custom emitters (math, and any future module
using the `*_table[]` + `customEmitter` pattern)**: declare **one
`@native fn` per arity**, mirroring `floor` / `ceil` / `pow` /
`round`:

```ry
@public @native fn digits(n: int) -> List<int>
@public @native fn digits(n: int, base: int) -> List<int>
```

The custom emitter (`emitMathDigits`) still handles both arities
internally — supplying a default `ConstantInt::get(i64Ty_, 10)` when
`e.args.size() == 1`. Only the *declaration side* needs the per-arity
duplication so each signature passes the arity gate.

**How to tell which dispatch path applies**:
- Look up the module's emitter in `src/codegen_call.cpp`. If the table
  entry has a non-null `customEmitter`, the call goes through
  `emitTableDrivenNativeCall` and the strict arity gate applies.
- If the emitter is a name-keyed dispatcher (str: `dispatch` map in
  `emitBuiltinString`), default-arg form is fine.
- When in doubt, write the test first: a unit test exercising every
  documented arity will catch the missing arity gate immediately.

