---
paths:
  - "docs/**/*.md"
  - "README.md"
---

# Documentation Reference Conventions

### `@native` declarations can silently drift from their dispatcher implementation

**Tags**: stdlib, native, codegen, drift

`share/std/*.ry` declarations are **not** the source of truth for `@native` functions. The hand-written dispatcher in `src/codegen_call_*.cpp` determines the actual return shape, and type inference reads `ListElem` metadata from the dispatcher — not the declaration. Inaccuracies in the declaration are silently ignored by the compiler.

Confirmed structural drift from #890:

| Declaration | Was | Correct |
|---|---|---|
| `share/std/map.ry` `items(Map<str, int>)` | `List<int>` | `List<(str, int)>` |
| `share/std/builtins.ry` `enumerate(List<int>)` | `List<int>` | `List<(int, int)>` |
| `share/std/builtins.ry` `zip(List<int>, List<int>)` | `List<int>` | `List<(int, int)>` |

All three dispatchers called `llvm::StructType::get(ctx, {T1, T2})` and `setTypeMeta(TypeMeta::ListElem, newHeader, tupleTy)`. Any `emitBuiltin*` helper that calls `setTypeMeta(TypeMeta::ListElem, ..., tupleTy)` is a candidate for this drift class.

Drift-capable patterns: Pattern A2 (table entry with `customEmitter`, bypasses type-checking at `codegen_call_native.cpp:135-149`) and Pattern B (hand-written `emitBuiltin*`, does not consult the sig). Pattern C (generic fallback via `emitGenericNativeCall`, infers wrapping from the declaration's return type) is structurally immune.

Polymorphism constraint: because declarations have no generics / `Any`, element-type-agnostic ops (e.g. `list::filter`) carry monomorphic placeholders like `List<int>`. Only structural drift (`List<int>` vs `List<tuple>`, `Unit` vs `int`) is actionable; placeholder-only inaccuracies are deferred until generics are implemented.

When a doc disagrees with a `@native` declaration, diff the declaration, the dispatcher (`src/codegen_call_*.cpp`), and the tests (`tests/spec/`). The dispatcher/test pair is the effective contract; fix the declaration if it is wrong.

### For heterogeneous return types on custom-emitter natives, use `any` as the declaration placeholder

**Tags**: `stdlib, @native, codegen, type-declaration, hygiene`

When a custom-emitter native returns multiple concrete types (e.g. `threadSpawn`/`threadJoin` returning `Unit`/`int`/`float`/`bool` depending on the worker body), the following alternatives all break:

| Option | Why it fails |
|---|---|
| `<T>` generic | `src/codegen_fn.cpp:471-481` branches on `type_params` into `generic_fn_templates_`, returns early — `NativeFnSignature` never registered, dispatch impossible |
| Bare `T` at top-level | `src/codegen_type.cpp:160-162`: `resolveType` fails on unbound identifier |
| `fn() -> T` nested | Parser passes (internals unverified) but is meaningless — fragile |
| Overloads differing only in return type | `src/codegen_fn.cpp:531-545` deduplicates on param types — the second entry is silently dropped |

Use `any` as the hygienic placeholder. `any` resolves cleanly at `src/codegen_type.cpp:23` in both top-level and generic-argument position (`Result<any, Error>`, `fn() -> any`). The custom emitter drives actual type dispatch through `TypeMeta` metadata — the `any` declaration is not consulted at codegen time.

Add a comment at the top of the `.ry` file explaining the `any` ↔ `T` gap to prevent future readers from "correcting" `any` to a concrete type.

### Ry has soft keywords that are NOT in the lexer `keyword_map`

**Tags**: documentation, syntax, lexer

`weak`, `None`, `Some`, `Ok`, `Err`, and `_` are absent from the `keyword_map` in `src/lexer/lexer.cpp`. `weak` is handled as a contextual identifier at `src/parser/parser_expr.cpp:463` and `src/parser/parser_decl.cpp:726` via `t.kind == TokenKind::Ident && t.value == "weak"`.

Do not call these "keywords." Use "contextual identifier," or describe the syntax without naming the token class. Anything absent from `keyword_map` (lines 35–66) is not a reserved keyword.

### Multi-file design intent must be empirically verified before documenting

**Tags**: documentation, design-vs-implementation, verification, multi-file, package

When documenting features that depend on multi-file or multi-package layout (visibility, module resolution, package boundary, anonymous package, `RY_PATH` search, relative import, directory module, etc.), do not trust design specs or issue requirements alone. Build a minimal multi-file reproduction under `/tmp/` that matches the documented layout and run `./build/ry` end-to-end before publishing.

Background: `docs/guide/visibility.md` documented a wrapper pattern (helper hidden in a sub-module, thin `@public` facade exposed) directly from the design spec. Building the actual multi-file layout under `/tmp/` revealed it does not work — the compiler resolves all symbols in a single linkage unit and cross-package import filtering drops non-`@public` helpers from the importer's program before codegen. The bug was undetectable with a single-snippet `./build/ry -c`. Implementation gap tracked in #1560.

Test every import form the doc describes: selective (`from foo import x`), wildcard (`from foo`), relative (`.foo`), cross-package vs same-package caller. As of v0.0.17, selective import drops unselected helpers even within the same package, and wildcard cross-package silently filters non-`@public` symbols. If documented behavior diverges from observation, apply `/triage-side-finding`. Do not ship aspirational examples in user-facing guides. A single-snippet `./build/ry -c` is insufficient for multi-file scenarios.
