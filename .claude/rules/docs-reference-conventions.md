---
paths:
  - "docs/reference/**/*.md"
  - "docs/**/*.md"
  - "README.md"
---

# Documentation Reference Conventions

### Example code in docs must match the current Ry syntax

**Source**: #825 PR review (CodeRabbit)
**Tags**: documentation, syntax

**Context**: #825 docs used `val x: i32 = 1` — the `val` keyword does
not exist in Ry; the actual syntax is `x: i32 = 1`. The snippet was
written from memory (probably by analogy to Scala/Kotlin) and never
verified.

**Rule**: When writing doc examples, don't rely on memory. Either
(a) copy a working example from an existing `tests/spec/*.test.ry`, or
(b) verify the snippet with `printf '...\n' | ./build/ry -c` before committing.

**How to verify**: in `docs/reference/` PRs, grep for keywords that
don't exist in Ry's lexer:

```bash
grep -nE '\bval\b|\bvar\b|\bpublic\b|\bprivate\b' docs/reference/*.md
```

### `@native` declarations can silently drift from their dispatcher implementation

**Source**: #889 docs audit
**Tags**: stdlib, native, codegen, drift

**Context**: During a docs audit, `docs/reference/collections.md` was
suspected of being wrong because `share/std/list.ry` declared
`remove_at(...) -> Unit` while the docs described a return value.
Investigation showed the opposite was true: `CodeGen::emitCollOp_remove_at`
(`src/codegen_call_collection.cpp`) actually returns the removed element,
and `tests/spec/collections.test.ry` has long asserted on it
(`v = remove_at(xs, 1); expect(v).to_eq(2)`). The `-> Unit` declaration
in `list.ry` was the bug — it had been ignored by the custom dispatcher
for so long that nobody noticed.

**Rule**: The declared signature in `share/std/*.ry` is **not** the
source of truth for `@native` functions that go through a hand-written
codegen dispatcher (`src/codegen_call_*.cpp`). The dispatcher can —
and for historical reasons does — produce a different shape than the
declaration. Before treating a declaration as spec, cross-check the
dispatcher and the tests.

**How to apply**:
- When docs disagree with a `@native` signature, diff all three: the
  declaration file, the dispatcher in `src/codegen_call_*.cpp`, and an
  existing test in `tests/spec/`. The test's assertions reveal the
  actually observable return shape.
- If the declaration is wrong, fix the declaration rather than the
  docs or the tests; the dispatcher/test pair is the de-facto contract.
- Consider this whenever you audit or regenerate stdlib signatures.

**Follow-up audit (#890)**: A systematic sweep of every `@native`
declaration in `share/std/**/*.ry` against its dispatcher uncovered two
more drifts of the same shape and fixed all three:

| Declaration | Was | Correct |
|---|---|---|
| `share/std/map.ry` `items(Map<str, int>)` | `List<int>` | `List<(str, int)>` |
| `share/std/builtins.ry` `enumerate(List<int>)` | `List<int>` | `List<(int, int)>` |
| `share/std/builtins.ry` `zip(List<int>, List<int>)` | `List<int>` | `List<(int, int)>` |

In all three the dispatcher (`src/codegen_call.cpp` `emitBuiltinQuery`
for `enumerate`/`zip`, `src/codegen_call_collection.cpp:925`
`emitCollOp_items` for `items`) builds an `llvm::StructType::get(ctx,
{T1, T2})` tuple and calls
`setTypeMeta(TypeMeta::ListElem, newHeader, tupleTy)`. Type inference at
the call site consults the dispatcher's `ListElem` metadata, not the
declaration's return type — which is exactly why these drifts survived
so long: the compiler silently ignores `pairs: List<int> = enumerate(xs)`
and lets you call `pairs[0].0` on it without complaint. The declaration
is dormant documentation, not a gate. **Watch for this drift class in
any `emitBuiltin*` helper that calls `setTypeMeta(TypeMeta::ListElem,
..., tupleTy)` where `tupleTy` is a `StructType`.**

**Audit scope limits**: Pattern C (generic fallback, e.g. `base64`,
`filesystem`, `gc` — `emitGenericNativeCall` infers wrapping from the
declaration's return type) cannot drift by construction and was skipped.
Pattern A1 entries (table-driven with `ReturnWrapping` enum but no
`customEmitter`, e.g. `io`) are validated against the sig's return type
and got only a spot check. The audit focused on Pattern A2 (table
entries with `customEmitter`, which bypass type checking per
`codegen_call_native.cpp:135-149`) and Pattern B (hand-written
`emitBuiltin*` helpers that never consult the sig). Those are the two
places where drift is actually possible.

**Polymorphism constraint**: Ry declaration syntax has no generics /
`List<T>` / `Any`, so polymorphic collection ops (`list::filter`,
`list::map`, `map::keys`, etc.) will keep using monomorphic placeholder
element types like `List<int>` even when the dispatcher is
element-type-agnostic. Only drifts where the **structural shape** is
wrong (`List<int>` vs `List<tuple>`, `Unit` vs `int`) are actionable
until generics land. Don't re-chase placeholder-only inaccuracies as
drift.

### For heterogeneous return types on custom-emitter natives, use `any` as the declaration placeholder

**Source**: #1135
**Tags**: `stdlib, @native, codegen, type-declaration, hygiene`

When a custom-emitter native can return multiple concrete types at runtime
(e.g. `thread_spawn` / `thread_join` which return `Unit`, `int`, `float`,
or `bool` depending on the worker body), the declaration in
`share/std/**/*.ry` cannot use a true generic — several options fail:

| Option | Why it fails |
|---|---|
| `<T>` generic | `src/codegen_fn.cpp:471-481` diverts any function with `type_params` into `generic_fn_templates_` and returns early — no `NativeFnSignature` is registered, breaking dispatch |
| Bare `T` at top-level | `src/codegen_type.cpp:160-162`: `resolveType` fails for unbound identifier |
| `fn() -> T` nested | Parser free-pass (interior not validated), but the identifier is semantically meaningless — fragile |
| Overloads differing only in return type | `src/codegen_fn.cpp:531-545` dedupes on param types only; the second declaration is silently dropped |

**Rule**: Use `any` as the hygienic placeholder. `any` resolves cleanly via
`src/codegen_type.cpp:23` at both top-level and generic argument positions
(e.g. `Result<any, Error>`, `fn() -> any`). The custom emitter still
drives actual runtime type dispatch through `TypeMeta` metadata — `any` in
the declaration is never consulted at codegen time.

**Precedent for `any` at generic argument position**: `tests/spec/any.test.ry`
uses `Map<str, any>` and `List<any>` in 30+ places; `src/parser_decl.cpp:159`
uses `any` as the default element type for inferred collections. `Result<any, Error>`
was introduced by #1135 as a new pattern, but the surrounding type machinery
already handled `any` as a generic argument.

**Annotation in the `.ry` file**: Add a top-of-file comment explaining the
`any` ↔ `T` spelling gap, e.g.:
```ry
# `any` in thread_spawn / thread_join declarations below stands in for `T`
# in docs/reference/thread.md. The runtime narrows the actual type to
# Unit / int / float / bool via TypeMeta::ThreadResult metadata.
```
This prevents future readers from "fixing" `any` back to `Unit`.

### Ry has soft keywords that are NOT in the lexer `keyword_map`

**Source**: #1118 PR 1 docs audit
**Tags**: documentation, syntax, lexer

**Context**: Auditing `docs/reference/types.md` found `weak` described as "a keyword".
`src/lexer.cpp` keyword_map contains only hard keywords. `weak` is handled in
`parser_expr.cpp:463` and `parser_decl.cpp:726` as a contextual identifier
(`t.kind == TokenKind::Ident && t.value == "weak"`). Other examples:
`None`, `Some`, `Ok`, `Err` (variant constructors), and `_` (wildcard) are
similarly soft — they are legal variable names and only gain special meaning
in specific syntactic positions.

**Rule**: When writing or auditing docs, do **not** call soft keywords "keywords".
Use "contextual identifier" instead, or describe the syntax without labeling the
token class.

**How to verify**: Check `src/lexer.cpp`'s `keyword_map` (lines 35–66). If the
token is not there, it is not a reserved keyword — the parser must be checking
`t.value == "..."` explicitly to give it special meaning.

### Low-level integer types have different arithmetic semantics from `int`

**Source**: #1118 PR 1 docs audit
**Tags**: documentation, types, operators

**Context**: `operators.md` described `/` as "always float, IEEE 754". This is
correct for high-level `int`/`float` operands, but for low-level integer types
(`i8`..`i64`, `u8`..`u64`) `/` performs **integer division** and returns the
same type (`7i32 / 2i32` → `3i32`). Mixing a low-level type with `int` in the
same expression is a compile-time type error.

**Rule**: When auditing or writing docs for arithmetic operators, always
spot-check behavior with low-level integer types using `./build/ry -c`:

```bash
printf 'print(7i32 / 2i32)\nprint(type_of(7i32 / 2i32))' | ./build/ry -c -
```

The pattern extends to `+`, `-`, `*`, `%`, `//` — all produce the same type
without promotion when both operands are low-level integers.

### `docs/reference/` directory paths in prose can silently drift when source layout changes

**Source**: #1118 PR 2 docs audit
**Tags**: documentation, drift, stdlib

**Context**: `docs/reference/directives.md` referred to `core/*.ry` as the location of
`@native` prelude declarations (e.g. `core/builtins.ry`, `core/str.ry`). The actual
layout has been `share/std/` for a long time (e.g. `share/std/builtins.ry`). No `core/`
directory exists. The prose survived because it was never executed — only read.

**Rule**: When auditing stdlib-adjacent docs (PR 3–5 of #1118), grep for hardcoded
directory names in prose and verify them against the actual filesystem:

```bash
grep -rn 'core/' docs/reference/
ls share/std/           # verify actual layout
```

If a table lists filenames or paths, run `ls` on the claimed directory before trusting it.

### CLI flag value sets can drift between doc pages independently

**Source**: #1118 PR 2 docs audit
**Tags**: documentation, drift, CLI

**Context**: `project.md` global options table listed `--env=<env>` as accepting
`production|development|internal` (3 values). `packages.md` RY_ENV table had
5 values (`prod`, `dev`, `test`, `staging`, `internal`) plus their aliases. The
authoritative source is `src/dotenv.cpp:resolveRyEnv` + `src/cli.cpp` help text.

**Rule**: When two doc pages both describe the same CLI flag or env var value set,
verify they are consistent with each other AND with the implementation. Quick check:

```bash
# Find all pages that mention --env or RY_ENV
grep -rn 'env\|RY_ENV' docs/reference/ | grep -v '\.env\.'
# Check implementation's documented values
grep -n 'env.*=.*\|' src/cli.cpp | head -10
```

### Runtime errors in stdlib dispatchers must be mirrored in docs/reference/

**Source**: #1118 PR 3 docs audit
**Tags**: documentation, drift, stdlib

**Context**: During the PR 3 audit of `docs/reference/{builtins,collections,math}.md`, several
runtime (and compile-time) errors emitted inside `src/codegen_call_*.cpp` were found to be
completely absent from the corresponding reference pages. Examples found:

| Error message | Emitter location |
|---|---|
| `runtime error: reduce() on empty list` | `codegen_call_higher_order.cpp:237` |
| `runtime error: min() on empty list` | `codegen_call_higher_order.cpp:480` |
| `runtime error: max() on empty list` | `codegen_call_higher_order.cpp:480` |
| `runtime error: floor(): cannot convert %g to int` (also `ceil()`, `round()`, `trunc()`) | `codegen_call.cpp` math path via `emitCheckedFPToInt` |
| `flatten() requires a list of lists` (compile) | `codegen_call_collection.cpp` |
| `fold() initial value type must match function return type` (compile) | `codegen_call_higher_order.cpp:291-294` |

**Rule**: When auditing stdlib docs (PR 3–5 of #1118), grep all `codegenError()` and
`runtime error:` strings in `src/codegen_call_*.cpp` and `src/runtime_*.cpp`, then verify
each is mentioned (at least as a brief note) in the matching docs page:

```bash
grep -rn '"runtime error:' src/codegen_call_*.cpp src/runtime_*.cpp
grep -rn 'codegenError' src/codegen_call_*.cpp | grep '"' | grep -v '//'
```

Cross-check each hit against the corresponding `docs/reference/*.md` page. Missing ones are α drift.

### Cross-check operator-level specs before flagging per-file code examples as drift

**Source**: #1118 PR 4 docs audit
**Tags**: documentation, audit, drift, operators

**Context**: During PR 4 audit of `docs/reference/base64.md`, a code example using
`?` at the top level was initially flagged as drift ("top-level `?` is a compile error").
However, `docs/reference/operators.md:165-177` explicitly documents that `?` and `!!` work
at the top level (Err maps to exit 1 with error message). The implementation in
`src/codegen_expr.cpp:1861-1895` confirms this with a dedicated top-level `?` desugar path.
The false alarm arose because only per-package docs were consulted, not the canonical
operator spec.

**Rule**: When a per-file doc example uses a language operator (`?`, `!!`, `case`, `@`
directives, etc.), do NOT judge it solely by analogy with how that operator appears in
fn bodies. Always:
1. Check `docs/reference/operators.md` for the full spec (including top-level usage rules).
2. Cross-check `src/codegen_expr.cpp` or the relevant codegen for the operator's desugar
   behavior in different contexts (function body vs. top level vs. lambda).

**How to verify**: For `?` operator specifically, the top-level desugar constraint is
`src/codegen_expr.cpp:1873-1874`: the `Err` type must be `Error` exactly.

### Regenerate enumerated doc tables from implementation, not by hand

**Source**: #1118 PR 5 docs audit
**Tags**: documentation, audit, drift, http, enumerated-tables

**Context**: `__ry_http_reason_phrase` in `src/runtime_http.cpp:639-696` had 50 `case` statements, but `docs/reference/http.md` listed only 38 status codes — 12 were silently missing (402, 407, 412, 418, 421, 425, 428, 431, 451, 507, 508, 511).

**Rule**: When the implementation contains an enumerated list (status code switch, kind string registry, format specifier table) that is mirrored in a documentation table, **regenerate the doc table from the implementation** rather than hand-editing. For HTTP status codes specifically:

```bash
grep -nE '^\s*case [0-9]+:' src/runtime_http.cpp
```

gives the authoritative list; compare it against the `docs/reference/http.md` table before declaring clean. Apply the same pattern to any other enumerated registry (error kind strings, format specifiers, etc.).

**How to verify**: Count `case` statements in the switch vs. rows in the doc table. A mismatch always signals drift.

### Canonical-source pattern for deduplicating reference doc sections

**Source**: #1125 (2026-04-18)
**Tags**: documentation, dedup, drift, builtins, collections

**Rule**: When two reference pages redundantly describe the same function with full sections (signature + description + examples), pick one as canonical and reduce the other to a stub (`heading + signature + "See also" link`), not parallel bodies. Link stubs to per-function anchors where they exist; fall back to the parent section anchor when heading punctuation (e.g. `!`) would collide with an existing anchor after GitHub slugification (GitHub strips `!`, so `## sort!` → `#sort` which collides with `## sort`). Precedent: `docs/reference/functions.md:778`.

**Context**: `builtins.md` and `collections.md` both fully described `filter`, `map`, `sort`, `sort!`, `reverse!`, `appended`, `append!` (~200 lines of drift-prone parallel content). After #1125, `collections.md` is canonical; `builtins.md` sections are stubs. The quick-reference table in `builtins.md` stays (it is a useful at-a-glance index) with an umbrella note under the section header pointing to the canonical page.

**Anchor collision detail**: Bang variants (`sort!`, `reverse!`, `append!`) must link to the parent section (e.g. `#in-place-mutating-variants`) rather than a per-function anchor, because GitHub slugifies `### sort!` to `#sort` — identical to the slug for `### sort`. Stubs for these functions must not link to `#sort!` or similar; the parent section anchor is the correct fallback.
