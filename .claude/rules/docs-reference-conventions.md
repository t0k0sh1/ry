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

**Rule**: Don't write doc examples from memory. Either (a) copy a working example from `tests/spec/*.test.ry`, or (b) verify the snippet with `printf '...\n' | ./build/ry -c` before committing.

**How to verify**: grep for non-Ry keywords (the `val`-from-Scala/Kotlin trap):
```bash
grep -nE '\bval\b|\bvar\b|\bpublic\b|\bprivate\b' docs/reference/*.md
```

### `@native` declarations can silently drift from their dispatcher implementation

**Source**: #889 / #890 docs audit
**Tags**: stdlib, native, codegen, drift

**Rule**: The declared signature in `share/std/*.ry` is **not** the source of truth for `@native` functions that go through a hand-written codegen dispatcher (`src/codegen_call_*.cpp`) or a custom emitter. The dispatcher can — and historically does — produce a different shape than the declaration; type inference at the call site consults dispatcher metadata (`setTypeMeta(TypeMeta::ListElem, …)`), not the declared return type.

**How to apply**:
- When docs disagree with a `@native` signature, diff all three: declaration / dispatcher / `tests/spec/`. The test's assertions reveal the actually observable shape.
- If the declaration is wrong, fix the declaration (the dispatcher/test pair is the de-facto contract).
- Watch for drift in any `emitBuiltin*` helper that calls `setTypeMeta(TypeMeta::ListElem, …, tupleTy)` where `tupleTy` is a `StructType`.

**Audit scope** (#890 sweep): drift is possible only in **Pattern A2** (table entries with `customEmitter`, bypassing type checking per `codegen_call_native.cpp:135-149`) and **Pattern B** (hand-written `emitBuiltin*` helpers that never consult the sig). Pattern C (`emitGenericNativeCall`, infers wrapping from the declaration) cannot drift by construction. Pattern A1 (table-driven w/ `ReturnWrapping` enum, no custom emitter) is validated against the sig. Drifts found and fixed: `map.items` (`List<int>` → `List<(str, int)>`), `enumerate` / `zip` (same shape).

**Polymorphism caveat**: Ry has no `List<T>` / `Any` in declaration syntax, so polymorphic ops (`list::filter`, `map::keys`) keep monomorphic placeholder element types. Only **structural-shape** drift (`List<int>` vs `List<tuple>`, `Unit` vs `int`) is actionable until generics land — don't chase placeholder-only inaccuracies.

### For heterogeneous return types on custom-emitter natives, use `any` as the declaration placeholder

**Source**: #1135
**Tags**: stdlib, @native, codegen, type-declaration, hygiene

**Rule**: When a custom-emitter native returns multiple concrete types at runtime (e.g. `threadSpawn` / `threadJoin` returning `Unit`/`int`/`float`/`bool`), use `any` in the declaration. `any` resolves cleanly via `src/codegen_type.cpp:23` at both top-level and generic-arg positions (`Result<any, Error>`, `fn() -> any`). The custom emitter still drives runtime dispatch through `TypeMeta` — `any` is never consulted at codegen time.

**Why other options fail**:
| Option | Why it fails |
|---|---|
| `<T>` generic | `src/codegen_fn.cpp:471-481` diverts to `generic_fn_templates_` and returns early — no `NativeFnSignature` registered |
| Bare `T` at top-level | `src/codegen_type.cpp:160-162`: `resolveType` fails for unbound identifier |
| `fn() -> T` nested | parser free-pass but semantically meaningless |
| Overloads differing only in return type | `src/codegen_fn.cpp:531-545` dedupes on param types only; second decl silently dropped |

**Annotation**: add a top-of-file comment in the `.ry` file so future readers don't "fix" `any` back to `Unit`:
```ry
# `any` here stands in for `T`; runtime narrows via TypeMeta::ThreadResult.
```

### Ry has soft keywords that are NOT in the lexer `keyword_map`

**Source**: #1118 PR 1
**Tags**: documentation, syntax, lexer

**Rule**: `weak`, `None`, `Some`, `Ok`, `Err`, `_` are **contextual identifiers**, not reserved keywords. The lexer's `keyword_map` (`src/lexer.cpp:35-66`) contains only hard keywords; soft ones are matched by `t.kind == TokenKind::Ident && t.value == "..."` in the parser. When auditing or writing docs, use "contextual identifier", not "keyword". Verify by grepping `keyword_map` — if absent, it's not reserved.

### Low-level integer types have different arithmetic semantics from `int`

**Source**: #1118 PR 1
**Tags**: documentation, types, operators

**Rule**: For low-level int types (`i8..i64`, `u8..u64`), `/` performs **integer division** and returns the same type (`7i32 / 2i32` → `3i32`); `+`, `-`, `*`, `%`, `//` similarly produce the same type without promotion. Mixing with `int` is a compile-time type error. The "always float / IEEE 754" description applies only to high-level `int`/`float`. Spot-check with:
```bash
printf 'print(7i32 / 2i32)\nprint(typeOf(7i32 / 2i32))' | ./build/ry -c -
```

### `docs/reference/` directory paths in prose can silently drift when source layout changes

**Source**: #1118 PR 2
**Tags**: documentation, drift, stdlib

**Rule**: Hardcoded directory names in prose (e.g. `core/*.ry`) drift when the source layout changes (actual: `share/std/`). Prose is read but never executed — drift is invisible until audit. When auditing stdlib-adjacent docs, grep + `ls`:
```bash
grep -rn 'core/' docs/reference/
ls share/std/
```

### CLI flag value sets can drift between doc pages independently

**Source**: #1118 PR 2
**Tags**: documentation, drift, CLI

**Rule**: When two doc pages describe the same CLI flag or env var value set (e.g. `project.md` listed 3 values for `--env`, `packages.md` listed 5 for `RY_ENV`), verify they match each other AND the implementation (`src/dotenv.cpp:resolveRyEnv` + `src/cli.cpp` help text).

### Runtime errors in stdlib dispatchers must be mirrored in docs/reference/

**Source**: #1118 PR 3
**Tags**: documentation, drift, stdlib

**Rule**: Many `codegenError()` and `runtime error: ...` strings in `src/codegen_call_*.cpp` are missing from corresponding `docs/reference/` pages (e.g. `reduce()/min()/max() on empty list`, `floor(): cannot convert %g to int`, `flatten() requires a list of lists`, `fold() initial value type must match...`). When auditing stdlib docs:
```bash
grep -rn '"runtime error:' src/codegen_call_*.cpp src/runtime_*.cpp
grep -rn 'codegenError' src/codegen_call_*.cpp | grep '"' | grep -v '//'
```
Cross-check each hit against the matching docs page.

### Regenerate enumerated doc tables from implementation, not by hand

**Source**: #1118 PR 5
**Tags**: documentation, audit, drift, http, enumerated-tables

**Rule**: When the implementation has an enumerated list (status code switch, kind string registry, format specifier table) mirrored in docs, **regenerate the table from the implementation**. `__ry_http_reason_phrase` had 50 cases; docs listed 38 (12 silently missing). For HTTP:
```bash
grep -nE '^\s*case [0-9]+:' src/runtime_http.cpp
```
Apply the same to any other enumerated registry. Mismatched count of `case` vs doc rows always signals drift.

### Canonical-source pattern for deduplicating reference doc sections

**Source**: #1125
**Tags**: documentation, dedup, drift, builtins, collections

**Rule**: When two reference pages describe the same function with full sections (signature + description + examples), pick one as canonical and reduce the other to a stub (`heading + signature + "See also" link`). Never parallel bodies. Precedent: `docs/reference/functions.md:778`; `collections.md` is canonical for `filter`/`map`/`sort`/`sort!`/`reverse!`/`appended`/`append!`, with `builtins.md` as stubs.

**Anchor collision**: bang variants (`sort!`, `reverse!`, `append!`) must link to the parent section anchor (e.g. `#in-place-mutating-variants`), not a per-function anchor. GitHub slugifies `### sort!` to `#sort` — collides with `### sort`. Don't link to `#sort!`; the parent section is the correct fallback.

### Doc-wide identifier migrations need multi-pattern sweeps, not just `fn` declarations

**Source**: #1444
**Tags**: documentation, migration, audit, naming, sweep

**Rule**: Snake_case → camelCase (or any rename) sweeps must run **multiple complementary greps**. A `fn xxx_yyy` grep alone misses (a) variable assignments / type ascriptions, (b) record fields and `require/ensure` locals, (c) function-call expressions in prose, (d) inline-code metalanguage placeholders (`<fn_name>`, `param_type1`), (e) error-kind table example columns.

```bash
# Pattern 1: fn declarations
grep -nE 'fn [a-z]+_[a-z]+\b' docs/reference/*.md
# Pattern 2: variable assignments / type ascriptions at line start
grep -nE '^\s*[a-z][a-zA-Z0-9]*_[a-z][a-zA-Z0-9_]*\s*[:=]' docs/reference/*.md
# Pattern 3: any snake_case in code-like positions (broadest, noisy)
grep -nE '\b[a-z][a-zA-Z0-9]*_[a-z][a-zA-Z0-9_]*\b' docs/reference/*.md
```

Pattern 3 needs aggressive false-positive filtering: file paths (`hello_copy.txt`), C++ binaries (`ry_tests`), env vars (`RY_ENV`), npm conventions (`node_modules`), ARC prose (`strong_count`/`weak_count`), HTTP form keys (`content_type`/`session_id`), historical-rename log in `naming.md`. Build the filter incrementally.

→ See `/horizontal-sweep` for the integrated 4-step procedure that builds on this rule.

### Cross-check inline-code placeholders against the implementation, not just the doc-wide rename pattern

**Source**: #1463 review (CodeRabbit)
**Tags**: documentation, migration, naming, c-runtime, placeholder

**Rule**: When a doc-wide rename touches a placeholder describing a contract with code outside the Ry source (C symbol names, `manifest.json` keys, JSON field names, env vars, generated artefact names), do not assume the rename applies. The C runtime symbols are mixed: `filesystem`/`path`/`gc`/`json` are camelCase (`__ry_filesystem_listDir`), but `base64`/`string` keep snake_case in the C ABI regardless of the Ry name (`base64::encodeUrlSafe` → `__ry_base64_encode_url_safe`).

```bash
grep -nE '__ry_[a-z]+_[a-zA-Z_]+' src/runtime_*.cpp | head -30
grep -nE '__ry_[a-z]+_[a-z]+_[a-z]' src/runtime_*.cpp  # snake_case after prefix
```

If reality is mixed, replace the single-form placeholder (`<functionName>` / `<fn_name>`) with a neutral term like `<symbol>` and document the carve-outs with an example mapping. Applies in both rename directions.

### Migration notes sections drift when a release rolls out through multiple incremental issues

**Source**: #1474
**Tags**: documentation, drift, migration-notes, naming, release-rollout

**Rule**: When a release rolls out through follow-up issues sharing a root (e.g. v0.0.16 camelCase: #1443 → #1449 / #1450 / #1470), update the corresponding Migration notes in `docs/reference/*.md` **in the same PR as each follow-up**, not a "doc cleanup at the end" PR. Reviewers should treat a follow-up enforcement PR with no `docs/reference/*.md` diff as suspicious.

```bash
grep -rn '#1443' docs/reference/   # before opening a follow-up PR
```

Applies symmetrically to other rolling rollouts (stdlib rename waves, error-message format changes, etc.). Any time `[Unreleased]` accumulates multiple `(#NNNN)` entries sharing a theme, check the matching `docs/reference/*.md` prose.

### Terminology sweeps must include lexical derivatives, not just the canonical word

**Source**: #1482
**Tags**: documentation, migration, audit, terminology, sweep

**Rule**: For terminology sweeps (renaming a concept), the canonical-word grep (`[Pp]ackage`) is necessary but not sufficient — derivatives (`pkg`, `pkgs`) survive a strict canonical grep. The "package" → "module" sweep missed `pkg/mod.ry` and `from pkg import directiveName` placeholders in `directives.md`.

| Canonical | Common derivatives |
|-----------|--------------------|
| `package` | `pkg`, `pkgs` |
| `arguments` | `args`, `argv` |
| `directory` | `dir`, `dirs` |
| `function` | `fn`, `func` |
| `parameter` | `param`, `params` |
| `extension` | `ext` |
| `length` | `len` |
| `substring` | `substr` |
| `temporary` | `tmp` |
| `configuration` | `config`, `cfg` |

Approved abbreviations for Ry are listed in `docs/reference/naming.md` "Approved abbreviations".

**How to apply**: After the canonical-word sweep returns zero, run an OR-pattern grep (`grep -nE '[Pp]kg|[Pp]ackage' docs/reference/*.md`) until that also returns zero (or only intentional carve-outs like `package.toml`). Be especially careful with `<...>`-style metalanguage placeholders (`<pkg>/mod.ry`) — they don't show up in code-syntax greps. Complements the multi-pattern sweep rule above: identifier migrations are about code-shape patterns; terminology migrations are about lexical equivalences.

→ See `/horizontal-sweep` for the integrated 4-step procedure that builds on this rule.

### Multi-file design intent must be empirically verified before documenting

**Source**: #1547 / #1560
**Tags**: documentation, design-vs-implementation, verification, multi-file, package

**Rule**: When documenting a feature whose behavior depends on **multi-file or multi-package layout** (visibility, module resolution, package boundaries, anonymous packages, `RY_PATH`, relative imports, directory modules), do not trust the design spec or issue requirements alone. Construct a minimal multi-file repro under `/tmp/` matching the docs and run `./build/ry` end-to-end. Single-snippet `./build/ry -c` is necessary but not sufficient — failures often depend on cross-package import behavior.

**How to apply**:
- Use `[project]` in `package.toml` (not `[package]`) — see `share/std/package.toml`.
- Test all import shapes: selective (`from foo import x`), wildcard (`from foo`), relative (`from .foo`), cross-package vs same-package callers. As of v0.0.17, selective imports drop unselected helpers even within the same package; wildcard imports across packages filter non-`@public` symbols silently.
- If documented behavior diverges from observed, file an issue (`/scope-out-issue`), link it, and revise the docs to describe what works today rather than the design spec.

**Origin**: while writing `docs/guide/visibility.md` for #1547, the planned wrapper pattern (REQ-B3 from #1543) was documented straight from design intent — self-verification with actual `.ry` files revealed it doesn't work because the compiler resolves all symbols in a single linkage unit and cross-package imports drop non-`@public` helpers before code-gen. Tracked in #1560.
