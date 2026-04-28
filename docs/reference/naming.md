# Naming Conventions

Ry uses two casing styles for identifiers: `camelCase` for runtime values and `PascalCase` for type-like declarations. This page is the single source of truth for the convention; per-feature pages link here instead of restating the rules.

**Scope.** This page covers casing rules and abbreviations. Two related conventions live elsewhere: the trailing `!` suffix that marks mutating functions is documented in [Functions](functions.md), and the leading `_` prefix that marks package-private names is documented in [Packages](packages.md).

## Overview

| Identifier | Casing | Examples |
|------------|--------|----------|
| Functions, methods | `camelCase` | `httpGet`, `parseJson`, `readText` |
| Local variables, parameters | `camelCase` | `userName`, `byteCount`, `nextId` |
| Record fields | `camelCase` | `firstName`, `responseCode`, `headerMap` |
| Records | `PascalCase` | `Point`, `HttpRequest`, `UserProfile` |
| Enums and enum variants | `PascalCase` | `Color`, `LogLevel`, `Red`, `Green` |
| Type aliases | `PascalCase` | `UserId`, `Bytes` |
| Built-in constructors (unchanged) | `PascalCase` | `Some`, `None`, `Ok`, `Err`, `Error` |

## Functions, variables, and fields

Names of functions, methods, local variables, parameters, and record fields use `camelCase`: the first word is lowercase and each subsequent word starts with an uppercase letter. Treat record fields as a kind of variable — they follow the same rule as ordinary bindings, not the type-name rule of their enclosing record.

Examples: `httpGet`, `parseJson`, `userName`, `byteCount`, `firstName`, `responseCode`.

## Records, enums, and type aliases

Names that introduce a type — `record` declarations, `enum` declarations (and the variants inside them), and type aliases — use `PascalCase`: every word starts with an uppercase letter and there are no separators.

Examples: `Point`, `HttpRequest`, `UserProfile`, `LogLevel`, `Red`, `Green`, `UserId`, `Bytes`.

## Built-in constructors

The standard library's variant constructors `Some`, `None`, `Ok`, `Err`, and `Error` were already `PascalCase` before v0.0.16 and are unchanged.

## Acronym rule

When an acronym appears inside a `camelCase` or `PascalCase` identifier, capitalize only its first letter. The rest of the acronym stays lowercase, so it visually behaves like any other word.

| Convention | Correct | Incorrect |
|------------|---------|-----------|
| `camelCase` | `httpGet`, `parseJson`, `urlEncode` | `HTTPGet`, `parseJSON`, `URLEncode` |
| `PascalCase` | `HttpRequest`, `JsonParser`, `UrlBuilder` | `HTTPRequest`, `JSONParser`, `URLBuilder` |

The reason is uniformity: a fully-capitalized acronym (`HTTPGet`) breaks the visual word boundary that `camelCase` relies on, leaving the next word ambiguous (`HTTPGet` vs. `HttpGet`). The first-letter-only rule keeps every identifier scannable left-to-right.

## Approved abbreviations

The following abbreviations are accepted across the language and standard library. Outside this table, prefer the full word.

| Abbreviation | Full form | Used for |
|--------------|-----------|----------|
| `args` | arguments | Function and CLI argument lists |
| `substr` | substring | String slicing helpers |
| `mkdir` | make directory | Filesystem package (POSIX-aligned) |
| `mkdirAll` | make directory (recursive) | Filesystem package |
| `ext` | extension | Path and filename helpers |
| `len` | length | Sequence size queries |
| `flat` | flatten | Collection helpers |
| `glob` | glob pattern | Filesystem helpers |

`length` is removed entirely in v0.0.16. Use `len` everywhere — there is no `length` alias and no deprecation period.

## Verbose-by-intent: `toInt`, `toStr`, ...

The conversion helpers `toInt`, `toStr`, `toFloat`, and `toBool` keep their `to`-prefix instead of becoming `int`, `str`, `float`, `bool`. The short forms collide with the type names of the same spelling, so `int(x)` could mean either "convert `x` to an int" or "the type `int` itself", and disambiguating would force the parser into context-sensitive lookup. Keeping the `to`-prefix removes the collision at the source.

The verbosity is intentional, not an oversight; do not propose `int` / `str` aliases.

## No ad-hoc abbreviations

Outside the approved table above, do not invent shortenings. Names like `cnt` (for `count`), `n` (for any meaningful integer), `buf` (for `buffer`), `idx` (for `index`), `tmp` (for `temporary`), and `cfg` (for `config`) are not accepted in any Ry code — the rule applies to user code as well as the standard library, matching the scope of the approved abbreviation table above.

The motivation is reading-time clarity. `count` reads correctly on the first pass; `cnt` requires the reader to expand it mentally. Multiplied across an entire standard library and the prompts that include slices of it, the cumulative cost is non-trivial.

## Rationale

Two reasons drove the move from `snake_case` to `camelCase` (functions, variables, fields) and a stricter `PascalCase` (records, enums, type aliases):

1. **AI tooling and token efficiency.** `camelCase` and `PascalCase` identifiers tokenize more compactly than `snake_case` in most modern LLM tokenizers, because the underscore frequently splits an identifier into multiple tokens. Compactness matters when prompts include large slices of stdlib source.
2. **Alignment with mainstream standard libraries.** JavaScript (`fetch`, `parseInt`, `Map`, `Promise`) and Go (`strings.Builder`, `http.Request`, `unicode/utf8`) consistently combine `camelCase` for callables with `PascalCase` for types. Python is the notable counterexample, mandating `snake_case` for callables, but Ry targets the same systems-level niche as Go and aligning with that stack reduces cognitive friction for users coming from it.

## Migration notes (v0.0.15 → v0.0.16)

This page describes the convention as of v0.0.16. Several other reference pages currently still describe the v0.0.15 compiler's behavior:

- `functions.md`, `records.md`, and `types.md` still describe the v0.0.15 rule that user-defined names must be `snake_case`.
- Those pages will be updated by the per-package rename PRs that ship under issue #1409 (sub-issues #1411 through #1417).

If a per-feature page disagrees with this page during the transition, **this page is authoritative for v0.0.16 and the per-feature page reflects the v0.0.15 status quo until its rename PR lands**. Compiler enforcement of the new casing flips with #1417.

There is no `snake_case → camelCase` alias layer. The change is a hard switch; old names are removed, not deprecated.
