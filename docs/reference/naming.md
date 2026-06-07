# Naming Conventions

Ry uses two casing styles for identifiers: `camelCase` for runtime values and `PascalCase` for type-like declarations. This page is the single source of truth for the convention; per-feature pages link here instead of restating the rules.

**Scope.** This page covers casing rules and abbreviations. The trailing `!` suffix that marks mutating functions is documented in [Functions](functions.md). Visibility (package-internal default vs. `@public` for cross-package access) is documented in [Modules — Visibility](modules.md#visibility); the leading `_` prefix carries no visibility meaning and is purely a stylistic convention.

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
| `mkdir` | make directory | Filesystem module (POSIX-aligned) |
| `mkdirAll` | make directory (recursive) | Filesystem module |
| `ext` | extension | Path and filename helpers |
| `len` | length | Sequence size queries |
| `flat` | flatten | Collection helpers |
| `glob` | glob pattern | Filesystem helpers |

`length` is removed entirely in v0.0.16. Use `len` everywhere — there is no `length` alias and no deprecation period.

## Conversion functions: `int`, `float`, `str`

The conversion helpers are spelled `int`, `float`, and `str` — the same as the type names. `int("1")` / `float("3.14")` parse a string and return `Result`; `str(v)` renders any value to its string form. (Earlier versions spelled these `toInt` / `toFloat` / `toStr`; those names were removed.)

Sharing the spelling with the type names is unambiguous in practice: a type annotation (`x: int = ...`) and a call expression (`int(x)`) are parsed by separate grammar productions, and a bare `int` in expression position has no value meaning of its own (it resolves to the conversion function), so the parser never has to guess. The one accepted trade-off is forward-looking: if Ry ever makes a bare type name a first-class value (so that `typeOf(x) == int` becomes legal), `int(x)` would stay the special-cased conversion call rather than "construct from the type value `int`", and an indirect `f = int; f(x)` could diverge from the direct `int(x)`. That minor cost is judged acceptable in exchange for the shorter, more familiar spelling.

For number-to-number and number-to-string conversions, use the `as` cast (`3.14 as int`, `n as str`) — `int` and `float` only parse strings.

## No ad-hoc abbreviations

Outside the approved table above, do not invent shortenings. Names like `cnt` (for `count`), `n` (for any meaningful integer), `buf` (for `buffer`), `idx` (for `index`), `tmp` (for `temporary`), and `cfg` (for `config`) are not accepted in any Ry code — the rule applies to user code as well as the standard library, matching the scope of the approved abbreviation table above.

The motivation is reading-time clarity. `count` reads correctly on the first pass; `cnt` requires the reader to expand it mentally. Multiplied across an entire standard library and the prompts that include slices of it, the cumulative cost is non-trivial.

## Rationale

Two reasons drove the move from `snake_case` to `camelCase` (functions, variables, fields) and a stricter `PascalCase` (records, enums, type aliases):

1. **AI tooling and token efficiency.** `camelCase` and `PascalCase` identifiers tokenize more compactly than `snake_case` in most modern LLM tokenizers, because the underscore frequently splits an identifier into multiple tokens. Compactness matters when prompts include large slices of stdlib source.
2. **Alignment with mainstream standard libraries.** JavaScript (`fetch`, `parseInt`, `Map`, `Promise`) and Go (`strings.Builder`, `http.Request`, `unicode/utf8`) consistently combine `camelCase` for callables with `PascalCase` for types. Python is the notable counterexample, mandating `snake_case` for callables, but Ry targets the same systems-level niche as Go and aligning with that stack reduces cognitive friction for users coming from it.

## Migration notes (v0.0.15 → v0.0.16)

This page describes the convention as of v0.0.16. The compiler enforces the new casing rules for all user-defined identifiers as of issue #1443 — fn names, parameters, record fields, enum variant fields, loop variables, and `@directive` names/parameters all require `camelCase`.

Three follow-up issues extended the enforcement to the remaining gaps: lambda parameters such as `(myX, myY) => ...` (issue #1449); tuple-destructure LHS identifiers in both the parenthesized form `(a, b) = expr` and the bare form `a, b = expr` (issue #1450, with the `_` placeholder still accepted at any position); and module-global typed declarations of the keywordless implicit-binding form `name: Type = value` (issue #1470, with `SCREAMING_SNAKE_CASE` still accepted on `@native` and `@const` declarations).

The builtins rename (issue #1411) renamed `length` → `len`, `arguments` → `args`, and `available_parallelism` → `availableParallelism`. `print`, `input`, `range`, `zip`, `exit`, `sleep`, `env`, and `enumerate` keep their existing names (`enumerate` cannot be shortened because `enum` is a reserved keyword).

There is no `snake_case → camelCase` alias layer. The change is a hard switch; old names are removed, not deprecated.
