# Visibility Guide

This guide explains how Ry decides which definitions can be imported from where, and how to organize symbols within a package.

For the formal specification see [Modules — Visibility](../reference/modules.md#visibility) and [Glossary — Visibility scopes](../reference/glossary.md#visibility-scopes); for the directive itself see [Directives — `@public`](../reference/directives.md#public).

## Overview

Ry's visibility model has two levels:

| Level | Marker | Visible from |
|---|---|---|
| **package-internal** (default) | none | the same package only |
| **public** | `@public` | any package (universe scope) |

There is no third level — every definition is either package-internal or `@public`. Designing a multi-file package therefore reduces to one question per definition: should this be importable from outside the package?

## Default: package-internal

A definition with no `@public` marker is visible only inside its **package** — the directory tree rooted at the nearest ancestor directory containing a `package.toml` file. Two source files belong to the same package when they share the same package root.

```text
mylib/
  package.toml
  calc.ry        # fn add() — package-internal
  helpers.ry     # fn fmt() — package-internal
```

```ry
# mylib/calc.ry
from .helpers import fmt   # OK — same package

fn add(a: int, b: int) -> int:
    return a + b
```

```ry
# mylib/helpers.ry
fn fmt(n: int) -> str:
    return "value: " + toStr(n)
```

Any importer **outside** `mylib/` cannot see `add` or `fmt` until they are marked `@public`.

Files that have no `package.toml` ancestor — for example, ad-hoc scripts run with `./build/ry script.ry`, or expressions passed via REPL `-c` — share a single **anonymous package**. They all behave as one package for visibility purposes, so a script can freely import any non-`@public` definition from another script in the same anonymous-package universe.

## Making a definition public

Apply `@public` on its own line immediately above the declaration. Multiple directives can be stacked.

```ry
# mylib/calc.ry
@public
fn add(a: int, b: int) -> int:
    return a + b

@public
record Point:
    x: int
    y: int

@public
@const
PI = 3.14159
```

`@public` is accepted on functions, records, enums, type aliases, variable declarations (with or without `@const`), and `@directive` declarations.

### Cross-package import behavior

Suppose `mylib/calc.ry` defines one `@public` function and one package-internal helper:

```ry
# mylib/calc.ry
@public
fn add(a: int, b: int) -> int:
    return a + b

fn helper(n: int) -> int:    # no @public — package-internal
    return n * 2
```

From outside the `mylib/` package:

| Import | Result |
|---|---|
| `from mylib import add` | OK — `add` is `@public` |
| `from mylib import helper` | Compile error — `'helper' is not @public` |
| `from mylib` (wildcard) | Imports `add` only; `helper` is silently filtered |

The error vs. silent filter distinction is intentional: a named import expresses intent, so importing a non-`@public` symbol by name is treated as a mistake. A wildcard expresses "give me everything you make available," so non-`@public` symbols are simply omitted from "everything."

From **inside** the same package, both `add` and `helper` are importable regardless of `@public`.

## Cross-file private helpers (currently unsupported)

> **Status (v0.0.17):** the wrapper pattern shown below is **not yet implemented**. Tracking issue: [#1560](https://github.com/t0k0sh1/ry/issues/1560).

In a multi-file package, you might want a sub-module to keep a helper hidden while a parent module exposes a thin `@public` facade that calls through. The intent is that external callers can use `bas()` but cannot reach `xxx()`:

```text
foo/
  package.toml
  foo.ry         # @public fn bas() — facade
  bar.ry         # fn xxx()          — internal helper
```

```ry
# foo/foo.ry
from foo.bar import xxx

@public
fn bas() -> int:
    return xxx()
```

This pattern looks reasonable, but `from foo import bas` from another package fails with `undefined function: xxx`. The Ry compiler resolves all symbols within a single linkage unit, and the cross-package import filter omits non-`@public` symbols — so `bas()`'s body cannot find `xxx` at code-gen time. The same failure occurs with `from .bar import xxx` (relative), with helper and facade in the same `.ry` file, and with `from foo` wildcard. **Today there is no layout that exposes `bas` while hiding `xxx` across a package boundary.**

**Workarounds until #1560 is resolved:**

1. **Mark the helper `@public` too**, accepting that external code can call it directly. Signal "internal use" by naming/comments rather than the type system.
2. **Inline the helper into the facade function** when the body is small enough to avoid extracting a helper.

(Note that the same-package wildcard import — `from .other` from another file in the same package — is the only multi-file scenario in which a `@public` facade can call a non-`@public` helper today. Selective imports such as `from .other import name` drop the helper from the importer's program even within the same package.)

Records, enums, and type aliases face the same restriction by a different mechanism — only values, not type names, can flow through a function call, so a wrapper function cannot re-export a type even after #1560 is fixed. Types intended to cross package boundaries must be declared directly on a public-facing module with `@public`.

## `_` prefix has no visibility meaning

Earlier versions of Ry treated a leading underscore (`_helper`) as a private marker. As of v0.0.17 this is no longer true — the prefix is purely a naming convention. Visibility is controlled exclusively by `@public`.

See [Naming Conventions](../reference/naming.md) for current style guidance.

## Standard library is a single package

The entire `share/std/` tree is **one** package — `share/std/package.toml` is its package root. Every stdlib sub-module (`math`, `io`, `path`, `filesystem`, `regex`, `thread`, …) lives inside that single package boundary.

Two consequences follow:

1. **From user code**, only stdlib symbols marked `@public` are importable. `from math import sqrt` resolves to a `@public` symbol that the stdlib package chooses to expose. Non-`@public` helpers inside the stdlib (for instance, internal implementation files under `share/std/runtime_internal/` or under `share/std/core/`) remain package-internal and are invisible to importers.
2. **Inside the stdlib itself**, stdlib modules can freely call each other's package-internal helpers. This keeps maintenance ergonomics close to a single-codebase project even though the stdlib spans many sub-modules.

If the stdlib is ever split into independently-released packages (a future possibility tied to the planned `ry add` system), each piece would gain its own `package.toml` and the cross-stdlib helper calls would need explicit `@public` markers.
