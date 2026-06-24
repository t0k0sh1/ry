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
    return "value: " + str(n)
```

Any importer **outside** `mylib/` cannot see `add` or `fmt` until they are marked `@public`.

Files that have no `package.toml` ancestor — for example, ad-hoc scripts run with `<build-dir>/ry run script.ry`, or code piped via `<build-dir>/ry -c` — share a single **anonymous package**. They all behave as one package for visibility purposes, so a script can freely import any non-`@public` definition from another script in the same anonymous-package universe.

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
| `from mylib` (wildcard) | OK — `add` is callable; `helper` is also reachable from `add`'s body but is not the recommended way to call it from importer code (see [Known limitations](#known-limitations-name-collisions)) |

The error on the named import is intentional: a `from foo import name` form expresses intent to use `name` directly, so spelling out a non-`@public` symbol is treated as a mistake. A wildcard expresses "give me everything you make available," and Ry implements that as "co-locate every exportable definition in the importer's program so a `@public` facade can transitively call its package-internal helpers (REQ-B3)." The trade-off is described in [Known limitations](#known-limitations-name-collisions) below.

From **inside** the same package, both `add` and `helper` are importable regardless of `@public`.

## Cross-file private helpers (wrapper pattern)

In a multi-file package, you can keep a helper hidden while a parent module exposes a thin `@public` facade that calls through. External callers can use `bas()` and cannot import `xxx()` by name; the helper is co-located in the importer's compilation unit only because the facade body needs to resolve it at code-gen.

```text
foo/
  package.toml
  foo.ry         # @public fn bas() — facade
  bar.ry         # fn xxx()          — internal helper
```

```ry
# foo/bar.ry
fn xxx() -> int:
    return 42
```

```ry
# foo/foo.ry
from .bar import xxx

@public
fn bas() -> int:
    return xxx()
```

```ry
# caller/main.ry
from foo import bas
print(bas())     # 42
```

`from foo import xxx` from another package still errors with `'xxx' is not @public` — the encapsulation across the package boundary is enforced at the import statement, not by erasing the helper from the linkage unit. The pattern works for both selective (`from foo import bas`) and wildcard (`from foo`) imports across any package boundary, including same-package selective imports (which used to drop helpers prior to v0.0.18).

> Records, enums, and type aliases cannot be smuggled across packages by a wrapper function — only values, not type names, flow through a function call. Types intended to cross package boundaries must be declared directly on a public-facing module with `@public`.

## Known limitations: name collisions

The wrapper pattern relies on every exportable definition from an imported module landing in the importer's single LLVM linkage unit. Two name collisions are therefore observable as compile-time `duplicate function` errors:

1. **Two cross-package modules with the same internal helper name.** If `mod_a` and `mod_b` both define a non-`@public` `helper()` and the importer pulls in both, codegen sees two functions named `helper` in the same module.
2. **Importer-local function colliding with an imported package-internal helper.** If the importer defines a local `helper()` and `from foo import bas` brings `foo`'s package-internal `helper()` into the linkage unit too, the same collision happens. This now also reproduces with same-package selective imports — historically those silently dropped the helper, masking the conflict.

Workarounds: (a) rename the colliding helper inside the package you control; (b) restructure to use a single canonical module for the helper; (c) for upstream collisions, file an issue against the offending package. A future symbol-mangling proposal (no tracking issue yet) would remove this restriction.

## `_` prefix has no visibility meaning

Earlier versions of Ry treated a leading underscore (`_helper`) as a private marker. As of v0.0.17 this is no longer true — the prefix is purely a naming convention. Visibility is controlled exclusively by `@public`.

See [Naming Conventions](../reference/naming.md) for current style guidance.

## Standard library is a single package

The entire `share/std/` tree is **one** package — `share/std/package.toml` is its package root. Every stdlib sub-module (`math`, `io`, `path`, `filesystem`, `regex`, `thread`, …) lives inside that single package boundary.

Two consequences follow:

1. **From user code**, only stdlib symbols marked `@public` are importable. `from ry.math import sqrt` resolves to a `@public` symbol that the stdlib package chooses to expose. Non-`@public` helpers inside the stdlib (for instance, internal implementation files under `share/std/runtime_internal/` or under `share/std/core/`) remain package-internal and are invisible to importers.
2. **Inside the stdlib itself**, stdlib modules can freely call each other's package-internal helpers. This keeps maintenance ergonomics close to a single-codebase project even though the stdlib spans many sub-modules.

If the stdlib is ever split into independently-released packages (a future possibility tied to the planned `ry add` system), each piece would gain its own `package.toml` and the cross-stdlib helper calls would need explicit `@public` markers.
