# Glossary

This page is the canonical definition of the core terminology used throughout the Ry reference documentation. Other reference pages assume these definitions; if any page reads ambiguously, this glossary wins.

The definitions reflect Ry as of **v0.0.17**. The manifest filename `package.toml` uses the word "package" in a looser sense than defined below; the name is kept for stability (by analogy with Rust's `Cargo.toml`).

## Module

A **module** is the `xxx` in `from xxx import ...` — either a single `.ry` file or a directory containing multiple `.ry` files. Modules are the unit by which Ry code is organized into namespaces.

The standard library, project source files, and any directory tree reachable via `RY_PATH` are addressed as modules through the `from ... import ...` syntax.

See [Module Reference](modules.md) for the full import syntax and resolution rules.

## Package

A **package** is an external library managed by the `ry` command — for example, the planned `ry add` and `ry remove` subcommands.

**Status:** Packages are **not yet implemented** as of v0.0.17. No on-disk artefact in the current Ry source corresponds to a "package" in this strict sense, and there is no `ry add` / `ry remove` subcommand. The term **package** is reserved for this future feature; constructs imported via `from ... import ...` are *modules*, not *packages*.

## `package.toml`

The **project manifest** at the root of a Ry project, describing project metadata and path settings. The filename is preserved by analogy with Rust's `Cargo.toml` for familiarity, even though Ry's manifest currently describes a *project* rather than a *package* in the sense above.

See [Project Management](project.md) for the manifest specification.

## Legacy internal identifiers

The following internal identifiers retain historical `package` naming for source or binary stability. Use them verbatim; use the current terminology elsewhere.

- `effectivePackage`
- `deriveNativePackage`
- `RY_REGISTER_STDLIB_PACKAGE`
- `__ry_<symbol>`

## Visibility scopes

The visibility model uses three named scopes:

| Scope | Definition |
|---|---|
| **file** | The single `.ry` file in which a definition is declared. |
| **package** | The directory tree rooted at the nearest ancestor directory containing a `package.toml` file. Two files belong to the same package when they share the same package root. |
| **universe** | Visible from any package — no boundary applies. Reserved for `@public` definitions. |

Source files that have no `package.toml` ancestor (for example, ad-hoc scripts and REPL `-c` input) share a single anonymous package — they all behave as one package for visibility purposes.

The two visibilities supported by Ry today map onto these scopes:

- **package-internal** (default, no marker) — `package` scope
- **`@public`** — `universe` scope

This is a third sense of the word "package" — distinct from the strict ["Package"](#package) definition above (external library) and looser than the ["`package.toml`"](#packagetoml) manifest itself. It is named "package" because the boundary is literally defined by the presence of `package.toml`; the visibility model uses no separate term. The intermediate `file` scope is reserved as vocabulary; no Ry directive currently targets it.

See [Module Reference — Visibility](modules.md#visibility) for the practical visibility rules and import semantics.

## ry namespace

The **canonical reserved namespace** for the official standard library, introduced in v0.0.30 (#1769). Documented public stdlib modules are addressable under the `ry.*` path: the implicit prelude is `ry.lang`, and each public submodule lives under `ry.<module>`.

The full set of public modules accepted via `ry.*` is the 13 documented entries: `ry.lang`, `ry.math`, `ry.io`, `ry.path`, `ry.filesystem`, `ry.json`, `ry.http`, `ry.thread`, `ry.regex`, `ry.testing`, `ry.base64`, `ry.net`, `ry.json5`. Anything else under the `ry.*` prefix — including internal stdlib helpers such as `ry.builtins`, `ry.gc`, `ry.core`, `ry.runtime_internal`, and any module not on the documented list — is **rejected** with an error that lists the public modules. Legacy bare aliases (`math`, `net`, `json5`, …) for these documented modules were rejected as of #2351; the canonical `ry.*` form is required.

Two import shapes resolve through this namespace:

```ry
from ry.math import sqrt, PI    # selective import
import ry.math                  # qualified import; binds `math` (last segment)
```

Bare `import ry` and `from ry import X` are **invalid** — `ry` is a reserved namespace with no top-level exports. The loader rejects both forms with an error advising `ry.<module>`.

Legacy `from std import` / `from std.<mod> import` / flat `from <mod> import` spellings for the 13 public modules listed above were rejected as of #2351 — the loader emits a hard error suggesting the canonical `ry.*` form.

A user-defined top-level `ry/` module (a local `ry/` directory or `ry.ry` file) does **not** shadow the reserved namespace — the loader resolves `ry.*` only against the stdlib search paths and emits a one-time warning advising the user to rename their local module. A future release will promote this warning to a hard error.

## ry.lang

The **explicit prelude module**, introduced in v0.0.30 (#1769). Every program implicitly imports `ry.lang` (the equivalent of `from ry.lang`), making `print`, `len`, `range`, `map`, `filter`, `sum`, `int(...)`, `float(...)`, `str(...)`, and similar helpers available without an explicit import.

`ry.lang` is also explicitly importable:

```ry
from ry.lang import map, filter, sum
```

The set of symbols `ry.lang` exposes is the union of the files directly under `share/std/`. See [Module Reference — Canonical paths](modules.md#canonical-module-paths) for the full prelude listing.

## stdlib (`std`)

The **standard library**. A collection of built-in modules — including `ry.math`, `ry.io`, `ry.path`, `ry.filesystem`, `ry.thread`, `ry.regex`, and others — that is automatically imported into every program. The canonical spelling for the stdlib namespace is [`ry`](#ry-namespace) (e.g. `ry.math`). Legacy `std` and bare forms (`from std import`, `from math import`, etc.) were rejected as of #2351.

The stdlib provides core types, conversion helpers (`int`, `float`, `str`), built-in functions (`print`, `len`, `range`), and common utilities.

The entire stdlib forms a single package — `share/std/package.toml` is its package root. Stdlib modules can therefore reference each other's package-internal helpers freely, while user code only sees `@public` stdlib symbols.

See [Module Reference — Standard Library](modules.md#standard-library) for the full list of stdlib sub-modules and import semantics.

## strict-any mode

An **opt-in compiler mode** that enables stricter `any` semantics ahead of those semantics becoming the default. Introduced in v0.0.30 (#2319) to give existing code a migration window before subsequent issues (#2316, #2317, #2321, #2323) plug additional rules into the framework and #2322 eventually flips strict-any to the default.

Activation:

- `RY_STRICT_ANY=1` environment variable (inherited by `ry test` subprocesses), or
- `--strict-any` CLI flag (which `setenv`-s the same variable so subprocess inheritance is automatic).

Diagnostics produced by the mode are tagged `[strict-any/<rule>]` in the message so users can grep for them and follow-up issues can extend the rule catalog without changing the diagnostic shape. Currently shipped rules:

- **`any-arithmetic`** — direct binary `+`/`-`/`*`/`/`/`%`/`//`/`**` and unary `-` on an `any`-typed operand is rejected. Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`) are still permitted because they always yield a concrete `bool`. The fix is to annotate the operand type or use `asType[T](...)` (#2315) to recover a concrete value before the operation.

See [Strict-any mode reference](strict-any.md) for the full rule catalog and migration guidance.
