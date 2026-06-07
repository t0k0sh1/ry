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

## stdlib (`std`)

The **standard library**, also written `std`. A collection of built-in modules — including `math`, `io`, `path`, `filesystem`, `thread`, `regex`, and others — that is automatically imported into every program. The stdlib provides core types, conversion helpers (`toInt`, `toStr`, `toFloat`, `toBool`, `parseInt`, `parseFloat`), built-in functions (`print`, `len`, `range`), and common utilities.

The entire stdlib forms a single package — `share/std/package.toml` is its package root. Stdlib modules can therefore reference each other's package-internal helpers freely, while user code only sees `@public` stdlib symbols.

See [Module Reference — Standard Library](modules.md#standard-library-std) for the full list of stdlib sub-modules and import semantics.
