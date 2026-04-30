# Glossary

This page is the canonical definition of the core terminology used throughout the Ry reference documentation. Other reference pages assume these definitions; if any page reads ambiguously, this glossary wins.

The definitions reflect Ry as of **v0.0.17**. Two pre-existing names — the filename [`packages.md`](packages.md) and the manifest filename `package.toml` — use the word "package" in a looser sense than defined below; those names are kept for stability.

## Module

A **module** is the `xxx` in `from xxx import ...` — either a single `.ry` file or a directory containing multiple `.ry` files. Modules are the unit by which Ry code is organized into namespaces.

The standard library, project source files, and any directory tree reachable via `RY_PATH` are addressed as modules through the `from ... import ...` syntax.

See [Package Reference](packages.md) for the full import syntax and resolution rules.

## Package

A **package** is an external library managed by the `ry` command — for example, the planned `ry add` and `ry remove` subcommands.

**Status:** Packages are **not yet implemented** as of v0.0.17. No on-disk artefact in the current Ry source corresponds to a "package" in this strict sense, and there is no `ry add` / `ry remove` subcommand. The term **package** is reserved for this future feature; constructs imported via `from ... import ...` are *modules*, not *packages*.

## `package.toml`

The **project manifest** at the root of a Ry project, describing project metadata and path settings. The filename is preserved by analogy with Rust's `Cargo.toml` for familiarity, even though Ry's manifest currently describes a *project* rather than a *package* in the sense above.

See [Project Management](project.md) for the manifest specification.

## stdlib (`std`)

The **standard library**, also written `std`. A collection of built-in modules — including `math`, `io`, `path`, `filesystem`, `thread`, `regex`, and others — that is automatically imported into every program. The stdlib provides core types, conversion helpers (`toInt`, `toStr`, `toFloat`, `toBool`), built-in functions (`print`, `len`, `range`), and common utilities.

See [Package Reference — Standard Library](packages.md#standard-library-std) for the full list of stdlib sub-packages and import semantics.
