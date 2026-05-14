### Added

- Extended qualified import (#1723 / #1724) to user-defined modules: after
  `import usermod`, the qualified forms `usermod.foo()`, `usermod.PI`, and
  `usermod.MyRecord(...)` resolve through a per-module namespace bucket on
  `CodeGen`, replacing the previous "throwaway Program" carve-out that
  outright rejected qualified calls into user-defined modules. The
  selective form (`from usermod import foo`) continues to share the same
  loader cache, so mixing both in one file (`import usermod` followed by
  `from usermod import foo`) reuses the AST without re-parsing. Bare-name
  leak isolation is preserved: `import usermod` alone never exposes
  `foo()` as a top-level identifier. (#1730)

### Changed

- Generic functions, `enum` declarations, and `type` aliases inside a
  user-defined module reached through qualified import are now rejected
  at codegen with an actionable diagnostic suggesting
  `from <module> import ...`. These constructs route through flat tables
  (`generic_fn_templates_` / `enum_types_` / `type_aliases_`) that the
  per-module namespace bucket cannot intercept; surfacing the limitation
  early avoids silent bare-name leaks. (#1730)
