### Changed

- Migrated 6 built-in directives from the C++ registry to stdlib `.ry` declarations. `@inline`, `@parallel`, `@const`, and `@deprecated` are now declared in `share/std/core/directive.ry` and remain implicitly available via the `share/std/builtins.ry` re-export. `@each` and `@property` are now declared in `share/std/testing/testing.ry` and require an explicit `from testing import each, property` (or the subset used) — consistent with `@it` / `@describe`. Only `@directive` and `@native` remain as compiler built-ins (the bootstrap pair). (#1390)
