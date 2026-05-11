### Added

- Added qualified import syntax: `import <module>` binds the module
  itself, and members are accessed via `<module>.<name>` (e.g.
  `import math; math.sqrt(2.0)`, `math.PI`). Qualified and selective
  imports compose — `import math` and `from math import PI` may both
  appear in the same file. Qualified import is the recommended way to
  resolve name collisions between modules: `from str import contains`
  alongside `import list` lets the importer use `contains(...)` for the
  string version and `list.append(...)` for the list version without
  ambiguity. v0.0.23 supports qualified import for standard library
  modules only; user-defined modules continue to use
  `from <mod> import ...`. Constraints: single-identifier modules only
  (`import a.b` is rejected, use `from a.b import ...`); the
  `import <mod> as <local>` alias form is parsed but rejected with a
  pointer to the follow-up issue
  [#1724](https://github.com/t0k0sh1/ry/issues/1724); duplicate
  `import` of the same module in a file and local bindings that shadow
  an imported module name are both parse errors. (#1723)

### Fixed

- Renamed the TU-local `struct ry::Parser` in `src/runtime_json.cpp` to
  `JsonParser` to remove a latent ODR collision with the public
  `class ry::Parser` declared in `include/ry/parser.hpp`. The collision
  was benign while both implicit destructors were trivially equivalent,
  but became a crash (`AddressSanitizer: unknown-crash` inside
  `__ry_json_parse`) on Linux libstdc++ once `ry::Parser` grew a
  non-trivial member as part of the qualified-import work in this PR.
  (#1723)
