---
paths:
  - "tests/spec/**/*.test.ry"
---

# Tests — Spec Conventions

This file covers only hazards that are not visible from reading the code.

### `expect(str).toEq("literal")` is NUL-truncating — use `expect(str == "literal").toEq(true)` for NUL-containing strings

**Tags**: testing, NUL-safety, codegen_test

`toEq` for strings emits `strcmp` (`codegen_test.cpp:784`) which stops at the first `\0`. `==` goes through `__ry_str_cmp` (byte_len + memcmp) and is NUL-safe. Testing a NUL-containing string with `expect(expr).toEq("a\0b")` truncates at `\0`, so differences after the NUL pass silently. The only NUL-safe matchers are `toHaveLen` / `toBeEmpty` / `toEq(bool)`.

### Naming-convention sweeps must include the implicit `name: type = value` form, not just `let`/`var`, and must cover module-global declarations

**Tags**: testing, naming, camelCase, sweep, blind-spot, module-global

Anchoring a sweep to `let` / `var` lets the implicit binding form (`name: type = value`, no keyword) pass the parser's camelCase check undetected. Requiring indentation with `^\s+` misses column-0 module-global declarations. Use `^\s*`, consume underscores with `[a-zA-Z0-9_]`, and prefer batch grep/sed over per-site edits.

### `tests/spec/<name>/` directories collide with stdlib module names

**Tags**: testing, module-loader, stdlib, layout, collision, gotcha

`ry test`'s module loader treats every directory on the resolution path as a potential package root. A `tests/spec/<name>/` directory whose name matches a stdlib module name (`testing`, `math`, `path`, `filesystem`, `crypto`, `io`, `json`, `regex`, `thread`, `time`, `http`, `str`, `list`, `map`, `set`, `option`, `result`, etc.) shadows `share/std/<name>/` and produces a runtime import error such as `'it' not found in module 'testing'` (silent at compile time). When adding a `.test.ry` file to a subdirectory, cross-check against `ls share/std/`; avoid colliding names and prefer the top-level flat form (`tests/spec/directive_skip.test.ry`). Existing directories `tests/spec/{braced_import,combinatorial,concurrency}/` are safe. When a stdlib module is renamed, audit `tests/spec/` for new collisions.
