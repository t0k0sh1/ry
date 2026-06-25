---
paths:
  - "tests/spec/**/*.test.ry"
---

# Tests - Spec Conventions

- For NUL-containing strings, use `expect(str == "literal").toEq(true)`; `expect(str).toEq("literal")` is NUL-truncating.
- Naming sweeps must include implicit `name: type = value` bindings and column-0 module-global declarations.
- Avoid `tests/spec/<name>/` directories that collide with `share/std/<name>/` module names; prefer flat `tests/spec/*.test.ry` files unless a subdirectory is necessary.
