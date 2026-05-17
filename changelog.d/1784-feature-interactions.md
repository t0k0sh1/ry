### Added

- Added a `Feature interactions` section to
  `docs/reference/testing.md` documenting how v0.0.24 testing features
  combine: `@beforeAll` / `@afterAll` with `@each` / `@property`
  (parameterized-aware lifecycle), `mock` / `spy` installed from
  `@beforeEach` (fresh per-`it` state via auto-restore), mutually
  exclusive combinations (`@beforeEach` / `@afterEach` with `@each` /
  `@property`, `@timeout` with `@each` / `@property`) with verbatim
  compile-error messages, and nested-`@describe` lifecycle
  (hooks are describe-local, not inherited). Adds
  `tests/spec/feature_combinations.test.ry` covering the four supported
  combinations, plus verbatim error text in
  `docs/reference/directives.md` for the `@timeout` mutual exclusion.
  (#1784)
