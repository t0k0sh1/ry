### Added

- Added Troubleshooting, Recipes, and Best Practices sections to
  `docs/reference/testing.md` covering common errors (missing
  `from testing import`, `verify` returning 0, `toEq` vs `toBeCloseTo`
  for floats, `@afterEach` skipped on `@timeout`,
  `@each` / `@property` + `@timeout` compile error), worked patterns for
  `mockReturnValueOnce` / `spy` / `toBeCloseTo` / `@property` /
  overloaded mock, and conventions to prevent footguns
  (`@only` in committed code, mock scope, `verify` paired with
  behavioral assertion, `@beforeAll` weight, `should ...` form). (#1783)
