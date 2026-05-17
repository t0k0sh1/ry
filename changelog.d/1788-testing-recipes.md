### Added

- Added two follow-up Recipes to `docs/reference/testing.md`:
  "Per-test mock setup with `@beforeEach`" (reusing the
  `mockInBeforeEach` fixture from
  `tests/spec/feature_combinations.test.ry`) and
  "Setup patterns for `@each` parameterized tests" (backed by new
  `tests/spec/parameterized_lifecycle.test.ry`, covering both
  inline per-iteration setup and `@beforeAll` hoist workarounds for
  the `@each` + `@beforeEach` compile-error case). Completes the
  recipes deferred from #1783. (#1788)
