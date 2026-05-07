### Changed

- **Breaking**: `@it("...")` and `@describe("...")` directives now
  require an explicit `from testing import it, describe` (or the
  subset used) declaration in the test file. Codegen rejects
  unimported usage with `'@it' requires 'from testing import it'`
  or `'@describe' requires 'from testing import describe'` after
  the existing test-mode check, so non-test-mode usage still wins
  the more useful "only allowed in test mode" diagnostic. This
  completes the enforcement story started in #715 (which covered
  `expect` / `mock` / `verify` / `fail`). All `tests/spec/*.test.ry`
  files already declare these imports after the #714 migration,
  so the Ry self-test suite remains green; downstream test files
  that omitted the imports must add them. (#716)
