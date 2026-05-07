### Changed

- **Breaking**: `expect` / `mock` / `verify` / `fail` now require an
  explicit `from testing import <name>` declaration in the test file.
  Previously, codegen tracked which testing intrinsics were imported
  (#713) but did not enforce the import; any `*.test.ry` file run via
  `ry test` could call these intrinsics without declaring them. The
  compiler now rejects unimported usage with `'<name>' requires
  'from testing import <name>'` at codegen time, after the existing
  test-mode check so non-test-mode usage still wins the more useful
  "only allowed in test mode" diagnostic. All 171 in-tree spec files
  were migrated to declare these imports under #714, so the suite
  remains green; downstream test files that omitted the imports must
  add them. `it` / `describe` enforcement is tracked separately under
  #716. (#715)
