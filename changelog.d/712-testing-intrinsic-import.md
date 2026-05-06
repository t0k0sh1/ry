### Added

- Allowed `from testing import expect / mock / verify / fail / it /
  describe` by introducing a compiler-intrinsic allow-list in
  `ModuleLoader` and permitting the `expect` keyword (the only
  intrinsic that lexes as a reserved token, used elsewhere by the
  matcher statement form) at the import-name position in the parser.
  Wildcard `from testing` is also recognized as importing all six
  intrinsics. Names imported this way are exposed via
  `ModuleLoader::importedTestingIntrinsics()` for the forthcoming
  codegen-side enforcement (#713 / #715 / #716). Non-intrinsic names
  still fail with the existing `'<name>' not found in module
  'testing'` diagnostic. (#712)
