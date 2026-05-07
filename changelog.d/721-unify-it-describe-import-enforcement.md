### Changed

- `@it` and `@describe` directive declarations are now resolved entirely
  through the general user-directive import mechanism. `share/std/testing/testing.ry`
  has carried `@directive(target=["function"])` declarations for both since #710,
  and #716 added a parallel set-based check (`testing_intrinsics_imported_`) that
  produced `'@it' requires 'from testing import it'` / `'@describe' requires
  'from testing import describe'` before the directive-resolution path ran. That
  bespoke check has been removed: usage without the import is now rejected by the
  same `unknown directive '@<name>'` path that handles every other unimported
  user-defined directive. The intrinsic enforcement set now tracks only `expect`,
  `mock`, `verify`, `fail`. Existing test files that already declare
  `from testing import it, describe` (or use a wildcard `from testing`) are
  unaffected; the only behavioural change is the diagnostic wording for the
  unimported case, which now reads `unknown directive '@it'` /
  `unknown directive '@describe'`. (#721)
