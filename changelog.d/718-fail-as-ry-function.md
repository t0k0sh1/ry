### Changed

- `fail()` is now implemented as a Ry function in
  `share/std/testing/testing.ry` that delegates to a new
  `@native("testing")` runtime call (`_reportFail`) backed by a new
  `libry_testing.dylib` shared library. The compiler still
  special-cases the `fail` callee to inject the call-site line
  number as the first argument (the `__LINE__` intrinsic from #705
  was closed as `NOT_PLANNED`, so a hybrid approach keeps
  line-number injection in codegen), but the function body itself
  runs as ordinary Ry code. User-facing behavior is unchanged:
  `fail()` and `fail("message")` still report the call-site line
  number and message exactly as before, and the
  `'fail' requires 'from testing import fail'` import-gate from
  #715 still fires for unimported usages. (#718)
