### Changed

- CodeQL Advanced workflow's c-cpp matrix Build step now compiles only
  the `ry` target on pull requests for faster feedback (~76 TU instead
  of the default target, which previously also dragged in `ry_tests`,
  `ry_<pkg>` native shared libraries, and fuzz harnesses); the full
  default-target build is retained for `push` to `main` and
  `workflow_dispatch` so the Code Scanning dashboard and the release
  `codeql-gate` keep the wider coverage. The `cmake --build` invocation
  now also passes `--parallel`. (#1740)
