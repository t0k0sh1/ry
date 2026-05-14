### Changed

- CI `scan-build` job now analyses only the `ry` target on pull requests
  for faster feedback (~76 TU instead of the default `all` target,
  which previously also dragged in `ry_tests`, `ry_<pkg>` native shared
  libraries, and fuzz harnesses); the full all-target scan is retained
  for `push` to `main` so mainline keeps the wider coverage. Both
  invocations now pass `--parallel`. (#1738)
