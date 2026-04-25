### Changed

- Heavy CI analysis (`clang-tidy`, `scan-build`, `asan`, `tsan`) now runs on every pull request instead of only on `v*.*.*` branch pushes. CodeQL also runs per PR plus on push to `main`, replacing the previous daily cron. The redundant `ci-scheduled.yml` workflow has been removed. (#1367)
