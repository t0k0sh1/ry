### Changed

- CI `clang-tidy` job's `Build` step now compiles only the `ry` target
  on pull requests for faster feedback (~76 TU instead of the default
  `all` target, which previously also dragged in `ry_tests`,
  `ry_<pkg>` native shared libraries, and fuzz harnesses); the full
  all-target build is retained for `push` to `main`. The `cmake --build`
  invocation now also passes `--parallel`, and the `Run clang-tidy`
  step now parallelises per-TU via `xargs -0 -P "$(nproc)"` instead of
  running clang-tidy sequentially. The PR `--target ry` narrows only
  the build step — clang-tidy still analyses every `src/*.cpp`
  (90 files) in both event modes. (#1741)
