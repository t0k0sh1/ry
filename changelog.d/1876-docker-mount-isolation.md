### Changed

- `docker/run.sh` now bind-mounts each source directory and config file
  individually (`src/`, `include/`, `tests/`, `share/`, `CMakeLists.txt`,
  `CMakePresets.json`, `package.toml`, `.clang-tidy`,
  `.cppcheck-suppressions`) instead of bind-mounting the entire project
  root. The per-preset build directory (`build-docker/`,
  `build-asan-docker/`, etc.) is still mounted into its container
  counterpart, but host macOS native build dirs (`build/`, `build-asan/`,
  `build-fuzz/`) are no longer visible inside the container. This closes
  the cross-OS contamination path where macOS Mach-O binaries leaked
  through the outer `PROJECT_DIR:/workspace` mount alongside the inner
  Docker build dir and caused `clang-tidy` to fail when
  `compile_commands.json` listed `/Users/...` host paths. The
  `./docker/run.sh <preset> <args>` invocation interface is unchanged;
  adding a new top-level source or config file the build reads now
  requires updating `docker/run.sh` `MOUNT_ARGS` (and the matching
  `entrypoint.sh` guard) in the same PR. (#1876)
- `docker/entrypoint.sh` validates the container state at startup and
  fails fast on three contamination patterns: required source/config
  mounts are missing (exit 70, signals a `docker/run.sh` mount-list
  drift), a `BUILD_DIR/ry` or `BUILD_DIR/ry_tests` binary that is not
  ELF (exit 71, signals a macOS Mach-O leak into the container build
  dir), or `BUILD_DIR/compile_commands.json` listing `/Users/...`
  directories (exit 72, the symptom that previously broke `clang-tidy`).
  Each failure message names the host-side build dir to `rm -rf` for
  recovery, via the `RY_HOST_BUILD_DIR` environment variable that
  `docker/run.sh` now exports into the container. (#1876)
