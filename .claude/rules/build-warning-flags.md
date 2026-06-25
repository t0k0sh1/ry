---
paths:
  - "CMakeLists.txt"
  - "cmake/**/*.cmake"
  - ".cppcheck-suppressions"
  - ".github/workflows/*.yml"
---

# Build Warning Flags

- LLVM and other third-party includes must be `SYSTEM`; project `include/` must not be `SYSTEM`.
- Keep warning flags in `RY_WARNING_FLAGS` and apply them per target with `target_compile_options(... PRIVATE ...)`; `add_ry_native_lib()` should inherit them.
- Maintain zero warnings under the configured `-Wall -Wextra -Wpedantic -Wconversion -Wshadow` set.
- New `.cppcheck-suppressions` entries need a comment and should be per-file when possible.
- Corrosion crate and CMake targets must avoid reserved names such as `codegen`, `test`, `all`, `clean`, `install`, and `package`.
- Static-analysis commands live in `.claude/skills/pre-commit-checklist/`; Docker details live in `docker/README.md`.
