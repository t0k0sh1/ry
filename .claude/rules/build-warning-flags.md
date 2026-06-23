---
paths:
  - "CMakeLists.txt"
  - "cmake/**/*.cmake"
  - ".cppcheck-suppressions"
  - ".github/workflows/*.yml"
---

# Build Warning Flags

Static-analysis commands and Docker behavior are documented in `docker/README.md`; required wrappers live under `.claude/skills/pre-commit-checklist/`.

### Compiler warning flags require SYSTEM includes for third-party headers

**Source**: #895
**Tags**: build, cmake, warnings, llvm, googletest, system-include

**Rule**: LLVM include directories must be added via
`target_include_directories(... SYSTEM PUBLIC ${LLVM_INCLUDE_DIRS})`,
not `include_directories(${LLVM_INCLUDE_DIRS})` — the global form does not support SYSTEM and causes
third-party warnings to leak. GoogleTest uses the `SYSTEM` keyword in `FetchContent_Declare`
(requires cmake 3.25+). The project's own `include/` must NOT be marked SYSTEM.

Warning flags are stored in `RY_WARNING_FLAGS` and applied per-target via
`target_compile_options(... PRIVATE ...)` so they do not leak into FetchContent targets. The
`add_ry_native_lib()` helper applies them automatically to all native shared libraries.

### Cppcheck: suppression strategy and known false positives

**Source**: #894
**Tags**: build, ci, cppcheck, static-analysis

Cppcheck runs in the `lint` CI job without `compile_commands.json` (build-free, fast), so project macros in
headers are not visible, causing `unknownMacro` false positives.

Known suppressions in `.cppcheck-suppressions`:
- **`unknownMacro`** (global): `RY_REGISTER_STDLIB_PACKAGE` and `DEFINE_LAST_ERROR` not resolved without
  `compile_commands.json`. Global suppression is intentional (avoids requiring a full build in `lint`).
- **`syntaxError:src/test_runtime.cpp`**: `__has_feature(address_sanitizer)` is a Clang builtin predicate;
  Cppcheck cannot evaluate it.

**Rule**: New suppressions in `.cppcheck-suppressions` must include a comment explaining whether it is a
false positive or a known acceptable deviation. Prefer per-file (`id:src/file.cpp`) over global. Inline
`// cppcheck-suppress <id>` (supported via `--inline-suppr`) is appropriate for narrowly-scoped sites.

### Zero-warnings policy and `-Werror` status

**Source**: #1498 (2026-05-02)
**Tags**: build, cmake, warnings, zero-warnings, werror

**Rule**: New code must maintain zero compiler warnings under the `-Wall -Wextra -Wpedantic -Wconversion -Wshadow` flag set. Warnings in LLVM / GoogleTest headers are suppressed via `SYSTEM` includes and do not count.

`-Werror` has not been introduced yet (tracked in a separate issue). The flag set applies to internal
targets (`ry_lib`, `ry`, `ry_tests`, native libs); see the sibling entry "Compiler warning flags require
SYSTEM includes" for the `RY_WARNING_FLAGS` mechanism.

Corrosion crate and CMake target names must not use reserved targets such as `codegen`, `test`, `all`, `clean`, `install`, or `package`; configure must complete without CMP0171 warnings.
