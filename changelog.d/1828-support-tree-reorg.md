### Changed

- Reorganized module/import resolution, semantic analysis, project/environment,
  CLI, application entry, JIT, trace, and coverage source files into
  role-specific subdirectories (`src/module/`, `src/sema/`, `src/project/`,
  `src/cli/`, `src/app/`, `src/jit/`, `src/trace/`, `src/coverage/`) and the
  matching `include/ry/` layout. `main.cpp` was placed under `src/app/` to keep
  the production binary entry separate from the CLI library layer. No behavior
  change — file rename plus `#include` and CMake path updates only. Stage 2 of
  the v0.0.26 C++ tree reorganization tracked under the #1819 umbrella.
