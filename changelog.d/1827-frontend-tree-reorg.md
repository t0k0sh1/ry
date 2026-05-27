### Changed

- Reorganized lexer, parser, AST, diagnostic, and source-manager source files
  into role-specific subdirectories (`src/lexer/`, `src/parser/`, `src/ast/`,
  `src/diagnostic/`, `src/source_manager/`) and the matching `include/ry/`
  layout. No behavior change — file rename plus `#include` path updates only.
  Stage 1 of the v0.0.26 C++ tree reorganization tracked under the #1819
  umbrella.
