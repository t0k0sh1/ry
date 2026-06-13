---
paths:
  - "editor/tree-sitter/grammar.js"
  - "editor/tree-sitter/src/scanner.c"
  - "editor/tree-sitter/queries/*.scm"
---

# tree-sitter Grammar Editing

- Read `editor/tree-sitter/README.md` for the canonical spec relationship, build, install, corpus tests, smoke check, live-editing tolerance, brace-newline tolerance, and contributor workflow.
- Scanner:
  - Keep `scanner.c` `enum TokenType` and `grammar.js` `externals` in identical ordinal order; update the scanner header comment when tokens change.
  - After consuming a non-zero-width external token, call `mark_end()`.
  - Early-return guards must use the requested `valid_symbols` set, not initial lookahead.
  - Implement brace-newline tolerance at grammar level, not by tracking bracket depth in the scanner.
- Queries:
  - `(node_name)` matches a named node; `"literal"` matches an anonymous source literal.
  - Use unquoted field labels in `field_name: (node)`; quote only literal source tokens.
- Run `/pre-commit-checklist`.
