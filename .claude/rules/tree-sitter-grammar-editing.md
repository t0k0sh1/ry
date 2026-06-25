---
paths:
  - "editor/tree-sitter/grammar.js"
  - "editor/tree-sitter/src/scanner.c"
  - "editor/tree-sitter/queries/*.scm"
---

# tree-sitter Grammar Editing

- Read `editor/tree-sitter/README.md` before grammar work.
- Keep `scanner.c` `enum TokenType` and `grammar.js` `externals` in identical ordinal order.
- After consuming a non-zero-width external token, call `mark_end()`.
- Scanner early-return guards must use requested `valid_symbols`, not only initial lookahead.
- Prefer grammar-level handling for brace-newline tolerance.
- Query syntax: `(node_name)` matches named nodes; `"literal"` matches anonymous source literals; field labels are unquoted.
- For multi-line delimited literals without interpolation, prefer internal `token(seq(...))` before adding scanner logic.
- Run applicable tree-sitter checks after edits.
