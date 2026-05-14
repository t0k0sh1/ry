### Fixed

- tree-sitter grammar now accepts newlines inside brace-delimited
  expressions: multi-line `list_literal` (`[\n  1,\n  2,\n]`),
  `map_literal` (`{\n  "a": 1,\n}`), `set_literal` (`{\n  1,\n  2,\n}`),
  and braced selective import (`from std.io import {\n  print,\n}`)
  no longer produce `(ERROR (UNEXPECTED '\n'))`. The fix is contained to
  `editor/tree-sitter/grammar.js` via a new `bracedSep1` helper that
  absorbs the external `_newline` token around list separators and at
  the brace boundaries; `_indent` / `_dedent` are intentionally not
  absorbed so the scanner's indent stack stays clean. This mirrors the
  C++ parser's `skipStructuralTokens` (`src/parser.cpp:352`) and the
  Phase 2 corpus gains four `#1727` cases under `imports` / `literals`
  including a function-body nesting case that exercises indent-stack
  health. Out of scope (still produce ERROR for multi-line forms):
  `tuple_literal` / `_parenthesized` / `argument_list` / `parameter_list`
  / `case_*` arm bodies. (#1727)
