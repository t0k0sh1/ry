### Added

- Added braced selective import syntax: `from x import { a, b }` and
  `from x import { a as b, c }`. Both single-line and multi-line forms
  are accepted, with an optional trailing comma. The new form parses to
  the same `ImportStmt` AST as the existing `from x import a, b` form,
  so semantics (including #1721 symbol aliases) are unchanged. Empty
  braces (`from x import {}`) are rejected with
  `expected import name after '{'`.

  The tree-sitter grammar accepts braced single-line imports; brace-
  internal newline suppression for the multi-line form is tracked in
  #1727 alongside the same gap for list / map / set literals. (#1722)
