### Added

- Parenthesized tuple destructuring assignment `(a, b) = expr` and
  `@const (a, b) = expr` (#1189). Mirrors the existing bare form
  `a, b = expr` and matches what the formatter has been emitting.

### Fixed

- Formatter no longer emits a stray `: ` between the pattern and `=` in
  `TupleDestructStmt` output, which previously broke formatter → parser
  round-tripping for `@const` variants (#1189).
