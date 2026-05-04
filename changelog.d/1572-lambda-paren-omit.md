### Added

- Single-parameter lambdas may now omit the parentheses when the parameter
  has no type annotation and the body is a single expression: `xs.filter(s => s == "1")`.
  Multi-arg, type-annotated, and block-bodied lambdas keep their existing paren-required
  syntax. (#1572)
