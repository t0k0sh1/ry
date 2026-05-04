### Added

- List destructuring assignment. `a, b = some_list` (and the parenthesized
  form `(a, b) = some_list`) now unpacks a `List<T>` whose runtime length
  matches the number of positions on the left, where each `_` wildcard
  still counts as a position (so `_, b = some_list` requires two RHS
  elements). The `_` wildcard, `@const` prefix, and function-return values
  work the same as for tuple destructuring. A length mismatch aborts with
  `runtime error: list destructuring expected N elements but got M`,
  matching Python's semantics. The motivating idiom `a, b = split(s, " ")`
  now works without an intermediate temporary. (#1567)
