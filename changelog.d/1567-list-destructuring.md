### Added

- List destructuring assignment. `a, b = some_list` (and the parenthesized
  form `(a, b) = some_list`) now unpacks a `List<T>` whose runtime length
  matches the number of names on the left. The `_` wildcard, `@const`
  prefix, and function-return values work the same as for tuple
  destructuring. A length mismatch aborts with
  `runtime error: list destructuring expected N elements but got M`,
  matching Python's semantics. The motivating idiom `a, b = split(s, " ")`
  now works without an intermediate temporary. (#1567)
