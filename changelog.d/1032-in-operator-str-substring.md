### Added

- `in` and `not in` operators now support substring check when the right operand is a `str`.
  `"world" in "hello world"` evaluates to `true`; empty-needle `"" in s` evaluates to `true`
  to match Python and the existing `contains` semantics. (#1032)
