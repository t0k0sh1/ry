### Changed

- `json.load` / `json.loadAs[T]` parse errors now report position as
  `at line <L>, column <C> (offset <O>)` instead of the previous
  `at position <N>` byte-offset-only form. `<L>` and `<C>` are 1-based
  and `<C>` counts UTF-8 codepoints (matching Python's `JSONDecodeError`
  and typical editor column numbers); the original byte offset is
  preserved in parentheses for precision. Affects 11 position-bearing
  error sites in `runtime_json.cpp` (`expected '<c>'`, `unexpected
  character`, `unescaped control character in string`, `invalid number`,
  `leading zeros not allowed`, `number out of range`, `integer overflow`,
  `invalid literal`, `expected string key`). Position-less messages
  (`unexpected end of input`, `unterminated string`, `json: maximum
  nesting depth exceeded`, etc.) are unchanged. (#1851)
