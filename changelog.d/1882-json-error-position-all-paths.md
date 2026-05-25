### Fixed

- `json.load` / `json.loadAs[T]` now report `at line <L>, column <C>
  (offset <O>)` for the 16 parse-error sites in `runtime_json.cpp` that
  were left position-less by #1851 — `unexpected end of input`,
  `unterminated string`, `unterminated string escape`, `incomplete
  unicode escape`, `invalid hex digit in unicode escape`, `unpaired
  high surrogate in unicode escape`, `invalid low surrogate in unicode
  escape`, `unpaired low surrogate in unicode escape`, `invalid escape
  character '\X'`, `invalid number: expected digit after decimal point`,
  `invalid number: expected digit in exponent`, `json: maximum nesting
  depth exceeded`, `array too large`, `unterminated array`, `object too
  large`, `unterminated object`. Number errors point at the start of
  the offending number; unterminated containers point at the opening
  `"` / `[` / `{`; nesting-depth and escape errors point at the failing
  token. All `json.load` parse-error messages now carry the position
  suffix consistently. (#1882)
