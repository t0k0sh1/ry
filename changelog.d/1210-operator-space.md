### Fixed

- Reject `function operator +(...)` with whitespace between `operator` and a symbolic operator. Only the canonical `function operator+(...)` (no space) is now accepted for symbolic operators. Keyword operators (`in`, `as`, `and`, `or`, `not`) and bracket/call operators (`[]`, `[]=`, `()`) are unaffected. (#1210)
