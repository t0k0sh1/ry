### Fixed

- `!!` error-propagation operator now works in expression position immediately after an identifier (e.g. `Ok(r!!)`, `Some(v!! + 1)`), matching the documented equivalence with `?`. The lexer previously consumed the trailing `!` as part of the identifier (to support mutating method names like `sort!`), so `r!!` tokenized as `r!` + `!` and failed to parse (#1211)
