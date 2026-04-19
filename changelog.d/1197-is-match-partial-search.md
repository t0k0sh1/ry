### Changed

- `is_match(text, /pattern/)` now performs **partial (unanchored) search** — it returns `true` if the pattern matches anywhere in the text, consistent with its name and with `search()` / `regex_search()`. Previously it performed a full-string match. To require a full-string match, anchor the pattern explicitly with `^` and `$` (e.g. `/^[a-z]+$/`). The legacy string-pattern `regex_match(text, pattern)` is unchanged and still requires a full-string match (#1197).
