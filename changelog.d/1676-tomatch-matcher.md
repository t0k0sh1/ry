### Added

- `expect(actual: str).toMatch(pattern: str)` — new test matcher for
  regex string matching, modeled on Jest's `toMatch`. Internally calls
  `__ry_regex_is_match` (unanchored search), so `expect("hello world")
  .toMatch("world")` and `expect("v1.2.3").toMatch("^v\\d+\\.\\d+\\.\\d+$")`
  both pass. Both `actual` and `pattern` must be `str`; passing any
  other type to `actual` (e.g. `expect(42).toMatch(...)`) is rejected
  at compile time. An invalid regex pattern (e.g. `toMatch("(")`) is
  surfaced as a runtime panic with the regex engine's error message,
  matching the behavior of other `regex` stdlib calls. (#1676)
