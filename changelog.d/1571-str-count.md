### Added

- `count(string: str, substring: str, ignoreCase: bool = false) -> int` to the
  `str` module. Returns the number of non-overlapping occurrences of
  `substring` in `string`, matching Python / Go semantics
  (`"aaaa".count("aa") == 2`). Empty `substring` returns `byteLen + 1` (gap
  count); `ignoreCase` performs ASCII-only folding; arguments may contain
  embedded NUL bytes. (#1571)
