### Fixed

- `weak <alias>` where the alias resolves to `str` now uses the correct `StringHeader`
  offset instead of the `ArcHeader` offset. Without this fix, weak upgrade of a str-alias
  weak ref could load the wrong `strong_count` and crash or return wrong results (#1060)
