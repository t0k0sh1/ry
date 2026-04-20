### Fixed

- `substring(s, start, end)` now resolves negative `start` / `end` as offsets from the end of the string (`length + idx`), consistent with Python-style indexing and matching `char_at()`, `slice()`, and `lst[-1]` subscript access. Over-negative inputs are silently clamped to `0`. (#1199)
