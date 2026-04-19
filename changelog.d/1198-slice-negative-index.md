### Fixed

- `slice(lst, start, end)` now resolves negative `start` / `end` as offsets from the end of the list (`length + idx`), consistent with Python-style indexing, subscript access, and the `lst[a..b]` range-index operator (#1184). Over-negative inputs are silently clamped to `0`. (#1198)
