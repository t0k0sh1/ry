### Fixed

- `replace(s, "", repl)` no longer hangs with an infinite loop; an empty pattern now returns a fresh copy of the input unchanged (#802)
- `NaN != NaN` now returns `true` as required by IEEE 754; float `!=` comparisons use `fcmp une` (unordered not-equal) instead of `fcmp one` (#803)
- `is_empty()` now accepts `str` arguments in addition to lists, maps, and sets (#831)
