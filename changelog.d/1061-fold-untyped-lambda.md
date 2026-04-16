### Fixed

- `fold()` now accepts untyped lambdas (e.g. `fold(xs, 0, (a, b) => a + b)`), matching the fix already applied to `reduce()` in #1038 (#1061)
