### Fixed

- `None()` and bare `none` in `if`/`case` branch-merge positions now correctly
  adopt the sibling arm's `Option<T>` inner type instead of defaulting to
  `Option<i8>` or `Option<i64>` (#1154)
