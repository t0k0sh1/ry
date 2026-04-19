### Fixed

- `None()` / `none` passed as a positional field value in a record/struct
  constructor now correctly inherits the field's `Option<T>` inner type,
  matching the behavior already available in `let` annotations, if/case
  branches, and lambda call arguments (#1186).
