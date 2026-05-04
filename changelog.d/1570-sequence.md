### Added

- `sequence(values: List<Result<T, E>>) -> Result<List<T>, E>` and
  `sequence(values: List<Option<T>>) -> Option<List<T>>` for folding
  a list of `Result`/`Option` into a single `Result`/`Option` of list,
  short-circuiting on the first `Err`/`None`. Empty list returns
  `Ok([])` / `Some([])`. UFCS form `xs.sequence()` is also supported.
  (#1570)
