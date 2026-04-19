### Changed

- `reduce(list, fn)` now returns `Option<T>` (previously `T`) and returns `None`
  for an empty list instead of raising a runtime error. Unwrap with `?? default`
  or pattern match, e.g. `(reduce(xs, fn)) ?? 0`. `fold(list, init, fn)` is
  unchanged and remains the preferred function when you have a seed value.
  (#1209)

### Fixed

- Calling `reduce(list, init, fn)` with 3 arguments (Python/JS style) now
  reports a targeted compile error suggesting `fold(list, init, fn)` instead of
  the generic "takes exactly 2 arguments" message. (#1209)
