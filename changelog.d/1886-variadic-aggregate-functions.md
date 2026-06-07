### Added

- `min`, `max`, and `sum` now accept a variadic scalar form in addition
  to the existing single-list form: `min(3, 5)`, `max(1, 2, 3)`,
  `sum(1.0, 2.0, 3.0)` (previously only the list form such as
  `min([3, 5])` worked, and `min(3, 5)` failed with
  `min() takes exactly 1 argument`). The variadic form takes two or more
  arguments (unbounded), all of the same type; `min`/`max` accept
  `int`/`float` and `sum` accepts `int`/`float`/`u8`, matching the
  element types each already supported for the list form. (#1886)
