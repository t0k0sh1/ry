### Fixed

- `Err([...])` and similar Err-constructor expressions can now be coerced to a
  `Result<Ok, Collection>` type annotation at variable declaration and reassignment
  sites (e.g., `a: Result<int, List<int>> = Err([1, 2, 3])`).  Previously this
  emitted a type error because the inferred struct layout differed from the
  annotation layout (#1001).
- Pattern-matching an `Err(binding)` arm now correctly propagates collection
  element-type metadata to the bound variable, enabling index access and
  collection operations on the Err payload without a "cannot determine list
  element type" error.
