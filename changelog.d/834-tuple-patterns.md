### Added

- Tuple destructuring patterns in `case` statements and expressions (#834). Supports binding patterns `(a, b)`, literal patterns `(1, 2)`, mixed `(1, n)`, wildcard `(_, n)`, 1-tuples `(v,)`, guard clauses `(a, b) if a > b`, and nested patterns such as `(Some(v), _)`. A fully irrefutable tuple pattern (all elements are variables or `_`) is treated as exhaustive.
