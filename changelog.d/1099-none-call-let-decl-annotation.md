### Fixed

- `None()` call-form is now recognised as a None literal in let-decl, local
  variable reassignment, and module-global reassignment contexts, matching the
  behaviour of bareword `None` and `none`. Previously `x: Option<int> = None()`
  and `x = None()` (on an already-declared `Option<T>` variable) produced a
  type-mismatch compile error (#1099).
