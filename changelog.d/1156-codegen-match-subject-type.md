### Fixed

- `case <subject>: (a, b)` where the subject is `Option<T>` or `Result<T, E>`
  no longer silently destructures the LLVM struct layout as a tuple.
  Previously the TuplePattern arm's source-name-based guard was skipped when
  the subject had no enum annotation, allowing `{i1, T}` to pass arity
  validation and producing wrong IR or an `ICmp` type-mismatch crash.
  The pattern test now rejects these subjects structurally via
  `isTupleStructType`, independent of any source-level type name. (#1156)
