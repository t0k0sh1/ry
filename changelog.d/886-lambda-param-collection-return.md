### Fixed

- Lambdas (expression-body and block-body) that return one of their own
  collection-typed parameters now correctly propagate the parameter's
  declared shape so that `result.length()` and indexing work on the
  returned value (#886).
