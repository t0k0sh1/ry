### Fixed

- Propagate collection return type metadata for block-bodied lambdas with inferred return types, so `result.length()` / indexing work on the value returned by `f = (x: int):\n  return [x, x * 2]` style lambdas (#790).
