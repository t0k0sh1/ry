### Fixed

- Closure capture analysis now handles `CastExpr`, `WhenCondExpr`, `MatchExpr`, `RangeExpr`, `ErrorPropagateExpr`, `AwaitExpr`, `WeakExpr`, and `SetExpr`, preventing "undefined variable" errors when these expression types reference captured variables (#776)
