### Fixed

- For-loops over captured collections (`VariableExpr` / `FieldAccessExpr` iterables) inside `thread_spawn` closures no longer crash the JIT optimizer (`LowerExpectIntrinsicPass`). The thread thunk now releases ARC-managed locals before its `ret void`, matching the parallel-for thunk pattern (#1090).
