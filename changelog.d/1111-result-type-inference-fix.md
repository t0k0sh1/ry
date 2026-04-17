### Fixed

- Result-returning lambda with unannotated parameter no longer loses its `Ok` payload when flowing into a typed `Result<T, E>` binding (#1111)
- Unannotated lambda body with 3+ branches constructing `Err(Error(...))` now compiles without "all branches must have the same type" error (#1111)
