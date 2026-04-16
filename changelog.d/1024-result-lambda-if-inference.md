### Fixed

- Lambda return-type inference now correctly unifies `Ok(T)` and `Err(Error)` branches in an if-expression body, so unannotated lambdas like `(x: int) => if x > 10 => Ok(x * 2) else Err(Error("too small"))` compile without a spurious "all branches must have the same type" error (#1024)
