### Fixed

- `Err(x)` with an unannotated lambda parameter no longer causes a branch-type mismatch when the enclosing function's Result Err slot is a primitive type (`int`, `float`, `bool`, `str`) (#1116)
