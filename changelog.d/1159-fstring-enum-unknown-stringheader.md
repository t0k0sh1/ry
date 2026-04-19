### Fixed

- Fix f-string interpolation of enums with explicit discriminant values (`enum E { A = 5 }`) no longer misreads `byte_len` via a non-StringHeader pointer, which could truncate output or trigger UB on the unreachable default branch (#1159)
