### Fixed

- `print` and `to_str` on `float` now use the shortest round-trip decimal representation (minimum digits to reconstruct the exact `double` value), matching Python 3, Rust, Go, and JavaScript. Imprecise arithmetic like `0.1 + 0.2` now prints as `"0.30000000000000004"` instead of `"0.3"`, accurately reflecting the stored value. Exact literals such as `3.14`, `3.0`, and `2.5` are unchanged (#1031)
