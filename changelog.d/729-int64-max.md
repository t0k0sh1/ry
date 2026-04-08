### Fixed

- Parser no longer crashes on out-of-range integer literals such as `9223372036854775808` (INT64_MAX + 1); a clear compile error is reported instead (#729)
