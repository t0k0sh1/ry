### Fixed

- `-9223372036854775808` (INT64_MIN) is now accepted as a bare integer
  literal. Previously it required the `i64` suffix or a workaround
  such as `-9223372036854775807 - 1`. A standalone
  `9223372036854775808` (without the unary minus) remains rejected,
  and `-9223372036854775809` is rejected at compile time (#1025).
