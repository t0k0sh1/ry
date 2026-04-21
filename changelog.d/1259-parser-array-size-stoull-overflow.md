### Fixed

- Parser no longer aborts on overflow or non-decimal integers in array type
  `T[N]`. `parseTypeNameSingle` now uses `strtoull` + `errno` instead of
  `std::stoull`, so inputs such as `T[99999999999999999999...]`, `T[0xFF]`,
  or `T[1_000]` are rejected with a structured diagnostic instead of crashing
  via uncaught `std::out_of_range` / `std::invalid_argument`. Discovered by
  `fuzz_parser`. (#1259)
