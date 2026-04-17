### Changed

- Writing an octal literal (`0o...`) now produces a targeted compile error
  explaining that octal literals are not supported and suggesting `0x...`
  (hex) or `0b...` (binary) instead. Previously it produced the generic
  `invalid character after numeric literal` diagnostic. (#1027)
