### Changed

- `bool` operands are now rejected at compile time for arithmetic operators
  (`+`, `-`, `*`, `/`, `//`, `%`, `**`, unary `-`) and bitwise operators
  (`&`, `|`, `^`, `<<`, `>>`, unary `~`). Previously, `bool` was silently promoted
  to `int`. Use `bool as int` for explicit conversion. This also aligns the bitwise
  implementation with the documentation (#1030).
