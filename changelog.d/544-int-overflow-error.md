### Changed

- `int` arithmetic (`+`, `-`, `*`, unary `-`) now raises a runtime error on overflow instead of silently wrapping (#544)
- Constant expressions that overflow are caught at compile time (#544)
