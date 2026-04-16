### Fixed

- `is_empty` on strings now honours embedded NUL bytes instead of returning `true` for strings that begin with `\0`. The check now reads `byte_len` from the StringHeader (via `emitStringByteLen`) instead of comparing only the first byte (#1069).
