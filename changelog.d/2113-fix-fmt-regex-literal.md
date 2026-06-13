### Fixed

- `ry fmt` now preserves regex literals (`/pattern/`) instead of replacing them with `/* unknown expr */`. The formatter's expression dispatch was missing a `RegexExpr` branch and silently fell through to the unknown-expression placeholder, so any source containing a regex literal formatted to a file that no longer ran. The fix emits the pattern verbatim (the lexer already preserves regex backslashes such as `\d` / `\w` / `\/` byte for byte) and reverses the lexer's only lossy translation by re-encoding embedded NUL bytes as `\0`. (#2113)
