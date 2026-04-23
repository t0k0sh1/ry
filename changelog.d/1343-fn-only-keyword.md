### Removed

- The `function` keyword is removed; use `fn` for all function definitions and `async fn` for async definitions (#1343).

### Changed

- Function types are written `fn(T1, ...) -> R` only; `function(...)` is no longer accepted as a type or declaration keyword.
- `type_of` / `to_str` category for function-typed values is reported as `"fn"` (was `"function"`).
- Trace `symbol_define` entries use kind `"fn"` for user-defined functions (was `"function"`).
