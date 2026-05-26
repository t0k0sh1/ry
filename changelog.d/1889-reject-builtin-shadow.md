### Fixed

- Top-level user `fn` declarations that collide with stdlib built-in function names (e.g. `sum`, `min`, `max`, `len`, `range`, `print`, `enumerate`, `zip`, `map`, `filter`, `Ok`, `Err`, `Some`, `None`) are now rejected at compile time with a clear diagnostic instead of silently being shadowed by the built-in. Generic-fn templates and `from <module> import <name> as <reserved>` aliases are checked the same way. Nested `fn`s, `@native` declarations, qualified-import module members, and type-aware overrides like `fn toStr(p: MyRecord)` remain accepted. (#1889)
