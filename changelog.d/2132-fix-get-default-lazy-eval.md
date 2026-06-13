### Fixed

- `get(list, index, default)` and `get(map, key, default)` now evaluate the `default` expression only when the index is out-of-bounds or the key is not found. Previously the default expression was always evaluated, so passing a function call as the default would invoke it (and run its side effects) even on the in-bounds / key-found path. (#2132)
