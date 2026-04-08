### Added

- `ry` and `ry test` can resolve a bare `*.ry` filename (e.g. `ry main.ry`) when the file is not in the current directory: the project root is tried first, then each `[paths]` directory in key order; the first match wins (#741).

### Fixed

- Missing explicit paths to `*.ry` files (e.g. `ry src/missing.ry`) now report **no such file** instead of unknown command (#741).
- `package.toml` `[paths]` entries (other than `src`) round-trip through `serialize`/`load` (#741).
