### Added

- `ry` and `ry test` can resolve a bare `*.ry` filename (e.g. `ry main.ry`) when the file is not in the current directory: the project root is tried first, then each `[paths]` directory in key order; the first match wins (#741).
