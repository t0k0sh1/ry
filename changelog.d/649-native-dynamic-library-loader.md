### Added

- Dynamic library loading for `@native("libname")` declarations — the JIT now loads shared libraries at startup (#649)
- Stdlib runtime packages are built as shared libraries (`.dylib`/`.so`) in addition to the existing static linking (#649)
