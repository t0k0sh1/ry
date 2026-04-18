### Fixed

- Restored `HeaderFilterRegex` in `.clang-tidy` to `^include/ry/.*\.hpp$`, removing the unintentional `src/` inclusion added defensively in #950 (#1150)
