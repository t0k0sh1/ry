### Changed

- Aligned `include/ry/codegen.hpp` and
  `include/ry/module_loader.hpp` testing-intrinsic header comments
  with the current allow-list (`expect` / `mock` / `fail`). The
  pre-existing 6-name listings (`expect` / `mock` / `verify` /
  `fail` / `it` / `describe`) had drifted from the actual
  enforcement set after #721 (`it` / `describe` → general
  user-directive resolution) and #722 (`verify` → Ry function).
  Documentation-only change; no behavior or ABI impact. (#1674)
