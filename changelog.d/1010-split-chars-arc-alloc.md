### Fixed

- `__ry_split_chars` (used by `split(str, "")`) now allocates its returned `ListHeader`
  with `arc_alloc` so that ARC retain/release in `emitVarDecl` reads a valid counter
  prefix. Previously the `checked_malloc` allocation placed malloc metadata at
  `header_ptr - 16`, which could be corrupted by retain and crash on scope-exit
  release with `pointer being freed was not allocated` on non-ASan macOS builds.
  Same bug class as #1007. (#1010)
