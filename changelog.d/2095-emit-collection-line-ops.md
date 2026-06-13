### Changed

- The collection linear-generation / linear-operation ops `listRemove` /
  `distinct` / `flatten` / list `reverse` / `items` / `enumerate` / `zip`
  now emit their LLVM IR from the Rust `crates/emit/` cdylib via the new
  `ry_emit_list_remove` / `_list_distinct` / `_list_flatten` /
  `_list_reverse` / `_map_items` / `_list_enumerate` / `_list_zip`
  boundary entry points. List header GEP positions (LEN/CAP/DATA) and
  Map header positions (LEN/CAP/KEYS/VALS/BUCKET_COUNT/BUCKETS) are
  centralized in `core::header_fields` as named constants and pinned by
  `tests/test_header_layout.cpp` against C++'s canonical struct so a
  constant-value drift fails the build. Pure refactor: the generated
  IR is byte-identical after ASLR normalization, no behavioral change.
  The per-element ARC retain callbacks for enumerate / zip / items
  cross the boundary via the new `RyRetainFn` trampoline + stack-struct
  pattern (mirroring `ry_emit_result_branch`'s re-entrant closure
  discipline from #2069). (#2095)
