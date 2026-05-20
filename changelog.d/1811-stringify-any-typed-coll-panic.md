### Fixed

- `json.stringify(any)` no longer reads out-of-bounds memory when `any`
  holds a typed (non-`any`-element) collection (`List<int>`,
  `Map<str, int>`, etc.). `wrapInAny` previously preserved the original
  collection header pointer untouched, so passing the resulting `any`
  to `stringify` walked the 8-byte-stride inner buffer as
  `RyAny[16]` — undefined behavior that could segfault. The runtime
  now records the source-level type name at `wrapInAny` time in a
  side-table (`std::unordered_map<void *, std::string>` guarded by
  `std::mutex`, with an `std::atomic<size_t>` fast-path counter and
  erase-if-present on `__ry_arc_free_counted`), and `stringify_any`'s
  List / Map arms look up the inner header pointer before walking the
  buffer. On hit the runtime exits deterministically with
  `stringify: any holds typed collection 'List<int>' — use List<any> /
  Map<str, any> / Set<any> instead` and `exit(1)`. ABI is unchanged;
  the happy path for `List<any>` / `Map<str, any>` pays nothing because
  the wrap arm only registers when at least one element type differs
  from `any`. (#1811)
