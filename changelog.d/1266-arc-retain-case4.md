### Fixed

- ARC retain now fires for container element loads (`xs = ys[i]`,
  `v = m["k"]`, function return, call-site argument passing) for nested
  ARC containers and `List<str>` / `Map<K,str>` borrows. Previously
  missed in `AssignStmt`, `return`, caller-side argument passing, match
  binding, type coercion, and lambda capture — every caller of
  `tryRetainArcSource`. Prerequisite for the `#1242` destructor fix that
  makes nested collection headers reclaimable. (#1266)
