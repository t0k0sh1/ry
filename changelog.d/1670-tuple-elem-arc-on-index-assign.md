### Fixed

- `xs[i] = (a, b)` slot overwrite on `List<(K, V)>` now retains the
  ARC-managed components of the new tuple and releases the components
  of the evicted tuple. Previously the IndexAssignStmt path was the
  remaining symmetry gap from #1667: the destructor recursed into
  inner tuple components, but slot overwrite did neither retain nor
  release, leaking the evicted tuple's inner ARC values on every
  reassignment. The fix mirrors #1667's per-component dispatch by
  source-level type name (str at offset −24, List/Map/Set at −16,
  nested tuples recurse), gates on a non-empty `list_elem_type_name`
  with tuple shape `"(...)"` (preserving pre-fix behavior for
  literal-built lists whose tuple sig is empty — same blind spot as
  `List<str>` literals), and orders retain-before-release so
  self-assignment `e[i] = e[i]` is safe. (#1670)
