### Fixed

- `items()`, `enumerate()`, and `zip()` now correctly retain ARC-managed
  tuple components when constructing their `List<(K, V)>` results, and
  the collection destructor now releases inner ARC components for
  tuple-element lists. Previously both halves were missing simultaneously,
  so a rebind of the source container (e.g.
  `m: Map<str, List<int>> = {"a": [1,2,3]}; its = items(m); m = {"z": [99]}`)
  freed the inner `List<int>` while `its` still held the raw pointer,
  producing a use-after-free on the next read. The fix lands the retain
  and release sides symmetrically (parallel to #1242's whole-collection
  rebind fix). The same retain symmetry is also applied to `slice`,
  `take`, `appended`, and `concat` on tuple-element lists, since these
  inherit the new tuple-aware destructor via `propagateMeta`. (#1667)
