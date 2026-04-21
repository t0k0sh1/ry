### Fixed

- Whole-list reassignment (`xs = [...]`) now releases ARC-managed inner elements, preventing the ~3 ARC headers per iteration leak observed when rebinding `List<List<T>>`, `List<Map<K,V>>`, `List<Set<T>>`, `Map<K, List<V>>`, etc. inside a loop. Applies to List/Map/Set element types; str elements remain on the existing path. (#1242)
- `appended(list, elem)`, `insert(list, i, elem)`, and `merge(map1, map2)` now retain ARC-managed collection elements they duplicate from source containers, matching the retain-on-store discipline already used by `slice` / `take`. Without these retains, the destructor fix above would have introduced UAFs when a source container was rebound or went out of scope. (#1242)
