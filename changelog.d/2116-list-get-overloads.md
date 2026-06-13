### Added

- `List<T>` now supports `get(list, index) -> Option<T>` and `get(list, index, default) -> T` overloads, symmetric to the existing `Map<K, V>` `get`. Semantics mirror `list[index]?`: negative indices wrap around, out-of-range (after wrap) returns `None` / the default. Both direct-call (`get(xs, i)`) and UFCS (`xs.get(i)`) forms are supported. (#2116)
