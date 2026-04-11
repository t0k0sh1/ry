### Fixed

- Chained writes through nested collections (`a[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`) no longer leak through aliases. Path copy-on-write walks the LHS from root to leaf and clones every level whose reference count is greater than one before the mutation (#854)
- Record-to-record assignment (`r2 = r1`) now retains ARC-managed fields (`List<T>`, `Map<K, V>`, `Set<T>`) so both aliases share ownership of the inner containers. A subsequent mutation through one alias is isolated from the other by path copy-on-write (#854)
