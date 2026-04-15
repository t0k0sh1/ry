### Added

- `Map<K, V>` `==` and `!=` now support complex key types: records, tuples, and nested collections (`Map<Point, int>`, `Map<(int, int), str>`, `Map<List<int>, str>`, etc.). Non-primitive keys use an O(n·m) structural linear-scan lookup; primitive keys continue using the existing hash-based path unchanged (#961)
