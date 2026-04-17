### Changed

- `str` values are now fully ARC-managed (#1046). Dynamic strings created by `+` concatenation, `repeat`, f-string interpolation, and runtime functions are automatically freed when their last reference goes out of scope, eliminating string leaks. `List<str>`, `Map<K, str>`, and `Set<str>` also release string payloads when the collection is freed.
