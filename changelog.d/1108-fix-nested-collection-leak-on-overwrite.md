### Fixed

- Fixed memory leak when overwriting a slot in `List<List<str>>`, `Map<K, List<str>>`, or a record field of a nested collection type containing `str` elements. The overwritten inner collection's `str` handles are now released correctly (#1108).
