### Changed

- Indexing a `str` value with `[]` now emits a clear diagnostic pointing to `char_at(s, i)`, instead of the misleading "cannot determine list element type" message (#1026)
