### Fixed

- ARC retain missing for fields extracted in pattern binding arms — `Some(xs)`, `Ok(xs)`, `Err(msg)`, record, enum-constructor, tuple, and variable patterns now correctly retain ARC-managed bindings, preventing use-after-free and refcount underflow under ASan (#997)
- `ListHeader` objects returned from runtime string-list builders (`makeStringList`, `makeMatchList`) are now allocated with `arc_alloc` so that Ry's ARC retain/release machinery can safely manage their lifetime (#997)
- `IOListHeader` objects returned from IO/network runtime functions (`receive`, `read_bytes`, `str_to_bytes`, TLS receive, HTTP body bytes) are now allocated with `arc_alloc`, fixing use-after-free when Ry's ARC retain/release accesses `header_ptr - 16` on pattern-bound byte-list values (#997)
