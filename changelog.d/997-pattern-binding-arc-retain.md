### Fixed

- ARC retain missing for fields extracted in pattern binding arms — `Some(xs)`, `Ok(xs)`, `Err(msg)`, record, enum-constructor, tuple, and variable patterns now correctly retain ARC-managed bindings, preventing use-after-free and refcount underflow under ASan (#997)
- `ListHeader` objects returned from runtime string-list builders (`makeStringList`, `makeMatchList`) are now allocated with `arc_alloc` so that Ry's ARC retain/release machinery can safely manage their lifetime (#997)
