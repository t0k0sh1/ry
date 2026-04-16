### Fixed

- Fix use-after-free when a function returns `Result` or `Option` wrapping a collection (List, Map, Set) — covers direct parameters (`Ok(v)`) and record/tuple field access (`Ok(rec.field)`) — the inner value is now retained before scope cleanup releases local variables (#999)
