### Fixed

- Fix use-after-free when a function returns `Result` or `Option` wrapping a collection (List, Map, Set) parameter — the inner value is now retained before scope cleanup releases local variables (#999)
