### Fixed

- Option branch-type merge in unannotated lambda if-expressions now prefers concrete types over `anyTy_` placeholders, matching the Result merge logic. Also propagates the `anyTy_` unwrap pattern from `Ok` to `Some` so concrete-vs-any branches produce matching `Option<T>` structs (#1115).
