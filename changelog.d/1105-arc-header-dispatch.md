### Changed

- Unified ARC header offset dispatch for str: added `CapturedArcKind::Str` variant and `emitArcHeaderForAlloca` helper to prevent closure capture retain/release from using the wrong header offset (−16 instead of −24) for str values (#1105).

### Fixed

- Closure construction and destructor were corrupting `StringHeader.byte_len` when a str value was captured, by retaining/releasing at the wrong ARC header offset. Fixed by dispatching through `CapturedArcKind::Str` in `codegen_lambda.cpp` and `codegen_arc_cow.cpp` (#1105).
- Bare-expression str temporaries (e.g., `"foo".to_upper()` used as a statement) were leaked because `emitStmt(ExprStmt)` only checked `arc_owned_values_` and missed `arc_str_owned_values_` (#1105).
