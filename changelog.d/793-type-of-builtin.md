### Added

- `type_of(expr)` built-in function that returns a `Type` value representing the compile-time type identity of its argument. Supports `==` / `!=` for identity-based comparison and is printable via `print` / `to_str`. Covers primitives, low-level numeric types, collections (`List`, `Map`, `Set`), records, enums, `Option`, `Result`, functions/closures, `None`, and `Type` itself (reflective) (#793)
- `Type` primitive type representing the compile-time identity of a Ry type. Each distinct type definition receives a unique identity, so different records (or a record and an enum sharing a name) are always distinguishable by `==` (#793)
