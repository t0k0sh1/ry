### Fixed

- Enum values returned from user functions now print as variant names
  (or `Variant(payload)` for ADT enums) instead of raw integers. Simple
  enums, ADT enums, and already-instantiated generic enums are all
  handled. Enum-typed elements stored in `List<Color>` literals also
  propagate correctly. (#820)
- `for i, x in enumerate(...)`, `for a, b in zip(...)`, and
  `for k, v in Map<K, V>` now preserve collection-element metadata on
  destructured variables, so `print` / `sum` / `length` work correctly
  when the elements are themselves `List` / `Map` / `Set` / enum. (#813)
