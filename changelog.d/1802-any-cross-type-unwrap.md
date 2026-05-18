### Added

- Extended `any` record unwrap to admit subtype projection. Given
  `record Dog < Animal: ...`, `let a: Animal = anyHoldingDog` now
  succeeds and reads the Animal-prefix fields from the boxed `Dog`,
  rather than trapping as in v0.0.25. `RyRecordDescriptor` gains a
  fourth pointer `parent_desc` that links each record's descriptor to
  its parent's descriptor (or `null` for root records); the unwrap site
  walks this chain at runtime via a new
  `__ry_record_is_subtype_desc(actual, expected)` helper instead of
  doing a single descriptor-pointer equality check, so the actual
  dynamic type inside `any` is matched against the expected type's
  entire ancestor chain. Multi-level inheritance
  (`GuideDog < Dog < Animal`) and cross-function boundaries
  (`fn make() -> any: return Dog(...)` then `let a: Animal = make()`)
  both work because the descriptor stored in the box is the authoritative
  dynamic-type record. Parent-prefix ARC fields (e.g. `Animal.name: str`)
  are retained when projecting and released independently at scope end;
  Child-only fields keep being released through the box destructor, so
  no leak or double-free occurs. Unwrapping `any` to an unrelated record
  type (e.g. a `Point` held in `any` to an `Animal` slot) still traps at
  runtime. The typed-path subtype coercion
  (`fn f(p: Parent); f(child)`) is unchanged. (#1802)
