### Added

- Extended the `any` type to hold user-defined `record` values.
  `RyAnyTag` gains `Record=8`; the 16-byte struct layout is preserved
  by heap-boxing the record so `any.data[8]` holds a pointer to a box
  laid out as `[ ArcHeader (16B) ][ descriptor ptr (8B) ][ record
  struct ]`. Each record type emits a singleton
  `__ry_record_desc_<typename>` global carrying the destructor,
  equality function, and type name, so the dynamic type survives
  erasure across function boundaries — release / equality / `toStr`
  all dispatch through the descriptor word inside the box rather than
  the (possibly stale) static type name at the call site. Wrap-in-`any`
  emits an ARC retain on the box (and field-wise retains for ARC fields
  when the source is an existing record alias), and the enclosing
  variable's scope-end cleanup releases the box through a descriptor
  trampoline. `any == any` on two record-holding values does field-wise
  deep equality when the descriptor pointers match (different record
  types always compare unequal); `toStr` emits a `<TypeName>` marker
  (e.g. `<Point>`) using the descriptor's type name. Implicit unwrap
  is gated by a descriptor-pointer-equality check against the expected
  type's descriptor global, so only **exact-type unwrap**
  (`let q: Point = anyVal` where `anyVal` holds a `Point`) is
  permitted; cross-type unwrap to a parent record traps at runtime and
  is tracked as a follow-up. The typed-path subtype coercion
  (`fn f(p: Parent): ...; f(child)`) is unchanged. Function-pointer
  and `enum` types remain unsupported. (#1797)
