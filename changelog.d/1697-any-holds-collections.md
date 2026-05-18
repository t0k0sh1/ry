### Added

- Extended the `any` type to hold `List`, `Map`, and `Set` collections
  in addition to the existing primitive types. `RyAnyTag` gains
  `List=5`, `Map=6`, and `Set=7`; the 16-byte struct layout is
  preserved by storing the collection header pointer in `data[8]`.
  Wrap-in-`any` now emits an ARC retain on the collection, and the
  enclosing variable's scope-end cleanup emits a tag-dispatched
  release. Implicit unwrap (`let xs: List<int> = anyVal`) succeeds
  whenever the dynamic tag matches the target collection kind, trusting
  the static type annotation for element-type narrowing. `any == any`
  on two collection-holding values does best-effort deep equality
  (length + 8-byte-slot byte-equal data buffer) for `List` and pointer
  identity for `Map` / `Set`; `to_string` returns opaque markers
  (`<List>`, `<Map>`, `<Set>`) since element-type metadata is erased on
  wrap. Order comparisons and arithmetic on collection-holding `any`
  values continue to surface the existing "operator X not supported"
  runtime error. Record / enum / function-pointer / resource types
  remain unsupported and are tracked as follow-ups. (#1697)
