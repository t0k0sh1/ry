### Added

- Extended the `any` type to hold `enum` values — organic `enum`
  declarations (with or without payloads) plus the built-in
  `Option<T>` / `Result<T, E>` types. `RyAnyTag` gains `Enum=9`; the
  16-byte struct layout is preserved by heap-boxing the enum so
  `any.data[8]` holds a pointer to a box laid out as
  `[ ArcHeader (16B) ][ descriptor ptr (8B) ][ enum payload ]`. The
  payload is the enum's native representation (`i64` discriminant for
  simple enums; the ADT discriminated-union struct for ADT / `Option<T>`
  / `Result<T, E>`). Each enum type emits a singleton
  `__ry_enum_desc_<typename>` global carrying the destructor (which
  switches on the discriminant and releases the active variant's ARC
  fields), the equality function (variant-wise deep compare), and the
  type name — including the full generic parameterization, so
  `Option<int>` is distinct from `Option<str>` and `Result<List<int>,
  str>` is distinct from `Result<int, str>`. Even simple enums (no
  payload) flow through the new `Enum` tag rather than the prior
  `Int=0` shortcut, so the source-level enum identity survives the
  round-trip and `let c: Color = anyVal` only accepts an `any` that
  actually carries a `Color`. Wrap-in-`any` emits an ARC retain on the
  box (and field-wise retains for ARC fields in the active variant
  when the source is an existing enum alias); the enclosing variable's
  scope-end cleanup releases the box through a descriptor trampoline.
  `any == any` on two enum-holding values matches descriptor pointers
  first, then dispatches through the descriptor's equality function;
  enums of different types always compare unequal. `toStr` /
  f-string interpolation emits a `<TypeName>` marker (e.g. `<Color>`,
  `<Option<int>>`, `<Result<int, str>>`). Implicit unwrap is gated by
  descriptor-pointer equality, so only exact-type unwrap is permitted
  — enums do not participate in record-style subtype unwrap chains.
  Function-pointer and resource types (`TcpListener`, `TcpStream`,
  etc.) remain unsupported. (#1798)
