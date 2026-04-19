### Fixed

- Generic type constraint checks (`<T: RecordName>`) no longer reject
  type aliases that resolve to a record type. Both the bound and the
  concrete type argument are now resolved through the alias table
  before the subtype check, while error messages continue to report
  the user-written names. (#1155)
